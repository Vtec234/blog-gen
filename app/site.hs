{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (msum, (>=>))
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe, listToMaybe)

import qualified Data.Time.Format as TFmt
import qualified Data.Time.Clock as TClk
import qualified System.FilePath as Path
import qualified System.Directory as Dir
import qualified Data.Text as Txt

import qualified Text.Blaze.Html.Renderer.String as Html (renderHtml)

import           Hakyll hiding (pandocCompiler)
import           Hakyll.Images hiding (lookup)
import           Hakyll.Core.Compiler.Internal (compilerTellDependencies)

import           ParseIpynb (ipynbTitle, ipynbToHtml)
import           Utils

import qualified Data.ByteString.Lazy.UTF8 as BLU

--------------------------------------------------------------------------------

-- | Interprets the 'blah' in '$md("blah")$' as Markdown and renders it to HTML.
markdownCtx :: Context String
markdownCtx = functionField "md"
  (\args _ -> case args of
    arg : _ -> either (fail . show) (pure . Html.renderHtml) $
        pdocMarkdownToHtml $ Txt.pack arg
    _       -> fail "No argument given to $md(needsArg)$.") <>
  defaultContext

myCtx :: Context String
myCtx =
  constField "script" "/js/default.js" <>
  constField "siteLang" "en" <>
  constField "siteLocale" "en_US" <>
  constField "siteUrl" "https://voidma.in/" <>
  constField "siteAuthor" "Wojciech Nawrocki" <>
  constField "siteTitle" siteTitle <>
  constField "siteDescription" siteTitle <>
  markdownCtx
  where siteTitle = "void main"

-- | Context for posts.
postCtx :: Context String
postCtx =
  constField "script" "/js/post.js" <>
  dateField "date" "%B %e, %Y" <>
  dateField "w3cDate" "%Y-%m-%d" <>
  myCtx

-- | Context for Jupyter notebooks.
nbCtx :: Context String
nbCtx = constField "script" "/js/jupyter.js" <>
        field "title" (\it -> loadSnapshotBody (itemIdentifier it) "lbs" >>= ipynbTitle) <>
           -- The title retrieval is a bit screwed up.
           -- The "item" here is the _output_ of the compiler for this
           -- notebook, which means either HTML or in the CopyFile case
           -- an item whose body is just the filepath. Moreover getResourceLBS
           -- returns the contents of the _current template_ e.g. pages/index.html,
           -- so is also useless.
        postCtx

-- | A context to use in general pages like about/index.
--   Contains lists of everything on the site.
siteNavCtx :: Context String
siteNavCtx = listField "posts" postCtx posts <>
             listField "notebooks" nbCtx notebooks <>
             listField "galleries" myCtx galleries <>
             listField "pages" myCtx (do
               a <- posts
               d <- notebooks
               b <- galleries
               c <- pages
               pure $ a ++ d ++ b ++ c) <>
             myCtx
  where posts = loadAll "posts/**.md" >>= recentFirst
        notebooks = loadAll (fromGlob "posts/jupyter_notebooks/*.ipynb" .&&. hasVersion "html") >>= recentFirst
        galleries = loadAll "photos/*.gallery"
        pages = loadAll $ fromList ["pages/index.html", "pages/about.md", "pages/photos.html"]

-- | Drops n leading directories from the path.
dropDirsRoute :: Int -> Routes
dropDirsRoute n = customRoute $ Path.joinPath . drop n . Path.splitPath . toFilePath

-- | Extracts a date out of the identifier, if one is present.
parseDate :: Identifier -> TFmt.TimeLocale -> Maybe TClk.UTCTime
parseDate ident locale = msum
  [parseTime "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" cand | cand <- reverse paths]
  where parseTime = TFmt.parseTimeM True locale
        paths = Path.splitDirectories $ Path.dropExtension $ toFilePath ident

-- | Extracts a date out of the metadata, if one exists.
lookupDate :: Metadata -> TFmt.TimeLocale ->  Maybe TClk.UTCTime
lookupDate meta locale = msum $ [tryField "published" fmt | fmt <- formats] ++
                                [tryField "date"      fmt | fmt <- formats]
  where parseTime = TFmt.parseTimeM True locale
        tryField k fmt = lookupString k meta >>= parseTime fmt
        formats =
          [ "%a, %d %b %Y %H:%M:%S %Z"
          , "%Y-%m-%dT%H:%M:%S%Z"
          , "%Y-%m-%d %H:%M:%S%Z"
          , "%Y-%m-%d"
          , "%B %e, %Y %l:%M %p"
          , "%B %e, %Y"
          , "%b %d, %Y"
          ]

jekyllDateRouteAux :: String -> Metadata -> Identifier -> FilePath
jekyllDateRouteAux newExt meta ident =
    Path.joinPath $ init dirs ++ [ fmt "%Y", fmt "%m", fmt "%d", fileName ]
    where locale = TFmt.defaultTimeLocale
          idate = parseDate ident locale
          mdate = lookupDate meta locale
          date :: TClk.UTCTime
          date = fromMaybe (error $ "No date present for post " ++ toFilePath ident ++ ".") (msum [idate, mdate])
          dirs = Path.splitPath $ toFilePath ident
          fileName = maybe id (\_ -> drop 11) idate (Path.replaceExtension (last dirs) newExt)
          fmt f = TFmt.formatTime locale f date

-- | Emulates Jekyll's behaviour in routing posts to subdirectories
--   based on the date and replacing the extension with 'newExt',
--   e.g. blah/2002-02-02-post.md -> blah/2002/02/02/post.html.
jekyllDateRoute :: String -> Routes
jekyllDateRoute newExt = metadataRoute (customRoute . jekyllDateRouteAux newExt)

pandocCompiler :: Compiler (Item String)
pandocCompiler =
  pandocCompilerWithTransform pdocReaderOptions pdocWriterOptions pdocTransformToHtml

main :: IO ()
main = hakyll $ do
  -- Copy over assets.
  match ("assets/**"
    .||. "fonts/**"
    .||. "mathjax/**") $ do
    route idRoute
    compile copyFileCompiler

  -- Minify JS and process includes.
  match (fromRegex "^(\\./)?js/[^.]+.js$") $ do
    route idRoute
    compile $ do
      -- In their metadata, JS files can point at another file to be included verbatim.
      maybeInclude <- getUnderlying >>= \id -> getMetadataField id "include"
      includeContents :: String <-
        case maybeInclude of
          Just scriptName -> loadBody $ fromFilePath scriptName
          Nothing         -> pure ""
      getResourceBody
        >>= withItemBody (pure . (includeContents ++))
      -- FIXME: replace google closure with something more JSy that works on arm64
      --getResourceBody
      --  >>= withItemBody (unixFilter "npx" ["--", "google-closure-compiler"] . (include ++))

  -- Complete (non-underscored) .scss files depend on partial (underscored)
  -- .css and .scss files. We "compile" those by doing nothing.
  let partialCss = fromRegex "^(\\./)?css/_.+\\.(s)?css$"
  match partialCss $ compile $ makeItem ("" :: String)
  partialCssDep <- makePatternDependency partialCss

  rulesExtraDependencies [partialCssDep] $
    -- Matches any non-partial SCSS file
    match (fromRegex "^(\\./)?css/[^_]+.*\\.scss$") $ do
      route $ setExtension "css"
      compile $ do
        p <- getResourceFilePath
        let opts =
              [ "--"
              , "postcss-cli"
              , p
              , "--parser"
              , "postcss-scss"
              , "-u"
              , "postcss-import" -- Support `@import`
              , "postcss-nested" -- Support nesting selectors
              , "postcss-custom-media" -- Support `@custom-media`
              -- , "postcss-preset-env" -- Doesn't seem to do anything useful
              , "postcss-extend-rule" -- Support `@extend`
              , "autoprefixer" -- Autogenerate prefixes for browser support
              , "cssnano" -- Minify
              ]
        unixFilter "npx" opts "" >>= makeItem

  -- Compile templates.
  match "templates/*" $ compile templateBodyCompiler

  -- Copy photos and create thumbnails.
  match "photos/**.jpg" $ do
    version "copy" $ do
      route idRoute
      compile copyFileCompiler

    version "thumbnail" $ do
      route $ customRoute $
        (\s -> Path.replaceDirectory s $ Path.takeDirectory s Path.</> "thumbs/")
        . toFilePath
      -- Compiles a picture into a thumbnail to fit into a preview box.
      compile $ loadImage >>= ensureFitCompiler 500 500

  -- Build gallery pages.
  match "photos/*.gallery" $ do
    route $ customRoute $
      (\p -> Path.replaceFileName p $ Path.takeBaseName p Path.</> "index.html")
      . toFilePath
    compile $ do
      galleryName <- Path.takeBaseName <$> getResourceFilePath
      let pat = fromGlob ("photos" Path.</> galleryName Path.</> "*.jpg")
                .&&. hasVersion "copy"
      -- Add a list `images` with a `path` field on each to the context.
      let galleryCtx = listField "images"
                         (field "path" $ return .
                           Path.takeFileName . toFilePath . itemIdentifier)
                         (loadAll pat :: Compiler [Item CopyFile])
                       <> constField "script" "/js/gallery.js"
                       <> myCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/gallery_page.html" galleryCtx
        >>= loadAndApplyTemplate "templates/default.html" galleryCtx
        >>= relativizeUrls

  match "posts/**.md" $ do
    route $ jekyllDateRoute "html"
    compile $ cached "pandoc" pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  -- TODO just match by ipynb in posts/, no jupyter_notebooks dir
  match "posts/jupyter_notebooks/*.ipynb" $ do
    version "raw" $ do
      route idRoute
      compile copyFileCompiler

    version "html" $ do
      route $ jekyllDateRoute "html"
      compile $ getResourceLBS
        >>= saveSnapshot "lbs"
        >>= withItemBody ipynbToHtml
        >>= cached "pandoc" . withItemBody (pure . Html.renderHtml)
        >>= loadAndApplyTemplate "templates/jupyter.html" nbCtx
        >>= loadAndApplyTemplate "templates/post.html" nbCtx
        >>= loadAndApplyTemplate "templates/default.html" nbCtx
        >>= relativizeUrls

  match "drafts/**.md" $ do
    route $ jekyllDateRoute "html"
    compile $ cached "pandoc" pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  -- Compile dynamic pages into the webroot.
  match "dynamic/*.php" $ do
    route $ dropDirsRoute 1
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html" myCtx

  let pageCompiler = getResourceBody
                       >>= applyAsTemplate siteNavCtx
                       >>= loadAndApplyTemplate "templates/default.html" siteNavCtx
                       >>= relativizeUrls
  match "pages/index.html" $ do
    route $ constRoute "index.html"
    compile pageCompiler
  match "pages/photos.html" $ do
    route $ constRoute "photos/index.html"
    compile pageCompiler

  -- Map out the pages for daddy google.
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" siteNavCtx
