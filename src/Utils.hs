{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Data.List (dropWhileEnd)
import           Data.Char (isSpace)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Control.Monad (msum, (>=>))

import           System.Process (readProcess)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as Txt
import qualified Data.Text.Lazy as Txt (toStrict)

import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HAtr
import qualified Text.Blaze.Html.Renderer.Text as Html (renderHtml)

import qualified Text.Pandoc as PDoc
import qualified Text.Pandoc.Walk as PDoc (walk)

-- | @pygmentize lang src@ performs syntax highlighting on the source snippet @src@
--   written in language @lang@ and returns the HTML. If @lang == "text"@, no
--   highlighting is done.
pygmentize :: String -> String -> Html.Html
pygmentize "text" src = Html.toHtml src
pygmentize lang src =
  Html.preEscapedToHtml $
    -- NOTE whitespace is dropped from the end, since pygmentize outputs an extra newline
    dropWhileEnd isSpace $
    unsafePerformIO $
    -- TODO slow
    readProcess "pygmentize" ["-f", "html", "-O", "nowrap=True", "-l", lang] src

-- | Calls 'pygmentize' and wraps the result in
--   @\<code class="highlight language-$lang">@.
pygmentizeToHtml :: String -> String -> Html.Html
pygmentizeToHtml lang src =
  Html.code
    Html.! HAtr.class_ (Html.toValue $ "highlight language-" ++ lang) $
    pygmentize lang src

-- | An empty HTML tree.
emptyHtml :: Html.Html
emptyHtml = mempty -- #justhaskellthings

-- | Adds HTML-based syntax highlighting markup to a Pandoc code block. 
highlightCodeBlock :: PDoc.Block -> PDoc.Block
highlightCodeBlock (PDoc.CodeBlock (_, classes, keyvals) contents) =
  PDoc.RawBlock "html" $ Txt.toStrict $ Html.renderHtml composed

  where lang = fromMaybe ("text" :: String) $
                 msum [Txt.unpack <$> lookup "lang" keyvals,
                       Txt.unpack <$> listToMaybe classes]
        strContents = Txt.unpack contents
        colored = Html.pre $ pygmentizeToHtml lang strContents
        caption = maybe emptyHtml
                    (Html.figcaption . Html.span . Html.toHtml)
                    (lookup "text" keyvals)
        composed = Html.figure Html.! HAtr.class_ "codeblock" $
                     caption <> colored
highlightCodeBlock x = x

-- | Adds HTML-based syntax highlighting markup to a Pandoc code inline. 
highlightCodeInline :: PDoc.Inline -> PDoc.Inline
highlightCodeInline (PDoc.Code (_, classes, keyvals) contents) =
  PDoc.RawInline "html" $ Txt.toStrict $ Html.renderHtml composed

  where lang = fromMaybe ("text" :: String) $
                 msum [Txt.unpack <$> lookup "lang" keyvals,
                       Txt.unpack <$> listToMaybe classes]
        strContents = Txt.unpack contents
        composed = pygmentizeToHtml lang strContents
highlightCodeInline x = x

pdocReaderOptions :: PDoc.ReaderOptions
pdocReaderOptions = PDoc.def
  { PDoc.readerExtensions = PDoc.pandocExtensions <> PDoc.extensionsFromList exts }
  where exts =
          [ PDoc.Ext_tex_math_dollars
          , PDoc.Ext_emoji ]

pdocWriterOptions :: PDoc.WriterOptions
pdocWriterOptions = PDoc.def
  { PDoc.writerHTMLMathMethod = PDoc.MathJax ""
  , PDoc.writerHighlightStyle = Nothing }

-- | Nicely highlights code blocks in a Pandoc, embedding them as raw HTML in the output.
pdocTransformToHtml :: PDoc.Pandoc -> PDoc.Pandoc
pdocTransformToHtml = PDoc.walk highlightCodeBlock . PDoc.walk highlightCodeInline

-- | Parses the given text as Markdown, highlighting code blocks.
pdocParseMarkdown :: Txt.Text -> Either PDoc.PandocError PDoc.Pandoc
pdocParseMarkdown =
  fmap pdocTransformToHtml . PDoc.runPure . PDoc.readMarkdown pdocReaderOptions

-- | Transforms the givent text interpreted as Markdown into HTML
--   with highlighted code block.
pdocMarkdownToHtml :: Txt.Text -> Either PDoc.PandocError Html.Html
pdocMarkdownToHtml =
  pdocParseMarkdown >=> PDoc.runPure . PDoc.writeHtml5 pdocWriterOptions
