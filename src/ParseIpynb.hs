{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParseIpynb where

import           Data.Maybe (fromMaybe, listToMaybe)

import           Data.String (IsString)

import qualified Data.ByteString.Lazy as B

import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as JsonMap
import qualified Data.Aeson.Key as JsonKey

import qualified Data.Ipynb as Ipy

import qualified Data.Text as Txt

import qualified Text.Blaze.Html.Renderer.String as Html (renderHtml)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HAtr

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap

import           Utils

import           Hakyll (Compiler)

-- | MIME type for Jupyter widget state.
stateMime :: Txt.Text
stateMime = "application/vnd.jupyter.widget-state+json"

-- | MIME type for Jupyter widget view.
viewMime :: Txt.Text
viewMime = "application/vnd.jupyter.widget-view+json"

-- | Compiles the title of the notebook if it has one,
--   otherwise "".
ipynbTitle :: B.ByteString -> Compiler String
ipynbTitle bs = do
  Ipy.Notebook { notebookMetadata = Ipy.JSONMeta meta } :: Ipy.Notebook Ipy.NbV4 <-
    either fail pure $ Json.eitherDecode bs
  case Map.lookup "title" meta of
    Just (Json.String title) -> pure $ Txt.unpack title
    _                        -> pure ""

-- | Transforms a Jupyter notebook into HTML with all the right attributes.
--   Also returns a context including the parsed notebook metadata.
ipynbToHtml :: B.ByteString -> Compiler Html.Html
ipynbToHtml bs = do
  Ipy.Notebook { notebookMetadata = Ipy.JSONMeta meta, .. } :: Ipy.Notebook Ipy.NbV4 <-
    either fail pure $ Json.eitherDecode bs

  -- Retrieve the kernel language.
  spec <- case Map.lookup "kernelspec" meta of
    Just (Json.Object spec) -> pure spec
    _ -> fail "No kernelspec in .ipynb."
  nbLang <- case JsonMap.lookup "language" spec of
    Just (Json.String lang) -> pure $ Txt.unpack lang
    _ -> fail "No kernel language specified in .ipynb kernelspec."

  -- Format the embedded widget state if there is any.
  let widgetStateHtml = do -- Maybe monad
        Json.Object wdgts <- Map.lookup "widgets" meta
        state <- JsonMap.lookup (JsonKey.fromText stateMime) wdgts
        return $ Html.script Html.! HAtr.type_ (Html.toValue stateMime) $
                 Html.unsafeLazyByteString $ Json.encode state

  cellHtmls <- mapM (cellToHtml nbLang) notebookCells
  pure $ do -- HTML monad
    mconcat cellHtmls
    fromMaybe emptyHtml widgetStateHtml

cellToHtml :: String -> Ipy.Cell a -> Compiler Html.Html
cellToHtml lang Ipy.Cell { cellType = Ipy.Markdown, .. } =
  either (fail . show) (pure . mdDivWrap) $
    pdocMarkdownToHtml src
  where mdDivWrap = Html.div Html.! HAtr.class_ "ipynb-md"
        src = mconcat $ Ipy.unSource cellSource

cellToHtml lang Ipy.Cell { cellType = Ipy.Code { .. }, .. } = do
  outputHtmls <- mapM codeOutputToHtml codeOutputs
  pure $ do -- MarkupM monad concats HTML lines
    Html.pre
      Html.! HAtr.class_ "ipynb-code" $
      srcHtml
    mconcat outputHtmls
  where src = mconcat $ Ipy.unSource cellSource
        srcHtml = pygmentizeToHtml lang $ Txt.unpack src

cellToHtml _ cell = fail $ "Unhandled cell " ++ show cell ++ " in .ipynb."

codeOutputToHtml :: Ipy.Output a -> Compiler Html.Html
codeOutputToHtml Ipy.Stream { streamName = "stdout", .. } =
  pure $ Html.pre
    Html.! HAtr.class_ "ipynb-stdout" $
    Html.toHtml src
  where src = mconcat $ Ipy.unSource streamText

codeOutputToHtml Ipy.DisplayData { .. } =
  case Map.lookup viewMime meta of
    Just (Ipy.JsonData j) ->
      pure $ Html.div
        Html.! HAtr.class_ "ipynb-widget" $
        Html.script
          Html.! HAtr.type_ (Html.toValue viewMime) $
          Html.unsafeLazyByteString $ Json.encode j
    _ -> fail "Don't know how to transform non-widget ipynb DisplayData."
  where meta = Ipy.unMimeBundle displayData

codeOutputToHtml Ipy.ExecuteResult { .. } = pure $ Html.pre "execution result."

codeOutputToHtml Ipy.Err { .. } = pure $ Html.pre "error."

codeOutputToHtml _ = fail "Unhandled code output type in .ipynb."
