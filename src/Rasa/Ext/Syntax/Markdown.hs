{-# language NamedFieldPuns #-}
{-# language LambdaCase #-}
module Rasa.Ext.Syntax.Markdown
  ( lexer
  , styler
  , markdown
  ) where

import Rasa.Ext
import Rasa.Ext.Syntax

import qualified Yi.Rope as Y

import CMarkGFM

import Data.Default
import Data.Typeable

data MarkdownToken =
  MdTitle | MdBold | MdEmph | MdQuote | MdList | MdOther
  deriving (Typeable, Show)

instance Default MarkdownToken where
  def = MdOther

lexer :: Lexer MarkdownToken
lexer txt = lexNodes $ commonmarkToNode [] [] (Y.toText txt)

lexNodes :: Node -> Tokens MarkdownToken
lexNodes = \case
  (Node (Nothing) _ _)                 -> Tokens []  -- Ignore nodes without positions
  (Node (Just _) (TEXT _) _)           -> Tokens []  -- Ignore leaf nodes
  (Node (Just _) DOCUMENT nodes)       ->     -- Skip the root node
    Tokens $ concat $ ( (unToken . lexNodes) <$> nodes )
  (Node (Just posinfo) nodeType nodes) ->
    Tokens $ ( TokenRange (posToRange posinfo, nodeTypeToToken nodeType))
           : ( concat $ ((unToken . lexNodes) <$> nodes ) )

posToRange :: PosInfo -> CrdRange
posToRange PosInfo{startLine,startColumn,endLine,endColumn} =
  Range
    (Coord (startLine - 1) (startColumn - 1))
    (Coord (endLine - 1) endColumn)

nodeTypeToToken :: NodeType -> MarkdownToken
nodeTypeToToken = \case
  (HEADING _)   -> MdTitle
  (LIST _)      -> MdList
  EMPH          -> MdEmph
  STRONG        -> MdBold
  BLOCK_QUOTE   -> MdQuote
  _             -> MdOther

styler :: Styler MarkdownToken
styler = return . \case
  (TokenRange (r, MdTitle)) -> Span r ((flair Bold) <> (flair Underline))
  (TokenRange (r, MdList))  -> Span r (fg Blue)
  (TokenRange (r, MdBold))  -> Span r (flair Bold)
  (TokenRange (r, MdEmph))  -> Span r (flair Italic)
  (TokenRange (r, MdQuote)) -> Span r (fg Green)
  _                         -> Span (Range (Coord 0 0) (Coord 0 0)) mempty

markdown :: Mapper -> Syntax MarkdownToken
markdown mapper = Syntax "Markdown" mapper lexer styler
