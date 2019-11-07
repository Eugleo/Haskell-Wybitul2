{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PrettyPrinter where

import Data.Foldable
import Data.List        (intersperse)
import Prelude          hiding ((<>))
import Spec
import Text.PrettyPrint hiding (TextDetails (..))

indentLevel :: Int
indentLevel = 4

lineLen :: Int
lineLen = 88

class PrettyPrint a where
  pretty :: a -> Doc

instance PrettyPrint Symbol where
  pretty = text

instance PrettyPrint Expression where
  pretty (Var name)          = text name
  pretty (Num number)        = int number
  pretty (Str string)        = doubleQuotes (text string)
  pretty (e1 :+: e2)         = hsep [pretty e1, text "+", pretty e2]
  pretty (e1 :-: e2)         = hsep [pretty e1, text "-", pretty e2]
  pretty (e1 :/: e2)         = hsep [pretty e1, text "/", pretty e2]
  pretty (e1 :*: e2)         = hsep [pretty e1, text "*", pretty e2]
  pretty (e1 :<: e2)         = hsep [pretty e1, text "<", pretty e2]
  pretty (e1 :>: e2)         = hsep [pretty e1, text ">", pretty e2]
  pretty (FunCall name args) = text name <> parens (list args)
  pretty (Par exp)           = parens (pretty exp)

instance PrettyPrint Construct where
  pretty (FunDef name args body) = block header body <+> newline
    where
      header = text ("def " ++ name) <> parens (list args)
  pretty (Stmt exp) = pretty exp <> semi
  pretty (Assign name exp) = hsep [text name, text "=", pretty exp] <> semi
  pretty (If exp body []) = block (text "if" <+> pretty exp) body
  pretty (If exp ifBody elseBody) = ifBlock $+$ elseBlock
    where
      ifBlock = block (text "if" <+> pretty exp) ifBody
      elseBlock = block (text "else") elseBody
  pretty (While exp body) = block (text "while" <+> pretty exp) body

prettyConstruct :: (Construct, Int) -> Doc
prettyConstruct (x@FunDef {}, i)
  | i /= 1 = newline <+> pretty x
  | otherwise = pretty x
prettyConstruct (x, _) = pretty x

instance PrettyPrint Program where
  pretty (Program constructs) = vcat $ prettyConstruct <$> zip constructs [1 ..]

nested :: PrettyPrint a => [a] -> Doc
nested = nest indentLevel . vcat . fmap pretty

list :: PrettyPrint a => [a] -> Doc
list = hcat . intersperse (text ", ") . fmap pretty

block :: PrettyPrint a => Doc -> [a] -> Doc
block head body = head <> text ":" <+> lbrace $+$ nested body $+$ rbrace

newline :: Doc
newline = text "\n"

ppshow :: PrettyPrint a => a -> String
ppshow = renderStyle (style {lineLength = lineLen}) . pretty
