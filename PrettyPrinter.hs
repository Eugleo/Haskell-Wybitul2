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

class PDoc a where
  pdoc :: a -> Doc

instance PDoc Symbol where
  pdoc = text

instance PDoc Expression where
  pdoc (Var name)          = text name
  pdoc (Num number)        = int number
  pdoc (Str string)        = doubleQuotes (text string)
  pdoc (e1 :+: e2)         = hsep [pdoc e1, text "+", pdoc e2]
  pdoc (e1 :-: e2)         = hsep [pdoc e1, text "-", pdoc e2]
  pdoc (e1 :/: e2)         = hsep [pdoc e1, text "/", pdoc e2]
  pdoc (e1 :*: e2)         = hsep [pdoc e1, text "*", pdoc e2]
  pdoc (e1 :<: e2)         = hsep [pdoc e1, text "<", pdoc e2]
  pdoc (e1 :>: e2)         = hsep [pdoc e1, text ">", pdoc e2]
  pdoc (FunCall name args) = text name <> parens (list args)
  pdoc (Par exp)           = parens (pdoc exp)

instance PDoc Construct where
  pdoc (FunDef name args body) = hangover header body <+> newline
    where
      header = text ("def " ++ name) <> parens (list args) <> text ":"
  pdoc (Stmt exp) = pdoc exp
  pdoc (Assign name exp) = text name <+> text "=" <+> pdoc exp
  pdoc (If exp body []) = hangover (text "if" <+> pdoc exp <> text ":") body
  pdoc (If exp ifBody elseBody) = ifBlock $+$ elseBlock
    where
      ifBlock = hangover (text "if" <+> pdoc exp <> text ":") ifBody
      elseBlock = hangover (text "else:") elseBody
  pdoc (While exp body) = hangover (text "while" <+> pdoc exp <> text ":") body

instance PDoc Program where
  pdoc (Program constructs) = vcat $ pdoc <$> constructs

nested :: PDoc a => [a] -> Doc
nested = nest indentLevel . vcat . fmap pdoc

list :: PDoc a => [a] -> Doc
list = hcat . intersperse (text ", ") . fmap pdoc

block :: PDoc a => [a] -> Doc
block body = lbrace $+$ nested body $+$ rbrace

hangover :: PDoc a => Doc -> [a] -> Doc
hangover head body = head <+> lbrace $+$ nested body $+$ rbrace

newline :: Doc
newline = text "\n"

ppshow :: PDoc a => a -> String
ppshow = renderStyle (style {lineLength = lineLen}) . pdoc
