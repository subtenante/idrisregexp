module RegExpParser

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import RegExp
import SmartCons

%access public export

pChar : Parser RegExp
pChar = (Chr . toNat)  <$> noneOf "[]()*+?|"

pAtom : Parser RegExp
pAtom = foldl1 (.@.) <$> some pChar

pstar : Parser (RegExp -> RegExp)
pstar = const star <$> lexeme (char '*')

pPlus : Parser (RegExp -> RegExp)
pPlus = const (\e => Cat e (star e)) <$> lexeme (char '+')

pOpt : Parser (RegExp -> RegExp)
pOpt = const (\e => Alt Eps e) <$> lexeme (char '?')

pInBracketsChar : Parser RegExp
pInBracketsChar = (Chr . toNat) <$> noneOf "[]^"

pBrackets : Parser RegExp
pBrackets = foldl Alt Zero <$> (brackets (many pInBracketsChar))

pStar : Parser (RegExp -> RegExp)
pStar = pstar <|> pure id

mutual

  pAlts : Parser (List RegExp)
  pAlts = char '(' *!> (pExp `sepBy` (char '|')) <* char ')'

  pAltsTree : Parser (List RegExp) -> Parser RegExp
  pAltsTree ps = do xs <- ps
                    case xs of
                         []        => pure Eps
                         [r]       => pure r
                         (r :: rs) => do rest <- pAltsTree (pure rs)
                                         pure (Alt r rest)

  pAlt : Parser RegExp
  pAlt = pAltsTree pAlts

  pFactor : Parser RegExp
  pFactor =  pBrackets <|>| pAtom <|>| pAlt

  pTerm : Parser RegExp
  pTerm = f <$> pFactor <*> (pOpt <|>| pPlus <|>| pStar)
          where
            f e g = g e

  pExp : Parser RegExp
  pExp = foldl Cat Eps <$> many pTerm
