module Lexer where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (javaStyle)

import Data.Char

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = [ "+","*","-","/", "%", "&", "|", "^", ">>", "<<",
                "==","!=",">","<","<=",">=",
                "+=","*=","-=","/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "=",
                ".", "?", ":", "~", "'"]
        names = 
            ["true", "false"
            , "and", "or", "not"
            , "if", "else", "while", "return", "break", "continue", "defer"
            , "void", "byte", "int", "bool", "float"
            , "cast", "sizeof", "alignof", "ref", "len"
            , "let", "func", "type", "struct", "enum", "union"]
        style = javaStyle { Tok.reservedOpNames = ops,
                            Tok.identLetter = alphaNum <|> char '_',
                            Tok.reservedNames = names,
                            Tok.caseSensitive = True,
                            Tok.commentLine = "//",
                            Tok.opLetter = oneOf ".:!%&*+/<=>?^|-~"
                          }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

natural :: Parser Integer
natural = Tok.natural lexer

float :: Parser Double
float = Tok.float lexer

numLit :: Parser Double
numLit = Tok.lexeme lexer numLit'

numLit' :: Parser Double
numLit' =
        zeroNumber 
    <|> floatLiteral

zeroNumber :: Parser Double
zeroNumber = do
    _ <- char '0'
    fromIntegral <$> (hexaLiteral <|> binaryLiteral <|> decimalLiteral <|> return 0)

decimalLiteral = number 10 digit
hexaLiteral = do { _ <- char 'x'; number 16 hexDigit }
binaryLiteral = do { _ <- char 'b'; number 2 (oneOf "01")  }

floatLiteral = do
    int <- decimalLiteral
    frac <- option "" fracLiteral
    readDouble (show int ++ frac)
  where
    readDouble s =
        case reads s of
            [(x, "")] -> return x
            _ -> parserZero

fracLiteral = do
    _ <- char '.'
    frac <- many digit
    return ('.' : frac)

number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

stringlit :: Parser String
stringlit = Tok.stringLiteral lexer

semi :: Parser String
semi = Tok.semi lexer

comma :: Parser String
comma = Tok.comma lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer
