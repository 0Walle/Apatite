module ApatiteParser where
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

import Lexer
import Syntax

apatiteFile :: Parser [ApatiteDec]
apatiteFile = 
    do  result <- many globalDec
        eof
        return result

--------------------------------------------------
-- Types
--------------------------------------------------

parseType :: Parser ApatiteType
parseType = 
        parsePtrType 
    <|> parseArrayType 
    <|> parseFuncType 
    <|> parseBasicType 
    <|> parseUserType
    <|> parseStructType

parsePtrType =
    do reservedOp "'"
       ty <- parseType
       return $ TypePtr ty

parseArrayType =
    do symbol "["
       size <- optionMaybe natural
       symbol "]"
       ty <- parseType
       return $ case size of
          Just n -> TypeSArray (fromInteger n) ty
          otherwise -> TypeArray ty

parseStructType = do
    reserved "struct"
    members <- braces (parseMember `sepEndBy` semi)
    return $ TypeStruct members

parseFuncType = do
    reserved "func"
    args <- parens (commaSep parseType)
    ret <- parseType
    return $ TypeFunc ret args 
    

basicTypes = [
    ("int", TypeInt),
    ("byte", TypeByte),
    ("float", TypeFloat),
    ("bool", TypeBool),
    ("void", TypeVoid)]

parseBasicType =
    choice (map doType basicTypes)
    where doType (st, ty) = do { reserved st; return ty }

parseUserType =
    do name <- identifier
       return $ TypeDef name

----------------------------------------------------------------------------------------------------
-- Member
----------------------------------------------------------------------------------------------------

parseMember :: Parser ApatiteMember
parseMember =
    do ty <- parseType
       name <- identifier
       return $ (ty, name)

----------------------------------------------------------------------------------------------------
-- Global Declarations
----------------------------------------------------------------------------------------------------

typeDec :: Parser ApatiteDec
typeDec =
    do reserved "type"
       name <- identifier
       reservedOp "="
       ty <- parseType
       return $ TypeDec name ty

unionDec :: Parser ApatiteDec
unionDec =
    do reserved "union"
       name <- identifier
       members <- braces (parseMember `sepEndBy` semi)
       return $ UnionDec name members

enumDec :: Parser ApatiteDec
enumDec =
    do reserved "enum"
       name <- identifier
       members <- braces (identifier `sepEndBy` comma)
       return $ EnumDec name members

varDec :: Parser ApatiteDec
varDec = do
    reserved "let"
    ty <- parseType
    name <- identifier
    undefVar ty name <|> defVar ty name
  where
    undefVar ty name = do 
        semi
        return $ VarDec ty name Nothing

    defVar ty name = do
        reservedOp "="
        expr <- parseExpr
        semi
        return $ VarDec ty name (Just expr)
    
funcDec :: Parser ApatiteDec
funcDec = do
    -- CHANGE?!?!
    reserved "func"
    name <- identifier
    args <- parens (commaSep parseMember)
    ty <- (maybe TypeVoid id) <$> (optionMaybe parseType)
    stmt <- parseBlock
    return $ FuncDec ty args name stmt

globalDec :: Parser ApatiteDec
globalDec = 
    typeDec <|> unionDec <|> enumDec <|> varDec <|> funcDec

----------------------------------------------------------------------------------------------------
-- Expression
----------------------------------------------------------------------------------------------------

parseInitExpr :: Parser ApatiteInitExpr
parseInitExpr =  do init_list <- braces (parseInitExpr `sepEndBy` comma)
                    return $ InitList init_list
             <|> InitExpr <$> parseExpr

parseExpr :: Parser ApatiteExpr
parseExpr = parseCondExpr `chainr1` assignOp

assignOp =  do{ reservedOp "="; return (AssignExpr) }
        <|> do{ reservedOp "+="; return (makeOp BinAddOp) }
        <|> do{ reservedOp "*="; return (makeOp BinMulOp) }        
        <|> do{ reservedOp "-="; return (makeOp BinSubOp) }        
        <|> do{ reservedOp "/="; return (makeOp BinDivOp) }        
        <|> do{ reservedOp "%="; return (makeOp BinModOp) }
        <|> do{ reservedOp "&="; return (makeOp BinAndOp) }        
        <|> do{ reservedOp "|="; return (makeOp BinOrOp) }        
        <|> do{ reservedOp "^="; return (makeOp BinXorOp) }        
        <|> do{ reservedOp "<<="; return (makeOp BinShrOp) }
        <|> do{ reservedOp ">>="; return (makeOp BinShlOp) }
        <?> "AssignExpression"
  where
    makeOp op a b =
      AssignExpr a (BinaryExpr op a b)


makeCondExpr :: ApatiteExpr -> ApatiteExpr -> ApatiteExpr -> ApatiteExpr
makeCondExpr TrueLiteral t f = t
makeCondExpr FalseLiteral t f = f
makeCondExpr (NumberLiteral 0) t f = f
makeCondExpr (NumberLiteral n) t f = t
makeCondExpr c t f = CondExpr c t f

parseCondExpr :: Parser ApatiteExpr
parseCondExpr = try condExpr <|> parseBinaryExpr
    where condExpr = do cond <- parseBinaryExpr
                        reservedOp "?"
                        truey <- parseBinaryExpr
                        reservedOp ":"
                        falsey <- parseCondExpr
                        return $ makeCondExpr cond truey falsey

parseBinaryExpr :: Parser ApatiteExpr
parseBinaryExpr = Ex.buildExpressionParser exprTable parsePrimaryExpr

exprTable = [ [repeatPostfix]
            , [repeatPrefix]
            , [binary "*" BinMulOp, binary "/"  BinDivOp, binary "%"  BinModOp ]
            , [binary "+" BinAddOp, binary "-"  BinSubOp ]
            , [binary ">>"BinShlOp, binary "<<" BinShrOp ]
            , [binary ">" BinGtOp , binary "<"  BinLtOp, binary ">=" BinGeOp, binary "<=" BinLeOp ]
            , [binary "==" BinEqOp, binary "!=" BinNeOp ]
            , [binary "&" BinAndOp, binary "|"  BinOrOp, binary "^"  BinXorOp ]
            , [Ex.Prefix notExpr]
            , [binaryAnd]
            , [binaryOr]
            ]

repeatPrefix = Ex.Prefix . chainl1 (choice [castExpr, unary "-" UnNegOp, unary "~" UnNotOp, refExpr]) $ return (.)
repeatPostfix = Ex.Postfix . chainr1 (choice [ derefExpr, indexExpr, callExpr, memberExpr ]) $ return (flip (.))

indexExpr = (do { expr <- brackets parseExpr; return $ flip IndexExpr expr})
callExpr = (do { args <- parens (commaSep parseExpr); return $ (flip CallExpr) args})
memberExpr = (do { reservedOp "."; name <- identifier; return $ (flip MemberExpr) name})
derefExpr = (do { reservedOp "\'"; return DerefExpr})

unary op name = (do { reservedOp op; return $ UnaryExpr name }) 
castExpr = (do { reserved "cast"; ty <- parens parseType; return (\e -> CastExpr e ty) })
notExpr = (do { reserved "not"; return NotExpr })
refExpr = (do { reserved "ref"; return RefExpr })

binary op name = Ex.Infix (do { reservedOp op; return $ BinaryExpr name }) Ex.AssocLeft
binaryAnd = Ex.Infix (do { reserved "and"; return $ AndExpr }) Ex.AssocLeft
binaryOr = Ex.Infix (do { reserved "or"; return $ OrExpr }) Ex.AssocLeft

parsePrimaryExpr =  parens parseExpr 
                <|> parseLen
                <|> parseSizeof
                <|> parseAlignof
                <|> parseNatural
                <|> parseString
                <|> parseArrayLiteral
                <|> parseStructLiteral
                <|> TrueLiteral <$ (reserved "true")
                <|> FalseLiteral <$ (reserved "false")
                <|> parseIdentifier
                <?> "PrimaryExpression"

parseLen = do { reserved "len"; ty <- parens parseExpr; return $ LenExpr ty }
parseSizeof = do { reserved "sizeof"; ty <- parens parseType; return $ SizeofExpr ty }
parseAlignof = do { reserved "alignof"; ty <- parens parseType; return $ AlignofExpr ty }

parseArrayLiteral = do
    size <- brackets $ (NumberLiteral(-1) <$ symbol "_") <|> parseNatural
    type_ <- parseType
    exprs <- braces (commaSep parseExpr)
    return $ ArrayLiteral size type_ exprs

parseStructLiteral = do
    reserved "struct"
    name <- identifier
    exprs <- braces (commaSep parseExpr)
    return $ StructLiteral name exprs

parseNatural = (NumberLiteral ) <$> numLit
parseIdentifier = IdentifierExpr <$> identifier

parseCharacter = fmap return nonEscape <|> escape
    where escape = do { d <- char '\\'; c <- oneOf "\\\"0nrtb"; return [d, c] }
          nonEscape = noneOf "\\\"\0\n\r"

parseString = do
    char '"'
    strings <- many parseCharacter
    char '"'
    return $ StringLiteral (concat strings)

----------------------------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------------------------

parseBlock :: Parser ApatiteStmt
parseBlock = do items <- braces (many parseBlockItem)
                return $ BlockStmt items

parseBlockItem = parseStmt <|> (DeclStmt <$> globalDec)

parseStmt :: Parser ApatiteStmt
parseStmt =  parseBlock 
         <|> parseIfStmt
         <|> parseDeferStmt
         <|> parseReturnStmt
         <|> parseBreakContinue
         <|> try parseWhileStmt
         <|> parseExprStmt

parseIfStmt = do reserved "if"
                 cond <- parseExpr
                 truy <- parseBlock
                 fals <- optionMaybe (do { reserved "else"; parseIfStmt <|> parseBlock})
                 return $ IfStmt cond truy fals

parseWhileStmt = do 
    label <- optionMaybe $ identifier <* symbol ":"
    reserved "while"
    cond <- parseExpr
    loop <- parseBlock
    return $ WhileStmt label cond loop

parseDeferStmt = do reserved "defer"
                    body <- parseStmt
                    return $ DeferStmt body

parseReturnStmt = do reserved "return"
                     expr <- optionMaybe parseExpr
                     symbol ";"
                     return $ ReturnStmt expr

parseBreakContinue = kw "break" BreakStmt <|> kw "continue" ContinueStmt
  where
    kw name st = do
        reserved name
        label <- optionMaybe identifier
        semi
        return $ st label

parseExprStmt = do expr <- parseExpr
                   semi
                   return $ ExprStmt expr

----------------------------------------------------------------------------------------------------
-- Final
----------------------------------------------------------------------------------------------------

parseApatite :: String -> Either ParseError [ApatiteDec]
parseApatite input = parse (contents apatiteFile) "main" input

parseApatiteExpr :: String -> Either ParseError ApatiteExpr
parseApatiteExpr input = parse (contents parseExpr) "main" input

parseApatiteStmt :: String -> Either ParseError ApatiteStmt
parseApatiteStmt input = parse (contents parseStmt) "main" input