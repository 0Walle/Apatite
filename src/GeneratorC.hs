{-# OPTIONS_GHC -Wno-unused-matches #-}
module GeneratorC where


import Syntax
import AnaliserPass
import Data.List (intercalate) 

newtype ApatiteError = ApatiteError String
    deriving Show

data CompilerState = CompilerState
    { labelsId :: Int
    , tempsId :: Int
    , currentScope :: Int
    , currentReturn :: ApatiteType
    , labelStack :: [(Name, Int)]
    , deferStack :: [(Int, String)]
    , typeStack :: [(Name, TypeName)]
    , localStack :: [Symbol]
    , codeStack :: String
    } deriving Show

newtype IRTranslator a = IRTranslator 
    { runIr :: CompilerState -> Either ApatiteError (a, CompilerState) }

instance Functor IRTranslator where
    fmap f (IRTranslator t) = IRTranslator $ \st -> do  
        (a, st') <- t st
        Right (f a, st')

instance Applicative IRTranslator where
    pure x = IRTranslator $ \st -> Right (x, st)
    (IRTranslator t1) <*> (IRTranslator t2) = IRTranslator $ \st -> do
        (f, st') <- t1 st
        (a, st'') <- t2 st'
        Right (f a, st'')

instance Monad IRTranslator where
    (IRTranslator t) >>= f = IRTranslator $ \st -> do
        (a, st') <- t st
        runIr (f a) st'

orT :: IRTranslator a -> IRTranslator a -> IRTranslator a
(IRTranslator t1) `orT` (IRTranslator t2) = IRTranslator $ \st ->
    case t1 st of
        Left _ -> t2 st
        x -> x

gets :: (CompilerState -> a) -> IRTranslator a
gets f = IRTranslator $ \st -> Right (f st, st)

guard :: Bool -> String -> IRTranslator ()
guard True _ = pure ()
guard False msg = compileError msg

guardM :: IRTranslator Bool -> String -> IRTranslator ()
guardM t msg = t >>= (flip guard $ msg)

compileError :: String -> IRTranslator a
compileError = IRTranslator . const . Left . ApatiteError

justOr :: IRTranslator (Maybe a) -> String -> IRTranslator a
justOr t msg = t >>= try
  where
    try (Just x) = return x
    try Nothing = compileError msg





------------------------------------------
-- Get/Set State
------------------------------------------

writeCode :: String -> IRTranslator ()
writeCode inst = IRTranslator $ f
  where 
    f st@(CompilerState { codeStack = insts }) = 
        Right ((), st { codeStack = insts ++ inst } )

startScope :: IRTranslator ()
startScope = IRTranslator f
  where f st@(CompilerState { currentScope = curr }) = Right ((), st { currentScope = curr + 1 } )

endScope :: IRTranslator ()
endScope = IRTranslator f
  where 
    f st@(CompilerState
        { currentScope = curr
        , localStack = locals
        , deferStack = defers 
        }) =
        Right ((), st 
            { currentScope = curr - 1
            , localStack = popScope curr locals
            , deferStack = popDefers' curr defers 
            } )
    popScope _ [] = []
    popScope level (sym:syms)
        | sym_level sym == level = popScope level syms
        | otherwise              = sym:syms
    popDefers' _ [] = []
    popDefers' level (sym:syms)
        | fst sym == level = popDefers' level syms
        | otherwise  = sym:syms

popDefers :: IRTranslator ()
popDefers = IRTranslator $ \st@(CompilerState{ currentScope = curr, deferStack = defers }) ->
        Right ((), st { deferStack = popDefers' curr defers } )
  where    
    popDefers' _ [] = []
    popDefers' level (sym:syms)
        | fst sym == level = popDefers' level syms
        | otherwise  = sym:syms


undefer :: Int -> IRTranslator String
undefer level = gets (getUntil "" . deferStack)
  where
    getUntil acc ((l, code):xs)
        | l > level = getUntil (acc ++ code) xs
        | otherwise = acc

    getUntil acc [] = acc

pushSymbol :: Symbol -> IRTranslator ()
pushSymbol sym = IRTranslator $ f
  where f st@(CompilerState { localStack = syms, currentScope = curr }) = 
            Right ((), st { localStack = (sym { sym_level = curr }) : syms } )

pushTypeDec :: String -> TypeName -> IRTranslator ()
pushTypeDec name dec = IRTranslator $ f
  where f st@(CompilerState { typeStack = decs }) = 
            Right ((), st { typeStack = (name, dec) : decs } )

pushLabel :: String -> IRTranslator ()
pushLabel name = IRTranslator $ \st ->
    Right ((), st { labelStack = (name, currentScope st) : labelStack st } )

popLabel :: IRTranslator ()
popLabel = IRTranslator $ \st ->
    Right ((), st { labelStack = tail $ labelStack st } )

getLabel :: String -> IRTranslator (Maybe (String, Int))
getLabel name = gets (f . labelStack)
  where
    f [] = Nothing
    f ((x, y):xs)
        | x == name = Just (x, y)
        | otherwise = f xs

getLastLabel :: IRTranslator (Maybe (String, Int))
getLastLabel = gets (f . labelStack)
  where
    f [] = Nothing
    f (x:_) = Just x

getVar :: String -> IRTranslator (Maybe Symbol)
getVar name = gets (f . localStack)
  where
    f [] = Nothing
    f (v:vs)
        | sym_name v == name = Just v
        | otherwise          = f vs

getType :: String -> IRTranslator TypeName
getType name = justOr (gets (f . typeStack))
    $ "Undefined named type `" ++ name ++ "`"
  where
    f [] = Nothing
    f ((n, ty):vs)
        | n == name = Just ty
        | otherwise = f vs

initialState :: CompilerState
initialState = CompilerState
    { labelsId = 0
    , tempsId = 0
    , currentScope = 0
    , currentReturn = TypeInt
    , deferStack = []
    , typeStack = []
    , labelStack = []
    , localStack = []
    , codeStack = []
    }

binOperatorPrec :: BinOp -> Int
binOperatorPrec BinMulOp = 3
binOperatorPrec BinDivOp = 3
binOperatorPrec BinModOp = 3
binOperatorPrec BinAddOp = 4
binOperatorPrec BinSubOp = 4
binOperatorPrec BinShrOp = 5
binOperatorPrec BinShlOp = 5
binOperatorPrec BinGtOp = 6
binOperatorPrec BinLtOp = 6
binOperatorPrec BinGeOp = 6
binOperatorPrec BinLeOp = 6
binOperatorPrec BinEqOp = 7
binOperatorPrec BinNeOp = 7
binOperatorPrec BinAndOp = 8
binOperatorPrec BinXorOp = 9
binOperatorPrec BinOrOp = 10

binOperatorString :: BinOp -> String
binOperatorString BinMulOp = "*"
binOperatorString BinDivOp = "/"
binOperatorString BinModOp = "%"
binOperatorString BinAddOp = "+"
binOperatorString BinSubOp = "-"
binOperatorString BinShrOp = ">>"
binOperatorString BinShlOp = "<<"
binOperatorString BinGtOp = ">"
binOperatorString BinLtOp = "<"
binOperatorString BinGeOp = ">="
binOperatorString BinLeOp = "<="
binOperatorString BinEqOp = "=="
binOperatorString BinNeOp = "!="
binOperatorString BinAndOp = "&"
binOperatorString BinXorOp = "^"
binOperatorString BinOrOp = "|"

expectType :: ApatiteType -> IRTranslator (ApatiteType, Int, String) -> IRTranslator (Int, String)
expectType ty_a t = do
    (ty_b, prec, str) <- t
    if coercion ty_a ty_b
        then return (prec, str)
        else compileError $ "Can't match type `" ++ show ty_a ++ "` with `" ++ show ty_b ++ "`"
  where
    coercion TypeInt TypeNum = True
    coercion TypeFloat TypeNum = True
    coercion TypeByte TypeNum = True
    coercion _ TypeNum = False
    coercion a b = a == b

addParens :: Int -> Int -> String -> String
addParens gt_prec prec expr
    | prec > gt_prec = "(" ++ expr ++ ")"
    | otherwise      = expr

isTypeNumeric :: ApatiteType -> IRTranslator Bool
isTypeNumeric TypeInt = pure True
isTypeNumeric TypeByte = pure True
isTypeNumeric TypeFloat = pure True
isTypeNumeric (TypeDef name) = do
    (_, t) <- getType name
    isTypeNumeric t

isTypeInteger :: ApatiteType -> IRTranslator Bool
isTypeInteger ty = (pure . or) $ (ty ==) <$> [TypeInt, TypeByte]

typeSize :: ApatiteType -> IRTranslator Int
typeSize (TypePtr _) = pure 8
typeSize (TypeArray _) = pure 16
typeSize (TypeSArray size ty) = (* size) <$> typeSize ty
typeSize (TypeFunc _ _) = pure 8

typeSize (TypeDef name) = do
    (_, t) <- getType name
    typeSize t

typeSize (TypeStruct members) = do
    getAll (fst <$> members) 0
  where
    getAll [] size = pure size
    getAll (x:xs) size =
        sizeofAligned size x >>= getAll xs

    sizeofAligned size t = do
        align <- typeAlign t 
        psize <- typeSize t

        return $ aligned size align + psize

    aligned size align = size + ((align - (size `mod` align)) `mod` align)

typeSize (TypeFloat) = pure 8
typeSize (TypeInt) = pure 8
typeSize (TypeByte) = pure 1
typeSize (TypeBool) = pure 1
typeSize (TypeVoid) = pure 0
typeSize (TypeNum) = pure 0

typeAlign :: ApatiteType -> IRTranslator Int
typeAlign (TypeStruct members) = maximum <$> (mapM (typeAlign . fst) members)
typeAlign t = typeSize t

------------------------------------------
-- Gen Expr
------------------------------------------

genExpr :: ApatiteExpr -> IRTranslator (ApatiteType, Int, String)

genExpr TrueLiteral = pure (TypeBool, 0, "true")
genExpr FalseLiteral = pure (TypeBool, 0, "false")
genExpr (NumberLiteral n) = pure (TypeNum, 0, (show n))
genExpr (StringLiteral s) = pure (TypePtr TypeByte, 0, "\"" ++ s ++ "\"")

genExpr (ArrayLiteral (NumberLiteral size) type_ list) = do
    list' <- mapM (expectType type_ . genExpr) list

    let size' = if size == -1 then length list' else floor size
    let ctype_ = TypeSArray size' type_

    return $ (ctype_, 0, "((" ++ ctype ctype_ "" ++ "){" ++ intercalate ", " (snd <$> list') ++ "})")

genExpr (ArrayLiteral _ type_ list) = compileError "No, you can't do that"

genExpr (StructLiteral name list) = do
    (_, type_) <- getType name
    members <- expectStruct type_

    list' <- mapM matchMember (zip members list)

    return $ (TypeDef name, 0, "((" ++ name ++ "){" ++ intercalate ", " (list') ++ "})")
  where
    expectStruct (TypeStruct members) = pure members
    expectStruct _ = compileError "Expected struct type"

    matchMember ((mtype, _), expr) = do
        (_, x) <- expectType mtype $ genExpr expr
        pure x

genExpr (IdentifierExpr name) = do
    sym <- getVar name
    case sym of
        Just sym -> do
            return $ (sym_type sym, 0, sym_name sym)
        Nothing -> compileError ("Symbol `" ++ name ++ "` not found")

genExpr (AndExpr expr_a expr_b) = do
    (prec_a, a) <- expectType TypeBool $ genExpr expr_a
    (prec_b, b) <- expectType TypeBool $ genExpr expr_b

    return $ (TypeBool ,11, (addParens 11 prec_a a) ++ " && " ++ (addParens 11 prec_b b))

genExpr (OrExpr expr_a expr_b) = do
    (prec_a, a) <- expectType TypeBool $ genExpr expr_a
    (prec_b, b) <- expectType TypeBool $ genExpr expr_b

    return $ (TypeBool, 12, (addParens 12 prec_a a) ++ " || " ++ (addParens 12 prec_b b))

genExpr (NotExpr expr) = do
    (prec_a, a) <- expectType TypeBool $ genExpr expr

    return $ (TypeBool, 2, "!" ++ (addParens 2 prec_a a))

genExpr (BinaryExpr op expr_a expr_b) = do
    (type_a, prec_a, a') <- genExpr expr_a
    (type_b, prec_b, b') <- genExpr expr_b

    let prec_op = binOperatorPrec op
    let a = addParens prec_op prec_a a'
    let b = addParens prec_op prec_b b'

    let code = a ++ " " ++ binOperatorString op ++ " " ++ b

    let callee = (case op of
            BinMulOp -> numericArithOperator
            BinDivOp -> numericArithOperator
            BinModOp -> numericArithOperator
            BinAddOp -> numericArithOperator
            BinSubOp -> numericArithOperator
            BinShrOp -> numericArithOperator
            BinShlOp -> numericArithOperator
            BinGtOp -> numericCompareOperator
            BinLtOp -> numericCompareOperator
            BinGeOp -> numericCompareOperator
            BinLeOp -> numericCompareOperator
            BinEqOp -> numericCompareOperator
            BinNeOp -> numericCompareOperator
            BinAndOp -> integerArithOperator
            BinXorOp -> integerArithOperator
            BinOrOp -> integerArithOperator)

    (callee type_a type_b) >>= binaryOperator code
  where
    binaryOperator code (cond, ty)
        | cond      = return $ (ty, binOperatorPrec op, code)
        | otherwise = compileError "Expected Numeric Type"

    integerArithOperator TypeInt TypeInt = pure (True, TypeInt)
    integerArithOperator TypeByte TypeByte = pure (True, TypeByte)
    integerArithOperator TypeNum TypeNum = pure (True, TypeNum)
    integerArithOperator TypeNum x = integerArithOperator x x
    integerArithOperator x TypeNum = integerArithOperator x x
    integerArithOperator _ _ = pure (False, TypeVoid)

    numericArithOperator TypeInt TypeInt = pure (True, TypeInt)
    numericArithOperator TypeByte TypeByte = pure (True, TypeByte)
    numericArithOperator TypeFloat TypeFloat = pure (True, TypeFloat)
    numericArithOperator TypeNum TypeNum = pure (True, TypeNum)
    numericArithOperator TypeNum x = integerArithOperator x x
    numericArithOperator x TypeNum = integerArithOperator x x
    numericArithOperator _ _ = pure (False, TypeVoid)

    numericCompareOperator (TypePtr a) (TypePtr b) = pure (a == b, TypeBool)
    numericCompareOperator TypeInt TypeInt = pure (True, TypeBool)
    numericCompareOperator TypeByte TypeByte = pure (True, TypeBool)
    numericCompareOperator TypeFloat TypeFloat = pure (True, TypeBool)
    numericCompareOperator TypeBool TypeBool = pure (True, TypeBool)
    numericCompareOperator TypeNum TypeNum = pure (True, TypeBool)
    numericCompareOperator TypeNum x = numericCompareOperator x x
    numericCompareOperator x TypeNum = numericCompareOperator x x
    numericCompareOperator _ _ = pure (False, TypeVoid)

genExpr (UnaryExpr UnNegOp expr) = do
    (type_a, prec_a, a) <- genExpr expr

    guardM (isTypeNumeric type_a)
        "Expected Numeric Type"
    return $ (type_a, 2, "-" ++ (addParens 2 prec_a a))

genExpr (UnaryExpr UnNotOp expr) = do
    (type_a, prec_a, a) <- genExpr expr

    guardM (isTypeInteger type_a)
        "Expected Integer Type"
    return $ (type_a, 2, "~" ++ (addParens 2 prec_a a))

genExpr (IndexExpr arr index) = do
    (type_arr, prec_arr, arr) <- genExpr arr
    (_, index) <- expectType TypeInt $ genExpr index

    case type_arr of
        TypeSArray size type_item -> return (type_item, 1, arr ++ "[" ++ index ++ "]")
        TypeArray type_item -> return (type_item, 1, "((" ++ (ctype type_item "") ++ "*)(" ++ arr ++ ".data))[" ++ index ++ "]")
        _ -> compileError "Expected Array Type"

genExpr (LenExpr expr) = do
    (type_arr, _, arr) <- genExpr expr

    case type_arr of
        TypeSArray size _ -> return (TypeInt, 1, show size)
        TypeArray _ -> return (TypeInt, 1, "(" ++ arr ++ ".len)")
        _ -> compileError "Expected Array Type"

genExpr (DerefExpr expr) = do
    (type_ptr, prec, ptr') <- genExpr expr

    let ptr = addParens 1 prec ptr'

    case type_ptr of
        TypePtr type_item -> return (type_item, 1, "*" ++ ptr)
        _ -> compileError "Expected Pointer Type"

genExpr (AssignExpr expr_a expr_b) 
    | isLvalue expr_a = do
        (type_item, _, lvalue) <- genExpr expr_a
        (_, b) <- expectType type_item $ genExpr expr_b

        return $ (type_item, 14, lvalue ++ " = " ++ b)
    | otherwise       = compileError "Invalid Expression in left side of assignment"
  where
    isLvalue (IdentifierExpr _) = True
    isLvalue (DerefExpr _) = True
    isLvalue (IndexExpr _ _) = True
    isLvalue (MemberExpr _ _) = True
    isLvalue _ = False

genExpr (SizeofExpr ty) = do
    size <- typeSize ty
    return (TypeInt, 0, show size)

genExpr (AlignofExpr ty) = do
    size <- typeAlign ty
    return (TypeInt, 0, show size)

genExpr (CondExpr cond expr_true expr_false) = do
    cond' <- (uncurry $ addParens 13) <$> (expectType TypeBool $ genExpr cond)
    (type_true, _, true') <- genExpr expr_true
    false' <- (uncurry $ addParens 13) <$> (expectType type_true $ genExpr expr_false)

    return (type_true, 13, cond' ++ " ? " ++ true' ++ " : " ++ false')

genExpr (RefExpr (StringLiteral s)) = pure (TypePtr (TypeSArray (length s) TypeByte), 0, "&\"" ++ s ++ "\"")

genExpr (RefExpr (DerefExpr expr)) = genExpr expr

genExpr (RefExpr expr)
    | isLvalue expr = do
        (type_item, prec_var, var') <- genExpr expr
        return (TypePtr type_item, 1, "&" ++ var')
    | otherwise = compileError "Invalid Expression in reference"
  where
    isLvalue (IdentifierExpr _) = True
    isLvalue (IndexExpr _ _) = True
    isLvalue (MemberExpr _ _) = True
    isLvalue _ = False

genExpr (CallExpr func args) = do
    (type_func, prec_func, func') <- genExpr func

    case type_func of
        TypeFunc ret params -> do 
            args' <- sequenceA (checkArgs <$> zip params args)
            return (ret, 0, func' ++ "(" ++ intercalate ", " args' ++ ")")
        _ -> compileError "Expected Function type"
  where
    checkArgs (param, arg) = snd <$> (expectType param $ genExpr arg)

genExpr (CastExpr expr ty) = do
    (type_expr, prec_expr, expr') <- genExpr expr
    s' <- castTo ty type_expr
    return (ty, 1, "((" ++ s' ++ ")(" ++ expr' ++ "))")
  where
    castTo TypeInt = mayCastIf isTypeNumeric $ "int"
    castTo TypeFloat = undefined
    castTo TypeByte = undefined
    castTo _ = undefined

    mayCastIf cond x type_ = do
        cond' <- cond type_
        if cond'
            then return x 
            else compileError "Invalid cast"

genExpr (MemberExpr obj name) = do
    (obj_type, obj_prec, obj') <- genExpr obj

    (isPtr, members) <- getStructMembers obj_type

    member_type <- justOr (pure $ getMember members name)
        ("Invalid member in struct `"++name++"`")
    
    return $ (member_type, 1, obj' ++ (if isPtr then "->" else ".") ++ name)
  where
    getMember [] ename = Nothing
    getMember ((ty, mname):ms) ename
        | mname == ename = Just ty
        | otherwise = getMember ms ename

    getStructMembers (TypePtr t@(TypeDef _)) = (\x -> (True, snd x)) <$> getStructMembers t

    getStructMembers (TypeDef tyname) = do
        (_, type_) <- getType tyname
            
        case type_ of
            TypeStruct members -> return (False, members)
            _ -> compileError "Expected Struct type"





------------------------------------------
-- Gen Stmt
------------------------------------------

genStmt :: ApatiteStmt -> IRTranslator (Bool, String)

genStmt EmptyStmt = pure (False, "")

genStmt (ExprStmt expr) = do
    (_, _, expr') <- genExpr expr
    return (False, expr' ++ ";")

genStmt (DeclStmt (VarDec ty name maybe_init)) = do

    tsize <- typeSize ty

    guard (tsize /= 0)
        $ "Invalid type `" ++ show ty ++ "` in declaration, type with undefined size"

    let decl' = ctype ty name

    pushSymbol $ Symbol name ty 0 0

    case maybe_init of
        Nothing -> return (False, decl' ++ ";")
        Just init -> do
            (_, init') <- expectType ty $ genExpr init
            return (False, decl' ++ " = " ++ init' ++ ";")

genStmt (DeclStmt _) = compileError "Unimplemented"

genStmt (BlockStmt stmts) = do
    startScope
    (isEarly, term, body) <- genBlockItem "" False stmts
    cs' <- gets currentScope
    defers' <- undefer (cs' - 1)
    endScope

    return (term, "{ " ++ body ++ defers' ++ " }")
  where
    genBlockItem code term [] = pure (False, term, code)
    
    genBlockItem code term (x@(ReturnStmt _):_) = do
        (term', code') <- genStmt x
        return $ (True, term || term', code ++ code') 

    genBlockItem code term (x@(BreakStmt _):_) = do
        (term', code') <- genStmt x
        return $ (True ,term || term', code ++ code') 

    genBlockItem code term (x@(ContinueStmt _):_) = do
        (term', code') <- genStmt x
        return $ (True, term || term', code ++ code') 
    
    genBlockItem code term (x:xs) = do
        (term', code') <- genStmt x

        genBlockItem (code ++ code') (term || term') xs

genStmt (IfStmt cond then_st Nothing) = do
    (_, cond') <- expectType TypeBool $ genExpr cond
    (_, then_st') <- genStmt then_st

    return (False, "if (" ++ cond' ++ ") " ++ then_st')

genStmt (IfStmt cond then_st (Just else_st)) = do
    (_, cond') <- expectType TypeBool $ genExpr cond
    (term_a, then_st') <- genStmt then_st
    (term_b, else_st') <- genStmt else_st

    return (term_a && term_b, "if (" ++ cond' ++ ") " ++ then_st' ++ " else " ++ else_st')

genStmt (WhileStmt (Just label) cond loop_st) = do
    (_, cond') <- expectType TypeBool $ genExpr cond
    pushLabel label
    (_, loop_st') <- genStmt loop_st
    popLabel

    return (False, "_"++label++"_s: while (" ++ cond' ++ ") " ++ loop_st' ++ "_" ++ label ++ "_e:;")

genStmt (WhileStmt Nothing cond loop_st) = do
    (_, cond') <- expectType TypeBool $ genExpr cond
    pushLabel "$"
    (_, loop_st') <- genStmt loop_st
    popLabel

    return (False, "while (" ++ cond' ++ ") " ++ loop_st')

genStmt (ReturnStmt (Just expr)) = do
    rt <- gets currentReturn

    (_, expr') <- expectType rt $ genExpr expr

    defers' <- undefer 0
    popDefers

    return (True, pretty defers' ("return " ++ expr' ++ ";"))
  where
    pretty "" str = str
    pretty x str = "{" ++ x ++ str ++ "}"

genStmt (ReturnStmt Nothing) = do
    matchReturn
    defers' <- undefer 0
    popDefers
    pure (True, pretty defers' "return;")
  where
    pretty "" str = str
    pretty x str = "{" ++ x ++ str ++ "}"

    matchReturn = IRTranslator $ \(st@CompilerState { currentReturn = rt }) ->
        if rt == TypeVoid
            then Right ((), st)
            else Left $ ApatiteError $ "Invalid return, expected `" ++ show rt ++ "` but got `void`"

genStmt (DeferStmt body) = do
    (term, body') <- genStmt body
    pushDefer body'
    return (term, ";")
  where
    pushDefer defer = IRTranslator (\st@(CompilerState { deferStack = syms, currentScope = curr }) ->
        Right ((), st { deferStack = (curr, defer) : syms } ))

genStmt (ContinueStmt (Just label)) = do
    (_, level) <- justOr (getLabel label)
        $ "Invalid continue"

    defers' <- undefer level

    return (False, "{" ++ defers' ++ "goto _" ++ label ++ "_s; }")

genStmt (ContinueStmt Nothing) = do
    (_, level) <- justOr (getLastLabel)
        $ "Invalid continue"

    defers' <- undefer level

    return (False, "{" ++ defers' ++ "continue; }")

genStmt (BreakStmt (Just label)) = do
    (_, level) <- justOr (getLabel label)
        $ "Invalid break"

    defers' <- undefer level

    return (False, "{" ++ defers' ++ "goto _" ++ label ++ "_e; }")

genStmt (BreakStmt Nothing) = do
    (_, level) <- justOr (getLastLabel)
        $ "Invalid break"

    defers' <- undefer level

    return (False, "{" ++ defers' ++ "break; }")






------------------------------------------
-- Gen Global Decl
------------------------------------------

genGlobalDec :: ApatiteDec -> IRTranslator [Char]
genGlobalDec (VarDec ty name maybe_init) = do

    size <- typeSize ty
    guard (size > 0)
        $ "Invalid type `" ++ show ty ++ "` in declaration"

    let decl' = ctype ty name

    pushSymbol $ Symbol name ty 0 0

    case maybe_init of
        Just initializer -> do
            (_, init') <- expectType ty $ genExpr initializer
            return $ decl' ++ " = " ++ init' ++ ";"
        Nothing -> return $ decl' ++ ";"

genGlobalDec (FuncDec ret_type members name body) = do
    
    let decl' = ctypeFunc ret_type members name
    let func_type = TypeFunc ret_type (fst $ unzip members)

    pushSymbol $ Symbol name func_type 0 0

    setReturn ret_type

    startScope
    mapM_ pushParameter members
    
    (term, body') <- genStmt body

    guard ((ret_type /= TypeVoid && term) || ret_type == TypeVoid)
        $ "Function `"++ name ++"` doesn't always terminates"

    endScope

    return $ decl' ++ body' ++ "\n"

  where
    pushParameter (arg_type, arg_name) = do

        size <- typeSize arg_type
        guard (size > 0)
            $ "Invalid type `" ++ show arg_type ++ "` in declaration"

        pushSymbol $ Symbol arg_name arg_type 0 0


    setReturn ty = IRTranslator $ \st -> Right ((), st { currentReturn = ty })

genGlobalDec (TypeDec declName _) = do
    generateType declName
  where
    declareMember (TypeDef tname, _) = do
        generateType tname

    declareMember (_, _) = pure ""

    generateType name = do
        types <- gets typeStack

        let mType = snd <$> find (matchName name) types

        case mType of
            Just (IncompleteType, type_@(TypeStruct members)) -> do
                setTypeDec name (ResolvingType, type_)

                siblings <- concat <$> mapM declareMember members

                setTypeDec name (ConcreteType, type_)

                return $ siblings ++ "struct _" ++ name ++ " {" ++ (concat $ ((++"; ") . uncurry ctype) <$> members) ++ "};"
            
            Just (ResolvingType, _) -> compileError "Recursive type definition"
            Just _ -> pure ""
            Nothing -> compileError "Unknown type definition"

    matchName :: Name -> (Name, TypeName) -> Bool
    matchName name (name', _) = name == name'

    find :: (a -> Bool) -> [a] -> Maybe a
    find p [] = Nothing
    find p (x:xs)
        | p x       = Just x
        | otherwise = find p xs

    setTypeDec typename tst = IRTranslator $ \st@(CompilerState { typeStack = decs }) ->
        Right ((), st { typeStack = map (replacer typename tst) decs } )
    
    replacer name st x@(n, _)
        | n == name = (n, st)
        | otherwise = x

genGlobalDec (EnumDec name members) = do
    pushTypeDec name (ConcreteType, TypeDef name)
    mapM_ pushMember members
    return $ "typedef enum _" ++ name ++ " {" ++ intercalate "; " members ++ "} " ++ name ++ ";"
  where
    pushMember mname = pushSymbol $ Symbol mname (TypeDef name) 0 0





------------------------------------------
-- Generate
------------------------------------------

generate :: CompilerState -> [ApatiteDec] -> Either ApatiteError String
generate st decls = do
    ((prelude_types, prelude_names), body) <- fst <$> runIr generate' st

    return $ concat prelude_types ++ concat prelude_names ++ body
  where
    generate' = do
        prelude <- makePrelude
        body <- concat <$> mapM genGlobalDec decls

        return (prelude, body)

    makePrelude = do
        types <- gets typeStack
        names <- gets localStack

        return $ ((typeDec <$> types), (nameDec <$> names))

    typeDec (name, (_, t)) = ctypedef t name

    nameDec (Symbol name (TypeFunc ret args) _ _) = ctypeFunc ret ((\i -> (i,"")) <$> args) name ++ ";"

    nameDec (Symbol name t _ _) = ctype t name ++ ";"

generateTest :: ApatiteExpr -> Either ApatiteError (ApatiteType, Int, String)
generateTest expr = fst <$> runIr (genExpr expr) initialState