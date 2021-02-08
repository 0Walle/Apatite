module Syntax where
import Data.List

type Name = String
type ApatiteMember = (ApatiteType, Name)

data ApatiteType =
      TypePtr ApatiteType
    | TypeArray ApatiteType
    | TypeSArray Int ApatiteType
    | TypeFunc ApatiteType [ApatiteType]
    | TypeStruct [ApatiteMember]
    | TypeDef String
    | TypeNum
    | TypeInt
    | TypeFloat
    | TypeByte
    | TypeBool
    | TypeVoid


instance Eq ApatiteType where
  (==) TypeInt TypeInt = True
  (==) TypeFloat TypeFloat = True
  (==) TypeByte TypeByte = True
  (==) TypeBool TypeBool = True
  (==) TypeVoid TypeVoid = True
  (==) (TypeArray type_a) (TypeArray type_b) = type_a == type_b
  (==) (TypeSArray sa type_a) (TypeSArray sb type_b) = sa == sb && type_a == type_b
  (==) (TypeStruct _) (TypeStruct _) = False
  (==) (TypePtr type_a) (TypePtr type_b) = type_a == type_b
  (==) (TypeDef name_a) (TypeDef name_b) = name_a == name_b
  (==) (TypeFunc ret_a args_a) (TypeFunc ret_b args_b) = ret_a == ret_b && args_a == args_b
  (==) _ _ = False
    
instance Show ApatiteType where
    show t = case t 
        of TypePtr ty -> "'" ++ show ty
           TypeArray ty -> "[]" ++ show ty
           TypeSArray size ty -> "[" ++ show size ++ "]" ++ show ty
           TypeFunc ret args -> "func(" ++ intercalate ", " (show <$> args) ++ ")" ++ show ret
           TypeStruct members -> "struct {" ++ (concat $ ((++"; ") . show) <$> members) ++ "}"
           TypeDef name -> name
           TypeNum -> "Number *"
           TypeInt -> "int"
           TypeFloat -> "float"
           TypeByte -> "byte"
           TypeBool -> "bool"
           TypeVoid -> "void"

ctype :: ApatiteType -> String -> String
ctype (TypePtr ty) "" = ctype ty "*"
ctype (TypePtr ty) middle =
    ctype ty ("*(" ++ middle ++ ")")

ctype (TypeSArray size ty) "" = ctype ty ("[" ++ show size ++ "]")
ctype (TypeSArray size ty) middle =
    ctype ty ("(" ++ middle ++ "[" ++ show size ++ "])")


ctype (TypeFunc ret args) middle =
    ctype ret ("(*" ++ middle ++ ")(" ++ intercalate ", " ((flip ctype) "" <$> args) ++ ")")

ctype (TypeStruct members) middle =
    "struct {" ++ (concat $ ((++"; ") . uncurry ctype) <$> members) ++ "}" ++ middle

ctype (TypeArray _) middle = "struct _Slice " ++ middle
ctype (TypeDef name) middle = name ++ " " ++ middle
ctype (TypeInt) middle = "intptr_t " ++ middle
ctype (TypeFloat) middle = "double " ++ middle
ctype (TypeByte) middle = "byte " ++ middle
ctype (TypeBool) middle = "bool " ++ middle
ctype (TypeVoid) middle = "void " ++ middle

ctype (TypeNum) middle = "void " ++ middle

ctypeFunc ret members name =
    ctype ret (name ++ "(" ++ intercalate ", " (uncurry ctype <$> members) ++ ")")

ctypedef :: ApatiteType -> String -> String
ctypedef (TypePtr ty) middle =
    "typedef " ++ ctype ty ("*(" ++ middle ++ ")") ++ ";"
ctypedef (TypeSArray size ty) middle =
    "typedef " ++ ctype ty ("(" ++ middle ++ "[" ++ show size ++ "])") ++ ";"
ctypedef (TypeFunc ret args) middle =
    "typedef " ++ (ctype ret ("(*" ++ middle ++ ")(" ++ intercalate ", " ((flip ctype) "" <$> args) ++ ")")) ++ ";"

ctypedef (TypeStruct members) middle =
    "typedef struct _" ++ middle ++ " " ++ middle ++ ";"

ctypedef (TypeArray _) middle = "typedef struct _Slice " ++ middle ++ ";"
ctypedef (TypeDef name) middle = "typedef " ++ name ++ " " ++ middle ++ ";"
ctypedef (TypeInt) middle = "typedef intptr_t " ++ middle ++ ";"
ctypedef (TypeFloat) middle = "typedef double " ++ middle ++ ";"
ctypedef (TypeByte) middle = "typedef byte " ++ middle ++ ";"
ctypedef (TypeBool) middle = "typedef bool " ++ middle ++ ";"
ctypedef (TypeVoid) middle = "typedef void " ++ middle ++ ";"
ctypedef (TypeNum) middle = "typedef void " ++ middle ++ ";"

data ApatiteDec = 
      -- StructDec Name [ApatiteMember]
      TypeDec Name ApatiteType
    | UnionDec Name [ApatiteMember]
    | EnumDec Name [Name]
    | VarDec ApatiteType Name (Maybe ApatiteExpr)
    | FuncDec ApatiteType [ApatiteMember] Name ApatiteStmt

instance Show ApatiteDec where
    show t = "Declaration: " ++ showDec t

showMember (ty, name) = show ty ++ " " ++ name
showMembers sep = (intercalate sep . fmap showMember)

showSep :: Show a => String -> [a] -> String
showSep sep = intercalate sep . fmap show

showDec t = case t 
    of TypeDec name ty -> "type " ++ name ++ " = " ++ show ty
       UnionDec name members -> "union " ++ name ++ " { " ++ (showMembers "; " members) ++ " }"
       EnumDec name names -> "enum " ++ name ++ " { " ++ (intercalate ", " names) ++ " }"
       VarDec ty name (Just ex) -> show ty ++ " " ++ name ++ " = " ++ show ex
       VarDec ty name Nothing -> show ty ++ " " ++ name ++ ";"
       FuncDec ty args name st -> show ty ++ " " ++ name ++ "(" ++ (showMembers ", " args) ++ ") " ++ show st

data BinOp = BinAddOp | BinSubOp 
           | BinMulOp | BinDivOp 
           | BinModOp | BinAndOp 
           | BinOrOp  | BinXorOp 
           | BinShrOp | BinShlOp
           | BinGtOp  | BinLtOp
           | BinGeOp  | BinLeOp
           | BinEqOp  | BinNeOp
     deriving (Show, Eq)
data UnOp = UnNegOp | UnNotOp 
     deriving (Show)

data ApatiteInitExpr =
      InitExpr ApatiteExpr
    | InitList [ApatiteInitExpr]
    deriving (Show)

data ApatiteExpr =
      AssignExpr ApatiteExpr ApatiteExpr
    | CondExpr ApatiteExpr ApatiteExpr ApatiteExpr
    | BinaryExpr BinOp ApatiteExpr ApatiteExpr
    | UnaryExpr UnOp ApatiteExpr
    | SizeofExpr ApatiteType
    | AlignofExpr ApatiteType
    | CastExpr ApatiteExpr ApatiteType
    | LenExpr ApatiteExpr
    | CallExpr ApatiteExpr [ApatiteExpr]
    | IndexExpr ApatiteExpr ApatiteExpr
    | MemberExpr ApatiteExpr Name
    | AndExpr ApatiteExpr ApatiteExpr
    | OrExpr ApatiteExpr ApatiteExpr
    | NotExpr ApatiteExpr
    | DerefExpr ApatiteExpr
    | RefExpr ApatiteExpr
    | ArrayLiteral ApatiteExpr ApatiteType [ApatiteExpr]
    | StructLiteral Name [ApatiteExpr]
    | StringLiteral String
    | NumberLiteral Double
    | IdentifierExpr String
    | TrueLiteral
    | FalseLiteral
    deriving (Show)

data ApatiteStmt =
      EmptyStmt
    | DeclStmt ApatiteDec
    | BlockStmt [ApatiteStmt]
    | IfStmt ApatiteExpr ApatiteStmt (Maybe ApatiteStmt)
    | WhileStmt (Maybe Name) ApatiteExpr ApatiteStmt
    | DeferStmt ApatiteStmt
    | ContinueStmt (Maybe Name)
    | BreakStmt (Maybe Name)
    | ReturnStmt (Maybe ApatiteExpr)
    | ExprStmt ApatiteExpr
    deriving (Show)