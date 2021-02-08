module AnaliserPass where
import Syntax

type TypeName = (TypeMode, ApatiteType)

data TypeMode = 
      IncompleteType
    | ResolvingType
    | ConcreteType
    deriving (Show)

data Symbol = Symbol
    { sym_name :: String
    , sym_type :: ApatiteType
    , sym_meta :: Int 
    , sym_level :: Int
    } deriving (Show)

lookupDeclarations :: [ApatiteDec] -> ([(Name, TypeName)], [Symbol]) -> ([(Name, TypeName)], [Symbol])

lookupDeclarations [] st = st

lookupDeclarations ((TypeDec name type_):decls) (types, names) = 
    lookupDeclarations decls ((name, (IncompleteType, type_)) : types, names)

lookupDeclarations ((EnumDec name _):decls) (types, names) = 
    lookupDeclarations decls ((name, (IncompleteType, TypeDef name)) : types, names)

lookupDeclarations ((UnionDec _ _):decls) (types, names) = 
    lookupDeclarations decls (types, names)

lookupDeclarations ((VarDec type_ name _):decls) (types, names) = 
    lookupDeclarations decls (types, Symbol name type_ 0 0 : names)

lookupDeclarations ((FuncDec type_ args name _):decls) (types, names) = 
    lookupDeclarations decls (types, Symbol name (TypeFunc type_ (fst $ unzip args)) 0 0 : names)