module Main where
import System.Environment (getArgs)
import ApatiteParser
import AnaliserPass
import GeneratorC

defaultSymTable :: CompilerState
defaultSymTable = initialState

main :: IO ()
main = do
    (arg:_) <- getArgs

    contents <- readFile arg
    let result = parseApatite contents
    let str = either
            ((++) "ParseError: " . show ) 
            (printString . processGenerator . processPrelude)
            result
    putStrLn str
  where 
    processPrelude decls = (lookupDeclarations decls ([],[]), decls)
    processGenerator ((types, names), decls) =
        generate (defaultSymTable
            { localStack = names
            , typeStack = types
            }) decls

    printString (Right str) = str
    printString (Left err) = ("CompileError: " ++ show err)
