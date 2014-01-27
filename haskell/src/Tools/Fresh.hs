import qualified System.Environment as SysEnv

import qualified Parser.Parse as Parse
import qualified Parser.AST as AST
import qualified Runtime.Evaluation as Evaluation
import qualified Parser.Env as Env

load_start :: String -> AST.Environment -> IO ()
load_start file_name env = do
    command <- readFile file_name
    print_ $ eval command env

loop_start :: AST.Environment -> IO ()
loop_start env = do
    putStrLn "Welcome to the Fresh Interpreter!"
    putStrLn "(+ 3 4)"
    putStrLn "7"
    _ <- loop env
    putStrLn "Exiting, goodbye"

read_ :: IO String
read_ = getLine

loop :: AST.Environment -> IO a
loop env = do
    command <- read_
    print_ $ eval command env
    loop env

eval :: String -> AST.Environment -> String
eval command env =
    let (ast, env1) = (Parse.parse command env)
    in
        AST.print_node $ Evaluation.start_evaluate ast env1

print_ :: String -> IO ()
print_ string = do putStrLn $ string

main :: IO ()
main = do
    args <- SysEnv.getArgs
    case args of
        [] -> error "Must supply file or run interactively"
        ["-i"] -> loop_start Env.defaultEnvironment
        _ -> load_start (head args) Env.defaultEnvironment
