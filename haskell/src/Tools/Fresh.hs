import qualified System.Environment as SysEnv
import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tokenize
import qualified Parser.Parse as Parse
import qualified Parser.AST as AST
import qualified Runtime.Evaluation as Evaluation
import qualified Parser.Env as Env

load_start :: String -> AST.Environment -> IO ()
load_start filename env = do
    command <- readFile filename
    print_ $ eval command env

print_tokens_start :: String -> IO ()
print_tokens_start filename = do
    command <- readFile filename
    putStrLn $ Tokenize.print_tokens $ Tokenize.make_tokens command

loop_start :: AST.Environment -> IO ()
loop_start env = do
    print_ "Welcome to the Fresh Interpreter!"
    print_ "(define main (+ 3 4))"
    print_ "7"
    _ <- loop env
    putStrLn "Exiting, goodbye"

print_env_start :: String -> AST.Environment -> IO ()
print_env_start filename env = do
    command <- readFile filename
    putStrLn $ print_ast command env

print_evaled_env_start :: String -> AST.Environment -> IO ()
print_evaled_env_start filename env = do
    command <- readFile filename
    putStrLn $ print_evaled_ast command env

help_start :: IO ()
help_start = do
    putStrLn "help\n-i\nprint_env <filename>\neval_env <filename>\n<filename>"

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


print_evaled_ast :: String -> AST.Environment -> String
print_evaled_ast command env =
    let (_, env1) = (Parse.parse command env)
        env2 = Evaluation.create_evaluated_env env1
    in
        "User Defined:\n" ++ (AST.print_env $ Map.difference env2 env) ++
        "\nBuiltin:\n" ++ AST.print_env env

print_ast :: String -> AST.Environment -> String
print_ast command env =
    let (_, env1) = (Parse.parse command env)
    in
        "User Defined:\n" ++ (AST.print_env $ Map.difference env1 env) ++
        "\nBuiltin:\n" ++ AST.print_env env

print_ :: String -> IO ()
print_ string = do putStrLn $ string

main :: IO ()
main = do
    args <- SysEnv.getArgs
    case args of
        [] -> error "Must supply file or run interactively"
        ["-i"] -> loop_start Env.defaultEnvironment
        ["help"] -> help_start
        ["print_env", name] -> print_env_start name Env.defaultEnvironment
        ["eval_env", name] -> print_evaled_env_start name Env.defaultEnvironment
        ["print_tokens", name] -> print_tokens_start name
        _ -> load_start (head args) Env.defaultEnvironment
