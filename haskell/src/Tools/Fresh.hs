import qualified System.Environment as SysEnv
import qualified Data.Map as Map

import qualified Tokens.Tokens as Tokens
import qualified Lexer.Lexer as Lexer
import qualified Parser.Parse as Parse
import qualified AST.AST as AST
import qualified AST.BuiltinEnv as BuiltinEnv
import qualified Runtime.Evaluation as Evaluation

load_start :: String -> AST.Environment -> IO ()
load_start filename env = do
    command <- readFile filename
    print_ $ eval command env

print_tokens_start :: String -> IO ()
print_tokens_start filename = do
    command <- readFile filename
    putStrLn $ Tokens.print_tokens $ Lexer.make_tokens command

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
    putStrLn "help\n-i\nprint_env <filename>\neval_env <filename>\nprint_tokens <filename>\n<filename>"

read_ :: IO String
read_ = getLine

loop :: AST.Environment -> IO a
loop env = do
    command <- read_
    print_ $ eval command env
    loop env

eval :: String -> AST.Environment -> String
eval command env =
    show $ Evaluation.start_evaluate (Parse.parse (Lexer.make_tokens command) env)


print_evaled_ast :: String -> AST.Environment -> String
print_evaled_ast command env =
    "User Defined:\n" ++ (AST.print_env (Map.difference
        (Evaluation.create_evaluated_env (Parse.parse (Lexer.make_tokens command) env))
        env)) ++
    "\nBuiltin:\n" ++ AST.print_env env

print_ast :: String -> AST.Environment -> String
print_ast command env =
    "User Defined:\n" ++ (AST.print_env $ Map.difference (Parse.parse (Lexer.make_tokens command) env) env) ++
    "\nBuiltin:\n" ++ AST.print_env env

print_ :: String -> IO ()
print_ string = do putStrLn $ string

main :: IO ()
main = do
    args <- SysEnv.getArgs
    case args of
        [] -> error "Must supply file or run interactively"
        ["-i"] -> loop_start BuiltinEnv.defaultEnvironment
        ["help"] -> help_start
        ["print_env", name] -> print_env_start name BuiltinEnv.defaultEnvironment
        ["eval_env", name] -> print_evaled_env_start name BuiltinEnv.defaultEnvironment
        ["print_tokens", name] -> print_tokens_start name
        _ -> load_start (head args) BuiltinEnv.defaultEnvironment
