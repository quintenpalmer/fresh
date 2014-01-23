import qualified System.Environment as SysEnv

import qualified Parser.Parse as Parse
import qualified Runtime.Evaluation as Evaluation
import qualified Runtime.Runtime as Runtime
import qualified Runtime.Env as Env

load_start :: String -> Runtime.Environment -> IO ()
load_start file_name env = do
    command <- readFile file_name
    print_ $ eval command env

loop_start :: Runtime.Environment -> IO ()
loop_start env = do
    putStrLn "Welcome to the Fresh Interpreter!"
    putStrLn "(+ 3 4)"
    putStrLn "7"
    loop env
    putStrLn "Exiting, goodbye"

read_ :: IO String
read_ = getLine

loop :: Runtime.Environment -> IO a
loop env = do
    command <- read_
    print_ $ eval command env
    loop env

eval :: String -> Runtime.Environment -> String
eval command env = Runtime.value $ Evaluation.evaluate (Parse.parse command) env

print_ :: String -> IO ()
print_ string = do putStrLn $ string

main :: IO ()
main = do
    args <- SysEnv.getArgs
    case args of
        [] -> error "Must supply file or run interactively"
        ["-i"] -> loop_start Env.defaultEnvironment
        _ -> load_start (head args) Env.defaultEnvironment
