import qualified Parser.Parse as Parse
import qualified Runtime.Evaluation as Evaluation
import qualified Runtime.Runtime as Runtime
import qualified Runtime.Env as Env

load :: Runtime.Environment -> IO ()
load env = do
    command <- readFile "lib/fib.fr"
    print_ $ eval command env

eval :: String -> Runtime.Environment -> String
eval command env = Runtime.value $ Evaluation.evaluate (Parse.parse command) env

print_ :: String -> IO ()
print_ string = do putStrLn $ string

main :: IO ()
main = do
    putStrLn "Loading 'fib.fr'"
    load Env.defaultEnvironment
