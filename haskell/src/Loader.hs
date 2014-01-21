import qualified Parse
import qualified Evaluation
import qualified Runtime
import qualified Env

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
