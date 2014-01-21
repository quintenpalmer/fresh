import qualified Parse
import qualified Evaluation
import qualified Runtime
import qualified Env


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


main :: IO a
main = do
    putStrLn "Welcome to the Fresh Interpreter!"
    putStrLn "(+ 3 4)"
    putStrLn "7"
    loop Env.defaultEnvironment
