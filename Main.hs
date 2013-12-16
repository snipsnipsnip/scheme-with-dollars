
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Cont
import Control.Applicative
import Data.List
import System.IO
import System.Directory
import Control.Exception (try, evaluate, PatternMatchFail (..), bracket)
import System.IO.Error (isEOFError, isUserError, catchIOError)
import Dcheme.Parser.CexpParser
import Dcheme.Interpreter

load :: FilePath -> IO (V, Env)
load file = do
    bracket (openFile file ReadMode) hClose $ \h -> do
      contents <- hGetContents h
      run contents

repl :: IO ()
repl = do
    b <- doesFileExist "init.scm"
    env <- if b
        then fmap snd $ load "init.scm"
        else return [prims]
    loop env $ \env -> do
    do
        putStr "> "
        line <- getLine
        (result, env) <- runEI env line
        print result
        return $ Just env
    `catchIOError` \e -> do
        unless (isEOFError e) (printError e)
        return $ do
            guard $ isUserError e
            return env
    where
    printError e = do
        putStr "error: "
        print e
    loop v m = do
        mv <- m v
        maybe (return ()) (flip loop m)  mv

run :: String -> IO (V, Env)
run = runEI [prims]

runEI :: Env -> String -> IO (V, Env)
runEI env code = case parseManyCexp code of
    Left e -> return (U $ "Parse error: " ++ e, env)
    Right (s, _) -> runI env $ evalBegin s
