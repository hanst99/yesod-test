module Main(main) where

import App(runApp)
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative
import System.Environment
import Debug.Trace

data Opts = Opts {
    port :: Int
} deriving Show

main :: IO ()
main = retrieveOpts >>= (runApp . port)

defaultOpts :: Opts
defaultOpts = Opts {port = 80}

type ArgReader a = ErrorT String (State [String]) a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

maybeRead :: Read a => String -> Maybe a
maybeRead s = fst <$> (safeHead . reads) s

popArg :: ArgReader String
popArg = get >>= \args -> case args of
    [] -> throwError "Expected more args!"
    (x:xs) -> put xs >> return x

getPort :: ArgReader Int
getPort = do
   pprefix <- popArg
   when (pprefix /= "-p") $ throwError "Expected prefix -p!"
   p <- maybeRead <$> popArg
   maybe (throwError "Couldn't parse port!") return p

getOpts :: Opts -> ArgReader Opts
getOpts opts = noArgsLeft <|> (setPort >>= getOpts) where
    noArgsLeft = do
        noneLeft <- gets null
        unless noneLeft $ throwError "Expected args to be empty but they aren't!"
        return opts
    setPort = getPort >>= \p -> return opts{port=p}


retrieveOpts :: IO Opts
retrieveOpts = do
    (res,_) <- (runState $ runErrorT $ getOpts defaultOpts) <$> getArgs
    either fail return res


traceThis :: Show a => a -> a
traceThis = traceShow <$> id <*> id
