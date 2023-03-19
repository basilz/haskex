module Ch6.Transformers where

import Control.Monad.Reader
  ( Reader,
    ReaderT (runReaderT),
    ask,
    asks,
    runReader,
  )
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.Strict
import Data.Foldable
import Data.Time.Clock.POSIX
import qualified System.Posix.Unistd as U

data ProtectedData a = ProtectedData String a

accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData pwd v) =
  if s == pwd then Just v else Nothing

type Protected s a = MaybeT (Reader (ProtectedData s)) a

run :: ProtectedData s -> Protected s a -> Maybe a
run d protected = runReader (runMaybeT protected) d

access :: String -> Protected a a
access pwd = do
  d <- ask
  MaybeT $ return $ accessData pwd d

-- case accessData pwd d of
--   Nothing -> fail "Incorrect password"
--   Just v -> return v

access'' :: String -> Protected a a
access'' = MaybeT . asks . accessData

type Protected' s a = MaybeT (ReaderT (ProtectedData s) IO) a

run' :: ProtectedData s -> Protected' s a -> IO (Maybe a)
run' d protected = runReaderT (runMaybeT protected) d

access' :: Protected' a a
access' = do
  pwd <- liftIO getLine
  d <- lift ask
  MaybeT $ return (accessData pwd d)

-- main :: IO ()
-- main = do
--   r <- run' (ProtectedData "test" (12345 :: Integer)) access'
--   print r

data Item = Msg String | Section String [Item] deriving (Show, Eq)

type Log = [Item]

type Logging a = Writer Log a

-- ‘log s‘ logs the messages ‘s‘.
log :: Show t => t -> Logging ()
log s = tell [Msg $ show s]

-- ‘with_section s m‘ executes m and add its log in a section titled ‘s‘.
withSection :: String -> Logging a -> Logging a
withSection s m = pass $ m >>= \x -> pure (x, \w -> [Section s w])

runLogging :: Logging a -> (a, Log)
runLogging = runWriter

-- main :: IO ()
-- main = do
--     let (_, items) = runLogging $ withSection "Sec1" $ Ch6.Transformers.log "test1"
--     print items

data Item' = Msg' POSIXTime String | Section' POSIXTime POSIXTime String [Item'] deriving (Show, Eq)

type Log' = [Item']

type Logging' a = WriterT Log' IO a

runLogging' :: Logging' a -> IO (a, Log')
runLogging' = runWriterT

log' :: Show t => t -> Logging' ()
log' s = do
  t <- liftIO getPOSIXTime
  tell [Msg' t (show s)]

withSection' :: String -> Logging' a -> Logging' a
withSection' s m = pass $ do
  t1 <- liftIO getPOSIXTime
  x <- m
  -- (a, items) <- liftIO $ runLogging' m
  t2 <- liftIO getPOSIXTime
  return (x, \it -> [Section' t1 t2 s it])

-- main :: IO ()
-- main = do
--     (_, items) <- runLogging' $ do
--      withSection' "Sec1" $ do
--         log' "test1"
--         liftIO $ U.sleep 5
--         log' "test2"
--      withSection' "Sec2" $ do
--         log' "test3"
--         liftIO $ U.sleep 5
--         log' "test4"
--     traverse_ print items

main :: IO ()
main = do
  (_, items) <-
    runLogging' $
      withSection'
        "Sec1"
        ( log' "test1"
            >> liftIO (U.sleep 5)
            >> log' "test2"
        )
  traverse_ print items
