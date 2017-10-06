{-# LANGUAGE OverloadedStrings #-}
module Lib (botMain) where


import Database.Models (migrate)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import System.Environment (getEnv)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client (Manager)
import Keyboard
import Database.Queries
import Database.Models
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad.Reader (ask)
import Control.Monad (forever, void, forM_)
import Data.Maybe
import Actions
import qualified State as S
import qualified Database.Queries as DB
import qualified Messages as M
import Data.Monoid
import qualified Database.Esqueleto as E
import Database.Persist hiding (Update, (==.))
import Database.Persist.Sqlite hiding (Update, (==.), migrate)
import Data.Maybe
import Data.Time


import Web.Telegram.API.Bot

-- | The main entry point of the bot. Here we obtain the TOKEN from the
-- enviroment variable and initialize the server to listen to new messages from
-- telegram.
botMain :: IO ()
botMain = do
    -- we need to do the migration of the DB
    migrate
    token <- Token . T.pack <$> getEnv "TOKEN"
    manager <- newManager tlsManagerSettings
    putStrLn "Updating talks..."
    updateAPI
    putStrLn "Starting notify thread..."
    forkIO $ void $ runClient (notifyThread manager) token manager
    putStrLn "Running"
    exit <- runClient (client manager) token manager
    print exit
    return ()


client :: Manager -> TelegramClient ()
client manager = forever $ do
    lastOffset <- liftIO . runDB $ getLastOffset
    token <- ask
    updates <- liftIO $ getUpdates token lastOffset Nothing Nothing  manager
    case updates of
        Left err -> liftIO $ print err
        Right us -> do
            newUpdates <- fmap catMaybes . liftIO $ runDB (mapM saveUpdateMessage (result us))
            mapM_ processAction newUpdates
    liftIO $ threadDelay 1000000


notifyThread :: Manager -> TelegramClient ()
notifyThread manager = forever $ do
    localTime <- liftIO $ fmap (utcToLocalTime (TimeZone ((-3)*60) True "CLTM") . zonedTimeToUTC) getZonedTime
    let day = localDay localTime
        time = localTimeOfDay localTime
    toNotify <- liftIO . runDB $ getTalksToNotify day time

    forM_ toNotify $ \(user, talks) -> do
        english <- liftIO . runDB $ DB.getEnglish (entityKey user)
        chatId <- liftIO . runDB . fmap (fmap E.unValue . listToMaybe) . E.select $ do
            E.from $ \um -> do
                E.where_ (um E.^. UpdateMessageFrom E.==. E.just (E.val $ entityKey user))
                return (um E.^. UpdateMessageChat)
        case chatId of
            Nothing -> return ()
            Just cId -> do
                let talksF = filter (\(tlk, _) -> isJust tlk) talks
                    talksTxt = foldl (\acc t -> acc <> S.buildTalkText (S.entity t) english M.NoTalksNow <> "\n\n") "" talksF
                if null talksF
                    then return ()
                    else do
                        sendText (ChatId cId) (M.messageToText (M.Notify talksTxt) english)
                        forM_ talksF $ \((Just (t, _, _)), _) -> liftIO . runDB . insert $ UserNotified (entityKey user) (entityKey t)


    liftIO $ threadDelay 3000000
