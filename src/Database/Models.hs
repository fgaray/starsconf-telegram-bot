{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Database.Models where

import Database.Persist hiding (Update, (==.))
import Database.Persist.Sqlite hiding (Update, (==.))
import Database.Persist.TH
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Web.Telegram.API.Bot.Data hiding (User, from)
import qualified Web.Telegram.API.Bot.Data as Telegram
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto
import qualified Database.Esqueleto as DB
import Data.Maybe
import Data.Traversable (sequence)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import Data.Int
import Data.List (sortBy)
import Data.Monoid
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Persist.TH
import API.StarsConf
import Database.Esqueleto
import Data.Time.Format (parseTimeM, iso8601DateFormat, defaultTimeLocale)
import Data.Time.LocalTime (TimeOfDay, LocalTime(..))
import Data.Time.Calendar (Day)
import Types
import System.FileLock

-- Types for the DB



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

-- Telegram related models

UpdateMessage
    updateId Int
    messageId Int
    from UserId Maybe
    date Int
    chat Int64
    message Text Maybe
    replyTo UpdateMessageId Maybe
    deriving Show

EntityMessage
    message UpdateMessageId
    entity EntityUpdateId
    deriving Show

EntityUpdate
    typeEntity Text
    offset Int
    url Text Maybe 
    user UserId Maybe

User
    userIdTelegram Int
    userName Text Maybe
    firstName Text
    lastName Text Maybe
    english Bool
    state State
    notify Bool
    deriving Show


UserNotified
    user UserId
    talk TalkDBId

-- Stars conf related models

SpeakerDB
    name Text
    deriving Show

CategoryDB
    name Text
    deriving Show

TalkDB
    name Text
    speaker SpeakerDBId Maybe
    room TalkRoom
    category CategoryDBId
    isPlaceholder Bool
    start TimeOfDay
    end TimeOfDay
    date Day
    deriving Show


|]



runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDB query = do
    lock <- lockFile "lock" Exclusive
    x <- runSqlite "stars-conf.sqlite3" query
    unlockFile lock
    return x

migrate = runDB $ runMigration migrateAll


-- Updates the internal database using the API of the Stars Conf
updateAPI :: IO ()
updateAPI = do
    talks <- runAPI (getTalks getTalksQuery)
    case talks of
        Left err -> print err
        Right (ResponseAPI tlks) -> mapM_ (runDB . createTalk) (allTalks tlks)
    return ()

    where
        createTalk :: Talk -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
        createTalk Talk{..} = do
            speaker <- do
                        case talkSpeaker of
                            Nothing -> return Nothing
                            Just spkr -> do
                                repsert (toSqlKey . read . speakerId $ spkr) (SpeakerDB (T.pack . speakerName $ spkr))
                                return . Just . toSqlKey . read . speakerId $ spkr
            category <- insertOrGetCategory (T.pack talkCategory)

            let start = (timeSlotDate talkTimeSlot) ++ "T" ++ (timeSlotStart talkTimeSlot)
                end = (timeSlotDate talkTimeSlot) ++ "T" ++ (timeSlotEnd talkTimeSlot)
            
            dateStart <- parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) start
            dateEnd <- parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) end

            let date = localDay dateStart
                timeStart = localTimeOfDay dateStart
                timeEnd = localTimeOfDay dateEnd

            let talk = TalkDB (T.pack talkName) speaker talkRoom category talkIsPlaceholder timeStart timeEnd date
            repsert (toSqlKey . read $ talkId) talk


        insertOrGetCategory :: Text -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Key CategoryDB)
        insertOrGetCategory category = do
            cat <- liftM listToMaybe . select $
                    from $ \c -> do
                    where_ (c ^. CategoryDBName ==. val category)
                    limit 1
                    return c
            case cat of
                Nothing -> insert $ CategoryDB category
                Just c -> return $ entityKey c
