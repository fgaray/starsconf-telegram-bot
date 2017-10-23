{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module State where


import qualified Keyboard as K
import Web.Telegram.API.Bot (ReplyKeyboard)
import Types
import Database.Models
import qualified Messages as M
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Database.Persist hiding (Update, (==.), update, (=.))
import Database.Persist.Sqlite hiding (Update, (==.), update, (=.))
import qualified Database.Queries as DB
import Data.Maybe
import Database.Esqueleto
import Data.Time
import Data.Monoid
import API.StarsConf
import Data.Text (Text)
import qualified Data.Text as T


-- Automat to handle the incomming messages acording to the pressed button and
-- current state
nextState :: UserId -> State -> Maybe K.Button -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (State, Bool -> ReplyKeyboard, M.Message)
nextState _ Init Nothing = return (SelectLanguage, \_ -> K.init, M.Main)

-- First time
nextState user Main (Just K.StartEnglish) = do
    DB.setLanguage user True
    notify <- DB.isNotifiy user
    return (Main, K.main notify, M.Main)

-- First time
nextState user Main (Just K.StartSpanish) = do
    DB.setLanguage user False
    notify <- DB.isNotifiy user
    return (Main, K.main notify, M.Main)

nextState user Main (Just K.Notification) = do
    DB.setNotify user True
    notify <- DB.isNotifiy user
    return (Main, K.main notify, M.NotificationActivated)

nextState user Main (Just K.DisableNotification) = do
    DB.setNotify user False
    notify <- DB.isNotifiy user
    return (Main, K.main notify, M.NotificacionDeactivated)

nextState user Main (Just K.NextTalks) = do
    english <- DB.getEnglish user
    notify <- DB.isNotifiy user
    localTime <- liftIO $ fmap (utcToLocalTime (TimeZone ((-3)*60) True "CLTM") . zonedTimeToUTC) getZonedTime
    let day = localDay localTime
        time = localTimeOfDay localTime
        {-day = fromGregorian 2017 11 4-}
        {-time = TimeOfDay 16 0 0-}
    talks <- DB.getNextTalks day time
    let talksTxt = foldl (\acc t -> acc <> buildTalkText (entity t) english M.NoTalks <> "\n\n") "" talks
    return (Main, K.main notify, M.NextTalks talksTxt)


nextState user Main (Just K.CurrentTalks) = do
    english <- DB.getEnglish user
    notify <- DB.isNotifiy user
    localTime <- liftIO $ fmap (utcToLocalTime (TimeZone ((-3)*60) True "CLTM") . zonedTimeToUTC) getZonedTime
    let day = localDay localTime
        time = localTimeOfDay localTime
        {-day = fromGregorian 2017 11 4-}
        {-time = TimeOfDay 16 0 0-}
    talks <- DB.getCurrentTalks day time
    let talksTxt = foldl (\acc t -> acc <> buildTalkText (entity t) english M.NoTalksNow <> "\n\n") "" talks
    return (Main, K.main notify, M.CurrentTalks talksTxt)

nextState user state btn = do
    liftIO $ putStrLn $ "Other: " ++ show state ++ " " ++ show btn
    notify <- DB.isNotifiy user
    return (Main, K.main notify, M.Main)


entity :: (Maybe (Entity TalkDB, Maybe SpeakerDB, CategoryDB), TalkRoom) -> (Maybe (TalkDB, Maybe SpeakerDB, CategoryDB), TalkRoom)
entity (Nothing, r) = (Nothing, r)
entity ((Just (tlk, spk, c)), r) = (Just (entityVal tlk, spk, c), r)




-- The DB modification
nextStateDB :: UserId -> Maybe K.Button -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Bool -> ReplyKeyboard, M.Message)
nextStateDB user btn = do
    currentState <- fmap listToMaybe . select $
        from $ \u -> do
        where_ (u ^. UserId ==. val user)
        limit 1
        return (u ^. UserState)
    case currentState of
        Nothing -> return (\_ -> K.init, M.SelectLanguage)
        Just st -> do
            (next, kbr, msg) <- nextState user (unValue st) btn
            update $ \u -> do
                set u [ UserState =. val next ]
                where_ (u ^. UserId ==. val user)
            return (kbr, msg)


buildTalkText :: (Maybe (TalkDB, Maybe SpeakerDB, CategoryDB), TalkRoom) -> Bool -> M.Message -> Text
buildTalkText (Just (talk, speaker, category), room) english _ =
    T.pack (show room) <>
    ":\n↳" <> talkDBName talk <>
    speakerName <>
    "\n⏰ " <> (T.pack . formatTime defaultTimeLocale "%R" $ talkDBStart talk) <>" - " <> 
    (T.pack . formatTime defaultTimeLocale "%R" $ talkDBEnd talk) <> ", total: " <> (T.pack . formatTime defaultTimeLocale "%R" $ diffMinutes) <> 
    categoryTxt
    where
        diffMinutes = timeToTimeOfDay (secondsEnd - secondsStart)
        secondsStart = timeOfDayToTime (talkDBStart talk)
        secondsEnd = timeOfDayToTime (talkDBEnd talk)

        speakerName =
            case speaker of
                Nothing -> ""
                Just spk -> "\n👨👩" <> speakerDBName spk
                
        categoryTxt =
            case categoryDBName category of
                "" -> ""
                txt -> "\n🔖" <> txt

buildTalkText (Nothing, room) english msg = T.pack (show room) <> ": " <> M.messageToText msg english
