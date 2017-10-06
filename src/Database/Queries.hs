{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Queries where


import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad
import Web.Telegram.API.Bot.Data hiding (User, from)
import Data.Maybe
import qualified Web.Telegram.API.Bot.Data as Telegram
import qualified Database.Esqueleto as DB
import Database.Esqueleto
import Data.Time
import API.StarsConf

import Database.Models
import qualified Types as T



getLastOffset :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe Int)
getLastOffset = liftM (fmap unValue . listToMaybe) . select $
    from $ \u -> do
    orderBy [desc (u ^. UpdateMessageMessageId)]
    limit 1
    return (u ^. UpdateMessageUpdateId)



saveUpdateMessage :: Update -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe Update)
saveUpdateMessage u@Update{..} = do
    case message of
        Nothing -> return Nothing
        Just Message{..} -> do
            -- lets check if we already have that message
            previous <- liftM listToMaybe . select $
                DB.from $ \m -> do
                where_ (m ^. UpdateMessageUpdateId ==. val update_id)
                return m
            case previous of
                Just _ -> return Nothing
                Nothing -> do
                    userId <- sequence $ fmap getOrInsertUser from
                    insertedEntities <- sequence $ fmap (mapM insertEntityDB) entities
                    replyTo <- liftM join . sequence . fmap getReplyTo $ reply_to_message 
                    insert $ UpdateMessage update_id message_id userId date (chat_id chat) text replyTo
                    return . Just $ u


getOrInsertUser :: Telegram.User -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) UserId
getOrInsertUser Telegram.User{..} = do
    -- check if we have the user
    user <- getUserDB user_id
    case user of
        Just u -> return . entityKey $ u
        Nothing -> insert $ User user_id user_username user_first_name user_last_name True T.Init False

getUserDB :: Int -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe (Entity User))
getUserDB userId = liftM listToMaybe . select $
    from $ \u -> do
    where_ (u ^. UserUserIdTelegram ==. val userId)
    limit 1
    return u


insertEntityDB :: MessageEntity -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe EntityUpdateId)
insertEntityDB MessageEntity{..} = do
    case me_user of
        Nothing -> liftM Just . insert $ EntityUpdate me_type me_offset me_url Nothing
        Just Telegram.User{..} -> do
            user <- getUserDB user_id
            liftM Just . insert $ EntityUpdate me_type me_offset me_url (fmap entityKey user)


-- | Find the message the user is responding to
getReplyTo :: Message -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe (Key UpdateMessage))
getReplyTo Message{..} = liftM (fmap entityKey . listToMaybe) . select $
    DB.from $ \u -> do
    where_ (u ^. UpdateMessageMessageId ==. val message_id)
    limit 1
    return u


getEnglish :: UserId -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) Bool
getEnglish user = do
    englishMay <- fmap listToMaybe . select $
                from $ \u -> do
                where_ (u ^. UserId ==. val user)
                limit 1
                return (u ^. UserEnglish)
    case englishMay of
        Nothing -> return True
        Just e -> return . unValue $ e

setLanguage :: UserId -> Bool -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
setLanguage user english = do
    update $ \u -> do
        set u [ UserEnglish =. val english ]
        where_ (u ^. UserId ==. val user)


isNotifiy :: UserId -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) Bool
isNotifiy user = fmap (unValue . head) $ select $
    from $ \u -> do
    where_ (u ^. UserId ==. val user)
    return (u ^. UserNotify)


setNotify :: UserId -> Bool -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
setNotify user notify = do
    update $ \u -> do
        set u [ UserNotify =. val notify ]
        where_ (u ^. UserId ==. val user)



-- Get the next talk in all the rooms
getNextTalks :: Day -> TimeOfDay -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [(Maybe (Entity TalkDB, Maybe SpeakerDB, CategoryDB), TalkRoom)]
getNextTalks day time = do
    let rooms :: [TalkRoom] = [minBound..maxBound]
    forM rooms $ \room -> do
        talk <- fmap (fmap (\(t, c) -> (t, entityVal c)) . listToMaybe) . select $
            from $ \(t, c) -> do
            where_ (t ^. TalkDBDate ==. val day)
            where_ (t ^. TalkDBStart >=. val time)
            where_ (t ^. TalkDBRoom ==. val room)
            where_ (c ^. CategoryDBId ==. t ^. TalkDBCategory)
            limit 1
            orderBy [asc (t ^. TalkDBStart)]
            return (t, c)

        case talk of
            Nothing -> return (Nothing, room)
            Just tlk -> do
                speaker <- 
                    case talkDBSpeaker . entityVal . fst $ tlk of
                        Nothing -> return Nothing
                        Just speaker -> fmap (fmap entityVal . listToMaybe) $ select $
                            from $ \s -> do
                            where_ (s ^. SpeakerDBId ==. val speaker)
                            limit 1
                            return s
                let talk = (fst tlk, speaker, snd tlk)
                return $ (Just talk, room)


getCurrentTalks :: Day -> TimeOfDay -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [(Maybe (Entity TalkDB, Maybe SpeakerDB, CategoryDB), TalkRoom)]
getCurrentTalks day time = do
    let rooms :: [TalkRoom] = [minBound..maxBound]
    forM rooms $ \room -> do
        talk <- fmap (fmap (\(t, c) -> (t, entityVal c)) . listToMaybe) . select $
            from $ \(t, c) -> do
            where_ (t ^. TalkDBDate ==. val day)
            where_ (t ^. TalkDBStart <=. val time)
            where_ (t ^. TalkDBEnd >=. val time)
            where_ (t ^. TalkDBRoom ==. val room)
            where_ (c ^. CategoryDBId ==. t ^. TalkDBCategory)
            limit 1
            orderBy [asc (t ^. TalkDBStart)]
            return (t, c)

        case talk of
            Nothing -> return (Nothing, room)
            Just tlk -> do
                speaker <- 
                    case talkDBSpeaker . entityVal . fst $ tlk of
                        Nothing -> return Nothing
                        Just speaker -> fmap (fmap entityVal . listToMaybe) $ select $
                            from $ \s -> do
                            where_ (s ^. SpeakerDBId ==. val speaker)
                            limit 1
                            return s
                let talk = (fst tlk, speaker, snd tlk)
                return $ (Just talk, room)



-- We find the talks that are going to be notified to the users with the correct
-- settings and that where no notified before
getTalksToNotify :: Day -> TimeOfDay -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [(Entity User, [(Maybe (Entity TalkDB, Maybe SpeakerDB, CategoryDB), TalkRoom)])]
getTalksToNotify day time = do
    -- users that wants to be notified
    users <- select $
        from $ \u -> do
        where_ (u ^. UserNotify ==. val True)
        return u
    -- We notify for talks in 5 minutes ahead
    let timeMoved = timeToTimeOfDay (timeOfDayToTime time + (60*5))
    talks <- getCurrentTalks day timeMoved
    forM users $ \u -> do
        talksUser <- (flip filterM) talks $ \(m, r) -> do
            case m of
                Nothing -> return False
                Just (talk, _, _) -> do
                    valid <- fmap listToMaybe . select $
                        from $ \un -> do
                        where_ (un ^. UserNotifiedTalk ==. val (entityKey talk))
                        where_ (un ^. UserNotifiedUser ==. val (entityKey u))
                        limit 1
                        return un
                    case valid of
                        Nothing -> return True
                        Just _ -> return False
        return $ (u, talksUser)
