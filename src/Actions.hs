{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Actions (processAction, sendText) where


import Web.Telegram.API.Bot
import Control.Monad.IO.Class
import qualified Keyboard as K
import Data.Text (Text)
import qualified Database.Models as DB
import qualified Database.Queries as DB
import Database.Persist hiding (Update, (==.))
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.Sqlite hiding (Update, (==.))
import qualified Messages as M
import Data.Maybe
import qualified State as S


runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> TelegramClient a
runDB = liftIO . DB.runDB


processAction :: Update -> TelegramClient ()
processAction u@Update{..} = do
    case message of
        Nothing -> return ()
        Just Message{..} -> do
            let chatId = ChatId . chat_id $ chat
            userId <- runDB . sequence $ fmap DB.getOrInsertUser from
            case userId of
                Nothing -> return ()
                Just userId -> 
                    case text of
                        Nothing -> return ()
                        Just msg ->
                            case msg of
                                "/start" -> sendKeyboard chatId userId M.SelectLanguage (\_ -> K.init)
                                txt ->
                                    case K.textToButton txt of
                                        Nothing -> sendText chatId "Invalid option"
                                        Just btn -> handleButton btn chatId  userId



sendKeyboard :: ChatId -> DB.UserId -> M.Message -> (Bool -> ReplyKeyboard) -> TelegramClient ()
sendKeyboard chat_id user msg kbr = do
    english <- runDB $ DB.getEnglish user
    let request = SendMessageRequest chat_id (M.messageToText msg english) Nothing Nothing Nothing Nothing (Just $ kbr english)
    sendMessageM request
    return ()

sendText :: ChatId -> Text -> TelegramClient ()
sendText chat_id txt = do
    let request = SendMessageRequest chat_id txt Nothing Nothing Nothing Nothing Nothing
    sendMessageM request
    return ()

sendMessage :: ChatId -> DB.UserId -> M.Message -> TelegramClient ()
sendMessage chat_id user msg = do
    english <- runDB $ DB.getEnglish user
    let request = SendMessageRequest chat_id (M.messageToText msg english) Nothing Nothing Nothing Nothing Nothing
    sendMessageM request
    return ()


-- Handle Button

handleButton :: K.Button -> ChatId -> DB.UserId -> TelegramClient ()
handleButton btn chat_id user = do
    (kbr, msg) <- runDB $ S.nextStateDB user (Just btn)
    sendKeyboard chat_id user msg kbr
