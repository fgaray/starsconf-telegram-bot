{-# LANGUAGE OverloadedStrings #-}
-- A collection of keyboards to interface with the user
module Keyboard where


import Data.Text (Text)
import Web.Telegram.API.Bot


-- Buttons

data Button =
      StartEnglish
    | StartSpanish
    | Notification
    | DisableNotification
    | NextTalks
    | CurrentTalks
    | Help
    deriving Show


buttonToText :: Button -> Bool -> Text
buttonToText StartEnglish _ = "Start (English)"
buttonToText StartSpanish _ = "Comenzar (Spanish)"
buttonToText Notification True = "Activate Notification"
buttonToText Notification False = "Activar Notificaciones"
buttonToText NextTalks True = "➡️ Next"
buttonToText NextTalks False = "➡️ Siguientes"
buttonToText DisableNotification True = "Disable Notifications"
buttonToText DisableNotification False = "Desactivar Notificaciones"
buttonToText CurrentTalks True = "🔥 Now"
buttonToText CurrentTalks False = "🔥 Ahora"
buttonToText Help True = "ℹ️ Help"
buttonToText Help False = "ℹ️ Ayuda"

textToButton :: Text -> Maybe Button
textToButton "Start (English)" = Just StartEnglish
textToButton "Comenzar (Spanish)" = Just StartSpanish
textToButton "/english" = Just StartEnglish
textToButton "/spanish" = Just StartSpanish
textToButton "Activate Notification" = Just Notification
textToButton "Activar Notificaciones" = Just Notification
textToButton "/notify" = Just Notification
textToButton "➡️ Next" = Just NextTalks
textToButton "➡️ Siguientes" = Just NextTalks
textToButton "/next" = Just NextTalks
textToButton "Disable Notifications" = Just DisableNotification
textToButton "Desactivar Notificaciones" = Just DisableNotification
textToButton "/disable" = Just DisableNotification
textToButton "🔥 Now" = Just CurrentTalks
textToButton "🔥 Ahora" = Just CurrentTalks
textToButton "/now" = Just CurrentTalks
textToButton "ℹ️ Help" = Just Help
textToButton "ℹ️ Ayuda" = Just Help
textToButton "/help" = Just Help
textToButton _ = Nothing




-- Utils
mkKeyboard :: [[Button]] -> Bool -> ReplyKeyboard
mkKeyboard buttons english  = ReplyKeyboardMarkup (map (map (mkB english)) buttons) Nothing (Just True) Nothing

mkB :: Bool -> Button -> KeyboardButton
mkB english b = KeyboardButton txt Nothing Nothing
    where
        txt = buttonToText b english


-- Keyboards


init :: ReplyKeyboard
init = ReplyKeyboardMarkup (map (map (mkB False)) button) Nothing (Just True) Nothing
    where
        button = [[ StartEnglish, StartSpanish ]]


main :: Bool-> Bool -> ReplyKeyboard
main activated = mkKeyboard [[ f activated, NextTalks, CurrentTalks, Help ]]
    where
        f True = DisableNotification
        f False = Notification
