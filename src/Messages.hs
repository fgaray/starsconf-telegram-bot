{-# LANGUAGE OverloadedStrings #-}
module Messages where


import Data.Text (Text)
import Data.Monoid

data Message =
      SelectLanguage
    | LanguageSet
    | Main
    | NotificationActivated
    | NotificacionDeactivated
    | NextTalks Text
    | CurrentTalks Text
    | NoTalks
    | NoTalksNow
    | Notify Text



messageToText :: Message -> Bool -> Text
messageToText SelectLanguage _ = "Please select an option / Selecciona una opción"
messageToText LanguageSet True = "English selected"
messageToText LanguageSet False = "Seleccionado Español"
messageToText Main True = "Select an option"
messageToText Main False = "Selecciona una opción"
messageToText NotificationActivated True = "Notificacion activated: You will be notified of all the next talks on the conference!"
messageToText NotificationActivated False = "Notificaciones activadas: Serás notificado sobre las siguientes charlas en la conferencia!"
messageToText (NextTalks talks) True = "➡️ The next talks are:\n" <> talks
messageToText (NextTalks talks) False = "➡️ Las siguientes charlas son:\n" <> talks
messageToText (CurrentTalks talks) True = "🔥 Right now:\n" <> talks
messageToText (CurrentTalks talks) False = "🔥 En este momento:\n" <> talks
messageToText NoTalks True = "There are no talks in this room today"
messageToText NoTalks False = "No hay charlas en esta sala hoy"
messageToText NoTalksNow True = "There are no talks right now"
messageToText NoTalksNow False = "No hay charlas en este momento"
messageToText (Notify talks) True = "🔜 Up next:\n" <> talks
messageToText (Notify talks) False = "🔜 A continuación:\n" <> talks
messageToText NotificacionDeactivated True = "Notificacion deactivated: You will no longer be notified about next talks of the conference"
messageToText NotificacionDeactivated False = "Notificaciones desactivadas: No serás notificado de las charlas en la conferencia"
