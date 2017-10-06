    {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module API.StarsConf where


import Servant.API
import Servant.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Aeson (decode)
import Network.HTTP.Client (Manager)
import Data.Proxy
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Database.Persist.TH (derivePersistField)
import Data.Aeson.TH
import Utils


data TalkRoom =
      DOS
    | A_
    | PRINCIPAL
    | CHICA
    | TALLERES
    deriving (Show, Read, Eq, Enum, Bounded)
$(derivePersistField "TalkRoom")



-- Types for JSON interface
------------------------------------


data Speaker = Speaker
    { speakerId     :: !String
    , speakerName   :: !String
    } deriving Show

data TimeSlot = TimeSlot
    { timeSlotId        :: !String
    , timeSlotStart     :: !String
    , timeSlotEnd       :: !String
    , timeSlotDate      :: !String
    } deriving Show

data Talk = Talk
    { talkId            :: !String
    , talkName          :: !String
    , talkTimeSlot      :: !TimeSlot
    , talkSpeaker       :: (Maybe Speaker)
    , talkRoom          :: !TalkRoom
    , talkCategory      :: !String
    , talkIsPlaceholder :: !Bool
    } deriving Show



data Talks = Talks
    { allTalks :: ![Talk]
    } deriving Show


data Speakers = Speakers
    { allSpeakers :: ![Speaker]
    } deriving Show

data TimeSlots = TimeSlots
    { allTimeSlots :: ![TimeSlot]
    }


data ResponseAPI a = ResponseAPI
    { responseAPIData :: a
    } deriving Show

$(deriveJSON defaultOptions ''Talks)
$(deriveJSON defaultOptions ''Speakers)
$(deriveJSON defaultOptions ''TimeSlots)
$(deriveJSON defaultOptions ''TalkRoom)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "speaker"}  ''Speaker)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "timeSlot"}  ''TimeSlot)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "talk"}  ''Talk)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "responseAPI"}  ''ResponseAPI)





-- API
------------------------------------

-- | Base URL for que API
baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api-starsconf.synaptic.cl" 443 ""


-- Example: https://api-starsconf.synaptic.cl/graphql.?variables=&query=query { allTalks { timeSlot {  start end } } }


type StarsConfAPI = 
         "graphql." :> QueryParam "query" String :> Post '[JSON] (ResponseAPI Talks)
    :<|> "graphql." :> QueryParam "query" String :> Post '[JSON] (ResponseAPI Speakers)
    :<|> "graphql." :> QueryParam "query" String :> Post '[JSON] (ResponseAPI TimeSlots)


starsConfAPI :: Proxy StarsConfAPI
starsConfAPI = Proxy

getTalks :<|> getSpeakers :<|> getTimeSlots = client starsConfAPI

runAPI :: ClientM a -> IO (Either ServantError a)
runAPI endpoint = do
    manager <- newManager tlsManagerSettings
    res <- runClientM endpoint (ClientEnv manager baseUrl)
    case res of
      Left err -> error $ show err
      Right x -> return . Right $ x


getTalksQuery = Just "query { allTalks { id name timeSlot { id start end date } speaker {id name} room category isPlaceholder}}"




