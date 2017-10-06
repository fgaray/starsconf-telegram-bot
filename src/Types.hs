{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Database.Persist.TH (derivePersistField)

data State =
      Init
    | SelectLanguage
    | Main
    deriving (Read, Show, Eq)
$(derivePersistField "State")
