{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB.Schema where
    import           Database.Persist          
    import           Database.Persist.TH
    import           Data.Text                 (Text, pack)
    import           Data.Aeson                

    share
        [mkPersist sqlSettings, mkMigrate "migrateAll"]
        [persistLowerCase|
    Movie json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
        name Text
        synopsis Text
        year Int
        deriving Show

    Review json
        movie MovieId
        author Text
        comment Text
        |]
