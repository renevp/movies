{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Logger      (runStdoutLoggingT)
import           Database.Persist
import           Database.Persist.Sqlite

import           DB.Schema
import           URLs.Api
import           URLs.ErrorHandler

main :: IO ()
main = do
    -- Define DB and pool settings
    pool <- runStdoutLoggingT $ createSqlitePool "movie.db" 5
    -- Configure Spock -> default settings Session DB GlobalState
    spockCfg' <- defaultSpockCfg () (PCPool pool) ()
    -- Configure error handler
    let spockCfg = spockCfg' {spc_errorHandler = jsonErrorHandler}
    -- Run migrations
    runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
    -- Create Wai.Middleware and run
    runSpock 8080 (spock spockCfg app)
