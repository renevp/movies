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
    pool <- runStdoutLoggingT $ createSqlitePool "movie.db" 5
    spockCfg' <- defaultSpockCfg () (PCPool pool) ()
    let spockCfg = spockCfg' {spc_errorHandler = jsonErrorHandler}
    runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
    runSpock 8080 (spock spockCfg app)
