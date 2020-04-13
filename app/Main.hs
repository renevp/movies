{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# language RecordWildCards #-}
-- {-# language ScopedTypeVariables #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson                hiding (json)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           Data.Text.Encoding        (decodeUtf8)
import           GHC.Generics

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger      (LoggingT, runStdoutLoggingT)
import           Database.Persist          hiding (delete, get)
import qualified Database.Persist          as P
import           Database.Persist.Sqlite   hiding (delete, get)
import           Database.Persist.TH
import           Network.HTTP.Types.Status

-- import Control.Exception
-- import System.IO.Error

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

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "movie.db" 5
  spockCfg' <- defaultSpockCfg () (PCPool pool) ()
  let spockCfg = spockCfg' {spc_errorHandler = jsonErrorHandler}
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "movies" $ do
    allMovies <- runSQL $ selectList [] [Asc MovieId]
    json allMovies
  post "movies" $ do
    maybeMovie <- jsonBody :: ApiAction (Maybe Movie)
    case maybeMovie of
      Nothing -> do
        setStatus badRequest400
        errorJson 1 "Failed to parse request body as Movie"
      Just theMovie -> do
        newId <- runSQL $ insert theMovie
        setStatus created201
        json $ object ["result" .= String "success", "id" .= newId]
  get ("movies" <//> var) $ \movieId -> do
    maybeMovie <- runSQL $ P.get movieId :: ApiAction (Maybe Movie)
    case maybeMovie of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a movie with matching id"
      Just theMovie -> json theMovie
  put ("movies" <//> var) $ \movieId -> do
    maybeMovie <- jsonBody
    case (maybeMovie :: Maybe Movie) of
      Nothing -> do
        setStatus badRequest400
        errorJson 1 "Failed to parse request body as Movie"
      Just theMovie -> do
        runSQL $ replace movieId theMovie
        setStatus created201
        json $ object ["result" .= String "success", "id" .= movieId]
  delete ("movies" <//> var) $ \movieId -> do
    runSQL $ P.delete (movieId :: MovieId)
    setStatus noContent204
  post ("movies" <//> "reviews") $ do
    maybeReview <- jsonBody :: ApiAction (Maybe Review)
    case maybeReview of
      Nothing -> do
        setStatus badRequest400
        errorJson 1 "Failed to parse request body as Review"
      Just theReview -> do
        newId <- runSQL $ insert theReview
        setStatus created201
        json $ object ["result" .= String "success", "id" .= newId]
        -- `catch` (\(e :: ErrorCall) -> errorJson 3 "Other error: " ++ show e)
  get ("movies" <//> "reviews") $ do
    allReviews <- runSQL $ selectList [] [Asc ReviewId]
    json allReviews

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: MonadIO m => Int -> Text -> ActionCtxT ctx m ()
errorJson code message =
  json $
  object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

jsonErrorHandler :: Status -> ActionCtxT ctx IO ()
jsonErrorHandler (Status code message) = errorJson code (decodeUtf8 message)
