{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module URLs.Api where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson                hiding (json)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           GHC.Generics

import           Control.Monad.Logger      (LoggingT, runStdoutLoggingT)
import           Database.Persist          hiding (delete, get)
import qualified Database.Persist          as P
import           Database.Persist.Sqlite   hiding (delete, get)
import           Database.Persist.TH
import           Network.HTTP.Types.Status

import           URLs.ErrorHandler
import           DB.Schema

-- import Control.Exception
-- import System.IO.Error

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

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
