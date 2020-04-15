{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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

-- App configuration (SpockM conn sess st a) monad
type Api = SpockM SqlBackend () () ()

-- Represents actions in the app
type ApiAction a = SpockAction SqlBackend () () a

app :: Api
app = do
-- Fecth all movies
-- Example: curl http://localhost:8080/movies | jq .
  get "movies" $ do
    allMovies <- runSQL $ selectList [] [Asc MovieId]
    json allMovies

-- Save movie
-- Example: curl -H "Content-Type: application/json" -d '{ "name": "Marriage Story", "synopsis": "Drama movie", "year": 2020 }' http://localhost:8080/movies | jq .
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

-- Get a movie by MovieId
-- Example: curl http://localhost:8080/movies/1 | jq .
  get ("movies" <//> var) $ \movieId -> do
    maybeMovie <- runSQL $ P.get movieId :: ApiAction (Maybe Movie)
    case maybeMovie of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a movie with matching id"
      Just theMovie -> json theMovie

-- Update movie
-- Example: curl -XPUT -H "Content-Type: application/json" -d '{ "name": "Doctor Sleep", "synopsis": "Fantasy movie", "year": 2020 }' http://localhost:8080/movies/5 | jq .
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

-- Delete a movie
-- Example: curl -XDELETE  http://localhost:8080/movies/6 | jq .
  delete ("movies" <//> var) $ \movieId -> do
    maybeMovie <- runSQL $ P.get movieId :: ApiAction (Maybe Movie)
    case maybeMovie of
      Nothing -> do 
        setStatus noContent204
        errorJson 4 "Could not find a movie with matching id"
      Just theMovie -> do
        runSQL $ P.delete movieId 
        json $ object ["result" .= String "deleted", "id" .= movieId]

-- Save a review
-- Example: curl -H "Content-Type: application/json" -d '{ "movie": 5, "author": "Rene", "comment": "No too bad" }' http://localhost:8080/movies/reviews | jq .
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

-- Fetch reviews
-- Example: curl http://localhost:8080/movies/reviews | jq .
  get ("movies" <//> "reviews") $ do
    allReviews <- runSQL $ selectList [] [Asc ReviewId]
    json allReviews

-- Helper function for running queries
runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn
