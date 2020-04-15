{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module URLs.ErrorHandler where
    import           Control.Monad.IO.Class    (MonadIO)
    import           Data.Text                 (Text, pack)
    import           Network.HTTP.Types.Status
    import           Web.Spock
    import           Data.Aeson                hiding (json)
    import           Data.Text.Encoding        (decodeUtf8)

    errorJson :: MonadIO m => Int -> Text -> ActionCtxT ctx m ()
    errorJson code message =
        json $
        object
            [ "result" .= String "failure"
            , "error" .= object ["code" .= code, "message" .= message]
            ]

    jsonErrorHandler :: Status -> ActionCtxT ctx IO ()
    jsonErrorHandler (Status code message) = errorJson code (decodeUtf8 message)
