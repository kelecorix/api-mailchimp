{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Mailchimp.TransactionalMail
    ( TransactionalMail (..)
    , Message (..)
    , To (..)
    , KeyWrap(..)
    , MCResponse(..)
    , MCResponseElement(..)
    , decodeTopLevel
    , decodeMCResponse
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)


----------------------------------------------
data KeyWrap =
  KeyWrap
    { key :: Text
    } deriving (Eq, Show)

instance ToJSON KeyWrap where
    toJSON (KeyWrap k) =
        object
          [ "key" .= k
          ]

----------------------

type MCResponse = [MCResponseElement]

data MCResponseElement = MCResponseElement
    { emailMCResponseElement :: Maybe Text
    , statusMCResponseElement :: Maybe Text
    , mcResponseIDMCResponseElement :: Maybe Text
    , rejectReasonMCResponseElement :: Maybe (Maybe Text)
    } deriving (Show)

decodeMCResponse :: ByteString -> Maybe MCResponse
decodeMCResponse = decode

instance ToJSON MCResponseElement where
    toJSON (MCResponseElement emailMCResponseElement statusMCResponseElement mcResponseIDMCResponseElement rejectReasonMCResponseElement) =
        object
        [ "email" .= emailMCResponseElement
        , "status" .= statusMCResponseElement
        , "_id" .= mcResponseIDMCResponseElement
        , "reject_reason" .= rejectReasonMCResponseElement
        ]

instance FromJSON MCResponseElement where
    parseJSON (Object v) = MCResponseElement
        <$> v .:? "email"
        <*> v .:? "status"
        <*> v .:? "_id"
        <*> v .:? "reject_reason"



----------

data TransactionalMail = TransactionalMail
    { keyTransactionalMail :: Maybe Text
    , messageTransactionalMail :: Maybe Message
    } deriving (Show)

data Message = Message
    { fromEmailMessage :: Maybe Text
    , subjectMessage :: Maybe Text
    , messageTextMessage :: Maybe Text
    , toMessage :: Maybe ([To])
    } deriving (Show)

data To = To
    { emailTo :: Maybe Text
    , toTypeTo :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe TransactionalMail
decodeTopLevel = decode

instance ToJSON TransactionalMail where
    toJSON (TransactionalMail keyTransactionalMail messageTransactionalMail) =
        object
        [ "key" .= keyTransactionalMail
        , "message" .= messageTransactionalMail
        ]

instance FromJSON TransactionalMail where
    parseJSON (Object v) = TransactionalMail
        <$> v .:? "key"
        <*> v .:? "message"

instance ToJSON Message where
    toJSON (Message fromEmailMessage subjectMessage messageTextMessage toMessage) =
        object
        [ "from_email" .= fromEmailMessage
        , "subject" .= subjectMessage
        , "text" .= messageTextMessage
        , "to" .= toMessage
        ]

instance FromJSON Message where
    parseJSON (Object v) = Message
        <$> v .:? "from_email"
        <*> v .:? "subject"
        <*> v .:? "text"
        <*> v .:? "to"

instance ToJSON To where
    toJSON (To emailTo toTypeTo) =
        object
        [ "email" .= emailTo
        , "type"  .= toTypeTo
        ]

instance FromJSON To where
    parseJSON (Object v) = To
        <$> v .:? "email"
        <*> v .:? "type"
