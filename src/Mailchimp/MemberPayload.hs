{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Mailchimp.MemberPayload
    ( MemberPayload (..)
    , MergeFields (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

---------------------

data MemberPayload = MemberPayload
    { emailAddress :: Text
    , status       :: Maybe Text
    , mergeFields  :: Maybe MergeFields
    } deriving (Show)

data MergeFields = MergeFields
    { fnameMergeFields :: Maybe Text
    , lnameMergeFields :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MemberPayload
decodeTopLevel = decode

instance ToJSON MemberPayload where
    toJSON ( MemberPayload emailAddress status mergeFields) =
        object
        [ "email_address" .= emailAddress
        , "status"        .= status
        , "merge_fields"  .= mergeFields
        ]

instance FromJSON MemberPayload where
    parseJSON (Object v) =
      MemberPayload
        <$> v .: "email_address"
        <*> v .:? "status"
        <*> v .:? "merge_fields"

instance ToJSON MergeFields where
    toJSON (MergeFields fnameMergeFields lnameMergeFields) =
        object
        [ "FNAME" .= fnameMergeFields
        , "LNAME" .= lnameMergeFields
        ]

instance FromJSON MergeFields where
    parseJSON (Object v) = MergeFields
        <$> v .:? "FNAME"
        <*> v .:? "LNAME"
