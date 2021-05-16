{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Mailchimp.MemberResponse
    ( MemberResponse (..)
    , Link (..)
    , Location (..)
    , MarketingPermission (..)
    , MergeFields (..)
    , Stats (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data MemberResponse = MemberResponse
    { purpleIDMemberResponse :: Text
    , emailAddressMemberResponse :: Text
    , uniqueEmailIDMemberResponse :: Text
    , webIDMemberResponse :: Int
    , emailTypeMemberResponse :: Text
    , statusMemberResponse :: Text
    , mergeFieldsMemberResponse :: MergeFields
    , statsMemberResponse :: Stats
    , ipSignupMemberResponse :: Text
    , timestampSignupMemberResponse :: Text
    , ipOptMemberResponse :: Text
    , timestampOptMemberResponse :: Text
    , memberRatingMemberResponse :: Int
    , lastChangedMemberResponse :: Text
    , languageMemberResponse :: Text
    , vipMemberResponse :: Bool
    , emailClientMemberResponse :: Text
    , locationMemberResponse :: Location
    , marketingPermissionsMemberResponse :: [MarketingPermission]
    , sourceMemberResponse :: Text
    , tagsCountMemberResponse :: Int
    , tagsMemberResponse :: [(Maybe Text)]
    , listIDMemberResponse :: Text
    , linksMemberResponse :: [Link]
    } deriving (Show)

data Link = Link
    { relLink :: Text
    , hrefLink :: Text
    , methodLink :: Text
    , targetSchemaLink :: Maybe Text
    , schemaLink :: Maybe Text
    } deriving (Show)

data Location = Location
    { latitudeLocation :: Int
    , longitudeLocation :: Int
    , gmtoffLocation :: Int
    , dstoffLocation :: Int
    , countryCodeLocation :: Text
    , timezoneLocation :: Text
    } deriving (Show)

data MarketingPermission = MarketingPermission
    { marketingPermissionIDMarketingPermission :: Text
    , marketingPermissionTextMarketingPermission :: Text
    , enabledMarketingPermission :: Bool
    } deriving (Show)

data MergeFields = MergeFields
    { phoneMergeFields :: Text
    , birthdayMergeFields :: Text
    , fnameMergeFields :: Text
    } deriving (Show)

data Stats = Stats
    { avgOpenRateStats :: Int
    , avgClickRateStats :: Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MemberResponse
decodeTopLevel = decode

instance ToJSON MemberResponse where
    toJSON (MemberResponse purpleIDMemberResponse emailAddressMemberResponse uniqueEmailIDMemberResponse webIDMemberResponse emailTypeMemberResponse statusMemberResponse mergeFieldsMemberResponse statsMemberResponse ipSignupMemberResponse timestampSignupMemberResponse ipOptMemberResponse timestampOptMemberResponse memberRatingMemberResponse lastChangedMemberResponse languageMemberResponse vipMemberResponse emailClientMemberResponse locationMemberResponse marketingPermissionsMemberResponse sourceMemberResponse tagsCountMemberResponse tagsMemberResponse listIDMemberResponse linksMemberResponse) =
        object
        [ "id" .= purpleIDMemberResponse
        , "email_address" .= emailAddressMemberResponse
        , "unique_email_id" .= uniqueEmailIDMemberResponse
        , "web_id" .= webIDMemberResponse
        , "email_type" .= emailTypeMemberResponse
        , "status" .= statusMemberResponse
        , "merge_fields" .= mergeFieldsMemberResponse
        , "stats" .= statsMemberResponse
        , "ip_signup" .= ipSignupMemberResponse
        , "timestamp_signup" .= timestampSignupMemberResponse
        , "ip_opt" .= ipOptMemberResponse
        , "timestamp_opt" .= timestampOptMemberResponse
        , "member_rating" .= memberRatingMemberResponse
        , "last_changed" .= lastChangedMemberResponse
        , "language" .= languageMemberResponse
        , "vip" .= vipMemberResponse
        , "email_client" .= emailClientMemberResponse
        , "location" .= locationMemberResponse
        , "marketing_permissions" .= marketingPermissionsMemberResponse
        , "source" .= sourceMemberResponse
        , "tags_count" .= tagsCountMemberResponse
        , "tags" .= tagsMemberResponse
        , "list_id" .= listIDMemberResponse
        , "_links" .= linksMemberResponse
        ]

instance FromJSON MemberResponse where
    parseJSON (Object v) = MemberResponse
        <$> v .: "id"
        <*> v .: "email_address"
        <*> v .: "unique_email_id"
        <*> v .: "web_id"
        <*> v .: "email_type"
        <*> v .: "status"
        <*> v .: "merge_fields"
        <*> v .: "stats"
        <*> v .: "ip_signup"
        <*> v .: "timestamp_signup"
        <*> v .: "ip_opt"
        <*> v .: "timestamp_opt"
        <*> v .: "member_rating"
        <*> v .: "last_changed"
        <*> v .: "language"
        <*> v .: "vip"
        <*> v .: "email_client"
        <*> v .: "location"
        <*> v .: "marketing_permissions"
        <*> v .: "source"
        <*> v .: "tags_count"
        <*> v .: "tags"
        <*> v .: "list_id"
        <*> v .: "_links"

instance ToJSON Link where
    toJSON (Link relLink hrefLink methodLink targetSchemaLink schemaLink) =
        object
        [ "rel" .= relLink
        , "href" .= hrefLink
        , "method" .= methodLink
        , "targetSchema" .= targetSchemaLink
        , "schema" .= schemaLink
        ]

instance FromJSON Link where
    parseJSON (Object v) = Link
        <$> v .: "rel"
        <*> v .: "href"
        <*> v .: "method"
        <*> v .:? "targetSchema"
        <*> v .:? "schema"

instance ToJSON Location where
    toJSON (Location latitudeLocation longitudeLocation gmtoffLocation dstoffLocation countryCodeLocation timezoneLocation) =
        object
        [ "latitude" .= latitudeLocation
        , "longitude" .= longitudeLocation
        , "gmtoff" .= gmtoffLocation
        , "dstoff" .= dstoffLocation
        , "country_code" .= countryCodeLocation
        , "timezone" .= timezoneLocation
        ]

instance FromJSON Location where
    parseJSON (Object v) = Location
        <$> v .: "latitude"
        <*> v .: "longitude"
        <*> v .: "gmtoff"
        <*> v .: "dstoff"
        <*> v .: "country_code"
        <*> v .: "timezone"

instance ToJSON MarketingPermission where
    toJSON (MarketingPermission marketingPermissionIDMarketingPermission marketingPermissionTextMarketingPermission enabledMarketingPermission) =
        object
        [ "marketing_permission_id" .= marketingPermissionIDMarketingPermission
        , "text" .= marketingPermissionTextMarketingPermission
        , "enabled" .= enabledMarketingPermission
        ]

instance FromJSON MarketingPermission where
    parseJSON (Object v) = MarketingPermission
        <$> v .: "marketing_permission_id"
        <*> v .: "text"
        <*> v .: "enabled"

instance ToJSON MergeFields where
    toJSON (MergeFields phoneMergeFields birthdayMergeFields fnameMergeFields) =
        object
        [ "PHONE" .= phoneMergeFields
        , "BIRTHDAY" .= birthdayMergeFields
        , "FNAME" .= fnameMergeFields
        ]

instance FromJSON MergeFields where
    parseJSON (Object v) = MergeFields
        <$> v .: "PHONE"
        <*> v .: "BIRTHDAY"
        <*> v .: "FNAME"

instance ToJSON Stats where
    toJSON (Stats avgOpenRateStats avgClickRateStats) =
        object
        [ "avg_open_rate" .= avgOpenRateStats
        , "avg_click_rate" .= avgClickRateStats
        ]

instance FromJSON Stats where
    parseJSON (Object v) = Stats
        <$> v .: "avg_open_rate"
        <*> v .: "avg_click_rate"
