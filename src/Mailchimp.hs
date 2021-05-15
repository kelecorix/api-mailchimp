{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Mailchimp
  ( addMemberToList'
  , ping'
  ) where

import           Control.Concurrent
import           Control.Lens                 ((.~), (<&>), (^.), (^..), (^?),
                                               _1)
import           Control.Lens.Cons
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import qualified Data.ByteString              as BB
import qualified Data.ByteString.Base64       as B64
import           Data.ByteString.Lazy
import           Data.HashMap.Strict          as HM
import           Data.List                    as L
import           Data.Maybe
import           Data.String
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Network.HTTP.Conduit
import           System.IO                    (stdout)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Safe
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.HTTP.Client        (Manager, newManager)
import qualified Network.HTTP.Client        as C hiding (Proxy)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import qualified Network.HTTP.Conduit       as HttpConduit hiding (Proxy)
import qualified Network.HTTP.Simple        as S hiding (Proxy)
import           Servant.API
import           Servant.Client
import           Servant.Client.Core

import           Utils
import qualified Mailchimp.TransactionalMail           as MCTM
import qualified Mailchimp.MemberPayload               as MCTM
import qualified Mailchimp.MemberResponse              as MCR

--------------------------------------------------------------------------------

data MemberPayload =
  MemberPayload
   { key :: T.Text
   } deriving (Eq, Show)

msgFrom = "no-reply@example.com"
endpoint = "mandrillapp.com" -- do NOT specify protocol https,http or any subroutes via /
accessKey = "111111111111"

endpoint2 = "us19.api.mailchimp.com" --
accessKey2 = "11111111111111"
list_id ="11111"

--------------------------------------------------------------------------------

type MailchimpAPI =
     "users"
  :> "ping"
  :> ReqBody '[JSON] MCTM.KeyWrap
  :> Post    '[JSON] T.Text

  -- POST /messages
  :<|> "messages"
    :> "send"
    :> ReqBody '[JSON] MCTM.TransactionalMail
    :> Post    '[JSON] MCTM.MCResponse

  -- "https://$API_SERVER.api.mailchimp.com/3.0/lists/$list_id/members"
  -- "000000000"
  :<|> "lists"
    :> Capture "list_id" T.Text
    :> "members"
    :> Header "Authorization: Basic " T.Text
    :> ReqBody '[JSON] MCTM.MemberPayload
    :> Post    '[JSON] MCR.MemberResponse


mailchimpAPI :: Data.Proxy.Proxy MailchimpAPI
mailchimpAPI = Data.Proxy.Proxy

-- Derive call functions for the Api
ping              :: MCTM.KeyWrap -> ClientM T.Text
sendMessage       :: MCTM.TransactionalMail -> ClientM MCTM.MCResponse
addMemberToList   :: T.Text -> Maybe T.Text -> MCTM.MemberPayload -> ClientM MCR.MemberResponse
(     ping
 :<|> sendMessage
 :<|> addMemberToList ) = client mailchimpAPI

--------------------------------------------------------------------------------

ping' :: IO (Either ClientError T.Text)
ping' = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (ping (MCTM.KeyWrap accessKey)) env
    where
      host = (BaseUrl Https endpoint 443 "api/1.0")

sendMessage' :: T.Text -> T.Text -> T.Text -> IO (Either ClientError MCTM.MCResponse)
sendMessage' msgTo msgTitle msgBody = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (sendMessage (MCTM.TransactionalMail
                                (Just accessKey)
                                (Just (MCTM.Message (Just "no-reply@example.com")
                                         (Just "another test")
                                         (Just "bla bla bla")
                                         (Just [MCTM.To (Just "recepient@recepeint.com")
                                                   (Just "to")
                                               ]
                                         )
                                      )
                                )
                          ) ) env
    where
      host = (BaseUrl Https endpoint 443 "api/1.0")

addMemberToList' em fname lname = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
      auth = T.pack $ B.unpack $ B64.encode $ B.pack $ "key:" ++ accessKey2
      payload = MCTM.MemberPayload em
                                   (Just "subscribed")
                                   (Just (MCTM.MergeFields (Just fname) (Just lname)))

  runClientM (addMemberToList list_id (Just auth) payload) env
    where
      host = (BaseUrl Https endpoint2 443 "3.0")



------------------------------------------------------------------------------------------------------------------



-- MESSAGE='{"key": "$YOUR_API_KEY", "message": {"from_email": "hello@example.com", "subject": "Hello World", "text": "Welcome to Mailchimp Transactional!", "to": [{ "email": "freddie@example.com", "type": "to" }]}}'

-- curl -sS -X POST "https://mandrillapp.com/api/1.0/messages/send" --header 'Content-Type: application/json' --data-raw "$MESSAGE"
