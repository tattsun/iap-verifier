{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
-- | This module is a simple wrapper of AppStore In-App-Purchase Receipt Validate APIs.
-- Example:
--
-- > import Network.IAP.Verifier
-- > import qualified Data.ByteString as BS
-- > main :: IO ()
-- > main = do
-- >   receipt <- BS.readFile "./receipt"
-- >   result <- verify defaultIAPSettings receipt
-- >   case result of
-- >     0 -> putStrLn "OK"
-- >     _ -> putStrLn "Fail"
--
-- For more information, please see <https://developer.apple.com/library/ios/releasenotes/General/ValidateAppStoreReceipt/Introduction.html>.
module Network.IAP.Verifier
       ( -- * Settings
         IAPSettings(..)
       , defaultIAPSettings
       , sandboxIAPSettings
         -- * Result
       , Result(..)
         -- * Exception
       , IAPException(..)
         -- * Action
       , verify
       ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson                 hiding (Result)
import           Data.Aeson.TH
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit               as C
import qualified Data.Text.Encoding         as T
import           Data.Typeable
import           Network.HTTP.Conduit

-- | In-App-Purchase Verify Settings.
data IAPSettings = IAPSettings { verifyUrl :: String
                               }
                   deriving (Show)

-- | 'IAPSettings' for production.
defaultIAPSettings :: IAPSettings
defaultIAPSettings =
  IAPSettings { verifyUrl = "https://buy.itunes.apple.com/verifyReceipt"
              }

-- | 'IAPSettings' for development.
sandboxIAPSettings :: IAPSettings
sandboxIAPSettings =
  defaultIAPSettings { verifyUrl = "https://sandbox.itunes.apple.com/verifyReceipt"
                     }

-- | A result of 'verify'.
data Result = Result { status :: Int
                     }
              deriving (Show)
$(deriveJSON defaultOptions ''Result)

-- | Exceptions thrown by 'verify'.
data IAPException = UnknownJSONException { unUnknownJSONException :: BS.ByteString }
                  | NoResponseException
                  deriving (Show, Eq, Typeable)
instance Exception IAPException

----------------------------------------------------------------------

-- | Verify your receipt.
-- Throw 'IAPException' when request is failed.
verify :: IAPSettings -> BS.ByteString -> IO Result
verify settings receipt = do
  requestRaw <- parseUrl (verifyUrl settings)
  let payload = encode $ object ["receipt-data" .= (T.decodeUtf8 $
                                                    receipt)]
      request = requestRaw { requestBody = RequestBodyLBS payload
                           , method = "POST"
                           }
  withManager $ \manager -> do
    response <- http request manager
    responseBody response C.$$+- do
      value <- C.await
      case value of
       Just x ->  case decode . LBS.fromStrict $ x of
                   Just result -> return result
                   Nothing -> liftIO . throwIO $ UnknownJSONException x
       Nothing -> liftIO . throwIO $ NoResponseException
