{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
module Network.Haskoin.Faucet where

import System.Directory (doesFileExist)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, forM, unless)

import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Time (UTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Data.Default (Default, def)
import Data.List (nubBy, find)
import Data.Foldable (toList)
import Data.Word (Word32, Word64)
import Data.String (fromString)
import qualified Data.Yaml as YAML (encodeFile, decodeFile)
import qualified Data.ByteString as BS (ByteString, empty)
import qualified Data.ByteString.Lazy as BL (ByteString, empty)

import Yesod
import Database.Persist.MySQL
    ( ConnectionPool
    , SqlBackend
    , runSqlPool
    , runSqlPersistMPool
    , runMigrationSilent
    )

import Network.HTTP.Types (Status(..))
import Network.HTTP.Conduit 
    ( RequestBody(..)
    , Request(..)
    , Response(..)
    , httpLbs
    , withManager
    , setQueryString
    , parseUrl
    )
import Network.Wai.Handler.Warp 
    ( setHost
    , setPort
    , defaultSettings
    , runSettings
    )

import Network.Haskoin.Yesod.APIServer
    ( NewAccount(..)
    , AddressData(..)
    , AccTxAction(..)
    , NewWallet(..)
    )
import Network.Haskoin.Yesod.TokenAuth
import Network.Haskoin.Crypto
import Network.Haskoin.Constants
import Network.Haskoin.Util

data FaucetServer = FaucetServer
    { dbPool       :: ConnectionPool
    , faucetConfig :: FaucetConfig
    }

data FaucetConfig = FaucetConfig
    { configLimit       :: !Word64
    , configReset       :: !Word32
    , configMinConf     :: !Int
    , configWalletName  :: !String
    , configAccountName :: !String
    , configBind        :: !String
    , configPort        :: !Int
    , configWalletUrl   :: !String
    , configWalletToken :: !(Maybe (BS.ByteString, BS.ByteString))
    , configNceFile     :: !FilePath
    } 

instance Default FaucetConfig where
    def = FaucetConfig
        { configLimit       = 10000
        , configReset       = 3600
        , configMinConf     = 3
        , configWalletName  = "faucet"
        , configAccountName = "public"
        , configBind        = "127.0.0.1"
        , configPort        = faucetPort
        , configWalletUrl   = concat [ "http://localhost:", show walletPort ]
        , configWalletToken = Nothing
        , configNceFile     = "token-nonces"
        }
      where
        walletPort | networkName == "prodnet" = 8555 :: Int
                   | otherwise                = 18555
        faucetPort | networkName == "prodnet" = 80 :: Int
                   | otherwise                = 8080

share [ mkPersist (sqlSettings { mpsGeneric = True })
      , mkMigrate "migrateUserDB"
      ] [persistLowerCase|
User 
    ip Text maxlen=200
    withdrawTime UTCTime 
    withdrawAmount Word64
    UniqueIp ip

Audit
    ip Text maxlen=200
    event Text maxlen=200
    message Text
    withdrawAmount Word64
    created UTCTime
|]

mkYesod "FaucetServer" [parseRoutes|
/      HomeR GET
/coins CoinsR POST
|]

instance Yesod FaucetServer where
    makeSessionBackend _ = return Nothing

instance YesodPersist FaucetServer where
    type YesodPersistBackend FaucetServer = SqlBackend
    runDB action = do
        FaucetServer pool _ <- getYesod
        runSqlPool action pool

instance RenderMessage FaucetServer FormMessage where
    renderMessage _ _ = defaultFormMessage

runServer :: FaucetConfig -> ConnectionPool -> IO ()
runServer config pool = do
    flip runSqlPersistMPool pool $ do 
        _ <- runMigrationSilent migrateUserDB
        return ()
    app <- toWaiApp $ FaucetServer pool config
    runSettings settings app
  where
    bind     = fromString $ configBind config
    settings = setHost bind $ setPort (configPort config) defaultSettings

data Withdrawal = Withdrawal !Text
    deriving (Eq, Show, Read)

withdrawForm :: Html -> MForm Handler (FormResult Withdrawal, Widget)
withdrawForm = renderDivs $ Withdrawal
    <$> areq addressField "Address" Nothing
  where
    addressField = check validateAddress textField
    validateAddress a = case base58ToAddr (unpack a) of
        Just res -> Right $ pack $ addrToBase58 res
        _        -> Left $ ("Invalid address" :: Text)

getHomeR :: Handler Html
getHomeR = do
    cfg <- faucetConfig <$> getYesod
    let minconf = configMinConf cfg
        wallet = configWalletName cfg
        account = configAccountName cfg
        limit = configLimit cfg
        reset = configReset cfg
        url = concat [ "/wallets/", wallet, "/accounts/", account, "/balance" ]
        qs = [ ("minconf", Just $ stringToBS $ show minconf) ]
    balance <- sendRequest url qs "GET" Nothing

    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost withdrawForm
    defaultLayout
        [whamlet|
            <p>
              The widget generated contains only the contents
              of the form, not the form tag itself. So...
            <form method=post action=@{CoinsR} enctype=#{enctype}>
              ^{widget}
              <p>It also doesn't include the submit button.
              <button>Submit
        |]

postCoinsR :: Handler Html
postCoinsR = do
    ((result, widget), enctype) <- runFormPost withdrawForm
    case result of
        FormSuccess w -> defaultLayout [whamlet|<p>#{show w}|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{CoinsR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]
    
sendRequest :: String              -- Route
            -> [(BS.ByteString, Maybe BS.ByteString)] -- Query string
            -> BS.ByteString       -- Method
            -> Maybe BL.ByteString -- Body 
            -> Handler Value       -- Response
sendRequest p qs m bodyM = do
    walletUrl   <- (configWalletUrl . faucetConfig) <$> getYesod
    walletTokenM <- (configWalletToken . faucetConfig) <$> getYesod

    let noQsUrl = concat [ walletUrl, p ]
        bdy     = fromMaybe BL.empty bodyM

    urlReq <- (setQueryString qs) <$> parseUrl noQsUrl
    let url = concat [ noQsUrl, bsToString $ queryString urlReq ]

    authHeaders <- case walletTokenM of
        Nothing -> return []
        Just (ident, secret) -> do
            nceFile <- (configNceFile . faucetConfig) <$> getYesod
            n <- liftIO $ getNonce nceFile ident 
            let sigE = buildTokenSig n (pack url) (toStrictBS bdy) secret
            when (isLeft sigE) $ permissionDenied 
                "The server could not sign a wallet token"
            return [ ( "access_key", ident )
                   , ( "access_signature", fromRight sigE )
                   , ( "access_nonce", stringToBS $ show n )
                   , ( "host", stringToBS walletUrl )
                   ]

    let req = urlReq 
            { method = m
            -- Do not throw exceptions on error status codes
            , checkStatus = (\_ _ _ -> Nothing) 
            , requestBody = RequestBodyLBS bdy
            , requestHeaders = ("accept", "application/json") : authHeaders
            }

    res <- withManager $ \manager -> httpLbs req manager

    -- Just forward the response in case of error
    let code = responseStatus res
    when (statusCode code >= 400) $
        sendResponseStatus code $ fromMaybe Null $ decode $ responseBody res

    return $ fromJust $ decode $ responseBody res

newtype Nonces = Nonces [(BS.ByteString, Int)]
    deriving (Eq, Show, Read)

instance ToJSON Nonces where
    toJSON (Nonces xs) = toJSON $ map f xs
      where
        f (ident, n) = object
            [ "token" .= bsToString ident 
            , "nonce" .= n
            ]

instance FromJSON Nonces where
    parseJSON = withArray "nonces" $ \v -> 
        Nonces <$> mapM f (toList v)
      where
        f = withObject "nonce" $ \o -> 
            (,) <$> (stringToBS <$> o .: "token")
                <*> o .: "nonce"

getNonces :: FilePath -> IO Nonces
getNonces nceFile = do
    doesFileExist nceFile >>= \fileExists -> do
    unless fileExists $ YAML.encodeFile nceFile $ Nonces [] 
    resM <- YAML.decodeFile nceFile
    when (isNothing resM) $ error "Could not decode nonces file"
    return $ fromJust resM

getNonce :: FilePath -> BS.ByteString -> IO Int
getNonce nceFile ident = do
    Nonces xs <- getNonces nceFile
    let resM = find ((== ident) . fst) xs
    case resM of
        Just (_,n) -> setNonce nceFile ident (n+1) >> return n
        Nothing    -> setNonce nceFile ident 2 >> return 1

setNonce :: FilePath -> BS.ByteString -> Int -> IO ()
setNonce nceFile ident n = do
    Nonces xs <- getNonces nceFile
    YAML.encodeFile nceFile $ Nonces $ nubBy f $ (ident, n):xs
  where
    f a b = fst a == fst b

