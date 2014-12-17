module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 
    ( BootstrapFormLayout (..)
    , renderBootstrap3
    , withSmallInput
    )

import Data.Aeson (decode)
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, empty)

import Network.Haskoin.Yesod.TokenAuth
import Network.Haskoin.Crypto
import Network.Haskoin.Util

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    cfg <- appSettings <$> getYesod
    (formWidget, formEnctype) <- generateFormPost withdrawForm
    let addressM = Nothing :: Maybe Text
        minconf = appMinConf cfg
        wallet = appWalletName cfg
        account = appAccountName cfg
        limit = appLimit cfg
        reset = appReset cfg
        url = concat [ "/wallets/"
                     , unpack wallet
                     , "/accounts/"
                     , unpack account
                     , "/balance" 
                     ]
        qs  = [ ("minconf", Just $ stringToBS $ show minconf) ]
    balance <- sendHW url qs "GET" Nothing
    defaultLayout $ do
        setTitle "Haskoin Faucet"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost withdrawForm
    let addressM = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
        setTitle "Haskoin Faucet"
        $(widgetFile "homepage")

withdrawForm :: Form Text
withdrawForm = renderBootstrap3 BootstrapBasicForm $ 
    areq addressField "Testnet Address" Nothing
  where
    addressField = check validateAddress textField
    validateAddress a = case base58ToAddr (unpack a) of
        Just res -> Right $ pack $ addrToBase58 res
        _        -> Left $ ("Invalid address" :: Text)

sendHW :: String              -- Route
       -> [(BS.ByteString, Maybe BS.ByteString)] -- Query string
       -> BS.ByteString       -- Method
       -> Maybe BL.ByteString -- Body 
       -> Handler Value       -- Response
sendHW p qs m bodyM = do
    walletUrl    <- (unpack . appWalletUrl . appSettings) <$> getYesod
    walletTokenM <- (appWalletToken . appSettings) <$> getYesod

    let noQsUrl = concat [ walletUrl, p ]
        bdy     = fromMaybe BL.empty bodyM

    urlReq <- (setQueryString qs) <$> parseUrl noQsUrl
    let url = concat [ noQsUrl, bsToString $ queryString urlReq ]

    authHeaders <- case walletTokenM of
        Nothing -> return []
        Just (TokenPair ident secret) -> do
            n <- nextNonce ident
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

    res <- withManager (httpLbs req)

    -- Just forward the response in case of error
    let code = responseStatus res
    when (statusCode code >= 400) $
        sendResponseStatus code $ fromMaybe Null $ decode $ responseBody res

    -- TODO: What if decode fails ?
    return $ fromJust $ decode $ responseBody res

nextNonce :: BS.ByteString -> Handler Int
nextNonce ident = do
    let tIdent = pack $ bsToString ident
    tokenM <- runDB $ getBy $ UniqueToken tIdent
    case tokenM of
        Just (Entity ti tv) -> do
            let res = nonceValue tv
            runDB $ replace ti tv{ nonceValue = res + 1 }
            return res
        Nothing -> do
            runDB $ insert_ $ Nonce tIdent 2
            return 1

