module Handler.Home where

import Import

import Data.Aeson 
    ( decode, encode
    , withObject
    , (.:?)
    )
import Data.Text (append)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, empty)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.IP (fromHostAddress, fromHostAddress6)

import Text.Hamlet          (hamletFile)

import Network.Wai (Request(remoteHost))
import Network.Socket (SockAddr(..))

import Network.Haskoin.Yesod.APIServer
import Network.Haskoin.Yesod.TokenAuth
import Network.Haskoin.Wallet
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
getHomeR = renderHome

postHomeR :: Handler Html
postHomeR = do
    timeM <- nextWithdrawTime
    when (isJust timeM) $ do
        msg <- withUrlRenderer
            $(hamletFile "templates/not-authorized-message.hamlet")
        setMessage msg
        redirect HomeR

    cfg <- appSettings <$> getYesod
    result <- runInputPostResult $ ireq textField "address"
    case result of
        FormSuccess addr -> do
            case base58ToAddr $ unpack addr of
                Nothing -> do
                    msg <- withUrlRenderer
                        $(hamletFile "templates/invalid-address-message.hamlet")
                    setMessage msg
                Just x ->
                    withdraw x
            redirect HomeR
        _ -> renderHome

-- Display the home page
renderHome :: Handler Html
renderHome = do
    limit <- (appLimit . appSettings) <$> getYesod
    ip  <- getUserIP
    timeM <- nextWithdrawTime
    addrRes <- getDonationAddress
    balRes  <- getWalletBalance
    let balance = case balRes of
            BalanceConflict -> "Balance conflict" 
            Balance b       -> show b
        donation = addrToBase58 addrRes
    defaultLayout $ do
        setTitle "Haskoin Faucet"
        $(widgetFile "homepage")

-- Returns the time at which a user can withdraw again. Returns Nothing
-- if the user can withdraw now.
nextWithdrawTime :: Handler (Maybe UTCTime)
nextWithdrawTime = do
    userIp <- getUserIP
    userM  <- runDB $ getBy $ UniqueIp userIp
    reset <- (appReset . appSettings) <$> getYesod
    case userM of
        Just (Entity _ user) -> do
            now <- liftIO getCurrentTime
            let allowedTime = addUTCTime reset $ userWithdrawTime user
            return $ if now > allowedTime then Nothing else Just allowedTime
        Nothing -> return Nothing

-- Performs a withdrawal to the provided Address
withdraw :: Address -> Handler ()
withdraw addr = do
    userIP <- getUserIP
    now <- liftIO getCurrentTime
    cfg <- appSettings <$> getYesod
    let limit   = appLimit cfg
        wallet  = appWalletName cfg
        account = appAccountName cfg
        minconf = appMinConf cfg
        fee     = appFee cfg
        url = concat [ "/wallets/", unpack wallet
                     , "/accounts/", unpack account, "/txs"
                     ] 
        req = Just $ encode $ SendCoins [(addr, limit)] fee minconf False

    txE <- sendHW url [] "POST" req
    case txE of
        Left err -> do
            msg <- withUrlRenderer
                $(hamletFile "templates/error-message.hamlet")
            setMessage msg
        Right (TxHashStatusRes tid _) -> do
            msg <- withUrlRenderer
                $(hamletFile "templates/sent-message.hamlet")
            setMessage msg
            runDB $ do
                resE <- insertBy $ User userIP now limit
                case resE of
                    Left (Entity uid _) -> replace uid $ User userIP now limit
                    Right _ -> return ()


getDonationAddress :: Handler Address
getDonationAddress = do
    cfg <- appSettings <$> getYesod
    let wallet = appWalletName cfg
        account = appAccountName cfg
        url = concat [ "/wallets/", unpack wallet
                     , "/accounts/", unpack account, "/addrs" 
                     ]
        qs = [ ("unused", Just "true" ) ]
    addrE <- sendHW url qs "GET" Nothing
    case addrE of
        Left err -> invalidArgs [ pack err ]
        Right [] -> invalidArgs [ "Could not get a donation address" ]
        Right (x:_) -> return $ paymentAddress $ balanceAddress x

getWalletBalance :: Handler Balance
getWalletBalance = do
    cfg <- appSettings <$> getYesod
    let minconf = appMinConf cfg
        wallet = appWalletName cfg
        account = appAccountName cfg
        url = concat [ "/wallets/", unpack wallet
                     , "/accounts/", unpack account, "/balance" 
                     ]
        qs  = [ ("minconf", Just $ stringToBS $ show minconf) ]
    balE <- sendHW url qs "GET" Nothing
    case balE of
        Left err -> invalidArgs [ pack err ]
        Right (BalanceRes bal _) -> return bal

getUserIP :: Handler Text
getUserIP = do
    sock <- remoteHost <$> waiRequest
    return $ case sock of
        SockAddrInet _ ha -> pack $ show $ fromHostAddress ha
        SockAddrInet6 _ _ ha _ -> pack $ show $ fromHostAddress6 ha
        SockAddrUnix ha -> pack ha

sendHW :: FromJSON a
       => String              -- Route
       -> [(BS.ByteString, Maybe BS.ByteString)] -- Query string
       -> BS.ByteString       -- Method
       -> Maybe BL.ByteString -- Body 
       -> Handler (Either String a) -- Response
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

    -- Display an error message
    return $ if statusCode (responseStatus res) >= 400
        then case decode $ responseBody res of
            Just (ErrorMsg err msgs) -> Left $ 
                if null msgs 
                    then err 
                    else unwords $ (err ++ ":") : msgs
            Nothing -> Left "An error occured"
        else case decode $ responseBody res of
            Just a -> Right a
            Nothing -> Left "An error occured"

data ErrorMsg = ErrorMsg !String ![String]
    deriving (Eq, Show, Read)

instance FromJSON ErrorMsg where
    parseJSON = withObject "errormsg" $ \o -> do
        err  <- o .: "message"
        msgM <- o .:? "errors"
        return $ ErrorMsg err $ fromMaybe [] msgM

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

