module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 
    ( BootstrapFormLayout (..)
    , renderBootstrap3
    , withSmallInput
    )

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Data.Text (append)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, empty)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.IP (fromHostAddress, fromHostAddress6)

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
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost withdrawForm
    renderHome formWidget formEnctype

postHomeR :: Handler Html
postHomeR = nextWithdrawTime >>= \timeM -> if isNothing timeM
    then do
        cfg <- appSettings <$> getYesod
        ((result, formWidget), formEnctype) <- runFormPost withdrawForm
        case result of
            FormSuccess addr -> do
                withdraw $ fromJust $ base58ToAddr $ unpack addr
                redirect HomeR
            _ -> renderHome formWidget formEnctype
    else do
        setMessage $ toHtml ("You are not authorized to withdraw yet." :: Text)
        redirect HomeR

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

    runDB $ do
        resE <- insertBy $ User userIP now limit
        case resE of
            Left (Entity uid _) -> replace uid $ User userIP now limit
            Right _ -> return ()

    (TxHashStatusRes tid _) <- sendHW url [] "POST" req
    setMessage $ toHtml $ "Coins sent. Tx: " ++ encodeTxHashLE tid

-- Display the home page
renderHome :: Widget -> Enctype -> Handler Html
renderHome formWidget formEnctype = do
    cfg <- appSettings <$> getYesod
    ip  <- getUserIP
    timeM <- nextWithdrawTime
    let minconf = appMinConf cfg
        wallet = appWalletName cfg
        account = appAccountName cfg
        limit = appLimit cfg
        reset = appReset cfg
        balanceUrl = concat [ "/wallets/", unpack wallet
                            , "/accounts/", unpack account, "/balance" 
                            ]
        balanceQs  = [ ("minconf", Just $ stringToBS $ show minconf) ]
        addrUrl = concat [ "/wallets/", unpack wallet
                         , "/accounts/", unpack account, "/addrs" 
                         ]
        addrQs  = [ ("unused", Just "true" ) ]
    (BalanceRes balRes _) <- sendHW balanceUrl balanceQs "GET" Nothing
    addrRes <- sendHW addrUrl addrQs "GET" Nothing
    let balance = case balRes of
            BalanceConflict -> "Balance conflict" 
            Balance b       -> show b
        donation = case addrRes of
            []   -> ""
            x:xs -> addrToBase58 $ paymentAddress $ balanceAddress x
    defaultLayout $ do
        setTitle "Haskoin Faucet"
        $(widgetFile "homepage")

getUserIP :: Handler Text
getUserIP = do
    sock <- remoteHost <$> waiRequest
    return $ case sock of
        SockAddrInet _ ha -> pack $ show $ fromHostAddress ha
        SockAddrInet6 _ _ ha _ -> pack $ show $ fromHostAddress6 ha
        SockAddrUnix ha -> pack ha

withdrawForm :: Form Text
withdrawForm = renderBootstrap3 BootstrapBasicForm $ 
    areq addressField "Testnet Address" Nothing
  where
    addressField = check validateAddress textField
    validateAddress a = case base58ToAddr (unpack a) of
        Just res -> Right $ pack $ addrToBase58 res
        _        -> Left $ ("Invalid address" :: Text)

sendHW :: FromJSON a
       => String              -- Route
       -> [(BS.ByteString, Maybe BS.ByteString)] -- Query string
       -> BS.ByteString       -- Method
       -> Maybe BL.ByteString -- Body 
       -> Handler a           -- Response
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

