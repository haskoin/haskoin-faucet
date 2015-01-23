module Handler.Home where

import Import

import qualified System.ZMQ4.Monadic as Z
    ( Req(..)
    , runZMQ
    , socket
    , send
    , receive
    , connect
    )

import Data.Time.Clock (addUTCTime)
import Data.IP (fromHostAddress, fromHostAddress6)
import Data.Aeson (encode, eitherDecode)
import qualified Data.Text as T (strip, pack, unpack)

import Text.Hamlet (hamletFile)

import Network.Wai (Request(remoteHost))
import Network.Socket (SockAddr(..))

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
            case base58ToAddr $ T.unpack $ T.strip addr of
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
    cfg <- appSettings <$> getYesod
    let limit   = appLimit cfg
        wallet  = appWalletName cfg
        account = appAccountName cfg
        minconf = appMinConf cfg
        fee     = appFee cfg

    let action = CreateTx [(addr, limit)] fee minconf True
    txRes <- sendZmq $ PostTxsR wallet account action
    case txRes of
        ResponseError err -> setMessage =<< withUrlRenderer
            $(hamletFile "templates/error-message.hamlet")
        ResponseValid (TxHashStatusRes tid _) -> do
            setMessage =<< withUrlRenderer
                $(hamletFile "templates/sent-message.hamlet")
            runDB $ do
                now  <- liftIO getCurrentTime
                resE <- insertBy $ User userIP now limit
                case resE of
                    Left (Entity uid _) -> replace uid $ User userIP now limit
                    Right _ -> return ()

getDonationAddress :: Handler Address
getDonationAddress = do
    cfg <- appSettings <$> getYesod
    let wallet = appWalletName cfg
        account = appAccountName cfg
    addrRes <- sendZmq $ GetAddressesR wallet account Nothing 0 False False True
    case addrRes of
        ResponseError err -> invalidArgs [ err ]
        ResponseValid []  -> invalidArgs [ "Could not get a donation address" ]
        ResponseValid (x:_) -> return $ laAddress $ baAddress x

getWalletBalance :: Handler Balance
getWalletBalance = do
    cfg <- appSettings <$> getYesod
    let wallet  = appWalletName cfg
        account = appAccountName cfg
        minconf = appMinConf cfg
    balRes <- sendZmq $ GetBalanceR wallet account minconf
    case balRes of
        ResponseError err -> invalidArgs [ err ]
        ResponseValid (BalanceRes bal _) -> return bal

getUserIP :: Handler Text
getUserIP = do
    sock <- remoteHost <$> waiRequest
    return $ case sock of
        SockAddrInet _ ha -> pack $ show $ fromHostAddress ha
        SockAddrInet6 _ _ ha _ -> pack $ show $ fromHostAddress6 ha
        SockAddrUnix ha -> pack ha

sendZmq :: FromJSON a => WalletRequest -> Handler (WalletResponse a)
sendZmq req = do
    cfg <- appSettings <$> getYesod
    let sockName = T.unpack $ appWalletSocket cfg
    resE <- liftIO $ Z.runZMQ $ do
        sock <- Z.socket Z.Req
        Z.connect sock sockName
        Z.send sock [] (toStrictBS $ encode req)
        eitherDecode . toLazyBS <$> Z.receive sock
    return $ case resE of
        Right r  -> r
        Left err -> ResponseError $ T.pack err

