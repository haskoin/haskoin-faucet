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

import Data.Aeson (encode, eitherDecode)
import qualified Data.Text as T (strip, pack, unpack)
import qualified Data.ByteString.Lazy as BL (toStrict, fromStrict)

import Text.Hamlet (hamletFile)

import Network.Haskoin.Wallet
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction

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
    result <- runInputPostResult $ ireq textField "address"
    case result of
        FormSuccess addr -> do
            case base58ToAddr $ encodeUtf8 $ T.strip addr of
                Nothing -> do
                    msg <- withUrlRenderer
                        $(hamletFile "templates/invalid-address-message.hamlet")
                    setMessage msg
                Just x ->
                    withdraw x
            renderHome
        _ -> renderHome

-- Display the home page
renderHome :: Handler Html
renderHome = do
    addrRes <- getDonationAddress
    let donation = addrToBase58 addrRes
    defaultLayout $ do
        setTitle "Haskoin Faucet"
        $(widgetFile "homepage")

-- Performs a withdrawal to the provided Address
withdraw :: Address -> Handler ()
withdraw addr = do
    cfg <- appSettings <$> getYesod
    let limit   = appLimit cfg
        account = appAccountName cfg
        minconf = appMinConf cfg
        fee     = appFee cfg
        action = CreateTx [(addr, limit)] fee minconf False True
    txRes <- sendZmq $ PostTxsR account Nothing action
    case txRes of
        ResponseError err -> setMessage =<< withUrlRenderer
            $(hamletFile "templates/error-message.hamlet")
        ResponseValid (Just tx) -> do
            let tid = jsonTxHash tx
            setMessage =<< withUrlRenderer
                $(hamletFile "templates/sent-message.hamlet")
        _ -> do
            let err = "Unexpected error" :: Text
            setMessage =<< withUrlRenderer
                $(hamletFile "templates/error-message.hamlet")

getDonationAddress :: Handler Address
getDonationAddress = do
    cfg <- appSettings <$> getYesod
    let account = appAccountName cfg
    addrRes <- sendZmq $ GetAddressesUnusedR account AddressExternal
    case addrRes of
        ResponseError err -> invalidArgs [ err ]
        ResponseValid (Just (x:_)) ->
            return $ jsonAddrAddress x
        ResponseValid _ -> invalidArgs [ "Could not get a donation address" ]

sendZmq :: FromJSON a => WalletRequest -> Handler (WalletResponse a)
sendZmq req = do
    cfg <- appSettings <$> getYesod
    let sockName = T.unpack $ appWalletSocket cfg
    resE <- liftIO $ Z.runZMQ $ do
        sock <- Z.socket Z.Req
        Z.connect sock sockName
        Z.send sock [] (BL.toStrict $ encode req)
        eitherDecode . BL.fromStrict <$> Z.receive sock
    return $ case resE of
        Right r  -> r
        Left err -> ResponseError $ T.pack err

