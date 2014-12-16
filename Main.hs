{-# LANGUAGE OverloadedStrings     #-}
module Main where

import System.Console.GetOpt 
    ( getOpt
    , usageInfo
    , OptDescr (Option)
    , ArgDescr (ReqArg)
    , ArgOrder (Permute)
    )
import qualified System.Environment as E (getArgs)

import Control.Monad (forM_, mzero)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Logger (runNoLoggingT)

import Data.Default (Default, def)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS (ByteString, empty)
import Data.Aeson 
    ( FromJSON(..)
    , ToJSON(..)
    , Value(..)
    , object
    , toJSON
    , withObject
    , (.=), (.:), (.:?)
    )

import Database.Persist.MySQL
    ( ConnectInfo(..)
    , defaultConnectInfo
    , createMySQLPool
    )

import Network.Haskoin.StdOptions
import Network.Haskoin.Faucet
import Network.Haskoin.Constants
import Network.Haskoin.Util

type Args = [String]

data Options = Options
    { optLimit          :: !Word64
    , optReset          :: !Word32
    , optMinConf        :: !Int
    , optWalletName     :: !String
    , optAccountName    :: !String
    , optWalletUrl      :: !String
    , optWalletToken    :: !(Maybe (BS.ByteString, BS.ByteString))
    , optMySQL          :: !ConnectInfo
    , optPool           :: !Int
    , optNce            :: !(Maybe FilePath)
    , optStd            :: !StdOptions
    } 

instance Default Options where
    def = Options
        { optLimit         = configLimit def
        , optReset         = configReset def
        , optMinConf       = configMinConf def
        , optWalletName    = configWalletName def
        , optAccountName   = configAccountName def
        , optWalletUrl     = configWalletUrl def
        , optWalletToken   = configWalletToken def
        , optMySQL         = defaultConnectInfo
        , optPool          = 10
        , optNce           = Nothing
        , optStd           = def { optPort = configPort def
                                 , optLog  = "hf.log"
                                 , optPid  = "hf.pid"
                                 }
        } 

instance ToJSON Options where
    toJSON opt = object $
        [ "limit"       .= optLimit opt
        , "reset"       .= optReset opt
        , "minconf"     .= optMinConf opt
        , "walletname"  .= optWalletName opt
        , "accountname" .= optAccountName opt
        , "walleturl"   .= optWalletUrl opt
        , "mysql" .= object
            [ "host"     .= (connectHost $ optMySQL opt)
            , "post"     .= (connectPort $ optMySQL opt)
            , "user"     .= (connectUser $ optMySQL opt)
            , "password" .= (connectPassword $ optMySQL opt)
            , "database" .= (connectDatabase $ optMySQL opt)
            , "filepath" .= (connectPath $ optMySQL opt)
            ]
        , "pool"  .= optPool opt
        , "server" .= optStd opt
        ]
        ++ maybe [] (\x -> [("wallettoken" .= f x)]) (optWalletToken opt)
        ++ maybe [] (\x -> [("noncefile" .= x)]) (optNce opt)
      where
        f (i,s) = object 
            [ "token" .= bsToString i
            , "secret" .= bsToString s 
            ]

instance FromJSON Options where
    parseJSON = withObject "options" $ \o -> Options
        <$> o .: "limit"
        <*> o .: "reset"
        <*> o .: "minconf"
        <*> o .: "walletname"
        <*> o .: "accountname"
        <*> o .: "walleturl"
        <*> ( g =<< o .:? "wallettoken" )
        <*> ( mysqlConfig =<< o .: "mysql" )
        <*> o .: "pool"
        <*> o .:? "noncefile"
        <*> o .: "server"
      where
        g Nothing = return Nothing
        g (Just (Object o)) = do
            t <- o .: "token"
            s <- o .: "secret"
            return $ Just (stringToBS t, stringToBS s)
        g _ = mzero
        mysqlConfig = withObject "mysql" $ \o -> do
            h <- o .: "host"
            p <- o .: "post"
            u <- o .: "user"
            x <- o .: "password"
            d <- o .: "database"
            f <- o .: "filepath"
            return $ defaultConnectInfo
                { connectHost = h
                , connectPort = p
                , connectUser = u
                , connectPassword = x
                , connectDatabase = d
                , connectPath = f
                }

instance StdServer Options where
    getStdOptions a   = optStd a
    setStdOptions a n = a{ optStd = n }
    serviceName _     = "hf"
    newStdConfig a    = return a

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['l'] ["limit"]
        (ReqArg (\s opts -> opts{ optLimit = read s }) "SATOSHI")
        "Maximum withdrawal limit (default: 10000)"
    , Option ['r'] ["reset"] 
        (ReqArg (\s opts -> opts{ optReset = read s }) "SECONDS")
        "Withdrawal reset time in seconds (default: 3600)"
    , Option ['m'] ["minconf"] 
        (ReqArg (\s opts -> opts{ optMinConf = read s }) "INT")
        "Minimum number of confirmations to use (default: 3)"
    , Option ['w'] ["walletname"] 
        (ReqArg (\s opts -> opts{ optWalletName = s }) "STRING")
        "Name of the wallet in hw (default: faucet)"
    , Option ['a'] ["accountname"] 
        (ReqArg (\s opts -> opts{ optAccountName = s }) "STRING")
        "Name of the account in hw (default: public)"
    , Option ['u'] ["walleturl"]
        (ReqArg (\s opts -> opts{ optWalletUrl = s }) "URL")
        "URL of the hw wallet"
    , Option ['t'] ["token"] (ReqArg parseToken "STRING")
        "HMAC authentication token identifier"
    , Option ['s'] ["secret"] (ReqArg parseSecret "STRING")
        "HMAC authentication token secret"
    , Option [] ["sqlpool"]
        (ReqArg (\p opts -> opts{ optPool = read p }) "INT")
        "MySQL connection pool size (default: 10)"
    , Option [] ["sqlhost"] 
        (ReqArg (\s opts -> 
            opts{ optMySQL = (optMySQL opts) { connectHost = s } }) "HOST")
        "MySQL Host (Default: localhost)"
    , Option [] ["sqlport"] 
        (ReqArg (\s opts -> 
            opts{ optMySQL = (optMySQL opts) { connectPort = read s } }) "PORT")
        "MySQL Port (Default: 3306)"
    , Option [] ["sqluser"] 
        (ReqArg (\s opts -> 
            opts{ optMySQL = (optMySQL opts) { connectUser = s } }) "STRING")
        "MySQL User (Default: root)"
    , Option [] ["sqlpwd"] 
        (ReqArg (\s opts -> 
            opts{ optMySQL = (optMySQL opts) { connectPassword = s } }) "STRING")
        "MySQL Password (Default: \"\")"
    , Option [] ["sqldb"] 
        (ReqArg (\s opts -> 
            opts{ optMySQL = (optMySQL opts) { connectDatabase = s } }) "STRING")
        "MySQL Database (Default: test)"
    , Option [] ["sqlpath"] 
        (ReqArg (\s opts -> 
            opts{ optMySQL = (optMySQL opts) { connectPath = s } }) "STRING")
        "MySQL Path"
    , Option ['N'] ["noncefile"]
        (ReqArg (\n opts -> opts{ optNce = Just n }) "FILE")
        "Nonce file"
    ]
    ++ srvOptions
  where
    parseToken s opts =
        let m = optWalletToken opts
        in opts{ optWalletToken = Just (stringToBS s, maybe BS.empty snd m) }
    parseSecret s opts =
        let m = optWalletToken opts
        in opts{ optWalletToken = Just (maybe BS.empty fst m, stringToBS s) }

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

usageHeader :: String
usageHeader = "Usage: hf [<options>] <command>"

cmdHelp :: [String]
cmdHelp = 
    [ "Server commands:" 
    , "  start [--detach]   Start the haskoin faucet"
    , "  stop               Stop the haskoin faucet"
    , ""
    , "Other commands: "
    , "  version            Display version information"
    , "  help               Display this help information"
    ]

usage :: String
usage = unlines $ [usageInfo usageHeader options] ++ cmdHelp

processCommand :: Options -> Args -> IO ()
processCommand opts args = case args of
    ["start"] -> do
        let config = FaucetConfig
                { configLimit       = optLimit opts
                , configReset       = optReset opts
                , configMinConf     = optMinConf opts
                , configWalletName  = optWalletName opts
                , configAccountName = optAccountName opts
                , configBind        = optBind $ optStd opts
                , configPort        = optPort $ optStd opts
                , configWalletUrl   = optWalletUrl opts
                , configWalletToken = optWalletToken opts
                , configNceFile     = fromMaybe "token-nonces" $ optNce opts
                }

        maybeDetach opts $ do
            pool <- runNoLoggingT $ 
                createMySQLPool (optMySQL opts) (optPool opts)
            runServer config pool
    ["stop"] -> stopProcess opts
    [] -> formatStr usage
    ["help"] -> formatStr usage
    ["version"] -> putStrLn haskoinUserAgent
    _ -> error "Invalid command"

main :: IO ()
main = E.getArgs >>= \args -> case getOpt Permute options args of
    (o,xs,[]) -> do
        opts <- processOptions o
        processCommand opts xs
    (_,_,msgs) -> print $ unlines $ msgs ++ [usage]

