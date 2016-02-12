module TestImport
    ( module TestImport
    , module X
    ) where

import System.Process        (runCommand, system)

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X
import Foundation            as X
import Test.Hspec            as X
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X

withApp :: SpecWith (TestApp App) -> Spec
withApp = around (bracket startHaskoin stopHaskoin)
  where
    startHaskoin = do
        void $ runCommand "hw -t -w .test -d start >/dev/null 2>&1"
        void $ system "echo | hw -t -w .test newacc test >/dev/null 2>&1"
        settings <- loadAppSettings
            ["config/test-settings.yml", "config/settings.yml"]
            []
            ignoreEnv
        foundation <- makeFoundation settings
        logWare <- liftIO $ makeLogWare foundation
        return (foundation, logWare)
    stopHaskoin = const $ do
        void $ system "hw -t -w .test stop >/dev/null 2>&1"
