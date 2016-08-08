{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- This module will be deleted once we can parse
    configuration files
-}

module Hardcodedstuff where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Network.HTTP.Types.Header (Header)


logFilePath :: FilePath
logFilePath = "monitorlog.ascii"

dbTestnet :: ConnectInfo
dbTestnet = ConnectInfo { connectHost="127.0.0.1"
                        , connectPort=5432
                        , connectUser="wsmithston"
                        , connectPassword=""
                        , connectDatabase="dashblockchain"
                        }

connection :: IO Connection
connection = connect dbTestnet

mainnetURL = "http://localhost:9998"
testnetUrl = "http://localhost:19998"
----------------- HTTP stuff ------------------
contentType :: Header
contentType = ("content-type","text/plain")

testnet3auth :: Header
testnet3auth = ("Authorization","Basic c21pdGg6emlqZGV6YWNodAo=")

