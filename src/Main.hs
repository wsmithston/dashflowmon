{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Concurrent     (forkIO, threadDelay)
import Control.Exception      (catch)
import Control.Monad.Loops    (iterateUntilM)
import Control.Monad          ((>=>), liftM, forever, forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.UnixTime          (getUnixTime, toClockTime, utSeconds)
import Data.Aeson             (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Configurator      (load, lookup, require, display,
                               Worth (Required)
                              )
import Data.Configurator.Types (Config)

import System.Process         (proc,CreateProcess, readCreateProcessWithExitCode)
import System.Exit            (ExitCode  (ExitSuccess, ExitFailure), exitWith)
import System.Environment     (getArgs)
import System.Console.GetOpt
                              ( getOpt
                              , usageInfo
                              , OptDescr (Option)
                              , ArgDescr (NoArg, ReqArg, OptArg)
                              , ArgOrder (Permute, RequireOrder)
                              )

import Database.PostgreSQL.Simple
                                  ( execute_
                                  , execute
                                  , executeMany
                                  , query
                                  , query_
                                  , Query
                                  , connect
                                  , Connection
                                  , ConnectInfo (..)
                                  , SqlError
                                  , sqlErrorMsg
                                  , Only
                                  )


import PostDB
                             ( tables
                             , dropAllTables
                             , insertBlock
                             , insertBlockNext
                             , insertBlockTx
                             , insertBlockTxIn
                             , insertPubkey
                             , insertTx
                             , insertTxIn
                             , insertTxOut
                             , qryBestBlock
                             , qryCurrentTxId
                             )


import Types

import Data.Maybe ( fromMaybe, isJust )
import Data.Word (Word16)

data Options = Options
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 , optOutput      :: Maybe FilePath
 , optInput       :: Maybe FilePath
 , optLibDirs     :: [FilePath]
 , optConfigFile  :: FilePath
 , optBlockNr     :: Int
 } deriving Show

defaultOptions    = Options
 { optVerbose     = False
 , optShowVersion = False
 , optOutput      = Nothing
 , optInput       = Nothing
 , optLibDirs     = []
 , optConfigFile  = "dashflow.cfg"
 , optBlockNr     = 1
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v']     ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True }))
     "chatty output on stderr"
 , Option ['V','?'] ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True }))
     "show version number"
 , Option ['o']     ["output"]
     (OptArg ((\f opts -> opts { optOutput = Just f }) . fromMaybe "output")
             "FILE")
     "output FILE"
 , Option ['c']     ["config"]
     (ReqArg (\c opts -> opts { optConfigFile = c }) "FILE")
     "configuration file"
 , Option ['b']     ["block"]
     (ReqArg (\x opts -> opts { optBlockNr = read x}) "BLOCKNR")
     "start from block b"
 ]

header :: String
header = "Usage: dashflow [OPTION...]"

getOptions :: [String] -> IO (Options, [String])
getOptions argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

getConfig :: FilePath -> IO Config
getConfig fp = load [ Required fp ]

-----------------   Main Input/Output   ---------------------
main :: IO ()
main = do args <- getArgs
          print args
          when ("-h" `elem` args) (putStrLn (usageInfo header options) >>
                                   exitWith ExitSuccess)

          (opts, nonopts) <- getOptions args
          cfg  <- getConfig (optConfigFile opts)
          display cfg
          mainloop opts cfg

mainloop :: Options -> Config ->IO ()
mainloop opts cfg = do conn <- connection cfg
                       initDB conn
                       forever (threadDelay 2000000 >> syncDB opts cfg)
                       -- in microseconds


--------------------    Database    ------------------------------
connection :: Config -> IO Connection
connection cfg = do host     <- require cfg "db.host" :: IO String
                    port     <- require cfg "db.port" :: IO Word16
                    user     <- require cfg "db.username" :: IO String
                    password <- require cfg "db.password" :: IO String
                    database <- require cfg "db.dbname" :: IO String

                    let dbInfo = ConnectInfo { connectHost     = host
                                             , connectPort     = port
                                             , connectUser     = user
                                             , connectPassword = password
                                             , connectDatabase = database
                                             }

                    connect dbInfo



-- Initialize if new
initDB :: Connection -> IO ()
initDB c =  dropEm c >> createTables c tables

dropEm :: Connection -> IO ()
dropEm conn = execute_ conn dropAllTables >>= print

createTables :: Connection -> [Query] -> IO ()
createTables conn queries =  mapM_ (catcherrors . action) queries
                             where action qry = (execute_ conn qry >>= print)
                                   catcherrors io = io `catch` handleSqlError

handleSqlError :: SqlError -> IO ()
handleSqlError e = putStrLn ("Caught SqlError: "++show (sqlErrorMsg e))

-- Synchronize if not up to date
syncDB :: Options -> Config -> IO ()
syncDB opts cfg =  do coreHeight <- readInt <$> getblockcount --Catch exc if client off!
                      --dbHeight <-
                      print $ "Core Height: "++ show coreHeight
                      --print $ "DB Height:   "++ show coreHeight

                      chunksize <- require cfg "monitor.chunksize" :: IO Int
                      let blk = optBlockNr opts
                      mapM_ (pushBlocks cfg) (chunks chunksize [blk..coreHeight])
                      -- updateDB -- Or: until (isUpToDate) updateDB

readInt :: String -> Int
readInt = read

-- | there is also Data.List.Split.splitEvery
chunks :: (Num a) => Int -> [a] -> [[a]]
chunks s [] = []
chunks s xs =  foo : chunks s bar
             where foo = fst (splitAt s xs)
                   bar = snd (splitAt s xs)

pushBlocks :: Config -> [Int] -> IO ()
pushBlocks cfg xs = do conn <- connection cfg
                       (blocks, txs) <- getBlockchain xs
                       (dbPushBlocks conn) blocks
                       (dbPushTxs conn) txs

getBlockchain :: [Int] -> IO ([Maybe Block], [Maybe [Tx]])
getBlockchain xs = do blocks <- mapM retrieveBlock xs  --[Maybe Block]
                      -- if this has Nothing's then ....?
                      when (not.and $ map isJust blocks) (putStrLn "Something is amiss")

                      let lastblock = last blocks
                      case lastblock of
                          Just b -> putStrLn ("Block height:" ++ ((show . height) b))
                          Nothing -> (print "Lastblock -> Nothing" >> exitWith (ExitFailure 1))

                      txs    <- mapM txsFromBlock blocks  --[Maybe [Tx]]
                      -- if this has Nothing's then ....?
                      return (blocks, txs)


retrieveBlock :: Int -> IO (Maybe Block)
retrieveBlock = getblockhash >=> getblock >=> (return . toBlock)

txsFromBlock :: Maybe Block -> IO (Maybe [Tx])
txsFromBlock mb = case mb of
                       Just b  -> (retrieveTxs (txs b))
                       Nothing -> return (Nothing)

-- fails for genesis coinbase transaction
retrieveTxs :: [String] -> IO (Maybe [Tx])
retrieveTxs xs =  bar (map retrieveTx xs)

retrieveTx :: String -> IO (Maybe Tx)
retrieveTx = getrawtransaction >=> decoderawtransaction >=> (return . toTx)

toTx :: String -> Maybe Tx
toTx s = decode (pack s) :: Maybe Tx

toBlock :: String -> Maybe Block
toBlock s = decode (pack s) :: Maybe Block

bar :: [IO (Maybe Tx)] -> IO (Maybe [Tx])
bar ios = sequence ios >>= return . (\mtxs -> sequence mtxs)
---------------------------------------------------

dbPushTxs :: Connection -> [Maybe [Tx]] -> IO ()
dbPushTxs conn mtxss = case sequence mtxss of
                            Just txss -> dbStoreTx conn (concat txss)
                            Nothing   -> print "Nothing!!"

dbStoreTx :: Connection -> [Tx] -> IO ()
dbStoreTx conn txs = do
    txid <- dbGetCurrentTxId conn
    x <- executeMany conn insertTx txs
    let txouts = zipWith attachTxId [txid+1..] (map txVout txs) -- [[(Int, Output)]]
    y <- executeMany conn insertTxOut (map toRowTxOut $ concat txouts)
--    let txins  = concat (map txVin txs)  -- txs >>= txVin?
--    z <- executeMany conn insertTxIn txins
    print ("Current TxId: "++show txid)
    print (show x ++ " txs")

--toRowTxIn :: Input -> (txid, txin_pos, txout_id, scriptSig, Int)
type TxOutRow = (Int, Int, Double,  String, Int)

attachTxId :: Int -> [Output] -> [(Int,Output)]
attachTxId id outputs = map (\o -> (id, o)) outputs

toRowTxOut :: (Int, Output) -> TxOutRow
toRowTxOut (txid, o) = (txid, n o, value o, hexPub (scriptPubKey o), pubkeyid o)

pubkeyid :: Output -> Int
pubkeyid o = 1234


dbGetCurrentTxId :: Connection -> IO (Int)
dbGetCurrentTxId conn = do xs   <- query_ conn qryCurrentTxId :: IO [TxId]
                           print xs
                           case xs of
                               []  -> return 0
                               [a] -> return (getTxId $ head xs)

                           --return (getTxId $ head xs)
                           --forM_ xs $ \id -> putStrLn (show (id :: Int))

getTxId :: TxId -> Int
getTxId (TxId id _) = id

dbPushBlocks :: Connection -> [Maybe Block] -> IO ()
dbPushBlocks c ms = case sequence ms of
                         Just blocks -> dbStoreBlocks c blocks
                         Nothing     -> return ()

-- | This is where you execute queries
dbStoreBlocks :: Connection -> [Block] -> IO ()
dbStoreBlocks c blocks = executeMany c insertBlock blocks >>= \x->
                         print (show x ++ " blocks inserted.")
                        --executeMany c insertBlockNext blocks



----------------------   cli commands   --------------------------
callClient :: CreateProcess -> IO (String)
callClient p = do result <- readCreateProcessWithExitCode p ""
                  case result of
                       (ExitSuccess  , stdout, _    ) -> return stdout
                       (ExitFailure 1, _     , err  ) -> return err
                       (ExitFailure _, _     , err  ) -> return err

cli :: [String] -> CreateProcess
cli args = proc "dash-cli" args

getbestblockhash :: IO (String)
getbestblockhash = callClient $ cli ["getbestblockhash"]

getblockcount :: IO (String)
getblockcount = callClient $ cli ["getblockcount"]

getblockhash :: Int -> IO (String)
getblockhash index = callClient $ cli ["getblockhash", show index]

getblockchaininfo :: IO (String)
getblockchaininfo = callClient $ cli ["getblockchaininfo"]

getblock :: String -> IO (String)
getblock hash = callClient $ cli ["getblock", hash]

getrawtransaction :: String -> IO (String)
getrawtransaction txid = callClient $ cli ["getrawtransaction", txid]

decoderawtransaction :: String -> IO (String)
decoderawtransaction hex = callClient $ cli ["decoderawtransaction", cleanhex]
                           where cleanhex = filter (/= '\n') hex

createrawtransaction :: String -> IO (String)
createrawtransaction s = callClient $ cli ["createrawtransaction", s]

getrawmempool ::IO (String)
getrawmempool = callClient $ cli ["getrawmempool"]

getrawmempooltrue ::IO (String)
getrawmempooltrue = callClient $ cli ["getrawmempool", "true"]

------------------    Logging Facilities    -----------------------
--logStart :: [String] -> IO ()
--logStart args = mapM_ writeToLog [ "Initializing System..."
--                                 , "Arguments received:  "
--                                 , (unwords args)
--                                 ]
--
--logEnd :: IO ()
--logEnd = writeToLog "Shutting Down"
--
--writeToLog :: String -> IO ()
--writeToLog entry = do time <- getTime
--                      logFilePath <- require cfg "log.filepath" :: IO String
--                      appendFile logFilePath (time++": "++entry++"\n")

getTime :: IO String
getTime = liftM (show . toClockTime) getUnixTime

