{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

               {-   DASH Masternodes Monitor   -}

import Control.Concurrent     (forkIO, threadDelay)
import Control.Exception      (catch)
import Control.Monad.Loops    (iterateUntilM)
import Control.Monad          ((>=>), liftM, forever, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.UnixTime          (getUnixTime, toClockTime, utSeconds)
import Data.Aeson             (decode)
import Data.ByteString.Lazy.Char8 (pack)

import System.Process         (proc,CreateProcess, readCreateProcessWithExitCode)
import System.Exit (ExitCode  (ExitSuccess, ExitFailure))
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


import Hardcodedstuff
                             ( logFilePath
                             , connection
                             )

import Types

import Data.Maybe ( fromMaybe )

data Options = Options
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 , optOutput      :: Maybe FilePath
 , optInput       :: Maybe FilePath
 , optLibDirs     :: [FilePath]
 } deriving Show

defaultOptions    = Options
 { optVerbose     = False
 , optShowVersion = False
 , optOutput      = Nothing
 , optInput       = Nothing
 , optLibDirs     = []
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
     (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
             "FILE")
     "output FILE"
 , Option ['L']     ["libdir"]
     (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
     "library directory"
 ]


-----------------   Main Input/Output   ---------------------
main :: IO ()
main =  getArgs >>= \args ->
        case getOpt Permute options args of
             (o, n, [])   -> mainloop o n
             (_, _, errs) -> forM_ (errs ++ [usageInfo header options]) putStrLn
        where header = "Usage: dashflow [OPTION...]"

mainloop :: [(Options -> Options)] -> [String] ->IO ()
mainloop o n = connection >>= \c ->
               initDB c >>
               forever (threadDelay 2000000 >> syncDB)
                     -- in microseconds

--------------------    Database    ------------------------------
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
syncDB :: IO ()
syncDB =  do coreHeight <- readInt <$> getblockcount --Catch exc if client off!
             --dbHeight <-
             print $ "Core Height: "++ show coreHeight
             --print $ "DB Height:   "++ show coreHeight
             mapM_ pushBlocks (chunks 1 [270000..coreHeight])
             -- updateDB -- Or: until (isUpToDate) updateDB

readInt :: String -> Int
readInt = read

-- | there is also Data.List.Split.splitEvery
chunks :: (Num a) => Int -> [a] -> [[a]]
chunks s [] = []
chunks s xs =  foo : chunks s bar
             where foo = fst (splitAt s xs)
                   bar = snd (splitAt s xs)

pushBlocks :: [Int] -> IO ()
pushBlocks xs = do c <- connection
                   (blocks, txs) <- getBlockchain xs
                   (dbPushBlocks c) blocks
                   (dbPushTxs c) txs

getBlockchain :: [Int] -> IO ([Maybe Block], [Maybe [Tx]])
getBlockchain xs = do blocks <- mapM retrieveBlock xs  --[Maybe Block]
                      txs    <- mapM txsFromBlock blocks  --[Maybe [Tx]]
                      return (blocks, txs)

txsFromBlock :: Maybe Block -> IO (Maybe [Tx])
txsFromBlock mb = case mb of
                       Just b  -> (retrieveTxs (txs b))
                       Nothing -> return (Nothing)

-------------- SIMPLIFY THIS ----------------------
retrieveTx :: String -> IO (Maybe Tx)
retrieveTx = getrawtransaction >=> decoderawtransaction >=> toTxIO
-- fails for genesis coinbase transaction,

retrieveTxs :: [String] -> IO (Maybe [Tx])
retrieveTxs xs =  bar (foo xs)

foo :: [String] -> [IO (Maybe Tx)]
foo xs = map retrieveTx xs

bar :: [IO (Maybe Tx)] -> IO (Maybe [Tx])
bar ios = sequence ios >>= return . (\mtxs -> sequence mtxs)
---------------------------------------------------

dbPushTxs :: Connection -> [Maybe [Tx]] -> IO ()
dbPushTxs conn mtxss = case sequence mtxss of
                            Just txss -> dbStoreTx conn (concat txss)
                            Nothing   -> print "Nothing!!"

dbStoreTx :: Connection -> [Tx] -> IO ()
dbStoreTx conn txs = do
    txid <- dbGetCurrentTxId
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


dbGetCurrentTxId :: IO (Int)
dbGetCurrentTxId = do c <- connection
                      xs <- query_ c qryCurrentTxId :: IO [TxId]
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
                         print (show x ++ " blocks")
                        --executeMany c insertBlockNext blocks

toTx :: String -> Maybe Tx
toTx s = decode (pack s) :: Maybe Tx

toTxIO :: String -> IO (Maybe Tx)
toTxIO s = return (toTx s)

toBlock :: String -> Maybe Block
toBlock s = decode (pack s) :: Maybe Block

toBlockIO :: String -> IO (Maybe Block)
toBlockIO s = return (toBlock s)

retrieveBlock :: Int -> IO (Maybe Block)
retrieveBlock = getblockhash >=> getblock >=> toBlockIO

retrieveBestBlock :: IO (Maybe Block)
retrieveBestBlock = getbestblockhash >>= (getblock >=> toBlockIO)



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
logStart :: [String] -> IO ()
logStart args = mapM_ writeToLog [ "Initializing System..."
                                 , "Arguments received:  "
                                 , (unwords args)
                                 ]

logEnd :: IO ()
logEnd = writeToLog "Shutting Down"

writeToLog :: String -> IO ()
writeToLog entry = do time <- getTime
                      appendFile logFilePath (time++": "++entry++"\n")

getTime :: IO String
getTime = liftM (show . toClockTime) getUnixTime

