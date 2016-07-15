{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Applicative    ((<$>), (<*>), empty)
import Data.Aeson
import GHC.Generics           (Generic)
import Control.Monad
import Data.Text              (Text)
import Data.Scientific        (Scientific)
import Data.ByteString.Lazy   (ByteString)

import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow



---------------------------  ScriptSig ---------------------------------
data ScriptSig = ScriptSig { asmSig :: String
                           , hexSig :: String
                           } deriving (Show, Generic)

instance ToJSON ScriptSig
instance FromJSON ScriptSig where
    parseJSON (Object v) = ScriptSig <$> v .: "asm"
                                     <*> v .: "hex"

---------------------------  ScriptPubKey -------------------------------
data ScriptPubKey = ScriptPubKey { asmPub    :: String
                                 , hexPub    :: String
                                 , reqSigs   :: Int
                                 , type_     :: String
                                 , addresses :: [String]
                                 } deriving (Show, Generic)

instance ToJSON ScriptPubKey
instance FromJSON ScriptPubKey where
    parseJSON (Object v) = ScriptPubKey <$> v .: "asm"
                                        <*> v .: "hex"
                                        <*> v .: "reqSigs"
                                        <*> v .: "type"
                                        <*> v .: "addresses"

---------------------------  Input -----------------------------------------
data Input = Input { inTx         :: Maybe String
                   , coinbase   :: Maybe String
                   , inVout       :: Maybe Int
                   , inScriptSig  :: Maybe ScriptSig
                   , inSequence   :: Int
                   } deriving (Show, Generic)

instance ToJSON Input
instance FromJSON Input where
    parseJSON (Object v) = Input <$> v .:? "txid"
                                 <*> v .:? "coinbase"
                                 <*> v .:? "vout"
                                 <*> v .:? "scriptSig"
                                 <*> v .: "sequence"

------------------------  Output  ------------------------------------------
data Output = Output { value        :: Double
                     , n            :: Int
                     , scriptPubKey :: ScriptPubKey
                     } deriving (Show, Generic)

instance ToJSON Output
instance FromJSON Output

------------------------  Transactions  -----------------------------------
data Tx = Tx { txHash      :: String
             , txVersion :: Int
             , txLocktime  :: Int
             , txVin       :: [Input]
             , txVout      :: [Output]
             } deriving (Show, Generic)

instance ToJSON Tx
instance FromJSON Tx where
    parseJSON (Object v) = Tx <$> v .: "txid"
                              <*> v .: "version"
                              <*> v .: "locktime"
                              <*> v .: "vin"
                              <*> v .: "vout"

instance ToRow Tx where
    toRow (Tx hash version locktime vin vout) =
        toRow (hash, getsize, version, locktime)
            where getsize = 180*(length vin) + 34*(length vout) + 10
                    -- needfix

------------------------  Blocks  ---------------------------------------
data Block = Block { hash              :: String
                  -- , confirmations     :: Int
                   , size              :: Int
                   , height            :: Int
                   , versionBlock      :: Int
                   , merkleroot        :: String
                   , txs               :: [String]
                   , time              :: Int
                   , nonce             :: Int
                   , bits              :: String
                   , difficulty        :: Scientific
                   , chainwork         :: String
      --             , previousblockhash :: String
      --             , nextblockhash     :: Maybe String
                   } deriving (Show, Generic)

instance FromJSON Block where
    parseJSON (Object v) = Block <$> v .: "hash"
    --                             <*> v .: "confirmations"
                                 <*> v .: "size"
                                 <*> v .: "height"
                                 <*> v .: "version"
                                 <*> v .: "merkleroot"
                                 <*> v .: "tx"
                                 <*> v .: "time"
                                 <*> v .: "nonce"
                                 <*> v .: "bits"
                                 <*> v .: "difficulty"
                                 <*> v .: "chainwork"
     --                            <*> v .: "previousblockhash"
     --                            <*> v .:? "nextblockhash"
    parseJSON _ = mzero

instance ToRow Block where
    toRow (Block hash size height ver me tx time nonce bits diff work ) =
        toRow (hash, size, height, ver, me, time, nonce,diff,work)

--instance FromRow Block where
--    fromRow = Block <$> field <*> field <*> field <*> field <*> field <*> field
--                    <*> field <*> field <*> field <*> field <*> field <*> field

data TxId = TxId Int String deriving (Show)

instance FromRow TxId where
    fromRow = TxId <$> field <*> field

data MasterNode = MasterNode { vinM           :: String
                             , status        :: String
                             , protocol      :: Int
                             , pubKey        :: String
                             , ipAddr        :: String
                             , lastseen      :: Int
                             , activeseconds :: Int
                             , lastpaid      :: Int
                             } deriving (Show)

instance ToRow MasterNode where
    toRow (MasterNode vinM sta pro pub ipA las act last) =
       toRow (vinM, sta, pro, pub, ipA, las, act, last)

--------- Mock data --------
mnA = MasterNode "e99bae11718efd44c266e3fbb248f2961733e4d0f233a48bf7ca7749cb56a6e4-0" "  ENABLED" 70103 "XbRh8HX1txRTphKcDCgDtK3iLWVKPEBrcs" "95.85.1.197:9999" 1440021444 181970 0


