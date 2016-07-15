{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PostDB where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Types (Block (..))

tables :: [Query]  --order MATTERS, since FK must refer to existing PK.
tables = [ ctBlock
         , ctTx
         , ctBlockNext
         , ctBlockTx
         , ctBlockTxIn
         , ctPubkey
         , ctTxOut
         , ctTxIn
         ]

---------------------     Tables definitions      ------------------------
ctBlock :: Query
ctBlock = [sql| CREATE TABLE block
                ( id            BIGSERIAL PRIMARY KEY
                , hash          CHAR(64)  NOT NULL
                , size          INT
                , height        INT
                , version       INT
                , merkleroot    CHAR(64)
                , time          BIGINT
                , nonce         BIGINT
                , bits          CHAR(8)
                , difficulty    NUMERIC
                , chainwork     CHAR(64)
                )|]

ctBlockNext :: Query
ctBlockNext = [sql| CREATE TABLE block_next
                    ( block_id      BIGSERIAL REFERENCES block(id)
                    , next_block_id BIGSERIAL REFERENCES block(id)
                    , PRIMARY KEY (block_id, next_block_id)
                    )|]

ctBlockTx :: Query
ctBlockTx = [sql| CREATE TABLE block_tx
                  ( block_id BIGSERIAL REFERENCES block(id)
                  , tx_id    BIGSERIAL REFERENCES tx(id)
                  , tx_pos   INT       NOT NULL
                  , PRIMARY KEY (block_id, tx_id)
                  , UNIQUE (block_id, tx_pos)
                  )|]

ctBlockTxIn :: Query
ctBlockTxIn = [sql| CREATE TABLE block_txin
                    ( block_id     BIGSERIAL REFERENCES block(id)
                    , txin_id      BIGSERIAL REFERENCES tx(id)
                    , out_block_id BIGSERIAL REFERENCES block(id)
                    , PRIMARY KEY (block_id, txin_id)
                    )|]

ctPubkey :: Query
ctPubkey = [sql| CREATE TABLE pubkey
                 ( id     BIGSERIAL    PRIMARY KEY
                 , hash   CHAR(40)
                 , pubkey VARCHAR(130)
                 )|]

ctTx :: Query
ctTx = [sql| CREATE TABLE tx
             ( id       BIGSERIAL PRIMARY KEY
             , hash     CHAR(64)  UNIQUE NOT NULL
             , size     INT       NOT NULL
             , version  INT       NOT NULL
             , locktime BIGINT
             )|]

ctTxIn :: Query
ctTxIn = [sql| CREATE TABLE txin
               ( id        BIGSERIAL     PRIMARY KEY
               , tx_id     BIGSERIAL     REFERENCES tx(id)
               , pos       INT
               , txout_id  BIGSERIAL     REFERENCES txout(id)
               , scriptsig VARCHAR(5000)
               , sequence  INT
               )|]

ctTxOut :: Query
ctTxOut = [sql| CREATE TABLE txout
                ( id           BIGSERIAL     PRIMARY KEY
                , tx_id        BIGSERIAL     REFERENCES tx(id)
                , pos          INT
                , value        BIGINT
                , scriptpubkey VARCHAR(5000)
--                , sequence     INT
                , pubkey_id    BIGSERIAL     --REFERENCES pubkey(id) fix!
                )|]

-----------------------    SQL Insert Statements    ----------------------
insertBlock :: Query
insertBlock = [sql| INSERT INTO block
                        (hash, size, height,
                         version, merkleroot, time,
                         nonce, difficulty, chainwork)
                    VALUES (?,?,?,?,?,?,?,?,?)|]

insertBlockNext :: Query
insertBlockNext = [sql| INSERT INTO block_next (block_id, next_block_id)
                        VALUES (?,?)|]

insertBlockTx :: Query
insertBlockTx = [sql| INSERT INTO block_tx (block_id, tx_id, tx_pos)
                      VALUES (?,?,?)|]

insertBlockTxIn :: Query
insertBlockTxIn = [sql| INSERT INTO block_txin (block_id, txin_id, out_block_id)
                        VALUES (?,?,?)|]

insertPubkey :: Query
insertPubkey = [sql| INSERT INTO pubkey (id, hash, pubkey )
                     VALUES (?,?,?)|]

insertTx :: Query
insertTx = [sql| INSERT INTO tx (hash, size, version, locktime)
                 VALUES (?,?,?,?)|]

insertTxIn :: Query
insertTxIn = [sql| INSERT INTO txin (tx_id, pos, txout_id, scriptsig, sequence)
                   VALUES (?,?,?,?,?)|]

insertTxOut :: Query
insertTxOut = [sql| INSERT INTO txout (tx_id, pos, value, scriptpubkey, pubkey_id)
                    VALUES (?,?,?,?,?)|]

--------------------------------------------------------------------
qryBestBlock :: Query
qryBestBlock =  [sql| SELECT hash, height
                        FROM block
                        ORDER BY id DESC
                        limit 1 |]

qryCurrentTxId :: Query
qryCurrentTxId = [sql| SELECT id,hash FROM tx
                       ORDER BY id DESC
                       limit 1 |]

dropAllTables :: Query
dropAllTables = [sql| DROP TABLE IF EXISTS block, block_next, block_tx,
                      block_txin, pubkey, tx, txin, txout|]

--------------- Masternode Monitor Queries ------------------------------
insertMN :: Query
insertMN = [sql| INSERT INTO mn_full (vin, status, protocol, pubkey,
                                      ip, lastseen, activeseconds, lastpaid)
                 VALUES (?,?,?,?,?,?,?,?)|]

