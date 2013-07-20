-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module implement low-level UDP tracker protocol.
--   For more info see: http://www.bittorrent.org/beps/bep_0015.html
--
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.Tracker.UDP
       ( Request(..), Response(..)
       ) where

import Control.Applicative
import Control.Monad
import Data.Serialize
import Data.Word
import Data.Text
import Data.Text.Encoding

import Data.Torrent ()
import Network.BitTorrent.Tracker.Protocol


-- | Connection Id is used for entire tracker session.
newtype ConnId  = ConnId  { getConnId  :: Word64 }
                  deriving (Show, Serialize)

-- | Transaction Id is used for within UDP RPC.
newtype TransId = TransId { getTransId :: Word32 }
                  deriving (Show, Serialize)

genConnectionId :: IO ConnId
genConnectionId = return (ConnId 0)

genTransactionId :: IO TransId
genTransactionId = return (TransId 0)

data Request  = Connect
              | Announce  AnnounceQuery
              | Scrape    ScrapeQuery

data Response = Connected
              | Announced AnnounceInfo
              | Scraped   [ScrapeInfo]
              | Failed    Text

-- TODO rename to message?
data Transaction a = Transaction
  { connId  :: !ConnId
  , transId :: !TransId
  , body    :: !a
  } deriving Show

type MessageId = Word32

connectId, announceId, scrapeId, errorId :: MessageId
connectId  = 0
announceId = 1
scrapeId   = 2
errorId    = 3

instance Serialize (Transaction Request) where
  put Transaction {..} = do
    case body of
      Connect        -> do
        put connId
        put connectId
        put transId

      Announce query -> do
        put connId
        put announceId
        put transId
        put query

      Scrape   hashes -> do
        put connId
        put announceId
        put transId
        forM_ hashes put

  get = do
    cid <- get
    rid <- getWord32be
    tid <- get
    bod <- getBody rid

    return $ Transaction {
        connId  = cid
      , transId = tid
      , body    = bod
      }
    where
      getBody :: MessageId -> Get Request
      getBody msgId
        | msgId == connectId  = return Connect
        | msgId == announceId = Announce <$> get
        | msgId == scrapeId   = Scrape   <$> many get
        |       otherwise     = fail "unknown message id"

instance Serialize (Transaction Response) where
  put Transaction {..} = do
    case body of
      Connected -> do
        put connId
        put connectId
        put transId

      Announced info -> do
        put connId
        put announceId
        put transId
        put info

      Scraped infos -> do
        put connId
        put scrapeId
        put transId
        forM_ infos put

      Failed info -> do
        put connId
        put errorId
        put transId
        put (encodeUtf8 info)


  get = do
    cid <- get
    rid <- getWord32be
    tid <- get
    bod <- getBody rid

    return $ Transaction {
        connId  = cid
      , transId = tid
      , body    = bod
      }
    where
      getBody :: MessageId -> Get Response
      getBody msgId
        | msgId == connectId  = return $ Connected
        | msgId == announceId = Announced <$> get
        | msgId == scrapeId   = Scraped   <$> many get
        | msgId == errorId    = do
          bs <- get
          case decodeUtf8' bs of
            Left ex   -> fail (show ex)
            Right msg -> return $ Failed msg
        |      otherwise      = fail "unknown message id"
