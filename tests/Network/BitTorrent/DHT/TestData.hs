module Network.BitTorrent.DHT.TestData
       ( TestEntry (..)
       , testTorrents
       ) where

import Data.Torrent

data TestEntry = TestEntry
  { entryName  :: String
  , entryHash  :: InfoHash
  , entryPeers :: Int -- ^ approximate number of peers, may change with time
  }

testTorrents :: [TestEntry]
testTorrents =
  [ TestEntry
    { entryName  = "Automate with Arduino, Android..."
    , entryHash  = "8c0433e541dc5d1cfc095799cef171cd4eb586f7"
    , entryPeers = 300
    }

  , TestEntry
    { entryName  = "Beginning Programming with Java For Dummies"
    , entryHash  = "fd8967721731cc16c8b203a03e49ce839cecf184"
    , entryPeers = 200
    }

  , TestEntry
    { entryName  = "The C Programming Language"
    , entryHash  = "146d13f090e50e97091dbbe5b37678dd1471cfad"
    , entryPeers = 100
    }

  , TestEntry
    { entryName  = "The C++ Programming Language"
    , entryHash  = "8e8e8e6319031a22cff26d895afe050085c84a7f"
    , entryPeers = 50
    }

  , TestEntry
    { entryName  = "Game and Graphics Programming for iOS..."
    , entryHash  = "703d0595b727fccbfaa3d03be25f57347ccfd6de"
    , entryPeers = 30
    }
  ]
