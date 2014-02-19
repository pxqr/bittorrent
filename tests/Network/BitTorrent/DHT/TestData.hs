module Network.BitTorrent.DHT.TestData
       ( TestEntry (..)
       , testTorrents
       ) where

import Data.Torrent.InfoHash

data TestEntry = TestEntry
  { entryName  :: String
  , entryHash  :: InfoHash
  , entryPeers :: Int -- ^ approximate number of peers, may change with time
  }

testTorrents :: [TestEntry]
testTorrents =
  [ TestEntry
    { entryName  = "Automate with Arduino, Android..."
    , entryHash  = "8C0433E541DC5D1CFC095799CEF171CD4EB586F7"
    , entryPeers = 300
    }

  , TestEntry
    { entryName  = "Beginning Programming with Java For Dummies"
    , entryHash  = "FD8967721731CC16C8B203A03E49CE839CECF184"
    , entryPeers = 200
    }

  , TestEntry
    { entryName  = "The C Programming Language"
    , entryHash  = "146D13F090E50E97091DBBE5B37678DD1471CFAD"
    , entryPeers = 100
    }

  , TestEntry
    { entryName  = "The C++ Programming Language"
    , entryHash  = "8E8E8E6319031A22CFF26D895AFE050085C84A7F"
    , entryPeers = 50
    }

  , TestEntry
    { entryName  = "Game and Graphics Programming for iOS..."
    , entryHash  = "703D0595B727FCCBFAA3D03BE25F57347CCFD6DE"
    , entryPeers = 30
    }
  ]
