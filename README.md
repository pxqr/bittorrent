# Synopsis

This package aims to provide a thin layer for bittorrent protocol.
Basically it provides serialization\deserealization and some widely used routines.

# Description

The module hierarhy is tend to be:

* Data.Torrent — for torrent metainfo, data verification, etc
* Network.Torrent.PeerWire - peer wire TCP message passing.
* Network.Torrent.Tracker  — tracker HTTP message passing.
