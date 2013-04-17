# Synopsis

This package provides a thin layer for bittorrent protocol so we can with less effort.
Basically it provides serialization\deserealization and some widely used routines.

# Description

A module hierarhy is tend to be:

* Data.Torrent — for torrent metainfo, data verification, etc
* Network.Torrent.PeerWire - peer wire TCP message passing.
* Network.Torrent.Tracker  — tracker HTTP message passing.
