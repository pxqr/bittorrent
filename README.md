# Synopsis

This package aims to provide a thin layer for bittorrent protocol.
Basically it provides serialization\deserealization and some widely used routines.

# Description

The module hierarhy is tend to be:

* Data.Torrent — for torrent metainfo, data verification, etc
* Network.Torrent.PeerWire — peer wire TCP message passing.
* Network.Torrent.Tracker  — tracker HTTP message passing.

# Build Status

[![Build Status][1]][2]

[1]: https://travis-ci.org/pxqr/network-bittorrent.png
[2]: https://travis-ci.org/pxqr/network-bittorrent
