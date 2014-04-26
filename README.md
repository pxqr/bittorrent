### BitTorrent [![Build Status][1]][2]

A [BitTorrent][0] library implementation. It allows to read/write
torrent files, transfer data files, query trackers and DHT. The
library is still in active development and have some subsystems
partially implemented.

For lastest released version and reference documentation see [hackage][3] page.

[0]: http://bittorrent.org/beps/bep_0000.html
[1]: https://travis-ci.org/cobit/bittorrent.png
[2]: https://travis-ci.org/cobit/bittorrent
[3]: http://hackage.haskell.org/package/bittorrent

### Status

The protocol has [many enchancements][bep-list]. This table keep track
if a particular BEP is "todo", "in progress" or "complete":

| BEP # | Title                                              | Status
|:-----:|:--------------------------------------------------:|:-----------
| 3     | [The BitTorrent Protocol Specification][bep3]      | [In progress][bep3-impl]
| 4     | [Known Number Allocations][bep4]                   | [In progress][bep4-impl]
| 5     | [DHT][bep5]                                        | [In progress][bep5-impl]
| 6     | [Fast Extension][bep6]                             | [In progress][bep6-impl]
| 7     | [IPv6 Tracker Extension][bep7]                     | [In progress][bep7-impl]
| 9     | [Extension for Peers to Send Metadata Files][bep9] | [In progress][bep9-impl]
| 10    | [Extension protocol][bep10]                        | [In progress][bep10-impl]
| 12    | [Multitracker Metadata Extension][bep10]           | [In progress][bep12-impl]
| 15    | [UDP Tracker Protocol for BitTorrent][bep15]       | [In progress][bep15-impl]
| 20    | [Peer ID Conventions][bep20]                       | [Implemented][bep20-impl]
| 23    | [Tracker Return Compact Peer Lists][bep23]         | [Implemented][bep23-impl]

[bep-list]: http://www.bittorrent.org/beps/bep_0000.html
[bep3]:  http://www.bittorrent.org/beps/bep_0003.html
[bep4]:  http://www.bittorrent.org/beps/bep_0004.html
[bep5]:  http://www.bittorrent.org/beps/bep_0005.html
[bep6]:  http://www.bittorrent.org/beps/bep_0006.html
[bep7]:  http://www.bittorrent.org/beps/bep_0007.html
[bep9]:  http://www.bittorrent.org/beps/bep_0009.html
[bep10]: http://www.bittorrent.org/beps/bep_0010.html
[bep12]: http://www.bittorrent.org/beps/bep_0012.html
[bep15]: http://www.bittorrent.org/beps/bep_0015.html
[bep20]: http://www.bittorrent.org/beps/bep_0020.html
[bep23]: http://www.bittorrent.org/beps/bep_0023.html

[bep3-impl]:  src
[bep4-impl]:  src/Network/BitTorrent/Exchange/Message.hs
[bep5-impl]:  src/Network/BitTorrent/DHT/Protocol.hs
[bep6-impl]:  src/Network/BitTorrent/Exchange/Message.hs
[bep7-impl]:  src/Network/BitTorrent/Tracker/Message.hs
[bep9-impl]:  src/Network/BitTorrent/Exchange/Wire.hs
[bep10-impl]: src/Network/BitTorrent/Exchange/Message.hs
[bep12-impl]: src/Data/Torrent.hs
[bep15-impl]: src/Network/BitTorrent/Tracker/RPC/UDP.hs
[bep20-impl]: src/Network/BitTorrent/Core/Fingerprint.hs
[bep23-impl]: src/Network/BitTorrent/Tracker/Message.hs

### Hacking

The root directory layout is as follows:

* examples -- includes demo utilities to get started;
* src      -- the library source tree;
* tests    -- the library test suite;
* res      -- torrents and data files used in test suite.
* sub      -- subprojects and submodules used by the library and still in dev.

Some subdirectories includes README with futher explanations to get started.

### Contacts

* Discussions: IRC [#haskell-bittorrent][irc] at irc.freenode.net
* Bugs & issues: [issue tracker][tracker]
* Maintainer: <pxqr.sta@gmail.com>

[tracker]: https://github.com/cobit/bittorrent/issues/new
[irc]: http://webchat.freenode.net/?channels=haskell-bittorrent
