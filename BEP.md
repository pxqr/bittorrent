The protocol has [many extensions][bep-list] (more precisely
enchancements, but we'll use that word) and it's seems like no one
will want to use just core protocol, at least I'm not. Any modern
application that uses bittorrent protocol in any way will use some
subset of extensions.  Thus it's reasonable to implement at least some
part of widely used extensions here, so we could provide nice high
level API and well integrated interface.

This section should keep track current state of package in respect of
BEP's.  Please _don't_ use this list as issue or bug tracker or TODO
list or anything else: when in doubt don't change the table.

In order to keep table compact we describe table layout at first:

* **BEP #**   — Just number of enchancement.
* **Title**   — Full official enchancement name.
* **Status** — is the current state of the BEP. Status lifecycle has
  the only way: Want -> In Progress -> Implemented.

We should try keep table in order of priority, so first BEPs should be
are most important and last BEPs are least important. (but important
too)

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
