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
* **Modules** — modules that _directly_ relates to the BEP. This is where
  BEP implemented, where we should look for the BEP. If a module has
  only changes due to integration with the BEP then it _should not_ be
  listed in the **Modules** list.
* **Status** — is the current state of the BEP. Status lifecycle has the
  only way: Want -> Implemented -> Tested -> Integrated -> Done. You
  might use (A -> B) to indicate intermediate steps.  Note that
  implemented _doesn't_ means that BEP is ready to use, it _will_ be
  ready to use only after it pass Tested, Integrated and finally
  becomes Done.

We should try keep table in order of priority, so first BEPs should be
are most important and last BEPs are least important. (but important
too)

| BEP # | Title                                              | Status
|:-----:|:--------------------------------------------------:|:-----------
| 3     | [The BitTorrent Protocol Specification][bep3]      | Implemented
| 4     | [Known Number Allocations][bep4]                   | Want -> Implemented
| 20    | [Peer ID Conventions][bep20]                       | Want -> Implemented
| 15    | [UDP Tracker Protocol for BitTorrent][bep15]       | Want -> Implemented
| 9     | [Extension for Peers to Send Metadata Files][bep9] | Want
| 23    | [Tracker Return Compact Peer Lists][bep23]         | Implemented
| 5     | [DHT][bep5]                                        | Want
| 6     | [Fast Extension][bep6]                             | Want -> Implemented

[bep-list]: http://www.bittorrent.org/beps/bep_0000.html
[bep3]:  http://www.bittorrent.org/beps/bep_0003.html
[bep4]:  http://www.bittorrent.org/beps/bep_0004.html
[bep5]:  http://www.bittorrent.org/beps/bep_0005.html
[bep6]:  http://www.bittorrent.org/beps/bep_0006.html
[bep9]:  http://www.bittorrent.org/beps/bep_0009.html
[bep15]: http://www.bittorrent.org/beps/bep_0015.html
[bep20]: http://www.bittorrent.org/beps/bep_0020.html
[bep23]: http://www.bittorrent.org/beps/bep_0023.html
