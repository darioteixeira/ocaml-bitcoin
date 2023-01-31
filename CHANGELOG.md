unreleased to opam
========================
* Port to dune
* Separate each I/O interface into distinct opam packages
* Add cohttp-async
* Run ocamlformat
* Extend getblock to support verbosity > 1
* Extend gettxout to handle Null replies

Version 2.0 (2015-07-19)
==========================
* Port to Bitcoin Core 0.10.0 API.
* Functions grouped according to their categorisation (as defined by Bitcoin Core).
* New engines based on OCurl and Cohttp, contributed by Vincent Bernardoff.
* Change of license to LGPL-2.1 (with OCaml linking exception).

Version 1.1.1 (2013-05-23)
==========================
* Bug fix: now calling Ocsigen_stream.finalize in Bitcoin_ocsigen module,
  which should fix the problem of leaking file descriptors.

Version 1.1 (2013-02-26)
========================
* Updated API against the recently released Bitcoin 0.8.

Version 1.0 (2012-10-24)
========================
* First public release.
