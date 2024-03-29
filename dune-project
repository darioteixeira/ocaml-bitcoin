(lang dune 2.8)

(name bitcoin)

(authors "Dario Teixeira <dario.teixeira@yahoo.com>")

(maintainers
 "Dario Teixeira <dario.teixeira@yahoo.com>"
 "Michael Bacarella <m@bacarella.com>")

(source
 (github "darioteixeira/ocaml-bitcoin"))

(homepage "https://github.com/darioteixeira/ocaml-bitcoin")

(generate_opam_files true)

(package
 (name bitcoin)
 (synopsis "Bitcoin Client API logic-only")
 (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
 (description
  "An OCaml interface to the official Bitcoin client API.\n   Works by making JSON-RPC calls over the network to a running\n   Bitcoin daemon\n  fering the client API.")
 (depends
  cryptokit
  (yojson
   (>= 1.6.0))))

(package
 (name bitcoin-cohttp-lwt)
 (synopsis "Bitcoin Client API cohttp-lwt interface")
 (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
 (description "This is the cohttp-lwt I/O interface to the bitcoin client")
 (depends
  (bitcoin
   (= :version))
  cohttp-lwt
  cohttp-lwt-unix))

(package
 (name bitcoin-cohttp-async)
 (synopsis "Bitcoin Client API cohttp-async interface")
 (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
 (description "This is the cohttp-async I/O interface to the bitcoin client")
 (depends
  (bitcoin
   (= :version))
  (cohttp-async
   (>= 5.0.0))))

;; XXX: disabling ocamlnet because it doesn't support OCaml 5.0.0 yet --mbacarella
;(package
; (name bitcoin-ocamlnet)
; (synopsis "Bitcoin Client API ocamlnet interface")
; (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
; (description "The ocamlnet I/O interface to the bitcoin client")
; (depends bitcoin ocamlnet))

(package
 (name bitcoin-ocurl)
 (synopsis "Bitcoin Client API ocurl interface")
 (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
 (description "The ocurl I/O interface to the bitcoin client")
 (depends
  (bitcoin
   (= :version))
  ocurl))

;; XXX: not familiar enough with ocsigen to get this to build --mbacarella
;(package
; (name bitcoin-ocsigen)
; (synopsis "Bitcoin Client API ocsigen interface")
; (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
; (description "The ocsigen I/O interface to the bitcoin client")
; (depends bitcoin ocsigen-start))
