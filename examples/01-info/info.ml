(********************************************************************************)
(*  Info.ml
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

(*  Minimal example illustrating how to setup a connection with Bitcoin's Testnet
    (the test network whose coins have by convention no monetary value) and request
    some information about the network.  Note that OCamlnet's HTTP client is used
    for making the requests to the Bitcoin daemon.
*)

open Bitcoin

(********************************************************************************)
(** {1 Inner modules}                                                           *)
(********************************************************************************)

module Testnet_connection = struct
  let default =
    Some
      { inet_addr = Unix.inet_addr_loopback;
        host = "localhost";
        port = 18332;
        username = "user";
        password = "password"
      }
end

module Testnet = Bitcoin.Make (Bitcoin_ocamlnet.Httpclient) (Testnet_connection)

(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let () =
  let blockcount = Testnet.getblockcount () in
  let difficulty = Testnet.getdifficulty () in
  let networkhashps = Testnet.getnetworkhashps () in
  Printf.printf "Information about the Testnet:\n";
  Printf.printf "\tBlock count: %d\n" blockcount;
  Printf.printf "\tDifficulty: %f\n" difficulty;
  Printf.printf "\tNetwork hashes per second (estimated): %d\n" networkhashps
