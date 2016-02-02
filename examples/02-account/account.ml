(********************************************************************************)
(*  Account.ml
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

(*  This example displays the balance of the various users accounts, plus the
    overall balance.  Note that the connection to Bitcoin's Testnet is done
    using the Cohttp backend.  In practice this means that all API invocations
    are done under the Lwt monad.
*)

open Lwt
open Bitcoin


(********************************************************************************)
(** {1 Inner modules}                                                           *)
(********************************************************************************)

module Testnet_connection =
struct
    let default = Some
        {
        inet_addr = Unix.inet_addr_loopback;
        host = "localhost";
        port = 18332;
        username = "user";
        password = "password";
        }
end


module Testnet = Bitcoin.Make (Bitcoin_cohttp.Httpclient) (Testnet_connection)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let print_accounts () =
    let printer (account, amount) =
        let account' = match account with
            | `Default -> "Default account"
            | `Named s -> Printf.sprintf "Account '%s'" s in
        Lwt_io.printf "%s has a balance of %Ld satoshi.\n" account' amount in
    Testnet.listaccounts () >>= fun xs ->
    Lwt_list.iter_s printer xs

let print_balance () =
    Testnet.getbalance () >>= fun balance ->
    Lwt_io.printf "Total balance is %Ld satoshi (= %f BTC)\n" balance (Bitcoin.float_of_amount balance)

let () =
    Lwt_main.run
    begin
        print_accounts () >>
        print_balance ()
    end

