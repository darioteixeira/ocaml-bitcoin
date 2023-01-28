open! Core
open! Async

module Mainnet_connection = struct
  let default =
    Some
      { Bitcoin.inet_addr = Unix.Inet_addr.localhost;
        host = "my-ip";
        port = 8332;
        username = "my-username";
        password = "my-password";
      }
end

module Mainnet = Bitcoin.Make (Bitcoin_cohttp_async.Httpclient) (Mainnet_connection)

let cmd =
  (*let open Command.Let_syntax in*)
  Command.async
    ~summary:"test bitcoin cohttp async"
    (Command.Param.return (fun () ->
         Mainnet.getblockchaininfo ()
         >>= fun assoc_or_error ->
         match assoc_or_error with
         | Error e -> failwithf "getblockchaininfo: %s" (Exn.to_string e) ()
         | Ok assoc ->
           printf "getblockchaininfo:\n";
           List.iter assoc ~f:(fun (key, json) -> printf "%s: %s\n" key (Yojson.Safe.show json));
           return ()))

let () = Command_unix.run ~version:"foo" cmd
