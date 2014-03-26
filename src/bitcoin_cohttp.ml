(********************************************************************************)
(*	Bitcoin_cohttp.ml
	Copyright (c) 2013 Vincent Bernardoff (vb@luminar.eu.org)
*)
(********************************************************************************)

(**	Offers an implementation of a {!Bitcoin.HTTPCLIENT} using Cohttp's
	[Cohttp_lwt_unix.Client].
*)

(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception No_response


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

module C = Cohttp
module CU = Cohttp_lwt_unix
module CB = Cohttp_lwt_body

module Httpclient: Bitcoin.HTTPCLIENT with type 'a Monad.t = 'a Lwt.t =
struct
  module Monad = Lwt

  let (>>=) = Lwt.bind

  let post_string ~headers ~inet_addr ~host ~port ~uri request =
    let headers = C.Header.of_list headers in
    let uri = Uri.make ~scheme:"http" ~host ~port ~path:uri () in
    Lwt.try_bind
      (fun () -> CU.Client.call ~chunked:false ~headers ~body:(CB.of_string request) `POST uri)
      (fun (_, b) -> CB.to_string b)
      (fun exn ->
         Lwt_io.printf "Cohttp_lwt_unix.Client.call: caught %s\n" (Printexc.to_string exn) >>= fun () ->
         Lwt.fail exn)
end
