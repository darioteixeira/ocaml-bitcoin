(*  Bitcoin_cohttp_async.ml
    Copyright (c) 2021 Michael Bacarella <m@bacarella.com> *)
open! Core
open! Async

(** Offers an implementation of a {!Bitcoin.HTTPCLIENT} using Cohttp's [Cohttp_lwt_async.Client].
*)

(********************************************************************************)
(** {1 Exceptions}                                                              *)
(********************************************************************************)

exception No_response

(********************************************************************************)
(** {1 Private modules}                                                         *)
(********************************************************************************)

module C = Cohttp
module CA = Cohttp_async
module CB = Cohttp_async.Body

(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Httpclient : Bitcoin.HTTPCLIENT with type 'a Monad.t = ('a, exn) Result.t Deferred.t = struct
  (* Unlike Lwt promises, Async promises don't attach errors to it.  So,
     we wrap Async promises around a Result.t to satisfy the interface. *)
  module Monad = struct
    type 'a t = ('a, exn) Result.t Deferred.t

    let return v = Deferred.return (Ok v)
    let fail e = Deferred.return (Error e)

    let bind d f =
      d
      >>= fun res ->
      match res with
      | Ok v -> f v
      | Error e -> Deferred.return (Error e)

    let catch f g =
      f ()
      >>= fun res ->
      match res with
      | Ok v -> Deferred.return (Ok v)
      | Error e -> g e
  end

  let post_string ~headers ~inet_addr:_ ~host ~port ~uri request =
    (* Leaving out the 'connection: close' here causes lingering old connections to pile up. *)
    let headers = C.Header.of_list (("connection", "close") :: headers) in
    let uri = Uri.make ~scheme:"http" ~host ~port ~path:uri () in
    Monitor.try_with (fun () ->
        Cohttp_async.Client.call ~chunked:false ~headers ~body:(CB.of_string request) `POST uri)
    >>= fun res ->
    match res with
    | Error exn -> Monad.fail exn
    | Ok (_, b) -> CB.to_string b >>= fun body -> Monad.return body
end
