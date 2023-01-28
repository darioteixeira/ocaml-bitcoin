(********************************************************************************)
(*  Bitcoin_ocamlnet.ml
    Copyright (c) 2012-2015 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

(** Offers an implementation of a {!Bitcoin.HTTPCLIENT} using Ocamlnet's
    [Http_client].
*)

(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Httpclient : Bitcoin.HTTPCLIENT with type 'a Monad.t = 'a = struct
  module Monad = struct
    type 'a t = 'a

    let return x = x
    let fail x = raise x
    let bind t f = f t

    let catch t f =
      try t () with
      | exc -> f exc
  end

  let post_string ~headers ~inet_addr ~host:_ ~port ~uri request =
    let dst = "http://" ^ Unix.string_of_inet_addr inet_addr ^ ":" ^ string_of_int port ^ uri in
    let pipeline = new Nethttp_client.pipeline in
    let request = new Nethttp_client.post_raw dst request in
    let () = List.iter (fun (k, v) -> request#set_req_header k v) headers in
    let () = pipeline#add request in
    let () = pipeline#run () in
    request#response_body#value
end
