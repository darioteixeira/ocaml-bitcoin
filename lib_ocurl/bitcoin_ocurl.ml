(********************************************************************************)
(*  Bitcoin_ocurl.ml
    Copyright (c) 2013 Vincent Bernardoff <vb@luminar.eu.org>
*)
(********************************************************************************)

(** Offers an implementation of a {!Bitcoin.HTTPCLIENT} using OCurl's
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

  let buf = Buffer.create 100

  let post_string ~headers ~inet_addr ~host ~port ~uri request =
    let open Curl in
    let h = new handle in
    let dst = "http://" ^ host ^ ":" ^ string_of_int port ^ uri in
    try
      h#set_post true;
      h#set_postfields request;
      h#set_postfieldsize (String.length request);
      h#set_url dst;
      h#set_writefunction (fun s ->
          Buffer.add_string buf s;
          String.length s);
      h#set_httpheader (List.map (fun (k, v) -> k ^ ": " ^ v) headers);
      h#perform;
      h#cleanup;
      let contents = Buffer.contents buf in
      Buffer.reset buf;
      contents
    with
    | CurlException (_, i, s) as exn ->
      Printf.eprintf "EXN: %d %s\n%!" i s;
      raise exn
end
