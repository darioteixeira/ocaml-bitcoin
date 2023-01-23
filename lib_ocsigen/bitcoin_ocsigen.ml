(********************************************************************************)
(*  Bitcoin_ocsigen.ml
    Copyright (c) 2012-2015 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

(** Offers an implementation of a {!Bitcoin.HTTPCLIENT} using Ocsigen's
    [Ocsigen_http_client].
*)


(********************************************************************************)
(** {1 Exceptions}                                                              *)
(********************************************************************************)

exception No_response


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Httpclient: Bitcoin.HTTPCLIENT with type 'a Monad.t = 'a Lwt.t =
struct
    module Monad = Lwt

    let post_string ~headers ~inet_addr ~host ~port ~uri request =
        let content = Ocsigen_stream.of_string request in
        let headers =
            let open Http_headers in
            let adder set (k, v) = add (name k) v set in
            List.fold_left adder empty headers in
        lwt frame = Ocsigen_http_client.raw_request
                ~https:false
                ~keep_alive:true
                ~headers
                ~content:(Some content)
                ~content_length:(Int64.of_int (String.length request))
                ~http_method:Ocsigen_http_frame.Http_header.POST
                ~inet_addr
                ~host
                ~port
                ~uri
                () ()
        in match frame.Ocsigen_http_frame.frame_content with
            | Some stream ->
                let buf = Buffer.create 4096 in
                let rec string_of_stream stream = match_lwt Ocsigen_stream.next stream with
                    | Ocsigen_stream.Cont (str, stream) -> Buffer.add_string buf str; string_of_stream stream
                    | Ocsigen_stream.Finished (Some stream) -> string_of_stream stream
                    | Ocsigen_stream.Finished None      -> Lwt.return (Buffer.contents buf) in
                lwt result = string_of_stream (Ocsigen_stream.get stream) in
                Ocsigen_stream.finalize stream `Success >>
                Lwt.return result
            | None ->
                Lwt.fail No_response
end

