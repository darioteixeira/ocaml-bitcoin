(********************************************************************************)
(*	Bitcoin.ml
	Copyright (c) 2012 Dario Teixeira (dario.teixeira@yahoo.com)
*)
(********************************************************************************)


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Unspecified_connection
exception Bitcoin_error of int * string
exception Internal_error of int * string
exception Httpclient_error of exn


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type address_t = string
type account_t = [ `Default | `Named of string ]
type amount_t = int64
type txid_t = string
type txoutput_t = txid_t * int
type blkhash_t = string
type priv_t = string
type pub_t = string
type sig_t = string
type sigcomp_t = [ `All | `None | `Single ]
type hextx_t = string
type hexspk_t = string
type hexblk_t = string
type hexwork_t = string
type node_t = string
type addnodeop_t = [ `Add | `Remove | `Onetry ]
type lockop_t = [ `Lock | `Unlock ]
type assoc_t = (string * Yojson.Safe.json) list

type conn_t =
	{
	inet_addr: Unix.inet_addr;
	host: string;
	port: int;
	username: string;
	password: string;
	}


(********************************************************************************)
(**	{1 Public module types}							*)
(********************************************************************************)

module type HTTPCLIENT =
sig
	module Monad:
	sig
		type 'a t

		val return: 'a -> 'a t
		val fail: exn -> 'a t
		val bind: 'a t -> ('a -> 'b t) -> 'b t
		val catch: (unit -> 'a t) -> (exn -> 'a t) -> 'a t
	end

	val post_string:
		headers:(string * string) list ->
		inet_addr:Unix.inet_addr ->
		host:string ->
		port:int ->
		uri:string ->
		string ->
		string Monad.t
end


module type CONNECTION =
sig
	val default: conn_t option
end


module type ENGINE =
sig
	type 'a monad_t

	val addnode: ?conn:conn_t -> node_t -> addnodeop_t -> unit monad_t
	val backupwallet: ?conn:conn_t -> string -> unit monad_t
	val createmultisig: ?conn:conn_t -> int -> pub_t list -> (address_t * hexspk_t) monad_t
	val createrawtransaction: ?conn:conn_t -> txoutput_t list -> (address_t * amount_t) list -> hextx_t monad_t
	val decoderawtransaction: ?conn:conn_t -> hextx_t -> assoc_t monad_t
	val dumpprivkey: ?conn:conn_t -> address_t -> priv_t monad_t
	val encryptwallet: ?conn:conn_t -> string -> unit monad_t
	val getaccount: ?conn:conn_t -> address_t -> account_t monad_t
	val getaccountaddress: ?conn:conn_t -> account_t -> address_t monad_t
        val getaddednodeinfo: ?conn:conn_t -> ?node:node_t -> unit -> assoc_t monad_t
        val getaddednodeinfo_verbose: ?conn:conn_t -> ?node:node_t -> unit -> assoc_t list monad_t
	val getaddressesbyaccount: ?conn:conn_t -> account_t -> address_t list monad_t
	val getbalance: ?conn:conn_t -> ?account:account_t -> ?minconf:int -> unit -> amount_t monad_t
	val getblock: ?conn:conn_t -> blkhash_t -> assoc_t monad_t
	val getblockcount: ?conn:conn_t -> unit -> int monad_t
	val getblockhash: ?conn:conn_t -> int -> blkhash_t monad_t
	val getblocktemplate: ?conn:conn_t -> ?obj:assoc_t -> unit -> assoc_t monad_t
	val getconnectioncount: ?conn:conn_t -> unit -> int monad_t
	val getdifficulty: ?conn:conn_t -> unit -> float monad_t
	val getgenerate: ?conn:conn_t -> unit -> bool monad_t
	val gethashespersec: ?conn:conn_t -> unit -> int monad_t
	val getinfo: ?conn:conn_t -> unit -> assoc_t monad_t
	val getmininginfo: ?conn:conn_t -> unit -> assoc_t monad_t
	val getnewaddress: ?conn:conn_t -> ?account:account_t -> unit -> address_t monad_t
	val getpeerinfo: ?conn:conn_t -> unit -> assoc_t list monad_t
	val getrawmempool: ?conn:conn_t -> unit -> txid_t list monad_t
	val getrawtransaction: ?conn:conn_t -> txid_t -> hextx_t monad_t
	val getrawtransaction_verbose: ?conn:conn_t -> txid_t -> assoc_t monad_t
	val getreceivedbyaccount: ?conn:conn_t -> ?minconf:int -> account_t -> amount_t monad_t
	val getreceivedbyaddress: ?conn:conn_t -> ?minconf:int -> address_t -> amount_t monad_t
	val gettransaction: ?conn:conn_t -> txid_t -> assoc_t monad_t
	val gettxout: ?conn:conn_t -> ?includemempool:bool -> txoutput_t -> assoc_t monad_t
	val gettxoutsetinfo: ?conn:conn_t -> unit -> assoc_t monad_t
	val getwork_with_data: ?conn:conn_t -> hexwork_t -> bool monad_t
	val getwork_without_data: ?conn:conn_t -> unit -> assoc_t monad_t
	val importprivkey: ?conn:conn_t -> ?account:account_t -> ?rescan:bool-> priv_t -> unit monad_t
	val keypoolrefill: ?conn:conn_t -> unit -> unit monad_t
	val listaccounts: ?conn:conn_t -> ?minconf:int -> unit -> (account_t * amount_t) list monad_t
	val listaddressgroupings: ?conn:conn_t -> unit -> (address_t * amount_t * account_t) list list monad_t
	val listlockunspent: ?conn:conn_t -> unit -> txoutput_t list monad_t
	val listreceivedbyaccount: ?conn:conn_t -> ?minconf:int -> ?includeempty:bool -> unit -> (account_t * amount_t * int) list monad_t
	val listreceivedbyaddress: ?conn:conn_t -> ?minconf:int -> ?includeempty:bool -> unit -> (address_t * account_t * amount_t * int) list monad_t
	val listsinceblock: ?conn:conn_t -> ?blockhash:blkhash_t -> ?minconf:int -> unit -> (assoc_t list * blkhash_t) monad_t
	val listtransactions: ?conn:conn_t -> ?account:account_t -> ?count:int -> ?from:int -> unit -> assoc_t list monad_t
	val listunspent: ?conn:conn_t -> ?minconf:int -> ?maxconf:int -> ?addresses:address_t list -> unit -> assoc_t list monad_t
	val lockunspent: ?conn:conn_t -> ?outputs:txoutput_t list -> lockop_t -> bool monad_t
	val move: ?conn:conn_t -> ?minconf:int -> ?comment:string -> account_t -> account_t -> amount_t -> bool monad_t
	val sendfrom: ?conn:conn_t -> ?minconf:int -> ?comment:string -> ?recipient:string -> account_t -> address_t -> amount_t -> txid_t monad_t
	val sendmany: ?conn:conn_t -> ?minconf:int -> ?comment:string -> account_t -> (address_t * amount_t) list -> txid_t monad_t
	val sendrawtransaction: ?conn:conn_t -> hextx_t -> txid_t monad_t
	val sendtoaddress: ?conn:conn_t -> ?comment:string -> ?recipient:string -> address_t -> amount_t -> txid_t monad_t
	val setaccount: ?conn:conn_t -> address_t -> account_t -> unit monad_t
	val setgenerate: ?conn:conn_t -> ?genproclimit:int -> bool -> unit monad_t
	val settxfee: ?conn:conn_t -> amount_t -> bool monad_t
	val signmessage: ?conn:conn_t -> address_t -> string -> sig_t monad_t
	val signrawtransaction: ?conn:conn_t -> ?parents:(txoutput_t * hexspk_t) list -> ?keys:priv_t list -> ?sighash:(sigcomp_t * bool) -> hextx_t -> (hextx_t * bool) monad_t
	val submitblock: ?conn:conn_t -> hexblk_t -> unit monad_t
	val validateaddress: ?conn:conn_t -> address_t -> assoc_t option monad_t
	val verifymessage: ?conn:conn_t -> address_t -> sig_t -> string -> bool monad_t
	val walletlock: ?conn:conn_t -> unit -> unit monad_t
	val walletpassphrase: ?conn:conn_t -> string -> int -> unit monad_t
	val walletpassphrasechange: ?conn:conn_t -> string -> string -> unit monad_t
end


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let amount_of_float x =
	Int64.of_float (x *. 1e8 +. (if x < 0.0 then -0.5 else 0.5))

let float_of_amount x =
	(Int64.to_float x) /. 1e8


(********************************************************************************)
(**	{1 Public functors}							*)
(********************************************************************************)

module Make (Httpclient: HTTPCLIENT) (Connection: CONNECTION) : ENGINE with type 'a monad_t = 'a Httpclient.Monad.t =
struct
	open Httpclient

	(************************************************************************)
	(**	{2 Type definitions}						*)
	(************************************************************************)

	type 'a monad_t = 'a Monad.t


	(************************************************************************)
	(**	{2 Private functions and values}				*)
	(************************************************************************)


	(************************************************************************)
	(**	{3 Common operators}						*)
	(************************************************************************)

	let (|>) x f = f x

	let (|?) x y = match x with
		| Some x -> x
		| None	 -> y

	let (>>=) t f = Monad.bind t f

	let (>|=) m f = Monad.bind m (fun x -> Monad.return (f x))


	(************************************************************************)
	(**	{3 Low-level functions}						*)
	(************************************************************************)

	let invoke ?conn ?(params = []) methode =
		begin match (conn, Connection.default) with
			| (Some conn, _)    -> Monad.return conn
			| (None, Some conn) -> Monad.return conn
			| (None, None)	    -> Monad.fail Unspecified_connection
		end >>= fun conn ->
		let headers =
			[
			("content-type", "application/json");
			("authorization", "Basic " ^ (Netencoding.Base64.encode (conn.username ^ ":" ^ conn.password)));
			] in
		let request = `Assoc
			[
			("method", `String methode);
			("params", `List params);
			("id", `Int 0);
			] in
		let xrequest = Yojson.Safe.to_string request in
		Monad.catch
			(fun () ->
				Httpclient.post_string
					~headers
					~inet_addr:conn.inet_addr
					~host:conn.host
					~port:conn.port
					~uri:"/"
					xrequest)
			(function exc -> Monad.fail (Httpclient_error exc)) >>= fun xresponse ->
		match Yojson.Safe.from_string xresponse with
			| `Assoc [("result", result); ("error", error); ("id", _)] ->
				begin match (result, error) with
					| (x, `Null) ->
						Monad.return x
					| (`Null, `Assoc [("code", `Int code); ("message", `String message)]) ->
						let exc =
							if code <= -3 && code >= -17
							then Bitcoin_error (code, message)
							else Internal_error (code, message)
						in Monad.fail exc
					| (_, _) ->
						assert false
				end
			| _ -> assert false


	(************************************************************************)
	(**	{3 Conversion between {!account_t}} and [string]		*)
	(************************************************************************)

	let account_of_string = function
		| ""  -> `Default
		| x   -> `Named x

	let string_of_account = function
		| `Wildcard -> "*"
		| `Default  -> ""
		| `Named x  -> x


	(************************************************************************)
	(**	{3 Conversion between {!sigcomp_t} and [string]			*)
	(************************************************************************)

	let string_of_sigcomp = function
		| `All	  -> "ALL"
		| `None	  -> "NONE"
		| `Single -> "SINGLE"


	(************************************************************************)
	(**	{3 Conversion between {!addnodeop_t} and [string]		*)
	(************************************************************************)

	let string_of_addnodeop = function
		| `Add	  -> "add"
		| `Remove -> "remove"
		| `Onetry -> "onetry"


	(************************************************************************)
	(**	{3 Conversion from JSON values to OCaml values}			*)
	(************************************************************************)

	let to_unit = function
		| `Null -> ()
		| _	-> assert false

	let to_int = function
		| `Int x -> x
		| _	 -> assert false

	let to_int64 = function
		| `Int x    -> Int64.of_int x
		| `Intlit x -> Int64.of_string x
		| _	    -> assert false

	let to_float = function
		| `Float x -> x
		| _	   -> assert false

	let to_bool = function
		| `Bool x -> x
		| _	  -> assert false

	let to_string = function
		| `String x -> x
		| _	    -> assert false

	let to_amount x =
		to_float x |> amount_of_float

	let to_account x =
		to_string x |> account_of_string

	let to_list f = function
		| `List xs -> List.map f xs
		| _	   -> assert false

	let to_assoc = function
		| `Assoc xs -> xs
		| _	    -> assert false


	(************************************************************************)
	(**	{3 Conversion from OCaml values to JSON values}			*)
	(************************************************************************)

	let of_int x = `Int x

	let of_float x = `Float x

	let of_bool x = `Bool x

	let of_string x = `String x

	let of_list conv xs = `List (List.map conv xs)

	let of_assoc x = `Assoc x

	let of_account x = string_of_account x |> of_string

	let of_account_with_wildcard x =
		let x = (x :> [ `Wildcard | account_t] option) |? `Wildcard in
		of_account x

	let of_amount x = float_of_amount x |> of_float

	let params_of_1tuple f1 = function
		| Some x -> [f1 x]
		| None   -> []

	let params_of_2tuple fname f1 f2 = function
		| (Some x, Some y) -> [f1 x; f2 y]
		| (Some x, None)   -> [f1 x]
		| (None, None)     -> []
		| _		   -> invalid_arg fname

	let params_of_3tuple fname f1 f2 f3 = function
		| (Some x, Some y, Some z) -> [f1 x; f2 y; f3 z]
		| (Some x, Some y, None)   -> [f1 x; f2 y]
		| (Some x, None, None)	   -> [f1 x]
		| (None, None, None)	   -> []
		| _			   -> invalid_arg fname


	(************************************************************************)
	(**	{2 Public functions and values}					*)
	(************************************************************************)

	let addnode ?conn node op =
		invoke ?conn ~params:[of_string node; string_of_addnodeop op |> of_string] "addnode" >|= to_unit

	let backupwallet ?conn dest =
		invoke ?conn ~params:[of_string dest] "backupwallet" >|= to_unit

	let createmultisig ?conn num pubs =
		let to_result = function
			| `Assoc [("address", v1); ("redeemScript", v2)] -> (to_string v1, to_string v2)
			| _						 -> assert false in
		invoke ?conn ~params:[of_int num; of_list of_string pubs] "createmultisig" >|= to_result

	let createrawtransaction ?conn inputs outputs =
		let of_input (txid, vout) =
			of_assoc [("txid", of_string txid); ("vout", of_int vout)] in
		let of_output (address, amount) =
			(address, of_amount amount) in
		let params =
			[
			of_list of_input inputs;
			of_assoc (List.map of_output outputs);
			] in
		invoke ?conn ~params "createrawtransaction" >|= to_string

	let decoderawtransaction ?conn hextx =
		invoke ?conn ~params:[of_string hextx] "decoderawtransaction" >|= to_assoc

	let dumpprivkey ?conn address =
		invoke ?conn ~params:[of_string address] "dumpprivkey" >|= to_string

	let encryptwallet ?conn passphrase =
		invoke ?conn ~params:[of_string passphrase] "encryptwallet" >|= to_unit

	let getaccount ?conn address =
		invoke ?conn ~params:[of_string address] "getaccount" >|= to_account

	let getaccountaddress ?conn account =
		invoke ?conn ~params:[of_account account] "getaccountaddress" >|= to_string

	let getaddednodeinfo ?conn ?node () =
		invoke ?conn ~params:(of_bool false :: params_of_1tuple of_string node) "getaddednodeinfo" >|= to_assoc

	let getaddednodeinfo_verbose ?conn ?node () =
		invoke ?conn ~params:(of_bool true :: params_of_1tuple of_string node) "getaddednodeinfo" >|= to_list to_assoc

	let getaddressesbyaccount ?conn account =
		invoke ?conn ~params:[of_account account] "getaddressesbyaccount" >|= to_list to_string

	let getbalance ?conn ?account ?(minconf = 1) () =
		invoke ?conn ~params:[of_account_with_wildcard account; of_int minconf] "getbalance" >|= to_amount

	let getblock ?conn hash =
		invoke ?conn ~params:[of_string hash] "getblock" >|= to_assoc

	let getblockcount ?conn () =
		invoke ?conn "getblockcount" >|= to_int

	let getblockhash ?conn index =
		invoke ?conn ~params:[of_int index] "getblockhash" >|= to_string

	let getblocktemplate ?conn ?obj () =
		invoke ?conn ~params:(params_of_1tuple of_assoc obj) "getblocktemplate" >|= to_assoc

	let getconnectioncount ?conn () =
		invoke ?conn "getconnectioncount" >|= to_int

	let getdifficulty ?conn () =
		invoke ?conn "getdifficulty" >|= to_float

	let getgenerate ?conn () =
		invoke ?conn "getgenerate" >|= to_bool

	let gethashespersec ?conn () =
		invoke ?conn "gethashespersec" >|= to_int

	let getinfo ?conn () =
		invoke ?conn "getinfo" >|= to_assoc

	let getmininginfo ?conn () =
		invoke ?conn "getmininginfo" >|= to_assoc

	let getnewaddress ?conn ?(account = `Default) () =
		invoke ?conn ~params:[of_account account] "getnewaddress" >|= to_string

	let getpeerinfo ?conn () =
		invoke ?conn "getpeerinfo" >|= to_list to_assoc

	let getrawmempool ?conn () =
		invoke ?conn "getrawmempool" >|= to_list to_string

	let getrawtransaction ?conn txid =
		invoke ?conn ~params:[of_string txid; of_int 0] "getrawtransaction" >|= to_string

	let getrawtransaction_verbose ?conn txid =
		invoke ?conn ~params:[of_string txid; of_int 1] "getrawtransaction" >|= to_assoc

	let getreceivedbyaccount ?conn ?(minconf = 1) account =
		invoke ?conn ~params:[of_account account; of_int minconf] "getreceivedbyaccount" >|= to_amount

	let getreceivedbyaddress ?conn ?(minconf = 1) address =
		invoke ?conn ~params:[of_string address; of_int minconf] "getreceivedbyaddress" >|= to_amount

	let gettransaction ?conn txid =
		invoke ?conn ~params:[of_string txid] "gettransaction" >|= to_assoc

	let gettxout ?conn ?(includemempool = true) (txid, num) =
		invoke ?conn ~params:[of_string txid; of_int num; of_bool includemempool] "gettxout" >|= to_assoc

	let gettxoutsetinfo ?conn () =
		invoke ?conn "gettxoutsetinfo" >|= to_assoc

	let getwork_with_data ?conn hexwork =
		invoke ?conn ~params:[of_string hexwork] "getwork" >|= to_bool

	let getwork_without_data ?conn () =
		invoke ?conn "getwork" >|= to_assoc

	let importprivkey ?conn ?(account = `Default) ?(rescan = true) priv =
		invoke ?conn ~params:[of_string priv; of_account account; of_bool rescan] "importprivkey" >|= to_unit

	let keypoolrefill ?conn () =
		invoke ?conn "keypoolrefill" >|= to_unit

	let listaccounts ?conn ?(minconf = 1) () =
		let to_result = function
			| `Assoc xs -> List.map (fun (k, v) -> ((account_of_string k), (to_amount v))) xs
			| _	    -> assert false in
		invoke ?conn ~params:[of_int minconf] "listaccounts" >|= to_result

	let listaddressgroupings ?conn () =
		let to_result = function
			| `List [a; b]	  -> (to_string a, to_amount b, `Default)
			| `List [a; b; c] -> (to_string a, to_amount b, to_account c)
			| _		  -> assert false in
		invoke ?conn "listaddressgroupings" >|= to_list (to_list to_result)

	let listlockunspent ?conn () =
		let to_locked = function
			| `Assoc [("txid", v1); ("vout", v2)] -> (to_string v1, to_int v2)
			| _				      -> assert false in
		invoke ?conn "listlockunspent" >|= to_list to_locked

	let listreceivedbyaccount ?conn ?(minconf = 1) ?(includeempty = false) () =
		let to_result = function
			| `Assoc [("account", v1); ("amount", v2); ("confirmations", v3)] -> (to_account v1, to_amount v2, to_int v3)
			| _								  -> assert false in
		invoke ?conn ~params:[of_int minconf; of_bool includeempty] "listreceivedbyaccount" >|= to_list to_result

	let listreceivedbyaddress ?conn ?(minconf = 1) ?(includeempty = false) () =
		let to_result = function
			| `Assoc [("address", v1); ("account", v2); ("amount", v3); ("confirmations", v4)] -> (to_string v1, to_account v2, to_amount v3, to_int v4)
			| _										   -> assert false in
		invoke ?conn ~params:[of_int minconf; of_bool includeempty] "listreceivedbyaddress" >|= to_list to_result

	let listsinceblock ?conn ?blockhash ?minconf () =
		let to_result = function
			| `Assoc [("transactions", v1); ("lastblock", v2)] -> (to_list to_assoc v1, to_string v2)
			| _						   -> assert false in
		invoke ?conn ~params:(params_of_2tuple "listsinceblock" of_string of_int (blockhash, minconf)) "listsinceblock" >|= to_result

	let listtransactions ?conn ?account ?(count = 10) ?(from = 0) () =
		invoke ?conn ~params:[of_account_with_wildcard account; of_int count; of_int from] "listtransactions" >|= to_list to_assoc

	let listunspent ?conn ?(minconf = 1) ?(maxconf = 999999) ?(addresses = []) () =
		invoke ?conn ~params:[of_int minconf; of_int maxconf; of_list of_string addresses] "listunspent" >|= to_list to_assoc

	let lockunspent ?conn ?outputs op =
		let bool_of_lockop = function
			| `Lock	  -> false
			| `Unlock -> true in
		let of_output (txid, vout) =
			of_assoc [("txid", of_string txid); ("vout", of_int vout)] in
		let params = (bool_of_lockop op |> of_bool) :: params_of_1tuple (of_list of_output) outputs in
		invoke ?conn ~params "lockunspent" >|= to_bool

	let move ?conn ?(minconf = 1) ?(comment = "") from_account to_account amount =
		let params = [of_account from_account; of_account to_account; of_amount amount; of_int minconf; of_string comment] in
		invoke ?conn ~params "move" >|= to_bool

	let sendfrom ?conn ?(minconf = 1) ?(comment = "") ?(recipient = "") from_account to_address amount =
		let params = [of_account from_account; of_string to_address; of_amount amount; of_int minconf; of_string comment; of_string recipient] in
		invoke ?conn ~params "sendfrom" >|= to_string

	let sendmany ?conn ?(minconf = 1) ?(comment = "") from_account targets =
		let params =
			[
			of_account from_account;
			of_assoc (List.map (fun (address, amount) -> (address, of_amount amount)) targets);
			of_int minconf;
			of_string comment;
			] in
		invoke ?conn ~params "sendmany" >|= to_string

	let sendrawtransaction ?conn hextx =
		invoke ?conn ~params:[of_string hextx] "sendrawtransaction" >|= to_string

	let sendtoaddress ?conn ?(comment = "") ?(recipient = "") to_address amount =
		let params = [of_string to_address; of_amount amount; of_string comment; of_string recipient] in
		invoke ?conn ~params "sendtoaddress" >|= to_string

	let setaccount ?conn address account =
		invoke ?conn ~params:[of_string address; of_account account] "setaccount" >|= to_unit

	let setgenerate ?conn ?(genproclimit = -1) gen =
		invoke ?conn ~params:[of_bool gen; of_int genproclimit] "setgenerate" >|= to_unit

	let settxfee ?conn amount =
		invoke ?conn ~params:[of_amount amount] "settxfee" >|= to_bool

	let signmessage ?conn address msg =
		invoke ?conn ~params:[of_string address; of_string msg] "signmessage" >|= to_string

	let signrawtransaction ?conn ?parents ?keys ?sighash hextx =
		let of_parent ((txid, vout), scriptpubkey) =
			of_assoc [("txid", of_string txid); ("vout", of_int vout); ("scriptPubKey", of_string scriptpubkey)] in
		let of_parents =
			of_list of_parent in
		let of_keys =
			of_list of_string in
		let of_sighash (sigcomp, anyone_can_pay) =
			(string_of_sigcomp sigcomp) ^ (if anyone_can_pay then "|ANYONECANPAY" else "") |> of_string in
		let to_result = function
			| `Assoc [("hex", `String hextx); ("complete", `Bool complete)] -> (hextx, complete)
			| _								-> assert false in
		let params = of_string hextx :: params_of_3tuple "signrawtransaction" of_parents of_keys of_sighash (parents, keys, sighash) in
		invoke ?conn ~params "signrawtransaction" >|= to_result

	let submitblock ?conn hexblk =
		invoke ?conn ~params:[of_string hexblk] "submitblock" >|= to_unit

	let validateaddress ?conn address =
		let to_result = function
			| `Assoc [("isvalid", `Bool false)] -> None
			| x				    -> Some (to_assoc x) in
		invoke ?conn ~params:[of_string address] "validateaddress" >|= to_result

	let verifymessage ?conn address signature msg =
		invoke ?conn ~params:[of_string address; of_string signature; of_string msg] "verifymessage" >|= to_bool

	let walletlock ?conn () =
		invoke ?conn "walletlock" >|= to_unit

	let walletpassphrase ?conn passphrase timeout =
		invoke ?conn ~params:[of_string passphrase; of_int timeout] "walletpassphrase" >|= to_unit

	let walletpassphrasechange ?conn old_passphrase new_passphrase =
		invoke ?conn ~params:[of_string old_passphrase; of_string new_passphrase] "walletpassphrasechange" >|= to_unit
end

