(********************************************************************************)
(*	Test.ml
	Copyright (c) 2012 Dario Teixeira (dario.teixeira@yahoo.com)
*)
(********************************************************************************)

open OUnit
open Bitcoin


(********************************************************************************)
(**	{1 Inner modules}							*)
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


module Testnet = Bitcoin.Make (Bitcoin_ocamlnet.Httpclient) (Testnet_connection)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type global_t =
	{
	account1: account_t;
	account2: account_t;
	account3: account_t;
	address1: address_t;
	address2: address_t;
	address3: address_t;
	}


(********************************************************************************)
(**	{1 Setup functions}							*)
(********************************************************************************)

let () = Random.self_init ()

let global = ref None

let random_account () =
	let num = Random.int64 Int64.max_int in
	`Named (Printf.sprintf "ocaml-bitcoin-test-%016Lx" num)

let () =
	global := match List.assoc "testnet" (Testnet.getinfo ()) with
		| `Bool false ->
			Printf.eprintf "Must be run with testnet!\n";
			None
		| `Bool true ->
			Random.self_init ();
			let account1 = random_account () in
			let account2 = random_account () in
			let account3 = random_account () in
			Some
				{
				account1;
				account2;
				account3;
				address1 = Testnet.getaccountaddress account1;
				address2 = Testnet.getaccountaddress account2;
				address3 = Testnet.getaccountaddress account3;
				}
		| _ ->
			assert false


(********************************************************************************)
(**	{1 Test functions}							*)
(********************************************************************************)

let (!!) test = match !global with
	| Some global -> fun () -> test global
	| None	      -> fun () -> assert_failure "not initialised"

let test_addnode global =
	let nodes_len () = List.length (Testnet.getaddednodeinfo ()) in
	let node = Printf.sprintf "node-%016Lx" (Random.int64 Int64.max_int) in
	let len1 = nodes_len () in
	let () = Testnet.addnode node `Add in
	let len2 = nodes_len () in
	assert_equal len2 (len1 + 1);
	let () = Testnet.addnode node `Remove in
	let len3 = nodes_len () in
	assert_equal len2 (len3 + 1)

let test_backupwallet global =
	bracket_tmpfile (fun (fname, _) -> Testnet.backupwallet fname) ()

let test_dumpprivkey global =
	assert_raises (Bitcoin_error (-5, "Invalid Bitcoin address")) (fun () -> Testnet.dumpprivkey "");
	let priv = Testnet.dumpprivkey global.address1 in
	assert_raises (Bitcoin_error (-5, "Invalid private key encoding")) (fun () -> Testnet.importprivkey "");
	Testnet.importprivkey priv

let test_getaccount global =
	assert_equal global.account3 (Testnet.getaccount global.address3)

let test_getaccountaddress global =
	assert_equal global.address1 (Testnet.getaccountaddress global.account1)

let test_getaddressesbyaccount global =
	assert_equal [global.address3] (Testnet.getaddressesbyaccount global.account3)

let test_getbalance global =
	let balance = Testnet.getbalance () in
	assert_bool "Global balance must be >= 0" (balance >= 0L)

let test_getblock global =
	let genesis_hash = Testnet.getblockhash 0 in
	let genesis_block = Testnet.getblock_verbose genesis_hash in
	assert_equal (`Int 0) (List.assoc "height" genesis_block)

let test_getblockcount global =
	assert_bool "block count must be > 0" (Testnet.getblockcount () > 0)

let test_getconnectioncount global =
	assert_bool "connection count must be >= 0" (Testnet.getconnectioncount () >= 0)

let test_getdifficulty global =
	assert_bool "difficulty must be >= 0.0" (Testnet.getdifficulty () >= 0.0)

let test_getgenerate global =
	let old = Testnet.getgenerate () in
	let () = Testnet.setgenerate (not old) in
	assert_equal (not old) (Testnet.getgenerate ());
	let () = Testnet.setgenerate old in
	assert_equal old (Testnet.getgenerate ())

let test_gethashespersec global =
	assert_bool "hashes/sec must be >= 0" (Testnet.gethashespersec () >= 0)

let test_getinfo global = match List.assoc "blocks" (Testnet.getinfo ()) with
	| `Int x -> assert_bool "block count must be > 0" (x > 0)
	| _	 -> assert_failure "test_getinfo"

let test_getreceivedbyaccount global =
	assert_bool "Received amount must be >= 0" (Testnet.getreceivedbyaccount global.account1 >= 0L)

let test_getreceivedbyaddress global =
	assert_bool "Received amount must be >= 0" (Testnet.getreceivedbyaddress global.address1 >= 0L)

let test_listaccounts global =
	assert_bool "number of accounts must be > 0" (List.length (Testnet.listaccounts ()) > 0)

let test_listaddressgroupings global =
	assert_bool "number of address groupings must be >= 0" (List.length (Testnet.listaddressgroupings ()) >= 0)

let test_listreceivedbyaccount global =
	assert_bool "number of accounts must be > 0" (List.length (Testnet.listreceivedbyaccount ()) > 0)

let test_listunspent global =
	assert_bool "number of unspent transactions must be > 0" (List.length (Testnet.listunspent ~minconf:0 ()) > 0)

let test_move global =
	let default_balance = Testnet.getbalance ~account:`Default () in
	let account1_balance = Testnet.getbalance ~account:global.account1 () in
	let amount = 1_0000_0000L in
	assert_bool "Move operation did not succeed" (Testnet.move `Default global.account1 amount);
	let default_balance' = Testnet.getbalance ~account:`Default () in
	let account1_balance' = Testnet.getbalance ~account:global.account1 () in
	assert_equal default_balance' (Int64.sub default_balance amount);
	assert_equal account1_balance' (Int64.add account1_balance amount)

let test_sendfrom global =	
	let default_balance = Testnet.getbalance ~account:`Default () in
	let amount = 1_0000_0000L in
	let txid = Testnet.sendfrom `Default global.address1 amount in
	assert_bool "Sendfrom operation did not succeed" (String.length txid > 0);
	let default_balance' = Testnet.getbalance ~account:`Default () in
	assert_bool "Old balance must be >= new balance + amount" (default_balance >= Int64.add default_balance' amount)

let test_sendmany global =	
	let default_balance = Testnet.getbalance ~account:`Default () in
	let amount1 = 1_0000_0000L in
	let amount2 = 2_0000_0000L in
	let txid = Testnet.sendmany `Default [(global.address1, amount1); (global.address2, amount2)] in
	assert_bool "Sendmany operation did not succeed" (String.length txid > 0);
	let default_balance' = Testnet.getbalance ~account:`Default () in
	assert_bool "Old balance must be >= new balance + amounts" (default_balance >= Int64.add default_balance' (Int64.add amount1 amount2))

let test_sendtoaddress global =
	let default_balance = Testnet.getbalance ~account:`Default () in
	let amount = 1_0000_0000L in
	let txid = Testnet.sendtoaddress global.address1 amount in
	assert_bool "Sendfrom operation did not succeed" (String.length txid > 0);
	let default_balance' = Testnet.getbalance ~account:`Default () in
	assert_bool "Old balance must be >= new balance + amount" (default_balance >= Int64.add default_balance' amount)

let test_setaccount global =
	let account = Testnet.getaccount global.address1 in
	Testnet.setaccount global.address1 global.account2;
	let account' = Testnet.getaccount global.address1 in
	Testnet.setaccount global.address1 account;
	let account'' = Testnet.getaccount global.address1 in
	assert_equal account account'';
	assert_equal account' global.account2

let test_settxfee global =
	let gettxfee () = match List.assoc "paytxfee" (Testnet.getinfo ()) with
		| `Float x -> amount_of_float x
		| _	   -> assert_failure "gettxfee" in
	let current = gettxfee () in
	let updated = Int64.add current 1L in
	assert_bool "Cannot change transaction fee" (Testnet.settxfee updated);
	let current' = gettxfee () in
	assert_equal updated current';
	assert_bool "Cannot restore transaction fee" (Testnet.settxfee current);
	let current' = gettxfee () in
	assert_equal current current'

let test_validateaddress global =
	assert_bool "valid address not reported as such" (Testnet.validateaddress global.address1 <> None);
	assert_bool "invalid address not reported as such" (Testnet.validateaddress "" = None)


(********************************************************************************)
(**	{1 Main functions and values}						*)
(********************************************************************************)

let suite = "OCaml-bitcoin" >:::
	[
	"addnode"		>:: !!test_addnode;
	"backupwallet"		>:: !!test_backupwallet;
	"dumpprivkey"		>:: !!test_dumpprivkey;
	"getaccount"		>:: !!test_getaccount;
	"getaccountaddress"	>:: !!test_getaccountaddress;
	"getaddressesbyaccount" >:: !!test_getaddressesbyaccount;
	"getbalance"		>:: !!test_getbalance;
	"getblock"		>:: !!test_getblock;
	"getblockcount"		>:: !!test_getblockcount;
	"getconnectioncount"	>:: !!test_getconnectioncount;
	"getdifficulty"		>:: !!test_getdifficulty;
	"getgenerate"		>:: !!test_getgenerate;
	"gethashespersec"	>:: !!test_gethashespersec;
	"getinfo"		>:: !!test_getinfo;
	"getreceivedbyaccount"	>:: !!test_getreceivedbyaccount;
	"getreceivedbyaddress"	>:: !!test_getreceivedbyaddress;
	"listaccounts"		>:: !!test_listaccounts;
	"listreceivedbyaccount" >:: !!test_listreceivedbyaccount;
	"listunspent"		>:: !!test_listunspent;
	"move"			>:: !!test_move;
	"sendfrom"		>:: !!test_sendfrom;
	"sendmany"		>:: !!test_sendmany;
	"sendtoaddress"		>:: !!test_sendtoaddress;
	"setaccount"		>:: !!test_setaccount;
	"settxfee"		>:: !!test_settxfee;
	"validateaddress"	>:: !!test_validateaddress;
	]

let _ =
	run_test_tt_main suite

