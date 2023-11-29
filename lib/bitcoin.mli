(** OCaml interface to the official Bitcoin client API.
*)

(********************************************************************************)
(** {1 Exceptions}                                                              *)
(********************************************************************************)

exception Unspecified_connection
(* Raised when connection parameter is not given and no default connection exists. *)

exception Bitcoin_error of int * string (* Error reported by the Bitcoin client. *)
exception Internal_error of int * string (* Unexpected response from the Bitcoin client *)
exception Httpclient_error of exn (* Connection error reported by the {!HTTPCLIENT} *)

(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type address_t = string
(* Bitcoin address (hash of the public portion of public/private ECDSA keypair) *)

type account_t =
  [ `Default
  | `Named of string
  ]
(* Besides the default account, one may also create named accounts *)

type amount_t = int64
(* Amount in BTC, represented as a multiple of Bitcoin's base unit (= 10 nanoBTC).*)

type txid_t = string (* Transaction identifier *)
type txoutput_t = txid_t * int (* Transaction output *)
type blkhash_t = string (* Block hash *)
type priv_t = string (* Private portion of public/private ECDSA keypair *)
type sig_t = string (* Message signature *)
type hextx_t = string (* Hex representation of raw transaction *)
type hexspk_t = string (* Hex representation of script public key *)
type hexblk_t = string (* Hex representation of block data *)
type hexwork_t = string (* Hex representation of mining work data *)

type multi_t =
  [ `Address of address_t
  | `Hexspk of hexspk_t
  ]
(* Multi-signature addresses may take either format *)

type node_t = string (* Node representation *)
type assoc_t = (string * Yojson.Safe.t) list (* Association list *)

type conn_t =
  { inet_addr : Unix.inet_addr;
    host : string;
    port : int;
    username : string;
    (* Should match [rpcuser] value in $HOME/.bitcoin/bitcoin.conf *)
    password : string (* Should match [rpcpassword] value in $HOME/.bitcoin/bitcoin.conf *)
  }

(********************************************************************************)
(** {1 Public module types}                                                     *)
(********************************************************************************)

(** Interface that any module offering HTTP POST client calls must obey.
    Note that the module may require POST calls to be wrapped under a
    custom monad, which must also be provided (use the identity monad
    if no actual monad is required).
*)
module type HTTPCLIENT = sig
  module Monad : sig
    type 'a t

    val return : 'a -> 'a t
    val fail : exn -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  end

  val post_string
    :  headers:(string * string) list ->
    inet_addr:Unix.inet_addr ->
    host:string ->
    port:int ->
    uri:string ->
    string ->
    string Monad.t
end

(** Module encapsulating all connection information.
*)
module type CONNECTION = sig
  val default : conn_t option
end

(** Actual engine offering the Bitcoin API.
*)
module type ENGINE = sig
  type 'a monad_t

  (****************************************************************************)
  (** {2 Block chain}                                                         *)
  (****************************************************************************)

  (** Returns the block hash for the head block in the best block chain. *)
  val getbestblockhash : ?conn:conn_t -> unit -> blkhash_t monad_t

  (** Returns the hex-encoded, serialised data for the block with the given block hash
      when verbosity=0 (the default). For verbosity=1 or verbosity=2, returns an assoc
      list. *)
  val getblock
    :  ?verbosity:int ->
    ?conn:conn_t ->
    blkhash_t ->
    [ `hexblk of hexblk_t | `assoc of assoc_t ] monad_t

  (** Returns information concerning the current state of the block chain. *)
  val getblockchaininfo : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Returns the number of blocks in the best block chain. *)
  val getblockcount : ?conn:conn_t -> unit -> int monad_t

  (** Returns the block hash for the block located at the given index in the longest block chain. *)
  val getblockhash : ?conn:conn_t -> int -> blkhash_t monad_t

  (** Returns information about the highest-height block (tip) of each local block chain. *)
  val getchaintips : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Returns the current difficulty (as a multiple of the minimum difficulty). *)
  val getdifficulty : ?conn:conn_t -> unit -> float monad_t

  (** Returns information about the node's current transaction memory pool. *)
  val getmempoolinfo : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Returns all transaction IDs currently in the memory pool. *)
  val getrawmempool : ?conn:conn_t -> unit -> txid_t list monad_t

  (** Returns detailed information about all transactions currently in the memory pool. *)
  val getrawmempool_verbose : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Returns detailed information concerning a given unspent transaction output. *)
  val gettxout : ?conn:conn_t -> ?includemempool:bool -> txoutput_t -> assoc_t option monad_t

  (** Returns some statistics about the current set of unspent transaction outputs. *)
  val gettxoutsetinfo : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Verifies the blockchain database.  The optional parameter [checklevel] is an integer between 0 and 4,
        indicating how thorough the verification should be (higher numbers indicate a more thorough checking;
        the default is 3).  The optional parameter [numblocks] indicates whether the verification should apply
        to all blocks (when zero), or be restricted to the last given blocks (when a positive integer).
        By default only the last 288 blocks are verified). *)
  val verifychain : ?conn:conn_t -> ?checklevel:int -> ?numblocks:int -> unit -> bool monad_t

  (****************************************************************************)
  (** {2 Overall control/query calls}                                         *)
  (****************************************************************************)

  (** Shuts down the server. *)
  val stop : ?conn:conn_t -> unit -> unit monad_t

  (****************************************************************************)
  (** {2 Generate}                                                            *)
  (****************************************************************************)

  (** Are we currently trying to generate new blocks? *)
  val getgenerate : ?conn:conn_t -> unit -> bool monad_t

  (** Turns on/off the generation of new blocks (a.k.a. "mining").
        If provided, [genproclimit] limits the number of CPUs to be used for generating. *)
  val setgenerate : ?conn:conn_t -> ?genproclimit:int -> bool -> unit monad_t

  (****************************************************************************)
  (** {2 Mining}                                                              *)
  (****************************************************************************)

  (** Returns data needed to construct a block to work on. *)
  val getblocktemplate : ?conn:conn_t -> ?obj:assoc_t -> unit -> assoc_t monad_t

  (** Returns an object containing mining related information. *)
  val getmininginfo : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Returns the estimated number of hashes per second of the entire network *)
  val getnetworkhashps : ?conn:conn_t -> ?blocks:int -> ?height:int -> unit -> int monad_t

  (** [prioritisetransaction txid delta_prio delta_fee] adjusts the priority of transaction [txid]
        by [delta_prio], while also adjusting the transaction fee by [delta_fee]. *)
  val prioritisetransaction : ?conn:conn_t -> txid_t -> float -> amount_t -> bool monad_t

  (** Attempts to submit new block to network. *)
  val submitblock : ?conn:conn_t -> hexblk_t -> unit monad_t

  (****************************************************************************)
  (** {2 P2P networking}                                                      *)
  (****************************************************************************)

  (** Allows manually adding/removing a node. *)
  val addnode : ?conn:conn_t -> node_t -> [ `Add | `Remove | `Onetry ] -> unit monad_t

  (** Returns the list of nodes manually added with {!addnode}. *)
  val getaddednodeinfo : ?conn:conn_t -> ?node:node_t -> unit -> node_t list monad_t

  (** Returns a list with more information about the nodes manually added with {!addnode}. *)
  val getaddednodeinfo_verbose : ?conn:conn_t -> ?node:node_t -> unit -> assoc_t list monad_t

  (** Returns the number of connections to peer nodes. *)
  val getconnectioncount : ?conn:conn_t -> unit -> int monad_t

  (** Returns information about network traffic. *)
  val getnettotals : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Returns miscelaneous information concerning the node's connection to the P2P network. *)
  val getnetworkinfo : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Returns information about each connected peer. *)
  val getpeerinfo : ?conn:conn_t -> unit -> assoc_t list monad_t

  (** Sends a ping to all other nodes.  Note that results are provided via {!getpeerinfo}. *)
  val ping : ?conn:conn_t -> unit -> unit monad_t

  (****************************************************************************)
  (** {2 Raw transactions}                                                    *)
  (****************************************************************************)

  (** [createrawtransaction inputs outputs] creates a raw transaction that transfers the given inputs
        (a list of transaction outputs) to the given outputs (a list of addresses and amounts).  This
        function returns the hex-encoded transaction, but neither does it transmit the transaction to
        the network nor does it store the transaction in the wallet.  In addition, the transaction inputs
        are not signed, and therefore the returned raw transaction cannot be directly transmitted to the
        network with {!sendrawtransaction} without it being previosuly signed with {!signrawtransaction}.

        A transaction fee is specified implicitly by making the total output amounts be smaller than the
        total input amounts (ie, fee = total inputs - total outputs).

        Beware that no checks are performed concerning the validity of the transaction.  If care is not taken,
        it is possible to create a transaction that will not be accepted by the network, either because it
        uses invalid inputs, has a greater total amount in the outputs than in the outputs, or does not offer
        a sufficient transaction fee. *)
  val createrawtransaction
    :  ?conn:conn_t ->
    txoutput_t list ->
    (address_t * amount_t) list ->
    hextx_t monad_t

  (** Returns an object containing information concerning the given raw transaction. *)
  val decoderawtransaction : ?conn:conn_t -> hextx_t -> assoc_t monad_t

  (** Returns the decoded version of a hex-encoded script. *)
  val decodescript : ?conn:conn_t -> hexspk_t -> assoc_t monad_t

  (** Returns the raw transaction corresponding to the given transaction ID. *)
  val getrawtransaction : ?conn:conn_t -> txid_t -> hextx_t monad_t

  (** Returns all the raw information concerning the transaction with the given ID.
        This function returns even more information than that available with {!gettransaction}. *)
  val getrawtransaction_verbose : ?conn:conn_t -> txid_t -> assoc_t monad_t

  (** Transmits the given raw transaction to the network.  Note that no attempt is made to verify the validity
        of the transaction, and other peers may choose to drop it if invalid.  Because a raw transaction may
        inadvertently specify a much higher fee than intended (such mistakes have happened in the past), the
        optional boolean parameter [allow_high_fees] must be set to true if a high fee is indeed intentional
        (this parameter defaults to false, of course). *)
  val sendrawtransaction : ?conn:conn_t -> ?allow_high_fees:bool -> hextx_t -> txid_t monad_t

  (** Signs a raw transaction, returning a pair with the signed transaction in hex format and a boolean indicating
        whether all private keys required for a successful signing have been found.  A raw transaction created with
        {!createrawtransaction} must be signed before broadcasting to the network with {!sendrawtransaction}.  The
        optional parameters [parents], [keys], and [sighash] are only required if you wish to chain transactions
        or if the private keys required for signing do not reside in the wallet.  Note that these optional parameters
        have the particularity that providing a value for parameter {i n} requires also providing a value for {i n-1}. *)
  val signrawtransaction
    :  ?conn:conn_t ->
    ?parents:(txoutput_t * hexspk_t) list ->
    ?keys:priv_t list ->
    ?sighash:[ `All | `None | `Single ] * bool ->
    hextx_t ->
    (hextx_t * bool) monad_t

  (****************************************************************************)
  (** {2 Utility functions}                                                   *)
  (****************************************************************************)

  (** Creates a m-of-n multi-signature address.  An invocation of [createmultisig num pubs] where [List.length pubs >= num]
        returns an address that requires the private keys of at least [num] members of [pubs] for spending. *)
  val createmultisig : ?conn:conn_t -> int -> multi_t list -> (address_t * hexspk_t) monad_t

  (** Estimates the transaction fee (per kilobyte) that needs to be paid for a transaction to be included within
        the given number of blocks. *)
  val estimatefee : ?conn:conn_t -> int -> amount_t monad_t

  (** Estimates the priority that a transaction needs to have in order to be included within the given number of
        blocks without paying a transaction fee.  A return value of [None] indicates that the node does not have
        enough information to make an estimate. *)
  val estimatepriority : ?conn:conn_t -> int -> float option monad_t

  (** Is the given address a valid Bitcoin address?  If so, this function returns an object
        containing miscelaneous information about that address.  Returns [None] otherwise. *)
  val validateaddress : ?conn:conn_t -> address_t -> assoc_t option monad_t

  (** Returns a boolean indicating whether a supposedly signed message does indeed correspond
        to a source message signed with the given address.  Use {!signmessage} for signing messages. *)
  val verifymessage : ?conn:conn_t -> address_t -> sig_t -> string -> bool monad_t

  (****************************************************************************)
  (** {2 Wallet}                                                              *)
  (****************************************************************************)

  (** Add a {i n-required-to-sign} multisignature address to the wallet, optionally associated with [account].
        The value of {i n} is an integer given by the first mandatory parameter.  Each address may be provided
        either as a {!address_t}, or as a hex-encoded public key ({!hexspk_t}). *)
  val addmultisigaddress
    :  ?conn:conn_t ->
    ?account:account_t ->
    int ->
    multi_t list ->
    address_t monad_t

  (** Safely backs up wallet file to the given destination, which can be either a directory or a path with filename. *)
  val backupwallet : ?conn:conn_t -> string -> unit monad_t

  (** Returns the private key corresponding to the given address.  This private key can then be imported
        into another wallet with {!importprivkey}.  {b (Requires unlocked wallet)}. *)
  val dumpprivkey : ?conn:conn_t -> address_t -> priv_t monad_t

  (** Dumps all wallet keys in a human readable format.  The mandatory parameter is the name
        of the file where to put the dump. {b (Requires unlocked wallet)}. *)
  val dumpwallet : ?conn:conn_t -> string -> unit monad_t

  (** Encrypts the wallet with the given passphrase.  Note that once encrypted, a wallet cannot be unencrypted
        (though the passphrase may be changed with {!walletpassphrasechange}), and operations requiring an unlocked
        wallet must be preceded by a call to {!walletpassphrase}. *)
  val encryptwallet : ?conn:conn_t -> string -> unit monad_t

  (** Returns the account associated with the given address. *)
  val getaccount : ?conn:conn_t -> address_t -> account_t monad_t

  (** Returns the receiving address currently associated with the given account.
        Note that a new address will automatically be generated upon funds being received on this address. *)
  val getaccountaddress : ?conn:conn_t -> account_t -> address_t monad_t

  (** Return all addresses associated with the given account. *)
  val getaddressesbyaccount : ?conn:conn_t -> account_t -> address_t list monad_t

  (** If [account] is provided, returns the balance available in that account.
        If not, returns the total balance of all accounts. *)
  val getbalance
    :  ?conn:conn_t ->
    ?account:account_t ->
    ?minconf:int ->
    ?includewatchonly:bool ->
    unit ->
    amount_t monad_t

  (** Returns a newly generated address.  If [account] is specified, the returned address is
        associated with that account.  If not, the address is associated with the default account. *)
  val getnewaddress : ?conn:conn_t -> ?account:account_t -> unit -> address_t monad_t

  (** Returns a new address for receiving change. *)
  val getrawchangeaddress : ?conn:conn_t -> unit -> address_t monad_t

  (** Returns the total amount received on the given account.  Note that only receiving transactions
        are considered and therefore this function does not compute the account's current balance;
        see {!getbalance} for that purpose. *)
  val getreceivedbyaccount : ?conn:conn_t -> ?minconf:int -> account_t -> amount_t monad_t

  (** Returns the total amount received on this address.  Note that only receiving transactions are considered
        and therefore this function does not compute the balance currently associated with the address. *)
  val getreceivedbyaddress : ?conn:conn_t -> ?minconf:int -> address_t -> amount_t monad_t

  (** Returns an object containing various information about the given transaction. *)
  val gettransaction : ?conn:conn_t -> ?includewatchonly:bool -> txid_t -> assoc_t monad_t

  (** Returns the total unconfirmed balance. *)
  val getunconfirmedbalance : ?conn:conn_t -> unit -> amount_t monad_t

  (** Returns miscelaneous information about the wallet. *)
  val getwalletinfo : ?conn:conn_t -> unit -> assoc_t monad_t

  (** Adds an address or pubkey script to the wallet without the associated private key, allowing watching
        for transactions affecting that address or pubkey script, but without being able to spend any of its outputs. *)
  val importaddress : ?conn:conn_t -> ?account:account_t -> ?rescan:bool -> multi_t -> unit monad_t

  (** Adds a private key to the wallet.  This can be an externally generated key or one previously
        obtained with {!dumpprivkey}.  If [account] is provided, the key is associated with that account.
        If not, the key is associated with the default account.  The [rescan] parameter indicates
        whether the blockchain should be rescanned for transactions involving the imported key.
        It is [true] be default.  {b (Requires unlocked wallet)}. *)
  val importprivkey : ?conn:conn_t -> ?account:account_t -> ?rescan:bool -> priv_t -> unit monad_t

  (** Imports keys from a wallet dump, produced for example by {!dumpwallet}.  The mandatory parameter
        is the name of the file containing the wallet dump. {b (Requires unlocked wallet)}. *)
  val importwallet : ?conn:conn_t -> string -> unit monad_t

  (** Refills the keypool.  The optional parameter [size] indicates the new size of the pool (default is 100).
        {b (Requires unlocked wallet)}. *)
  val keypoolrefill : ?conn:conn_t -> ?size:int -> unit -> unit monad_t

  (** Returns a list of all accounts and associated balance. *)
  val listaccounts
    :  ?conn:conn_t ->
    ?minconf:int ->
    ?includewatchonly:bool ->
    unit ->
    (account_t * amount_t) list monad_t

  (** Returns a list of the groups of addresses whose common ownership has been made
        public by common use as inputs or as the resulting change in past transactions. *)
  val listaddressgroupings
    :  ?conn:conn_t ->
    unit ->
    (address_t * amount_t * account_t) list list monad_t

  (** Returns a list of temporarily unspendable transaction outputs.  These are outputs previously locked by a call
        of {!lockunspent}, and will not be spent by the system unless explicitly used in a raw transaction. *)
  val listlockunspent : ?conn:conn_t -> unit -> txoutput_t list monad_t

  (** Returns a list of the total amount received by each account.  Each returned list element is a tuple
        consisting of an account, the total amount received for that account, and the number of confirmations.
        Optional parameter [includeempty] indicates whether accounts with nothing received will be included
        in the returned list (defaults to [false]). *)
  val listreceivedbyaccount
    :  ?conn:conn_t ->
    ?minconf:int ->
    ?includeempty:bool ->
    ?includewatchonly:bool ->
    unit ->
    (account_t * amount_t * int) list monad_t

  (** Returns a list of the total amount received by each address.  Each returned list element is a tuple
        consisting of a boolean indicating whether the address is watch only, the address itself, the associated
        account, the total amount received for that address, the number of confirmations, and a list of transaction
        IDs.  Optional parameter [includeempty] indicates whether accounts with nothing received should be included
        in the returned list (defaults to [false]). *)
  val listreceivedbyaddress
    :  ?conn:conn_t ->
    ?minconf:int ->
    ?includeempty:bool ->
    ?includewatchonly:bool ->
    unit ->
    (bool * address_t * account_t * amount_t * int * txid_t list) list monad_t

  (** Returns a pair consisting of a list of all transactions and the block hash of the latest block.
        If provided, the [blockhash] parameter limits the list of transactions to those occurring after
        (and not including) that block.  Note that if you want to provide a value for parameter [minconf],
        then you must also provide [blockhash]. *)
  val listsinceblock
    :  ?conn:conn_t ->
    ?blockhash:blkhash_t ->
    ?minconf:int ->
    ?includewatchonly:bool ->
    unit ->
    (assoc_t list * blkhash_t) monad_t

  (** Returns up to [count] most recent transactions skipping the first [from] transactions for [account].
        If [account] is not provided, then all recent transactions from all accounts will be returned.
        Note that [count] and [from] default to 10 and 0, respectively. *)
  val listtransactions
    :  ?conn:conn_t ->
    ?account:account_t ->
    ?count:int ->
    ?from:int ->
    ?includewatchonly:bool ->
    unit ->
    assoc_t list monad_t

  (** Returns a list of the unspent transaction outputs that have between [minconf] and [maxconf] confirmations
        (these default to 1 and 9_999_999, respectively).  If [addresses] is provided, the returned list is filtered
        to only include transaction outputs paid to the specified addresses. *)
  val listunspent
    :  ?conn:conn_t ->
    ?minconf:int ->
    ?maxconf:int ->
    ?addresses:address_t list ->
    unit ->
    assoc_t list monad_t

  (** Updates the list of temporarily unspendable transaction outputs.  If the operation is [`Lock], this function
        will lock the specified outputs, preventing them from being spent outside of a raw transaction.  If the operation
        is [`Unlock], this function will unlock the specified previously locked outputs.  To unlock all currently locked
        outputs, set the operation to [`Unlock] and do not provide any list of outputs.  Returns a boolean indicating
        whether the operation was successfull. *)
  val lockunspent : ?conn:conn_t -> ?outputs:txoutput_t list -> [ `Lock | `Unlock ] -> bool monad_t

  (** [move ?minconf ?comment from_account to_account amount] transfers the given amount from one account to another.
        If provided, [comment] allows you to record a comment associated with the move.  Note that this is operation
        only rearranges the internal balances on your wallet, and is not communicated to the Bitcoin network. *)
  val move
    :  ?conn:conn_t ->
    ?minconf:int ->
    ?comment:string ->
    account_t ->
    account_t ->
    amount_t ->
    bool monad_t

  (** Transfers a given amount to the specified address, deducting the balance of the given account.
        If successful, the function returns the ID of the transaction recording the transfer. The optional
        parameter [comment] allows you to record an arbitrary comment about this transaction, whereas the
        also optional [recipient] assigns a string identifier to the recipient address.  Note that [sendfrom]
        is only performed if the account balance is sufficient to fund the transfer.  {b (Requires unlocked wallet)}. *)
  val sendfrom
    :  ?conn:conn_t ->
    ?minconf:int ->
    ?comment:string ->
    ?recipient:string ->
    account_t ->
    address_t ->
    amount_t ->
    txid_t monad_t

  (** Makes multiple transfers from a given account.  Besides the source account, this function accepts as
        parameter a list whole elements are pairs consisting of each target address and the amount to be sent.
        If successful, the function returns the ID of the transaction recording the transfer.  The optional
        parameter [comment] allows you to record an arbitrary comment about this transaction.  Note that [sendmany]
        is only performed if the account balance is sufficient to fund the transfer.  {b (Requires unlocked wallet)}. *)
  val sendmany
    :  ?conn:conn_t ->
    ?minconf:int ->
    ?comment:string ->
    account_t ->
    (address_t * amount_t) list ->
    txid_t monad_t

  (** Transfers a given amount to the specified address, deducting the balance of the default account.
        If successful, the function returns the ID of the transaction recording the transfer. The optional
        parameter [comment] allows you to record an arbitrary comment about this transaction, whereas
        the also optional [recipient] assigns a string identifier to the recipient address.  Note that
        [sendtoaddress] is only performed if the total balance (not the balance of the default account!)
        is sufficient to fund the transfer.  {b (Requires unlocked wallet)}. *)
  val sendtoaddress
    :  ?conn:conn_t ->
    ?comment:string ->
    ?recipient:string ->
    address_t ->
    amount_t ->
    txid_t monad_t

  (** Associates the given address with the given account. *)
  val setaccount : ?conn:conn_t -> address_t -> account_t -> unit monad_t

  (** Sets the transaction fee to be used in subsequent transactions. *)
  val settxfee : ?conn:conn_t -> amount_t -> bool monad_t

  (** Signs the given message with the private key of the given address.  The resulting
        signed message can be validated with {!validateaddress}. {b (Requires unlocked wallet)}. *)
  val signmessage : ?conn:conn_t -> address_t -> string -> sig_t monad_t

  (** Removes the wallet decryption key from memory, thus effectively locking the wallet.  After this
        function is invoked, you may not perform any API calls that require an unlocked wallet unless
        you beforehand unlock the wallet again with {!walletpassphrase}.  Note that this function may
        not be called on an already locked wallet or in one which is altogether unencrypted. *)
  val walletlock : ?conn:conn_t -> unit -> unit monad_t

  (** [walletpassphrase passphrase timeout] unlocks an encrypted wallet, storing the decryption key
        in memory.  If your wallet is encrypted (see {!encryptwallet}) then you must invoke this function
        prior to calling any of the functions which require an unlocked wallet.  Note that the unlocking
        expires after [timeout] seconds.  Regardless of the timeout, you may at any moment lock again the
        wallet with {!walletlock}. *)
  val walletpassphrase : ?conn:conn_t -> string -> int -> unit monad_t

  (** [walletpassphrasechange old_passphrase new_passphrase] changes the wallet passphrase from
        [old_passphrase] to [new_passphrase].  Must only be called for an encrypted wallet. *)
  val walletpassphrasechange : ?conn:conn_t -> string -> string -> unit monad_t
end

(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(** Converts a BTC quantity expressed as a [float] into its {!amount_t} representation. *)
val amount_of_float : float -> amount_t

(** Converts a BTC quantity expressed as an {!amount_t} into its [float] representation. *)
val float_of_amount : amount_t -> float

(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

(** Functor that takes a concrete implementation of a {!HTTPCLIENT} and actual
    {!CONNECTION} information, and creates a module with signature {!ENGINE}
    offering an API for communicating with a Bitcoin client.
*)
module Make : functor (Httpclient : HTTPCLIENT) (Connection : CONNECTION) ->
  ENGINE with type 'a monad_t = 'a Httpclient.Monad.t
