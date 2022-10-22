module Encoding = Encoding
module Base58 = Base58
module Base32 = Base32

type t =
  [ `Base32
  | `Base32hex
  | `Base32hexpad
  | `Base32hexupper
  | `Base32hexpadupper
  | `Base32pad
  | `Base32upper
  | `Base58btc
  | `Base58flickr
  | `Base64
  | `Base64pad
  | `Base64url
  | `Base64urlpad ]

include Multibase_intf.Intf with type t := t
(** @inline *)
