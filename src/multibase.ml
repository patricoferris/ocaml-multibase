module Encoding = Encoding
module Base58 = Base58
module Base32 = Base32
include Multibase_intf

let base32_lower = Base32.default_lower
let base32_hex_lower = Base32.hex_lower

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

let encode_t (t : t) s =
  let encoded =
    match t with
    | `Base32hexupper ->
        Base32.encode ~pad:false ~alphabet:Base32.extended_hex s
    | `Base32hex -> Base32.encode ~pad:false ~alphabet:base32_hex_lower s
    | `Base32hexpad -> Base32.encode ~pad:true ~alphabet:base32_hex_lower s
    | `Base32hexpadupper ->
        Base32.encode ~pad:true ~alphabet:Base32.extended_hex s
    | `Base32 -> Base32.encode ~pad:false ~alphabet:base32_lower s
    | `Base32upper -> Base32.encode ~pad:false s
    | `Base32pad -> Base32.encode ~pad:true ~alphabet:base32_lower s
    | `Base58btc -> Ok (Base58.encode ~alphabet:Base58.default_alphabet s)
    | `Base58flickr -> Ok (Base58.encode ~alphabet:Base58.flickr_alphabet s)
    | `Base64 -> Base64.encode ~pad:false s
    | `Base64pad -> Base64.encode ~pad:true s
    | `Base64url ->
        Base64.encode ~pad:false ~alphabet:Base64.uri_safe_alphabet s
    | `Base64urlpad ->
        Base64.encode ~pad:true ~alphabet:Base64.uri_safe_alphabet s
  in
  Result.map (fun s -> Encoding.to_code (t :> Encoding.t) ^ s) encoded

let decode_string t s =
  match t with
  | `Base32hexupper -> Base32.decode ~pad:false ~alphabet:Base32.extended_hex s
  | `Base32hexpadupper ->
      Base32.decode ~pad:true ~alphabet:Base32.extended_hex s
  | `Base32hex -> Base32.decode ~pad:false ~alphabet:base32_hex_lower s
  | `Base32hexpad -> Base32.decode ~pad:true ~alphabet:base32_hex_lower s
  | `Base32 -> Base32.decode ~pad:false ~alphabet:base32_lower s
  | `Base32upper -> Base32.decode ~pad:false s
  | `Base32pad -> Base32.decode ~pad:true ~alphabet:base32_lower s
  | `Base58btc -> Ok (Base58.decode ~alphabet:Base58.default_alphabet s)
  | `Base58flickr -> Ok (Base58.decode ~alphabet:Base58.flickr_alphabet s)
  | `Base64 -> Base64.decode ~pad:false s
  | `Base64pad -> Base64.decode ~pad:true s
  | `Base64url -> Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s
  | `Base64urlpad ->
      Base64.decode ~pad:true ~alphabet:Base64.uri_safe_alphabet s

let convert : Encoding.t -> (t, [> `Unsupported of Encoding.t ]) result =
  function
  | #t as t -> Ok t
  | t -> Error (`Unsupported t)

let encode (t : Encoding.t) s =
  match convert t with Ok t -> encode_t t s | Error _ as e -> e

let decode s =
  let e, rest = String.(make 1 @@ get s 0, sub s 1 (length s - 1)) in
  match Encoding.of_code e with
  | Some t -> (
      match convert t with
      | Ok t ->
          Result.map (fun v -> ((t :> Encoding.t), v)) (decode_string t rest)
      | Error _ as e -> e)
  | None ->
      Error (`Msg ("Unknown code for " ^ e ^ string_of_int @@ Char.code e.[0]))
