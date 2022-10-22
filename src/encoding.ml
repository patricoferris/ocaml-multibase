type t =
  [ `Identity
  | `Base2
  | `Base8
  | `Base10
  | `Base16
  | `Base16upper
  | `Base32hex
  | `Base32hexupper
  | `Base32hexpad
  | `Base32hexpadupper
  | `Base32
  | `Base32upper
  | `Base32pad
  | `Base32padupper
  | `Base32z
  | `Base36
  | `Base36upper
  | `Base58btc
  | `Base58flickr
  | `Base64
  | `Base64pad
  | `Base64url
  | `Base64urlpad
  | `Proquint
  | `Base256emoji ]

let to_code : t -> string = function
  | `Identity -> "\x00"
  | `Base2 -> "0"
  | `Base8 -> "7"
  | `Base10 -> "9"
  | `Base16 -> "f"
  | `Base16upper -> "F"
  | `Base32hex -> "v"
  | `Base32hexupper -> "V"
  | `Base32hexpad -> "t"
  | `Base32hexpadupper -> "T"
  | `Base32 -> "b"
  | `Base32upper -> "B"
  | `Base32pad -> "c"
  | `Base32padupper -> "C"
  | `Base32z -> "h"
  | `Base36 -> "k"
  | `Base36upper -> "K"
  | `Base58btc -> "z"
  | `Base58flickr -> "Z"
  | `Base64 -> "m"
  | `Base64pad -> "M"
  | `Base64url -> "u"
  | `Base64urlpad -> "U"
  | `Proquint -> "p"
  | `Base256emoji -> "🚀"

let of_code : string -> t option = function
  | "\x00" -> Some `Identity
  | "0" -> Some `Base2
  | "7" -> Some `Base8
  | "9" -> Some `Base10
  | "f" -> Some `Base16
  | "F" -> Some `Base16upper
  | "v" -> Some `Base32hex
  | "V" -> Some `Base32hexupper
  | "t" -> Some `Base32hexpad
  | "T" -> Some `Base32hexpadupper
  | "b" -> Some `Base32
  | "B" -> Some `Base32upper
  | "c" -> Some `Base32pad
  | "C" -> Some `Base32padupper
  | "h" -> Some `Base32z
  | "k" -> Some `Base36
  | "K" -> Some `Base36upper
  | "z" -> Some `Base58btc
  | "Z" -> Some `Base58flickr
  | "m" -> Some `Base64
  | "M" -> Some `Base64pad
  | "u" -> Some `Base64url
  | "U" -> Some `Base64urlpad
  | "p" -> Some `Proquint
  | "🚀" -> Some `Base256emoji
  | _ -> None

let to_string : t -> string = function
  | `Identity -> "identity"
  | `Base2 -> "base2"
  | `Base8 -> "base8"
  | `Base10 -> "base10"
  | `Base16 -> "base16"
  | `Base16upper -> "base16upper"
  | `Base32hex -> "base32hex"
  | `Base32hexupper -> "base32hexupper"
  | `Base32hexpad -> "base32hexpad"
  | `Base32hexpadupper -> "base32hexpadupper"
  | `Base32 -> "base32"
  | `Base32upper -> "base32upper"
  | `Base32pad -> "base32pad"
  | `Base32padupper -> "base32padupper"
  | `Base32z -> "base32z"
  | `Base36 -> "base36"
  | `Base36upper -> "base36upper"
  | `Base58btc -> "base58btc"
  | `Base58flickr -> "base58flickr"
  | `Base64 -> "base64"
  | `Base64pad -> "base64pad"
  | `Base64url -> "base64url"
  | `Base64urlpad -> "base64urlpad"
  | `Proquint -> "proquint"
  | `Base256emoji -> "base256emoji"

let of_string : string -> t option = function
  | "identity" -> Some `Identity
  | "base2" -> Some `Base2
  | "base8" -> Some `Base8
  | "base10" -> Some `Base10
  | "base16" -> Some `Base16
  | "base16upper" -> Some `Base16upper
  | "base32hex" -> Some `Base32hex
  | "base32hexupper" -> Some `Base32hexupper
  | "base32hexpad" -> Some `Base32hexpad
  | "base32hexpadupper" -> Some `Base32hexpadupper
  | "base32" -> Some `Base32
  | "base32upper" -> Some `Base32upper
  | "base32pad" -> Some `Base32pad
  | "base32padupper" -> Some `Base32padupper
  | "base32z" -> Some `Base32z
  | "base36" -> Some `Base36
  | "base36upper" -> Some `Base36upper
  | "base58btc" -> Some `Base58btc
  | "base58flickr" -> Some `Base58flickr
  | "base64" -> Some `Base64
  | "base64pad" -> Some `Base64pad
  | "base64url" -> Some `Base64url
  | "base64urlpad" -> Some `Base64urlpad
  | "proquint" -> Some `Proquint
  | "base256emoji" -> Some `Base256emoji
  | _ -> None
