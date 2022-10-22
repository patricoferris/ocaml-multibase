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

val to_code : t -> string
val of_code : string -> t option
val to_string : t -> string
val of_string : string -> t option
