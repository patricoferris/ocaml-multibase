module type S = sig
  type t
  (** A subset of {!Encoding.t} that this multibase implementation can support. *)

  val encode_t : t -> string -> (string, [ `Msg of string ]) result
  (** [encode_t kind s] encodes [s] using base-encoding sheme [kind]. *)

  val encode :
    Encoding.t ->
    string ->
    (string, [ `Msg of string | `Unsupported of Encoding.t ]) result
  (** Similiar to {!encode_t} except it may return [`Unsupported kind] *)

  val decode :
    string ->
    ( Encoding.t * string,
      [ `Msg of string | `Unsupported of Encoding.t ] )
    result
  (** [decode s] will try to decode the multibase string [s] returning the
      encoding scheme and the decoded payload. It may be the case that we found
      a scheme we don't support.
      *)
end

module type Intf = sig
  module type S = S

  include S
end
