(*---------------------------------------------------------------------------
   Copyright (c) 2006-2009 Citrix Systems Inc.
   Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.com>
   Copyright (c) 2014-2016 Anil Madhavapeddy <anil@recoil.org>
   Copyright (c) 2016 David Kaloper Meršinjak
   Copyright (c) 2018 Romain Calascibetta <romain.calascibetta@gmail.com>
   Copyright (c) 2021-2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type alphabet = { emap : int array; dmap : int array }
type sub = string * int * int

let ( // ) x y =
  if y < 1 then raise Division_by_zero;
  if x > 0 then 1 + ((x - 1) / y) else 0
  [@@inline]

let unsafe_get_uint8 t off = Char.code (String.unsafe_get t off)
let unsafe_set_uint8 t off v = Bytes.unsafe_set t off (Char.chr v)

external unsafe_set_uint16 : bytes -> int -> int -> unit = "%caml_bytes_set16u"
  [@@noalloc]

external unsafe_get_uint16 : string -> int -> int = "%caml_string_get16u"
  [@@noalloc]

external swap16 : int -> int = "%bswap16" [@@noalloc]

let none = -1

(* We mostly want to have an optional array for [dmap] (e.g. [int option
   array]). So we consider the [none] value as [-1]. *)

let make_alphabet alphabet =
  if String.length alphabet <> 32 then
    invalid_arg "Length of alphabet must be 32";
  if String.contains alphabet '=' then
    invalid_arg "Alphabet can not contain padding character";
  let emap =
    Array.init (String.length alphabet) (fun i -> Char.code alphabet.[i])
  in
  let dmap = Array.make 256 none in
  String.iteri (fun idx chr -> dmap.(Char.code chr) <- idx) alphabet;
  { emap; dmap }

let length_alphabet { emap; _ } = Array.length emap

let alphabet { emap; _ } =
  String.init (Array.length emap) (fun i -> Char.chr emap.(i))

let default_alphabet = make_alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

let default_lower =
  make_alphabet (String.lowercase_ascii "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

let extended_hex = make_alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUV"

let hex_lower =
  make_alphabet (String.lowercase_ascii "0123456789ABCDEFGHIJKLMNOPQRSTUV")

let unsafe_set_be_uint16 =
  if Sys.big_endian then fun t off v -> unsafe_set_uint16 t off v
  else fun t off v -> unsafe_set_uint16 t off (swap16 v)

(* We make this exception to ensure to keep a control about which exception we
   can raise and avoid appearance of unknown exceptions like an ex-nihilo
   magic rabbit (or magic money?). *)
exception Out_of_bounds
exception Too_much_input

let get_uint8 t off =
  if off < 0 || off >= String.length t then raise Out_of_bounds;
  unsafe_get_uint8 t off

let padding = int_of_char '='
let error_msgf fmt = Format.ksprintf (fun err -> Error (`Msg err)) fmt

let encode_sub pad { emap; _ } ?(off = 0) ?len input =
  let len =
    match len with Some len -> len | None -> String.length input - off
  in

  if len < 0 || off < 0 || off > String.length input - len then
    error_msgf "Invalid bounds"
  else
    (* Length of input string *)
    let n = len in
    (* Every 40-bits (5 bytes) because 8 *)
    let n' = n // 5 * 8 in
    let res = Bytes.create n' in
    let emap i = Array.unsafe_get emap i in
    (* 5 bit mask *)
    let b5_mask = 0x1f in
    let emit b1 b2 b3 b4 b5 i =
      (* Bytes 1 and 2 *)
      unsafe_set_be_uint16 res i
        ((emap ((b1 lsr 3) land b5_mask) lsl 8)
        lor emap ((b1 lsl 2) lor (b2 lsr 6) land b5_mask));
      (* Bytes 3 and 4 *)
      unsafe_set_be_uint16 res (i + 2)
        ((emap ((b2 lsr 1) land b5_mask) lsl 8)
        lor emap ((b2 lsl 4) lor (b3 lsr 4) land b5_mask));
      (* Bytes 5 and 6 *)
      unsafe_set_be_uint16 res (i + 4)
        ((emap ((b3 lsl 1) lor (b4 lsr 7) land b5_mask) lsl 8)
        lor emap ((b4 lsr 2) land b5_mask));
      (* Bytes 7 and 8 *)
      unsafe_set_be_uint16 res (i + 6)
        ((emap ((b4 lsl 3) lor (b5 lsr 5) land b5_mask) lsl 8)
        lor emap (b5 land b5_mask))
    in
    let rec enc j i =
      if i = n then ()
      else if i = n - 1 then emit (unsafe_get_uint8 input (off + i)) 0 0 0 0 j
      else if i = n - 2 then
        emit
          (unsafe_get_uint8 input (off + i))
          (unsafe_get_uint8 input (off + i + 1))
          0 0 0 j
      else if i = n - 3 then
        emit
          (unsafe_get_uint8 input (off + i))
          (unsafe_get_uint8 input (off + i + 1))
          (unsafe_get_uint8 input (off + i + 2))
          0 0 j
      else if i = n - 4 then
        emit
          (unsafe_get_uint8 input (off + i))
          (unsafe_get_uint8 input (off + i + 1))
          (unsafe_get_uint8 input (off + i + 2))
          (unsafe_get_uint8 input (off + i + 3))
          0 j
      else (
        emit
          (unsafe_get_uint8 input (off + i))
          (unsafe_get_uint8 input (off + i + 1))
          (unsafe_get_uint8 input (off + i + 2))
          (unsafe_get_uint8 input (off + i + 3))
          (unsafe_get_uint8 input (off + i + 4))
          j;
        enc (j + 8) (i + 5))
    in

    let rec unsafe_fix = function
      | 0 -> ()
      | i ->
          unsafe_set_uint8 res (n' - i) padding;
          unsafe_fix (i - 1)
    in

    enc 0 0;

    let pad_to_write =
      match n mod 5 with 1 -> 6 | 2 -> 4 | 3 -> 3 | 4 -> 1 | _ -> 0
    in

    if pad then (
      unsafe_fix pad_to_write;
      Ok (Bytes.unsafe_to_string res, 0, n'))
    else Ok (Bytes.unsafe_to_string res, 0, n' - pad_to_write)

(* [pad = false], we don't want to write them. *)

let encode ?(pad = true) ?(alphabet = default_alphabet) ?off ?len input =
  match encode_sub pad alphabet ?off ?len input with
  | Ok (res, off, len) -> Ok (String.sub res off len)
  | Error _ as err -> err

let encode_string ?pad ?alphabet input =
  match encode ?pad ?alphabet input with
  | Ok res -> res
  | Error _ -> assert false

let encode_sub ?(pad = true) ?(alphabet = default_alphabet) ?off ?len input =
  encode_sub pad alphabet ?off ?len input

let encode_exn ?pad ?alphabet ?off ?len input =
  match encode ?pad ?alphabet ?off ?len input with
  | Ok v -> v
  | Error (`Msg err) -> invalid_arg err

module I63 = struct
  include Optint.Int63

  let ( lsl ) a b = shift_left a b
  let ( lor ) a b = logor a b
  let ( land ) a b = logand a b
  let ( lsr ) a b = shift_right_logical a b
end

let decode_sub ?(pad = true) { dmap; _ } ?(off = 0) ?len input =
  let len =
    match len with Some len -> len | None -> String.length input - off
  in

  if len < 0 || off < 0 || off > String.length input - len then
    error_msgf "Invalid bounds"
  else
    let n = len // 8 * 8 in
    let n' = n // 8 * 5 in
    let res = Bytes.create n' in
    let invalid_pad_overflow = pad in

    let get_uint8_or_padding =
      if pad then (fun t i ->
        if i >= len then raise Out_of_bounds;
        get_uint8 t (off + i))
      else fun t i ->
        try if i < len then get_uint8 t (off + i) else padding
        with Out_of_bounds -> padding
    in

    let set_be_uint16 t off v =
      (* can not write 2 bytes. *)
      if off < 0 || off + 1 > Bytes.length t then ()
        (* can not write 1 byte but can write 1 byte *)
      else if off < 0 || off + 2 > Bytes.length t then
        unsafe_set_uint8 t off (v lsr 8) (* can write 2 bytes. *)
      else unsafe_set_be_uint16 t off v
    in

    let set_uint8 t off v =
      if off < 0 || off >= Bytes.length t then () else unsafe_set_uint8 t off v
    in

    (* From 8 bytes to 5 bytes *)
    let emit b1 b2 b3 b4 b5 b6 b7 b8 j =
      let open I63 in
      let x =
        (b1 lsl 35) lor (b2 lsl 30) lor (b3 lsl 25) lor (b4 lsl 20)
        lor (b5 lsl 15) lor (b6 lsl 10) lor (b7 lsl 5) lor b8
      in
      set_be_uint16 res j (x lsr 24 |> to_int);
      set_be_uint16 res (j + 2) ((x lsr 8) land of_int 0xffff |> to_int);
      set_uint8 res (j + 4) (x land of_int 0xff |> to_int)
    in

    let dmap i =
      let x = Array.unsafe_get dmap i in
      if x = none then raise Not_found;
      x
    in

    let only_padding pad idx =
      (* because we round length of [res] to the upper bound of how many
         characters we should have from [input], we got at this stage only padding
         characters and we need to delete them, so for each [====], we delete 3
         bytes. *)
      let pad = ref (pad + 5) in
      let idx = ref idx in

      while !idx + 8 < len do
        (* use [unsafe_get_uint16] instead [unsafe_get_uint32] to avoid allocation
           of [int32]. Of course, [3d3d3d3d] is [====]. *)
        if
          unsafe_get_uint16 input (off + !idx) <> 0x3d3d
          || unsafe_get_uint16 input (off + !idx + 2) <> 0x3d3d
          || unsafe_get_uint16 input (off + !idx + 4) <> 0x3d3d
          || unsafe_get_uint16 input (off + !idx + 6) <> 0x3d3d
        then raise Not_found;

        (* We got something bad, should be a valid character according to
           [alphabet] but outside the scope. *)
        idx := !idx + 8;
        pad := !pad + 5
      done;
      while !idx < len do
        if unsafe_get_uint8 input (off + !idx) <> padding then raise Not_found;

        incr idx
      done;
      !pad
    in

    let rec dec j i =
      if i = n then 0
      else
        let b8, pad =
          let x = get_uint8_or_padding input (i + 7) in
          try (dmap x, 0) with Not_found when x = padding -> (0, 1)
        in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let b7, pad =
          let x = get_uint8_or_padding input (i + 6) in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 1 -> (0, 2)
        in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let b6, pad =
          let x = get_uint8_or_padding input (i + 5) in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 2 -> (0, 2)
        in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let b5, pad =
          let x = get_uint8_or_padding input (i + 4) in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 2 -> (0, 3)
        in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let b4, pad =
          let x = get_uint8_or_padding input (i + 3) in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 3 -> (0, 4)
        in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let b3, pad =
          let x = get_uint8_or_padding input (i + 2) in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 4 -> (0, 4)
        in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let b2, pad =
          let x = get_uint8_or_padding input (i + 1) in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 5 -> (0, 5)
        in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let b1, pad =
          let x = get_uint8_or_padding input i in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 5 -> (0, 5)
        in

        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        emit (I63.of_int b1) (I63.of_int b2) (I63.of_int b3) (I63.of_int b4)
          (I63.of_int b5) (I63.of_int b6) (I63.of_int b7) (I63.of_int b8) j;

        if i + 8 = n (* end of input in anyway *) then
          match pad with 0 -> 0 | 8 -> 7 | pad -> pad
        else
          (* XXX(patricoferris): pad is probably wrong *)
          match pad with
          | 0 -> dec (j + 5) (i + 8)
          | 8 ->
              (* assert (invalid_pad_overflow = false) ; *)
              only_padding 5 (i + 8)
          (* Same situation than above but we should get only more padding
             characters then. *)
          | pad ->
              if invalid_pad_overflow = true then raise Too_much_input;
              only_padding pad (i + 8)
    in

    match dec 0 0 with
    | 0 -> Ok (Bytes.unsafe_to_string res, 0, n')
    | pad -> Ok (Bytes.unsafe_to_string res, 0, n' - pad)
    | exception Out_of_bounds ->
        error_msgf "Wrong padding"
        (* appear only when [pad = true] and when length of input is not a multiple of 4. *)
    | exception Not_found ->
        (* appear when one character of [input] ∉ [alphabet] and this character <> '=' *)
        error_msgf "Malformed input"
    | exception Too_much_input -> error_msgf "Too much input"

let decode ?pad ?(alphabet = default_alphabet) ?off ?len input =
  match decode_sub ?pad alphabet ?off ?len input with
  | Ok (res, off, len) -> Ok (String.sub res off len)
  | Error _ as err -> err

let decode_sub ?pad ?(alphabet = default_alphabet) ?off ?len input =
  decode_sub ?pad alphabet ?off ?len input

let decode_exn ?pad ?alphabet ?off ?len input =
  match decode ?pad ?alphabet ?off ?len input with
  | Ok res -> res
  | Error (`Msg err) -> invalid_arg err

(*---------------------------------------------------------------------------
   Copyright (c) 2006-2009 Citrix Systems Inc.
   Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.com>
   Copyright (c) 2014-2016 Anil Madhavapeddy <anil@recoil.org>
   Copyright (c) 2016 David Kaloper Meršinjak
   Copyright (c) 2018 Romain Calascibetta <romain.calascibetta@gmail.com>
   Copyright (c) 2021-2022 Patrick Ferris <patrick@sirref.org>
   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.
   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
