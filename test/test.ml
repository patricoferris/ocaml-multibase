(*
 * Copyright (c) 2021 Patrick Ferris <pf341@patricoferris.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let pp_err ppf = function
  | `Msg s -> Fmt.pf ppf "%s" s
  | `Unsupported s -> Fmt.pf ppf "%s" (Multibase.Encoding.to_code s)

let err = Alcotest.of_pp pp_err

let encoding =
  Alcotest.of_pp (fun ppf s -> Fmt.string ppf (Multibase.Encoding.to_string s))

let test_encode input t output () =
  let encode = Multibase.encode t input in
  Alcotest.(check (result string err)) "same encoding" (Ok output) encode;
  let decode = Result.bind encode Multibase.decode in
  Alcotest.(check (result (pair encoding string) err))
    "same decoding"
    (Ok (t, input))
    decode

let tests =
  [
    ("helloworld", `Base58btc, "z6sBRWyteSSzHrs");
    ("helloworld", `Base58flickr, "Z6SbqvYTDrrZhRS");
    ("helloworld", `Base64, "maGVsbG93b3JsZA");
    ("helloworld", `Base64pad, "MaGVsbG93b3JsZA==");
    ("helloworld", `Base32hexupper, "VD1IMOR3FETNN4R34");
    ("helloworld", `Base32hexpadupper, "TD1IMOR3FETNN4R34");
    ("OCAML!!", `Base32hexpadupper, "T9T1K2JAC44GG====");
    ("OCAML!!", `Base32hexpad, "t9t1k2jac44gg====");
    ("helloworld", `Base32hex, "vd1imor3fetnn4r34");
    ("helloworld", `Base32hexpad, "td1imor3fetnn4r34");
    ("helloworld", `Base32, "bnbswy3dpo5xxe3de");
    ("OCaml is great!", `Base32hexpadupper, "T9T1M2RBC41KN6837E9IM2T11");
  ]

let encode =
  List.map
    (fun (i, t, o) ->
      Alcotest.test_case
        (i ^ " " ^ Multibase.Encoding.to_string t)
        `Quick (test_encode i t o))
    tests

let test_unsupported () =
  let encoded = Multibase.encode `Base8 "hello" in
  Alcotest.(check (result string err))
    "same error"
    (Error (`Unsupported `Base8))
    encoded;
  let decoded = Multibase.decode "72364150123246041102" in
  Alcotest.(check (result (pair encoding string) err))
    "same error"
    (Error (`Unsupported `Base8))
    decoded

let () =
  Alcotest.run "multibase"
    [
      ("encode_decode", encode);
      ("unsupported", [ Alcotest.test_case "base8" `Quick test_unsupported ]);
    ]
