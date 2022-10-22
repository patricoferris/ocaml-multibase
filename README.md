ocaml-multibase
---------------

A library providing self-identifying base encodings as per the [multibase](https://github.com/multiformats/multibase). Take the following encoded string. Not only can be decode it without specifying a base-encoding scheme, we also get that scheme back.

```ocaml
let encoded = "T9T1M2RBC41KN6837E9IM2T11"
```

The `Base.decode` function will return either the encoding scheme along with the decoded string, or an error.

```ocaml
# Multibase.decode encoded;;
- : (Multibase.Encoding.t * string,
     [ `Msg of string | `Unsupported of Multibase.Encoding.t ])
    result
= Ok (`Base32hexpadupper, "OCaml is great!")
```

So this string was encoded using Base32 with the _extended hexadecimal_ alphabet in upper-case. We can just as easily put it back the way it was.

```ocaml
# let encoded_ = Multibase.encode `Base32hexpadupper "OCaml is great!";;
val encoded_ :
  (string, [ `Msg of string | `Unsupported of Multibase.Encoding.t ]) result =
  Ok "T9T1M2RBC41KN6837E9IM2T11"
# String.equal encoded (Result.get_ok encoded_);;
- : bool = true
```

Note that the main `encode` function will let you specify _all_ possible encodings as per the specification. However, `encode_t` narrows this to the support encodings of this library, hence it won't return `` `Unsupported`.

```ocaml
# let encoded_ = Multibase.encode_t `Base32hexpadupper "OCaml is great!";;
val encoded_ : (string, [ `Msg of string ]) result =
  Ok "T9T1M2RBC41KN6837E9IM2T11"
```
