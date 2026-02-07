type t = { lang_cache : (string, nativeint) Hashtbl.t }

let create () = { lang_cache = Hashtbl.create 16 }
