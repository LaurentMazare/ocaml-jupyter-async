module String_set = Set.Make (String)

let default_keywords =
  [ "and"
  ; "as"
  ; "assert"
  ; "begin"
  ; "class"
  ; "constraint"
  ; "do"
  ; "done"
  ; "downto"
  ; "else"
  ; "end"
  ; "exception"
  ; "external"
  ; "for"
  ; "fun"
  ; "function"
  ; "functor"
  ; "if"
  ; "in"
  ; "include"
  ; "inherit"
  ; "initializer"
  ; "lazy"
  ; "let"
  ; "match"
  ; "method"
  ; "module"
  ; "mutable"
  ; "new"
  ; "object"
  ; "of"
  ; "open"
  ; "private"
  ; "rec"
  ; "sig"
  ; "struct"
  ; "then"
  ; "to"
  ; "try"
  ; "type"
  ; "val"
  ; "virtual"
  ; "when"
  ; "while"
  ; "with"
  ; "try_lwt"
  ; "finally"
  ; "for_lwt"
  ; "lwt"
  ]

let keywords = ref (String_set.of_list default_keywords)
let add_keyword kwd = keywords := String_set.add kwd !keywords
