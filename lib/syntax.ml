type storage_class_specifier =
  | Typedef
  | Extern
  | Static
  | ThreadLocal
  | Auto
  | Register

type type_specifier =
  | Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Signed
  | Unsigned
(* TODO: atomic, struct/union, enum, typedef*)

type type_qualifier = Const | Restrict | Volatile
(* TODO: atomic *)

type type_qualifiers = { const : bool; restrict : bool; volatile : bool }
[@@deriving show]

type function_specifier = Inline | NoReturn

(* TODO: aligment specifiers *)

type declaration_specifiers = {
  storage_classes : (storage_class_specifier * Token.t) list;
  type_specifiers : (type_specifier * Token.t) list;
  type_qualifiers : (type_qualifier * Token.t) list;
  func_specifiers : (function_specifier * Token.t) list;
}

type array_size = None | Size of Token.int_literal | Star [@@deriving show]

type declarator = {
  pointers : type_qualifiers list;
  direct_decl : direct_declarator;
}
[@@deriving show]

and direct_declarator =
  | Identifier of { name : string; info : Token.info }
  | Array of {
      decl : direct_declarator;
      size : array_size;
      type_qualifiers : type_qualifiers;
      is_static : bool;
    }
[@@deriving show]

let string_of_storage_class_specifier (spec : storage_class_specifier) : string
    =
  match spec with
  | Typedef -> "typedef"
  | Extern -> "extern"
  | Static -> "static"
  | ThreadLocal -> "_Thread_local"
  | Auto -> "auto"
  | Register -> "register"

let string_of_type_qualifier (spec : type_qualifier) : string =
  match spec with
  | Const -> "Const"
  | Restrict -> "Restrict"
  | Volatile -> "Volatile"

let string_of_function_specifier (spec : function_specifier) : string =
  match spec with Inline -> "inline" | NoReturn -> "_Noreturn"

let empty_type_qualifiers : type_qualifiers =
  { const = false; restrict = false; volatile = false }

let empty_declaration_specifiers : declaration_specifiers =
  {
    storage_classes = [];
    type_specifiers = [];
    type_qualifiers = [];
    func_specifiers = [];
  }

let reverse_specs (specs : declaration_specifiers) : declaration_specifiers =
  {
    storage_classes = List.rev specs.storage_classes;
    type_specifiers = List.rev specs.type_specifiers;
    type_qualifiers = List.rev specs.type_qualifiers;
    func_specifiers = List.rev specs.func_specifiers;
  }

let add_storage_class (specs : declaration_specifiers)
    (spec : storage_class_specifier) (token : Token.t) : declaration_specifiers
    =
  { specs with storage_classes = (spec, token) :: specs.storage_classes }

let add_type_specifier (specs : declaration_specifiers) (spec : type_specifier)
    (token : Token.t) : declaration_specifiers =
  { specs with type_specifiers = (spec, token) :: specs.type_specifiers }

let add_type_qualifier (specs : declaration_specifiers) (spec : type_qualifier)
    (token : Token.t) : declaration_specifiers =
  { specs with type_qualifiers = (spec, token) :: specs.type_qualifiers }

let add_function_specifier (specs : declaration_specifiers)
    (spec : function_specifier) (token : Token.t) : declaration_specifiers =
  { specs with func_specifiers = (spec, token) :: specs.func_specifiers }
