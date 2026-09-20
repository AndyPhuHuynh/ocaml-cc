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
[@@deriving show]
(* TODO: atomic, struct/union, enum, typedef*)

type type_qualifier = Const | Restrict | Volatile [@@deriving show]
(* TODO: atomic *)

type type_qualifiers = { const : bool; restrict : bool; volatile : bool }
[@@deriving show]

type function_specifier = Inline | NoReturn [@@deriving show]

(* TODO: aligment specifiers *)

type specifier_qualifier_list = {
  type_specifiers : (type_specifier * Token.t) list;
  type_qualifiers : (type_qualifier * Token.t) list; (* TODO: alignment *)
}
[@@deriving show]

type declaration_specifiers = {
  storage_classes : (storage_class_specifier * Token.t) list;
  type_specifiers : (type_specifier * Token.t) list;
  type_qualifiers : (type_qualifier * Token.t) list;
  func_specifiers : (function_specifier * Token.t) list; (* TODO: alignment *)
}

type pointers = type_qualifiers list [@@deriving show]
type array_size = NoSize | Size of Token.int_literal | Star [@@deriving show]

type array_suffix = {
  size : array_size;
  type_qualifiers : type_qualifiers;
  is_static : bool;
}
[@@deriving show]

type declarator_suffix = ArraySuffix of array_suffix [@@deriving show]

type declarator_base =
  | Identifier of { name : string; info : Token.info }
  | Declarator of declarator
[@@deriving show]

and declarator = {
  pointers : pointers;
  decl_base : declarator_base;
  suffixes : declarator_suffix list;
}
[@@deriving show]

type abstract_declarator_suffix = ArraySuffix of array_suffix
[@@deriving show]

type abstract_declarator = {
  pointers : pointers;
  decl_base : abstract_declarator option;
  suffixes : abstract_declarator_suffix list;
}
[@@deriving show]

type type_name = {
  specifier_qualifier_list : specifier_qualifier_list;
  decl : abstract_declarator option;
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

let empty_specifier_qualifier_list : specifier_qualifier_list =
  { type_specifiers = []; type_qualifiers = [] }

let empty_declaration_specifiers : declaration_specifiers =
  {
    storage_classes = [];
    type_specifiers = [];
    type_qualifiers = [];
    func_specifiers = [];
  }

let reverse_specifier_qualifier_list (specs : specifier_qualifier_list) :
    specifier_qualifier_list =
  {
    type_specifiers = List.rev specs.type_specifiers;
    type_qualifiers = List.rev specs.type_qualifiers;
  }

let reverse_declaration_specifiers (specs : declaration_specifiers) :
    declaration_specifiers =
  {
    storage_classes = List.rev specs.storage_classes;
    type_specifiers = List.rev specs.type_specifiers;
    type_qualifiers = List.rev specs.type_qualifiers;
    func_specifiers = List.rev specs.func_specifiers;
  }

let add_sq_type_specifier (specs : specifier_qualifier_list)
    (spec : type_specifier) (token : Token.t) : specifier_qualifier_list =
  { specs with type_specifiers = (spec, token) :: specs.type_specifiers }

let add_sq_type_qualifier (specs : specifier_qualifier_list)
    (spec : type_qualifier) (token : Token.t) : specifier_qualifier_list =
  { specs with type_qualifiers = (spec, token) :: specs.type_qualifiers }

let add_decl_storage_class (specs : declaration_specifiers)
    (spec : storage_class_specifier) (token : Token.t) : declaration_specifiers
    =
  { specs with storage_classes = (spec, token) :: specs.storage_classes }

let add_decl_type_specifier (specs : declaration_specifiers)
    (spec : type_specifier) (token : Token.t) : declaration_specifiers =
  { specs with type_specifiers = (spec, token) :: specs.type_specifiers }

let add_decl_type_qualifier (specs : declaration_specifiers)
    (spec : type_qualifier) (token : Token.t) : declaration_specifiers =
  { specs with type_qualifiers = (spec, token) :: specs.type_qualifiers }

let add_decl_function_specifier (specs : declaration_specifiers)
    (spec : function_specifier) (token : Token.t) : declaration_specifiers =
  { specs with func_specifiers = (spec, token) :: specs.func_specifiers }
