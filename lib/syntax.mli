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

type type_qualifier = Const | Restrict | Volatile [@@deriving show]

type type_qualifiers = { const : bool; restrict : bool; volatile : bool }
[@@deriving show]

type function_specifier = Inline | NoReturn [@@deriving show]

type specifier_qualifier_list = {
  type_specifiers : (type_specifier * Token.t) list;
  type_qualifiers : (type_qualifier * Token.t) list; (* TODO: alignment *)
}
[@@deriving show]

type declaration_specifiers = {
  storage_classes : (storage_class_specifier * Token.t) list;
  type_specifiers : (type_specifier * Token.t) list;
  type_qualifiers : (type_qualifier * Token.t) list;
  func_specifiers : (function_specifier * Token.t) list;
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

(**)
val string_of_storage_class_specifier : storage_class_specifier -> string
val string_of_type_qualifier : type_qualifier -> string
val string_of_function_specifier : function_specifier -> string

(**)
val empty_type_qualifiers : type_qualifiers
val empty_specifier_qualifier_list : specifier_qualifier_list
val empty_declaration_specifiers : declaration_specifiers

val reverse_specifier_qualifier_list :
  specifier_qualifier_list -> specifier_qualifier_list

val reverse_declaration_specifiers :
  declaration_specifiers -> declaration_specifiers

val add_sq_type_specifier :
  specifier_qualifier_list ->
  type_specifier ->
  Token.t ->
  specifier_qualifier_list

val add_sq_type_qualifier :
  specifier_qualifier_list ->
  type_qualifier ->
  Token.t ->
  specifier_qualifier_list

val add_decl_storage_class :
  declaration_specifiers ->
  storage_class_specifier ->
  Token.t ->
  declaration_specifiers

val add_decl_type_specifier :
  declaration_specifiers -> type_specifier -> Token.t -> declaration_specifiers

val add_decl_type_qualifier :
  declaration_specifiers -> type_qualifier -> Token.t -> declaration_specifiers

val add_decl_function_specifier :
  declaration_specifiers ->
  function_specifier ->
  Token.t ->
  declaration_specifiers
