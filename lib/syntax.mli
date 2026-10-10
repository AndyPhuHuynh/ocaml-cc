type storage_class_specifier =
  | Typedef
  | Extern
  | Static
  | ThreadLocal
  | Auto
  | Register
[@@deriving show]

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
  | Bool
  | Complex
  | Atomic of type_name
[@@deriving show]

and type_qualifier = Const | Restrict | Volatile [@@deriving show]

and type_qualifiers = { const : bool; restrict : bool; volatile : bool }
[@@deriving show]

and function_specifier = Inline | NoReturn [@@deriving show]

and specifier_qualifier_list = {
  type_specifiers : (type_specifier * Token.t) list;
  type_qualifiers : (type_qualifier * Token.t) list; (* TODO: alignment *)
}
[@@deriving show]

and declaration_specifiers = {
  storage_classes : (storage_class_specifier * Token.t) list;
  type_specifiers : (type_specifier * Token.t) list;
  type_qualifiers : (type_qualifier * Token.t) list;
  func_specifiers : (function_specifier * Token.t) list;
}
[@@deriving show]

and pointers = type_qualifiers list [@@deriving show]
and array_size = NoSize | Size of Token.int_literal | Star [@@deriving show]

and array_suffix = {
  size : array_size;
  type_qualifiers : type_qualifiers;
  is_static : bool;
}
[@@deriving show]

and function_param_declaration =
  | Declaration of declaration_specifiers * declarator
  | AbstractDeclaration of declaration_specifiers * abstract_declarator option
[@@deriving show]

and function_param_type_list = {
  declarators : function_param_declaration list;
  has_ellipses : bool;
}
[@@deriving show]

and function_identifier_list = { identifiers : string list } [@@deriving show]

and function_suffix =
  | ParamList of function_param_type_list
  | IdentList of function_identifier_list
[@@deriving show]

and declarator_suffix =
  | ArraySuffix of array_suffix
  | FunctionSuffix of function_suffix
[@@deriving show]

and declarator_base =
  | Identifier of { name : string; info : Token.info }
  | Declarator of declarator
[@@deriving show]

and declarator = {
  pointers : pointers;
  decl_base : declarator_base;
  suffixes : declarator_suffix list;
}
[@@deriving show]

and abstract_declarator = {
  pointers : pointers;
  decl_base : abstract_declarator option;
  suffixes : declarator_suffix list;
}
[@@deriving show]

and type_name = {
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
