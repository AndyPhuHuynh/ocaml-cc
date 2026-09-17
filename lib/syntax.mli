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

type type_qualifier = Const | Restrict | Volatile

type type_qualifiers = { const : bool; restrict : bool; volatile : bool }
[@@deriving show]

type function_specifier = Inline | NoReturn

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

(**)
val string_of_storage_class_specifier : storage_class_specifier -> string
val string_of_type_qualifier : type_qualifier -> string
val string_of_function_specifier : function_specifier -> string

(**)
val empty_type_qualifiers : type_qualifiers
val empty_declaration_specifiers : declaration_specifiers
val reverse_specs : declaration_specifiers -> declaration_specifiers

val add_storage_class :
  declaration_specifiers ->
  storage_class_specifier ->
  Token.t ->
  declaration_specifiers

val add_type_specifier :
  declaration_specifiers -> type_specifier -> Token.t -> declaration_specifiers

val add_type_qualifier :
  declaration_specifiers -> type_qualifier -> Token.t -> declaration_specifiers

val add_function_specifier :
  declaration_specifiers ->
  function_specifier ->
  Token.t ->
  declaration_specifiers
