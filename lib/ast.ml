(* Expressions *)
type literal_expression = IntLiteral of Z.t
type expression = LiteralExpression of literal_expression

(* Statements *)
type statement = CompoundStatement of compound_statement | JumpStatement
and compound_statement = block_item list
and block_item = Statement of statement
and jump_statement = Return of expression

(* Declarations *)
type object_storage =
  | NoStorage
  | Extern
  | Static
  | ThreadLocal
  | ThreadLocalStatic
  | ThreadLocalExtern
  | Auto
  | Register

type function_storage = NoStorage | Extern | Static
type type_ = Int
type function_declaration = { return_type : type_; name : string }

type function_definition = {
  declaration : function_declaration;
  body : compound_statement;
}

type declaration = FunctionDeclaration of function_declaration

type external_declaration =
  | FunctionDefinition of function_definition
  | Declaration of declaration

type translation_unit = external_declaration list
