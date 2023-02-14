module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id name "{" Question* questions "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question 
= "\"" Str string "\"" Id name ":" Type t "=" Expr expr
| "\"" Str string "\"" Id name ":" Type t !>> "=" // not followed by equal sign
| "{" Question* questions "}" // block to group questions
| "if" "(" Expr expr ") {" Question* qIf "}" "else" "{" Question* qElse "}" //if-then-else
| "if" "(" Expr expr ") {" Question* qIf "}" !>> "else" //if-then, not followed by else
;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | Bool b
  | Int i
  | "\"" Str s "\""
  | bracket "(" Expr expr ")"
  > "!" Expr expr
  > left Expr expr "*" Expr expr
  > left Expr expr "/" Expr expr
  > left Expr expr "+" Expr expr
  > left Expr expr "-" Expr expr
  > left Expr expr "\>" Expr expr
  > left Expr expr "\<" Expr expr
  > left Expr expr "\>=" Expr expr
  > left Expr expr "\<=" Expr expr
  > left Expr expr "!=" Expr expr
  > left Expr expr "==" Expr expr
  > left Expr expr "&&" Expr expr
  > left Expr expr "||" Expr expr
  ;
  
syntax Type = "boolean" | "integer" | "string";

lexical Str = ![\"]+; // a string is everything that is not a " character

lexical Int 
  = [0-9]+;

lexical Bool = "true" | "false";