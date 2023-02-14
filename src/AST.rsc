module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(str name, AId id, AType t)
  | computed_question(str name, AId id, AType t, AExpr expr)
  | expr_if(AExpr expr, list[AQuestion] if_questions)
  | expr_ifelse(AExpr expr, list[AQuestion] if_questions, list[AQuestion] else_questions)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | boole(bool boolean)
  | integ(int integer)
  | stri(str string)
  | brackets(AExpr arg)
  | not(AExpr arg)
  | mul(AExpr lhs, AExpr rhs)
  | div(AExpr lhs, AExpr rhs)
  | add(AExpr lhs, AExpr rhs)
  | sub(AExpr lhs, AExpr rhs)
  | greater(AExpr lhs, AExpr rhs)
  | less(AExpr lhs, AExpr rhs)
  | greater_or_equal(AExpr lhs, AExpr rhs)
  | less_or_equal(AExpr lhs, AExpr rhs)
  | not_equal(AExpr lhs, AExpr rhs)
  | equal(AExpr lhs, AExpr rhs)
  | and(AExpr lhs, AExpr rhs)
  | or(AExpr lhs, AExpr rhs)
  ;


data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
  = typing(str name);

