module CST2AST

import ParseTree;
import Syntax;
import AST;
import String;
import Boolean;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form("<f.name>", [ cst2ast(q) | Question q <- f.questions ], src=sf.src);
}

AQuestion cst2ast(Question q) {
  switch (q) {
    case (Question)`"<Str s>" <Id x> : <Type t> = <Expr e>`: return computed_question("<s>", id("<x>", src=x.src), cst2ast(t), cst2ast(e));
    case (Question)`"<Str s>" <Id x> : <Type t>`: return question("<s>", id("<x>", src=x.src), cst2ast(t));
    case (Question)`if (<Expr e>) { <Question* qs>}`: return expr_if(cst2ast(e), [ cst2ast(q) | Question q <- qs]);
    case (Question)`if (<Expr e>) { <Question* qs_if>} else { <Question* qs_else>}`: return expr_ifelse(cst2ast(e), [ cst2ast(q) | Question q <- qs_if ], [ cst2ast(q) | Question q <- qs_else ]);
    default: throw "Unhandled expression: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x.src));
    case (Expr)`<Bool x>`: return boole(fromString("<x>"), src=x.src);
    case (Expr)`<Int x>`: return integ(toInt("<x>"), src=x.src);
    case (Expr)`"<Str x>"`: return stri("<x>", src=x.src);
    case (Expr)`(<Expr e>)`: return brackets(cst2ast(e), src=e.src);
    case (Expr)`!<Expr e>`: return not(cst2ast(e), src=e.src);
    case (Expr)`<Expr e_l> * <Expr e_r>`: return mul(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> / <Expr e_r>`: return div(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> + <Expr e_r>`: return add(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> - <Expr e_r>`: return sub(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> \> <Expr e_r>`: return greater(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> \< <Expr e_r>`: return less(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> \>= <Expr e_r>`: return greater_or_equal(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> \<= <Expr e_r>`: return less_or_equal(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> \>= <Expr e_r>`: return greater_or_equal(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> != <Expr e_r>`: return not_equal(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> == <Expr e_r>`: return equal(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> && <Expr e_r>`: return and(cst2ast(e_l), cst2ast(e_r));
    case (Expr)`<Expr e_l> || <Expr e_r>`: return or(cst2ast(e_l), cst2ast(e_r));
    default: throw "Unhandled expression: <e>";
  }
}

default AType cst2ast(Type t) {
  switch (t) {
    case (Type)`<Type x>`: return typing("<x>", src=x.src);
    default: throw "Unhandled expression: <t>";
  }
}
