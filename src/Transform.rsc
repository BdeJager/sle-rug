module Transform

import Syntax;
import Resolve;
import AST;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
  list[AQuestion] quests = [];
  for(q <- f.questions) {
    quests += flattenQuestion(q, boole(true));
  }
  f.questions = quests;
  return f; 
}

AQuestion flattenQuestion(AQuestion q, AExpr expr) {
  switch(q) {
    case question(_, _, _): return expr_if(expr, [q]);
    case computed_question(_, _, _, _): return expr_if(expr, [q]);
    case expr_if(AExpr if_expr, list[AQuestion] if_questions): {
      for(if_ques <- if_questions) {
        return flattenQuestion(if_ques, and(expr, brackets(if_expr)));
      }
    }
    case expr_ifelse(AExpr if_expr, list[AQuestion] if_questions, list[AQuestion] else_questions): {
      for(if_ques <- if_questions) {
        return flattenQuestion(if_ques, and(expr, brackets(if_expr)));
      }
      for(else_ques <- else_questions) {
        return flattenQuestion(else_ques, and(expr, brackets(if_expr)));
      }
    }
  }
  // should not happen
  return expr_if(expr, []);
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
  set[loc] uses = {};
  if (useOrDef in useDef<1>) {
    uses += {loca | <loc loca, useOrDef> <- useDef};
  }
  if (useOrDef in useDef<0>) {
    uses = {loca | <useOrDef, loc loca> <- useDef};
  }
  visit(f) {
    case Id id => [Id]newName when id.src in uses
  }
  return f; 
} 
 
 
 

