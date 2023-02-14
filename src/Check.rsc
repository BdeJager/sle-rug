module Check

import AST;
import Resolve;
import Message; // see standard library
import List;
import IO;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

Type getTypeFromString(str name) {
  switch(name) {
    case "integer": return tint();
    case "boolean": return tbool();
    case "string": return tstr();
    default: return tunknown();
  }
}

str getStringFromType(Type t) {
  switch(t) {
    case tint(): return "integer";
    case tbool(): return "boolean";
    case tstr(): return "string";
    default: return "unknown";
  }
}

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv tenvs = {};
  
  for (/question(str name, AId id, AType t) := f) {
    tenvs += {<id.src, id.name, name, getTypeFromString("<t.name>")>};
  }

  for (/computed_question(str name, AId id, AType t, AExpr expr) := f) {
    tenvs += {<id.src, id.name, name, getTypeFromString("<t.name>")>};
  }

  return tenvs;
}

set[Message] check(AForm f) {
  resolves = resolve(f);
  UseDef usedefs = resolves[2];

  return check(f, collect(f), usedefs);
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};

  for (q <- f.questions) {
    msgs += check(q, tenv, useDef);
  }

  return msgs; 
}

set[Message] checkQuestion(str name, AId id, Type t, TEnv tenv, loc l) {
  set[Message] msgs = {};
  list[str] names = [n | <_, n, _, typ> <- tenv, (n == id.name) && (t != typ)];
  if(size(names) > 0) {
    msgs += {error("Declared questions with the same name but different types.", l)};
  }

  list[str] labels = [label_name | <_, _, label_name, _> <- tenv, (label_name == name)];
  if(size(labels) > 1) {
    msgs += {warning("Duplicate labels", l)};
  }

  return msgs;
}

set[Message] checkType(AExpr expr, Type t, TEnv tenv, loc l, UseDef usedef) {
  set[Message] msgs = {};
  Type type_of_expression = typeOf(expr, tenv, usedef);
  if(type_of_expression != t) {
    msgs += {error("Type of computed questions does not match the type of the expression - " + getStringFromType(t) + " vs " + getStringFromType(type_of_expression), l)};
  }

  return msgs;
}

set[Message] checkSetOfQuestions (list[AQuestion] questions, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  for (q <- questions) {
    msgs += check(q, tenv, useDef);
  }
  return msgs;
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  switch(q) {
    case question(str name, AId id, AType t): 
      return checkQuestion(name, id, getTypeFromString("<t.name>"), tenv, id.src);
    case computed_question(str name, AId id, AType t, AExpr expr): 
      return checkQuestion(name, id, getTypeFromString("<t.name>"), tenv, q.src) + checkType(expr, getTypeFromString("<t.name>"), tenv, id.src, useDef);
    case expr_if(AExpr expr, list[AQuestion] if_questions): 
      return check(expr, tenv, useDef) + checkSetOfQuestions(if_questions, tenv, useDef);
    case expr_ifelse(AExpr expr, list[AQuestion] if_questions, list[AQuestion] else_questions): 
      return check(expr, tenv, useDef) + checkSetOfQuestions(if_questions, tenv, useDef) + checkSetOfQuestions(else_questions, tenv, useDef);
    default: 
      return {error("Not implemented")};
  }
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(AId x):
      msgs += { error("Undeclared question", x.src) | useDef[x.src] == {} };
    case boole(_):
      return msgs;
    case integ(_):
      return msgs;
    case stri(_):
      return msgs;
    case brackets(AExpr arg):
      msgs += check(arg, tenv, useDef);
    case not(AExpr arg):
      msgs += { error("Incompatible typing", arg.src) | typeOf(arg, tenv, useDef) != tbool() };
    case mul(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tint() 
      || typeOf(rhs, tenv, useDef) != tint()};
    case div(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tint() 
      || typeOf(rhs, tenv, useDef) != tint()};
    case add(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tint() 
      || typeOf(rhs, tenv, useDef) != tint()};
    case sub(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tint() 
      || typeOf(rhs, tenv, useDef) != tint()};
    case greater(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tint() 
      || typeOf(rhs, tenv, useDef) != tint()};
    case less(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tint() 
      || typeOf(rhs, tenv, useDef) != tint()};
    case greater_or_equal(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tint() 
      || typeOf(rhs, tenv, useDef) != tint()};
    case less_or_equal(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tint() 
      || typeOf(rhs, tenv, useDef) != tint()};
    case not_equal(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != typeOf(rhs, tenv, useDef)};
    case equal(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != typeOf(rhs, tenv, useDef)};
    case and(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tbool() 
      || typeOf(rhs, tenv, useDef) != tbool()};
    case or(AExpr lhs, AExpr rhs):
      msgs += { error("Incompatible typing", rhs.src) | typeOf(lhs, tenv, useDef) != tbool() 
      || typeOf(rhs, tenv, useDef) != tbool()};
    // etc.
  }
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    case boole(_):
      return tbool();
    case integ(_):
      return tint();
    case stri(_):
      return tstr();
    case brackets(AExpr arg):
      return typeOf(arg, tenv, useDef);
    case not(AExpr arg):
      return typeOf(arg, tenv, useDef);
    case mul(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
        return tint();
      } else {
        return tunknown();
      }
    case div(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
        return tint();
      } else {
        return tunknown();
      }
    case add(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
        return tint();
      } else {
        return tunknown();
      }
    case sub(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
        return tint();
      } else {
        return tunknown();
      }
    case greater(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
        return tbool();
      } else {
        return tunknown();
      }
    case less(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
        return tbool();
      } else {
        return tunknown();
      }
    case greater_or_equal(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
        return tbool();
      } else {
        return tunknown();
      }
    case less_or_equal(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
        return tbool();
      } else {
        return tunknown();
      }
    case not_equal(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == typeOf(rhs, tenv, useDef) && typeOf(lhs, tenv, useDef) != tunknown()) {
        return tbool();
      } else {
        return tunknown();
      }
    case equal(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == typeOf(rhs, tenv, useDef) && typeOf(lhs, tenv, useDef) != tunknown()) {
        return tbool();
      } else {
        return tunknown();
      }
    case and(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tbool() && typeOf(rhs, tenv, useDef) == tbool()) {
        return tbool();
      } else {
        return tunknown();
      }
    case or(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tbool() && typeOf(rhs, tenv, useDef) == tbool()) {
        return tbool();
      } else {
        return tunknown();
      }
    default:
      return tunknown();
    

    // etc.
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

