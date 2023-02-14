module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  map[str name, Value \value] venv = ();
  for(/question(str name, AId id, AType t) := f) {
    switch(t.name) {
      case "integer":
        venv[id.name] = vint(0);
      case "string":
        venv[id.name] = vstr("");
      case "boolean":
        venv[id.name] = vbool(false);
    }
  }
  for(/computed_question(str name, AId id, AType t, AExpr expr) := f) {
    switch(t.name) {
      case "integer":
        venv[id.name] = vint(0);
      case "string":
        venv[id.name] = vstr("");
      case "boolean":
        venv[id.name] = vbool(false);
    }
  }
  return venv;
}

// testeval(astform, "Did you buy a house in 2010?", "yes")
VEnv testeval(AForm f, str q, str s) {
  return eval(f, input(q, vstr(s)), initialEnv(f));
}

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv initial_venv) {
  venv = initial_venv;
  for (q <- f.questions) {
    venv = eval(q, inp, venv);
  }

  return venv; 
}

VEnv eval(AQuestion q, Input inp, VEnv initial_venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  venv = initial_venv;
  switch(q) {
    case question(str name, AId id, AType t):
      if(name == inp.question) {
        venv[id.name] = inp.\value;
      }
    case computed_question(str name, AId id, AType t, AExpr expr):
      if(name == inp.question) {
        venv[id.name] = eval(expr, venv);
      }
    case expr_if(AExpr expr, list[AQuestion] if_questions):
      if(eval(expr, venv).b) {
        for(q <- if_questions) {
          venv = eval(q, inp, venv);
        }
      }
    case expr_ifelse(AExpr expr, list[AQuestion] if_questions, list[AQuestion] else_questions):
      if(eval(expr, venv).b) {
        for(q <- if_questions) {
          venv = eval(q, inp, venv);
        }
      } else {
        for(q <- else_questions) {
          venv = eval(q, inp, venv);
        }
      }
  }

  return venv;
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case boole(bool boolean): return vbool(boolean);
    case integ(int integer): return vint(integer);
    case stri(str string): return vstr(string);
    case brackets(AExpr arg): return eval(arg, venv);

    case not(AExpr arg): 
      if(eval(arg, venv).b) {
        return vbool(false);
      } else {
        return vbool(true);
      }

    case mul(AExpr lhs, AExpr rhs): return vint(eval(lhs, venv).n * eval(rhs, venv).n);
    case div(AExpr lhs, AExpr rhs): return vint(eval(lhs, venv).n / eval(rhs, venv).n);
    case add(AExpr lhs, AExpr rhs): return vint(eval(lhs, venv).n + eval(rhs, venv).n);
    case sub(AExpr lhs, AExpr rhs): return vint(eval(lhs, venv).n - eval(rhs, venv).n);
    case greater(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).n > eval(rhs, venv).n);
    case less(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).n < eval(rhs, venv).n);
    case greater_or_equal(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).n >= eval(rhs, venv).n);
    case less_or_equal(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).n <= eval(rhs, venv).n);
    case not_equal(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv) != eval(rhs, venv));
    case equal(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv) == eval(rhs, venv));
    case and(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).b && eval(rhs, venv).b);
    case or(AExpr lhs, AExpr rhs): return vbool(eval(lhs, venv).b || eval(rhs, venv).b);
    
    default: throw "Unsupported expression <e>";
  }
}