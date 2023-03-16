module Compile

import AST;
import Resolve;
import IO;
import lang::html::AST; // see standard library
import lang::html::IO;

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTMLElement type and the `str writeHTMLString(HTMLElement x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, writeHTMLString(form2html(f)));
}

str convertExprToString(AExpr expr) {
  switch (expr) {
    case ref(id(str x)): return x;
    case boole(bool boolean): return "<boolean>";
    case integ(int integer): return "<integer>";
    case stri(str string): return string;
    case brackets(AExpr arg): return "(" + convertExprToString(arg) + ")";
    case not(AExpr arg): return "!" + convertExprToString(arg);
    case mul(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "*" + convertExprToString(rhs);
    case div(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "/" + convertExprToString(rhs);
    case add(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "+" + convertExprToString(rhs);
    case sub(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "-" + convertExprToString(rhs);
    case greater(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "\>" + convertExprToString(rhs);
    case less(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "\<" + convertExprToString(rhs);
    case greater_or_equal(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "\>=" + convertExprToString(rhs);
    case less_or_equal(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "\<=" + convertExprToString(rhs);
    case not_equal(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "!=" + convertExprToString(rhs);
    case equal(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "==" + convertExprToString(rhs);
    case and(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "&&" + convertExprToString(rhs);
    case or(AExpr lhs, AExpr rhs): return convertExprToString(lhs) + "||" + convertExprToString(rhs);
    default: return "";
  }
}

HTMLElement form2html(AForm f) {
  list[HTMLElement] htmlquestion = [];
  for (AQuestion q <- f.questions) {
    htmlquestion += q2html(q);
  }


  return html([
    head([
      title([text(f.name)]),
      script([], src=f.src[extension="js"].file)
    ]),
    body([
      form(htmlquestion)
    ])
  ]);
}

HTMLElement q2html(AQuestion q) {
  list[HTMLElement] htmlelements = [];
  switch(q) {
    case question(str name, AId id, AType t): {
      htmlelements += [  
        label([text(name)], \for = id.name), 
        br()
      ];
      if(t.name == "string") {
        htmlelements += input(\type = "text", id = id.name, name = id.name);
      }
      else if(t.name == "boolean") {
        htmlelements += input(\type = "checkbox", id = id.name, name = id.name);
      }
      else {
        htmlelements += input(\type = "number", id = id.name, name = id.name);
      }
      htmlelements += br();
    } 
     
    case computed_question(str name, AId id, AType t, _): {
      htmlelements += [
        label([text(name)], \for = id.name), 
        br()
      ];
      if(t.name == "string") {
        htmlelements += input(\type = "text", id = id.name, name = id.name, disabled = "true");
      }
      else if(t.name == "boolean") {
        htmlelements += input(\type = "checkbox", id = id.name, name = id.name, disabled = "true");
      }
      else {
        htmlelements += input(\type = "number", id = id.name, name = id.name, disabled = "true");
      }
      htmlelements += br();
    }
      
    case expr_if(AExpr expr, list[AQuestion] if_questions): {
      list[HTMLElement] if_question_set = [];
      for (question <- if_questions) {
        if_question_set += q2html(question);
      }
      HTMLElement divIf = div(if_question_set);
      divIf.id = "if" + convertExprToString(expr);
      htmlelements += divIf;
    }

    case expr_ifelse(AExpr expr, list[AQuestion] if_questions, list[AQuestion] else_questions): {
      list[HTMLElement] if_question_set = [];
      list[HTMLElement] else_question_set = [];
      for (if_question <- if_questions) {
        if_question_set += q2html(if_question);
      }
      HTMLElement divIf = div(if_question_set);
      divIf.id = "if" + convertExprToString(expr);
      htmlelements += divIf;

      for (else_question <- else_questions) {
        else_question_set += q2html(else_question);
      }
      HTMLElement divElse = div(else_question_set);
      divElse.id = "else" + convertExprToString(expr);
      htmlelements += divElse;
    }
  }
  return div(htmlelements);
}

str form2js(AForm f) {
  str jsform = "";
  for (q <- f.questions) {
    jsform += q2js(q);
  }

  return jsform;
}

str getValue(str id) {
  return "document.getElementById(" + id + ").value";
}

str q2js(AQuestion q) {
  str jselements = "";
  switch(q) {
    case question(str name, AId id, AType t): {
      str val = getValue(id.name);
      jselements += "<id.name> = <val>\n";
    } 
     
    case computed_question(str name, AId id, AType t, AExpr expr): {
      str val = convertExprToString(expr);
      jselements += "<id.name> = <val>\n";
    }
      
    case expr_if(AExpr expr, list[AQuestion] if_questions): {
      jselements += "if (" + convertExprToString(expr) + ") {\n";
      for (ques <- if_questions) {
        jselements += q2js(ques);
      };
      jselements += "}\n";
    }

    case expr_ifelse(AExpr expr, list[AQuestion] if_questions, list[AQuestion] else_questions): {
      jselements += "if (" + convertExprToString(expr) + ") {\n";
      for (ques <- if_questions) {
        jselements += q2js(ques);
      };
      jselements += "} else {";
      for (ques <- else_questions) {
        jselements += q2js(ques);
      };
      jselements += "}\n";
    }
  }
  return jselements;
}
