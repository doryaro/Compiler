#use "reader.ml";;

type expr =
  | ScmConst of sexpr
  | ScmVar of string
  | ScmIf of expr * expr * expr
  | ScmSeq of expr list
  | ScmSet of expr * expr
  | ScmDef of expr * expr
  | ScmOr of expr list
  | ScmLambdaSimple of string list * expr
  | ScmLambdaOpt of string list * string * expr
  | ScmApplic of expr * (expr list);;

exception X_syntax_error of sexpr * string;;
exception X_reserved_word of string;;
exception X_proper_list_error;;
exception X_not_implemented;;

let rec list_to_proper_list = function
| [] -> ScmNil
| hd::[] -> ScmPair (hd, ScmNil)
| hd::tl -> ScmPair (hd, list_to_proper_list tl);;

let rec list_to_improper_list = function
| [] -> raise X_proper_list_error
| hd::[] -> hd
| hd::tl -> ScmPair (hd, list_to_improper_list tl);;

let rec scm_append scm_list sexpr =
match scm_list with
| ScmNil -> sexpr
| ScmPair (car, cdr) -> ScmPair (car, scm_append cdr sexpr)
| _ -> raise (X_syntax_error (scm_list, "Append expects a proper list"))

let rec scm_map f sexpr =
match sexpr with
| ScmNil -> ScmNil
| ScmPair (car, cdr) -> ScmPair (f car, scm_map f cdr)
| _ -> raise (X_syntax_error (sexpr, "Map expects a list"));;

let rec scm_zip f sexpr1 sexpr2 =
match sexpr1, sexpr2 with
| ScmNil, ScmNil -> ScmNil
| ScmPair (car1, cdr1), ScmPair (car2, cdr2) -> ScmPair (f car1 car2, scm_zip f cdr1 cdr2)
| _, _ ->
    let sepxrs = list_to_proper_list [ScmSymbol "sexpr1:"; sexpr1; ScmSymbol "sexpr2:"; sexpr2] in
    raise (X_syntax_error (sepxrs, "Zip expects 2 lists of equal length"));;

let rec scm_list_to_list = function
| ScmPair (hd, tl) -> hd::(scm_list_to_list tl)
| ScmNil -> []
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec scm_is_list = function
| ScmPair (hd, tl) -> scm_is_list tl
| ScmNil -> true
| _ -> false

let rec scm_list_length = function
| ScmPair (hd, tl) -> 1 + (scm_list_length tl)
| ScmNil -> 0
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec scm_find_last  = function
| ScmPair (hd, tl) -> scm_find_last tl
| c -> c;;

let rec scm_without_last sexpr =
match sexpr with
| ScmPair (car, cdr) -> ScmPair (car, scm_without_last cdr)
| _ -> ScmNil;;

let rec untag expr =
let untag_vars vars = List.map (fun s -> ScmSymbol s) vars in
let untag_tagged_list tag exprs = list_to_proper_list (ScmSymbol tag::(List.map untag exprs)) in

let untag_lambda_opt vars var body =
let vars = match vars with
| [] -> ScmSymbol var
| _ -> list_to_improper_list (untag_vars (vars@[var])) in
list_to_proper_list ([ScmSymbol "lambda"; vars]@body) in

match expr with
| (ScmConst (ScmSymbol(_) as sexpr)
    | ScmConst (ScmNil as sexpr)
    | ScmConst (ScmPair (_, _) as sexpr)) -> list_to_proper_list [ScmSymbol "quote"; sexpr]
| ScmConst s -> s
| ScmVar (name) -> ScmSymbol(name)
| ScmIf (test, dit, ScmConst (ScmVoid)) -> untag_tagged_list "if" [test; dit]
| ScmIf (test, dit, dif) -> untag_tagged_list "if" [test; dit; dif]
| ScmSeq(exprs) -> untag_tagged_list "begin" exprs
| ScmSet (var, value) -> untag_tagged_list "set!" [var; value]
| ScmDef (var, value) -> untag_tagged_list "define" [var; value]
| ScmOr (exprs) -> untag_tagged_list "or" exprs
| ScmLambdaSimple (vars, ScmSeq(body)) ->
    let vars = list_to_proper_list (untag_vars vars) in
    let body = List.map untag body in
    list_to_proper_list ([ScmSymbol "lambda"; vars]@body)
| ScmLambdaSimple (vars, body) ->
    let vars = list_to_proper_list (untag_vars vars) in
    list_to_proper_list ([ScmSymbol "lambda"; vars; untag body])
| ScmLambdaOpt (vars, var, ScmSeq(body)) ->
    let body = List.map untag body in
    untag_lambda_opt vars var body
| ScmLambdaOpt (vars, var, body) ->
    let body = [untag body] in
    untag_lambda_opt vars var body
| ScmApplic(procedure, args) -> list_to_proper_list (List.map untag (procedure::args));;


let rec string_of_expr expr =
string_of_sexpr (untag expr)

let scm_number_eq n1 n2 =
match n1, n2 with
| ScmRational(numerator1, denominator1), ScmRational(numerator2, denominator2) ->
        numerator1 = numerator2 && denominator1 = denominator2
| ScmReal(real1), ScmReal(real2) -> abs_float(real1 -. real2) < 0.001
| _, _ -> false

let rec sexpr_eq s1 s2 =
match s1, s2 with
| (ScmVoid, ScmVoid) | (ScmNil, ScmNil)  -> true
| ScmBoolean(bool1), ScmBoolean(bool2) -> bool1 = bool2
| ScmChar(char1), ScmChar(char2) -> char1 = char2
| ScmString(string1), ScmString(string2) -> String.equal string1 string2
| ScmSymbol(symbol1), ScmSymbol(symbol2) -> String.equal symbol1 symbol2
| ScmNumber(number1), ScmNumber(number2) -> scm_number_eq number1 number2
| ScmVector(sexprs1), ScmVector(sexprs2) -> List.for_all2 sexpr_eq sexprs1 sexprs2
| ScmPair(car1, cdr1), ScmPair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
| _, _ -> false

let rec expr_eq e1 e2 =
  match e1, e2 with
  | ScmConst (sexpr1), ScmConst (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar (var1), ScmVar (var2) -> String.equal var1 var2
  | ScmIf (test1, dit1, dif1), ScmIf (test2, dit2, dif2) -> (expr_eq test1 test2) &&
                                            (expr_eq dit1 dit2) &&
                                              (expr_eq dif1 dif2)
  | (ScmSeq(exprs1), ScmSeq(exprs2) | ScmOr (exprs1), ScmOr (exprs2)) ->
        List.for_all2 expr_eq exprs1 exprs2
  | (ScmSet (var1, val1), ScmSet (var2, val2) | ScmDef (var1, val1), ScmDef (var2, val2)) ->
        (expr_eq var1 var2) && (expr_eq val1 val2)
  | ScmLambdaSimple (vars1, body1), ScmLambdaSimple (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmLambdaOpt (vars1, var1, body1), ScmLambdaOpt (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmApplic (e1, args1), ScmApplic (e2, args2) ->
     (expr_eq e1 e2) && (List.for_all2 expr_eq args1 args2)
  | _ -> false;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
  val parse : sexpr -> string
end;;

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;

let rec tag_parse_expression sexpr =
let sexpr = macro_expand sexpr in
match sexpr with
| ScmNil -> ScmConst(ScmNil)  
| ScmBoolean(x) -> ScmConst(ScmBoolean(x))
| ScmChar(x) -> ScmConst(ScmChar(x))
| ScmNumber(x) -> ScmConst(ScmNumber(x))
| ScmString(x) -> ScmConst(ScmString(x))
| ScmVector(x) -> ScmConst(ScmVector(x))
| ScmPair(ScmSymbol("quote"), ScmPair(x, ScmNil)) -> ScmConst(x)
| ScmSymbol(x) -> (match (List.exists (fun (a) -> String.equal x a) reserved_word_list) with  
    | true -> raise (X_reserved_word x )
    | false ->   ScmVar x
)
|ScmPair (ScmSymbol "if", ScmPair (test, ScmPair (dit, ScmNil))) ->
ScmIf(tag_parse_expression test, tag_parse_expression dit,ScmConst ScmVoid)
| ScmPair(ScmSymbol("if"), ScmPair(test, ScmPair(dit, ScmPair(dif, ScmNil)))) ->
ScmIf(tag_parse_expression test, tag_parse_expression dit, tag_parse_expression dif)
| ScmPair (ScmSymbol "or", ScmNil) -> ScmConst(ScmBoolean(false))
| ScmPair (ScmSymbol "or", ScmPair (x, ScmNil)) -> tag_parse_expression x
| ScmPair(ScmSymbol "or", ScmPair (x, y)) -> ScmOr(tag_parse_expression x :: List.map (fun x->tag_parse_expression x) (scm_list_to_list y))
| ScmPair (ScmSymbol "lambda",ScmPair (args,body)) -> tag_lambda args body
| ScmPair (ScmSymbol "define",ScmPair (ScmSymbol x,ScmPair (y,ScmNil))) ->  ScmDef (ScmVar x,tag_parse_expression y)
| ScmPair (ScmSymbol "define" ,ScmPair (ScmPair(var,args) , body)) ->  ScmDef (ScmVar (string_of_sexpr var) , tag_lambda args body)
| ScmPair (ScmSymbol "set!", ScmPair (ScmSymbol x,ScmPair(y,ScmNil)))   ->
    ScmSet (ScmVar x,tag_parse_expression y)
|ScmPair(ScmSymbol "begin",body)-> (match body with
     | ScmPair (e1,ScmNil) -> tag_parse_expression e1
     | ScmPair (e1,e2) -> ScmSeq ((tag_parse_expression e1) :: (List.map (fun x->tag_parse_expression x) (scm_list_to_list e2)))
     | c -> tag_parse_expression c )

| ScmPair (x, ScmNil) -> ScmApplic (tag_parse_expression x, [])
| ScmPair(ScmPair(ScmSymbol("lambda"),x),y) ->
    ScmApplic((tag_parse_expression (ScmPair(ScmSymbol("lambda"),x))), (List.map (fun a -> tag_parse_expression a) (scm_list_to_list y)))
| ScmPair(x,y) ->
    ScmApplic(tag_parse_expression x, List.map (fun a -> tag_parse_expression a) (scm_list_to_list y))

| _ -> raise (X_syntax_error (sexpr, "Sexpr structure not recognized"))

and tag_lambda_checker_simple args args2 body list = match args with
| ScmNil -> (tag_lambda_simple args2 body)
| ScmPair(x,y) -> (match (List.exists (fun (a) -> String.equal (string_of_sexpr x) a)  (List.map (fun a -> string_of_sexpr a) (scm_list_to_list list))) with
                  | false -> tag_lambda_checker_simple y args2 body (list_to_proper_list(x::(scm_list_to_list list)))    
                  | true -> raise (X_syntax_error (x , "is already an argument for the lambdaSimple"))
                  )
| _ -> raise (X_syntax_error (args , "arguments with same name (for lambdaSimple)"))    

and tag_lambda_checker_opt args args2 body list = match args with
| ScmNil -> (tag_lambda_opt args2 body)
| ScmPair(x,y) -> (match (List.exists (fun (a) -> String.equal (string_of_sexpr x) a)  (List.map (fun a -> string_of_sexpr a) (scm_list_to_list list))) with
                  | false -> tag_lambda_checker_opt y args2 body (list_to_proper_list(x::(scm_list_to_list list)))    
                  | true -> raise (X_syntax_error (x , "is already an argument for the lambdaOpt"))  
                  )
| ScmSymbol (c) -> (match (List.exists (fun (a) -> String.equal c a)  (List.map (fun a -> string_of_sexpr a) (scm_list_to_list list))) with
                  | false -> tag_lambda_opt args2 body    
                  | true -> raise (X_syntax_error (ScmSymbol (c) , "is already an argument for the lambdaOpt"))  
                  )  
| _ -> raise (X_syntax_error (args , "arguments with same name (for lambdaOpt)"))    

and tag_lambda args body = match scm_is_list(args) with  
  | true ->  tag_lambda_checker_simple args args body ScmNil
  | false ->  tag_lambda_checker_opt args args body ScmNil          
and tag_lambda_simple args body = match body with  
  | ScmNil -> ScmConst(ScmVoid)        
  | ScmPair(x,ScmNil) -> ScmLambdaSimple (( List.map string_of_sexpr (scm_list_to_list args)),
                  tag_parse_expression x)    
  | ScmPair(x,y)-> ScmLambdaSimple (( List.map string_of_sexpr (scm_list_to_list args)),
                             ScmSeq((List.map (fun a -> tag_parse_expression a) (scm_list_to_list body))))                
  | _ -> ScmConst(ScmVoid)    

  and tag_lambda_one_args args body = match body with
 | ScmPair(y,ScmNil) -> ScmLambdaOpt([], string_of_sexpr (ScmSymbol(string_of_sexpr args)), tag_parse_expression y)
 | ScmPair (x,y) -> ScmLambdaOpt([], string_of_sexpr (ScmSymbol(string_of_sexpr args)),
                                    ScmSeq((List.map (fun a -> tag_parse_expression a) (scm_list_to_list body))))
 | c -> ScmConst(ScmVoid)

and tag_lambda_mul_args args body = match body with
            | ScmPair(y,ScmNil) -> ScmLambdaOpt(List.map (fun a -> string_of_sexpr a)
                (scm_list_to_list(scm_without_last (args)))
                  ,(string_of_sexpr (scm_find_last (args))) ,tag_parse_expression y)
           | ScmPair(x,y) -> ScmLambdaOpt(List.map (fun a -> string_of_sexpr a)
                (scm_list_to_list(scm_without_last (args)))
                  ,(string_of_sexpr (scm_find_last (args))) ,ScmSeq((List.map (fun a -> tag_parse_expression a) (scm_list_to_list body))))
           | _ -> ScmConst(ScmVoid)

and tag_lambda_opt args body = match args with    
  | ScmPair(x,y) -> tag_lambda_mul_args args body
  | ScmSymbol(x) -> tag_lambda_one_args args body
  | _ -> ScmConst(ScmVoid)


and macro_expand sexpr =
match sexpr with

| ScmPair(ScmSymbol "and", ScmNil) -> ScmBoolean true
| ScmPair(ScmSymbol "and", ScmPair(x,ScmNil)) -> x
| ScmPair(ScmSymbol "and", ScmPair (x,y)) -> ScmPair(ScmSymbol("if"),(ScmPair(x, ScmPair(macro_and y,ScmPair(ScmBoolean false, ScmNil)))))
| ScmPair(ScmSymbol "and", x) -> x
| ScmPair(ScmSymbol "let", ScmPair(list,body)) -> macro_let list body
| ScmPair(ScmSymbol "let*", ScmPair(list,body)) -> macro_let_star list body
| ScmPair(ScmSymbol "letrec", ScmPair(list,body)) -> macro_letrec list body
| ScmPair(ScmSymbol "quasiquote",ScmPair(sexpr,ScmNil)) -> macro_quasi sexpr
| ScmPair(ScmSymbol "cond", sexpr) -> macro_cond sexpr
| _ -> sexpr

and macro_cond sexpr =
              match sexpr with
              | ScmPair(ScmPair(x,ScmPair(ScmSymbol("=>"),y)),rest) ->
              let list = (ScmPair(ScmPair(ScmSymbol("value"),ScmPair(x,ScmNil)),
                           ScmPair(ScmPair(ScmSymbol("f"),ScmPair(ScmPair(ScmSymbol("lambda"),ScmPair(ScmNil,y)),ScmNil)),
                            ScmPair(ScmPair(ScmSymbol("rest"),ScmPair(ScmPair(ScmSymbol("lambda"),ScmPair(ScmNil,ScmPair((macro_cond rest),ScmNil))),ScmNil)),ScmNil))))in
              let body = ScmPair(ScmPair(ScmSymbol("if"),
                            ScmPair(ScmSymbol("value"),
                              ScmPair(ScmPair(ScmPair(ScmSymbol("f"),ScmNil),ScmPair(ScmSymbol("value"),ScmNil)),
                                ScmPair(ScmPair(ScmSymbol("rest"),ScmNil),ScmNil)))),ScmNil)in
              macro_let list body
              | ScmPair(ScmPair(ScmSymbol "else",ScmPair(x,y)),z) -> ScmPair(ScmSymbol "begin",ScmPair(x,y))
              | ScmPair(ScmPair(x,y),ScmNil) -> ScmPair(ScmSymbol "if", ScmPair(x, ScmPair(ScmPair(ScmSymbol "begin",y), ScmNil)))
              | ScmPair(ScmPair(x,y),z) -> ScmPair(ScmSymbol "if", ScmPair(x, ScmPair(ScmPair(ScmSymbol "begin",y), ScmPair(macro_cond z,ScmNil))))
              | _ -> ScmBoolean false

and macro_quasi sexpr =
              match sexpr with
              | ScmNil -> ScmPair(ScmSymbol"quote", (ScmPair (ScmNil,ScmNil)))
              | ScmSymbol(x) -> ScmPair(ScmSymbol "quote", ScmPair(ScmSymbol x,ScmNil))
              | ScmPair(ScmSymbol("unquote"),ScmPair(x,ScmNil)) -> x
              | ScmPair(ScmSymbol("unquote-splicing"),ScmPair(x,ScmNil)) -> ScmPair(ScmSymbol "quote",ScmPair(ScmPair(ScmSymbol "unquote-splicing",ScmPair (x,ScmNil)),ScmNil))
              | ScmVector (x) -> ScmPair(ScmSymbol "list->vector" ,ScmPair(macro_quasi (list_to_proper_list x),ScmNil))
              | ScmPair(a,b) -> (match a with              
                  | ScmPair(ScmSymbol("unquote-splicing"),ScmPair(rest,ScmNil))
                  -> ScmPair(ScmSymbol "append" ,ScmPair(rest,ScmPair(macro_quasi b,ScmNil)))         
                  | _ -> ScmPair(ScmSymbol "cons" ,ScmPair(macro_quasi a,ScmPair(macro_quasi b,ScmNil))))
              | _ -> ScmPair(ScmSymbol "quote" ,ScmPair (sexpr,ScmNil))  

and macro_letrec list body =
              match list with
              | ScmNil -> macro_let list body
              | ScmPair(x,y) -> let vars = List.map (fun a -> (List.hd (scm_list_to_list a))) (scm_list_to_list list); in
                                let vals = List.map (fun a -> (List.hd (List.tl (scm_list_to_list a)))) (scm_list_to_list list); in
                                let n_list = (List.map (fun a -> ScmPair(a,(ScmPair(ScmPair(ScmSymbol"quote",ScmPair(ScmSymbol "whatever",ScmNil)),ScmNil))) )vars)in
                                let n_body = (scm_zip(fun a b -> ScmPair(ScmSymbol "set!" , ScmPair(a, ScmPair(b,ScmNil)))) (list_to_proper_list vars) (list_to_proper_list vals))in
                                macro_let (list_to_proper_list n_list) (scm_append n_body body)
              | _ -> ScmBoolean false

and macro_let_star list body =
              match list with
              | ScmNil -> macro_let ScmNil body
              | ScmPair(x,ScmNil) -> macro_let list body
              | ScmPair(x,y) -> macro_let (ScmPair(x,ScmNil)) (ScmPair((macro_let_star (y) body),ScmNil))
              | c -> macro_let c body

and macro_let list body =
                let vars = List.map (fun a -> (List.hd (scm_list_to_list a))) (scm_list_to_list list); in
                let vals = List.map (fun a -> (List.hd (List.tl (scm_list_to_list a)))) (scm_list_to_list list); in
                ScmPair(ScmPair(ScmSymbol("lambda"),ScmPair((list_to_proper_list vars), body)) , (list_to_proper_list vals))

and macro_and pair =
match pair with
| ScmPair(x,ScmNil) -> x
| ScmPair(x,y) -> macro_expand(ScmPair(ScmSymbol "and", ScmPair(x,y)))
| c -> ScmNil;;

let rec parse sexpr = string_of_expr (tag_parse_expression sexpr);;

end;;