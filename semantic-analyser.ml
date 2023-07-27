(* semantic-analyser.ml
 * The semantic analysis phase of the compiler
 *
 * Programmer: Mayer Goldberg, 2021
 *)

#use "tag-parser.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type var' =
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | ScmConst' of sexpr
  | ScmVar' of var'
  | ScmBox' of var'
  | ScmBoxGet' of var'  
  | ScmBoxSet' of var' * expr'              
  | ScmIf' of expr' * expr' * expr'
  | ScmSeq' of expr' list
  | ScmSet' of var' * expr'
  | ScmDef' of var' * expr'
  | ScmOr' of expr' list  
  | ScmLambdaSimple' of string list * expr'  
  | ScmLambdaOpt' of string list * string * expr'
  | ScmApplic' of expr' * (expr' list)
  | ScmApplicTP' of expr' * (expr' list);;
 

let var_eq v1 v2 =
match v1, v2 with
  | VarFree (name1), VarFree (name2) -> String.equal name1 name2
  | VarBound (name1, major1, minor1), VarBound (name2, major2, minor2) ->
    major1 = major2 && minor1 = minor2 && (String.equal name1 name2)
  | VarParam (name1, index1), VarParam (name2, index2) ->
       index1 = index2 && (String.equal name1 name2)
  | _ -> false

let list_eq eq l1 l2 = (List.length l1) = (List.length l2) && List.for_all2 eq l1 l2;;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2  
  | ScmVar' (var1), ScmVar' (var2) -> var_eq var1 var2
  | ScmIf' (test1, dit1, dif1), ScmIf' (test2, dit2, dif2) -> (expr'_eq test1 test2) &&
                                            (expr'_eq dit1 dit2) &&
                                              (expr'_eq dif1 dif2)
  | (ScmSeq' (exprs1), ScmSeq' (exprs2) | ScmOr' (exprs1), ScmOr' (exprs2)) ->
       
        list_eq expr'_eq exprs1 exprs2
  | (ScmSet' (var1, val1), ScmSet' (var2, val2) | ScmDef' (var1, val1), ScmDef' (var2, val2)) ->
        (var_eq var1 var2) && (expr'_eq val1 val2)
  | ScmLambdaSimple' (vars1, body1), ScmLambdaSimple' (vars2, body2) ->
   
     (list_eq String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmLambdaOpt' (vars1, var1, body1), ScmLambdaOpt' (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       
       (list_eq String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmApplic' (e1, args1), ScmApplic' (e2, args2) ->
   
     (expr'_eq e1 e2) && (list_eq expr'_eq args1 args2)
  | ScmApplicTP' (e1, args1), ScmApplicTP' (e2, args2) ->
   
      (expr'_eq e1 e2) && (list_eq expr'_eq args1 args2)
  | ScmBox' (v1), ScmBox' (v2) -> var_eq v1 v2
  | ScmBoxGet' (v1), ScmBoxGet' (v2) -> var_eq v1 v2
  | ScmBoxSet' (v1, e1), ScmBoxSet' (v2, e2) -> (var_eq v1 v2) && (expr'_eq e1 e2)
  | _ -> false

;;
module type SEMANTIC_ANALYSIS = sig
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
  val run_semantics : expr -> expr'
  val checking_number :  expr' list ->  expr' list -> int
end;; (* end of module type SEMANTIC_ANALYSIS *)

module Semantic_Analysis : SEMANTIC_ANALYSIS = struct

  let rec lookup_in_rib name = function
    | [] -> None
    | name' :: rib ->
       if name = name'
       then Some(0)
       else (match (lookup_in_rib name rib) with
             | None -> None
             | Some minor -> Some (minor + 1));;

  let rec lookup_in_env name = function
    | [] -> None
    | rib :: env ->
       (match (lookup_in_rib name rib) with
        | None ->
           (match (lookup_in_env name env) with
            | None -> None  
            | Some(major, minor) -> Some(major + 1, minor))
        | Some minor -> Some(0, minor));;

  let tag_lexical_address_for_var name params env =
    match (lookup_in_rib name params) with
    | None ->  
       (match (lookup_in_env name env) with
        | None -> VarFree name
        | Some(major, minor) -> VarBound(name, major, minor))
    | Some minor -> VarParam(name, minor);;

  (* run this first! *)
  let annotate_lexical_addresses pe =
   let rec run pe params env =
      match pe with
      | ScmConst(pe) -> ScmConst'(pe)
      | ScmVar(pe) -> ScmVar'(tag_lexical_address_for_var pe params env)
      | ScmIf(x,y,z) -> ScmIf'((run x params env),(run y params env),(run z params env))
      | ScmSeq(x) -> ScmSeq'(List.map (fun (a)-> (run a params env)) x)
      | ScmSet(ScmVar(x),y) -> ScmSet'(tag_lexical_address_for_var x params env,run y params env)
      | ScmDef(ScmVar(x),y) -> ScmDef'(tag_lexical_address_for_var x params env,run y params env)
      | ScmOr(x) -> ScmOr'(List.map (fun (a)-> (run a params env)) x)
      | ScmLambdaSimple(list,exp) ->  
                          let env1 = params::env in
      ScmLambdaSimple'(list,run exp list env1)        
      | ScmLambdaOpt(list ,s1,expr) ->  
                          let env1 = params::env in      
                          let parm = list@[s1] in  
      ScmLambdaOpt'(list ,s1,run expr parm env1)    
      | ScmApplic(exp,exp_list) -> ScmApplic'(run exp params env,(List.map (fun (a)-> (run a params env)) exp_list))
      |_-> raise X_this_should_not_happen
 
  in
   run pe [] [];;  
           
  let rec rdc_rac s =  
    match s with
    | [e] -> ([], e)
    | e :: s ->  
       let (rdc, rac) = rdc_rac s
       in (e :: rdc, rac)
    | _ -> raise X_this_should_not_happen;;
 
  (* run this second! *)      
  let annotate_tail_calls pe =  
   let rec run pe in_tail =              
   match pe with      
   | ScmIf'(x,y,z) -> ScmIf'(run x false,run y in_tail,run z in_tail)                
   | ScmSeq'(list) -> let (list1,last) = rdc_rac list in      
                      let list1 = (List.map (fun (a) -> run a false) list1) in
                      let last = run last in_tail in  
                      let list1 = list1@[last] in
                      ScmSeq'(list1)        
       
   | ScmSet'(var,expr) ->  ScmSet'(var,run expr false)                              
   | ScmDef'(var,expr) -> ScmDef'(var,run expr false)    
   | ScmOr'(list)-> let (list1,last) = rdc_rac list in              
                      let list1 = (List.map (fun (a) -> run a false) list1) in
                      let last = run last in_tail in  
                      let list1 = list1@[last] in
                      ScmOr'(list1)                                    
   | ScmLambdaSimple'(list,exp) ->  ScmLambdaSimple'(list,run exp true)  
   
   | ScmLambdaOpt'(list,s1,exp) ->  ScmLambdaOpt'(list,s1,run exp true)
   | ScmApplic'(exp,list) -> (match in_tail with    
                      | true ->  let list1 = List.map (fun (l)-> run l false) list in
                                  ScmApplicTP'(run exp false ,list1)    
                      | false -> let list1= (List.map (fun (a)-> run a false) list) in
                                   ScmApplic'(run exp in_tail,list1)   )
   | _ -> pe  
 
   in            
   run pe false;;    
     
  (* boxing *)
  let rec find_write name enclosing_lambda expr bool= match expr with              
    | ScmSet'(x,exp) -> (match x with    
                    | VarParam(y,z) -> if (String.equal y name) then [enclosing_lambda]@(find_write name enclosing_lambda exp bool) else []
                    | VarBound(y,z,a) -> if (String.equal y name) then [enclosing_lambda]@(find_write name enclosing_lambda exp bool) else []
                    | _ -> find_write name enclosing_lambda exp bool  
                    )          
    | ScmDef'(var,x) -> find_write name enclosing_lambda x bool                  
    | ScmIf'(test,dit,dif) -> let test1 = find_write name enclosing_lambda test bool in
                             let dit1 = find_write name enclosing_lambda dit bool in
                             let dif1 = find_write name enclosing_lambda dif bool in
                             test1@dit1@dif1
    | ScmSeq'(list) -> (List.fold_right (fun (l) -> List.append (find_write name enclosing_lambda l bool)) list [] )
    | ScmOr'(list) -> (List.fold_right (fun (l) -> List.append (find_write name enclosing_lambda l bool)) list [] )
    | ScmLambdaSimple'(params,body) -> (match (List.find_opt (fun (p)-> (String.equal name p)) params) with
                                        | None ->
                                            (match bool with
                                            | false -> find_write name expr body true
                                            | true -> find_write name enclosing_lambda body true
                                            )
                                        | _ -> []
                                        )                                      
    | ScmLambdaOpt'(params,param_s,body) -> let unit_params = params@[param_s] in
                                                (match (List.find_opt (fun (p)-> (String.equal name p)) unit_params) with  
                                                  | None ->(match bool with
                                                  | false -> find_write name expr body true
                                                  | true -> find_write name enclosing_lambda body true          
                                                   )
                                                    | _ -> []
                                                      )                                      
    | ScmApplic'(exp,list) -> (find_write name enclosing_lambda exp bool)@
                               (List.fold_right (fun (l) -> List.append (find_write name enclosing_lambda l bool)) list [] )
    | ScmApplicTP'(exp,list) -> (find_write name enclosing_lambda exp bool)@
                               (List.fold_right (fun (l) -> List.append (find_write name enclosing_lambda l bool)) list [] )                                    
    | _ -> []    
   
  let rec find_reads name enclosing_lambda expr bool= match expr with    
    | ScmVar'(x) -> (match x with    
                    | VarParam(y,z) -> if (String.equal y name) then [enclosing_lambda] else []
                    | VarBound(y,z,a) -> if (String.equal y name) then [enclosing_lambda] else []  
                    | _-> []      
                    )                        
    | ScmDef'(var,x) -> find_reads name enclosing_lambda x bool                    
    | ScmIf'(test,dit,dif) -> let test1 = find_reads name enclosing_lambda test bool in
                             let dit1 = find_reads name enclosing_lambda dit bool in
                             let dif1 = find_reads name enclosing_lambda dif bool in
                             test1@dit1@dif1      
    | ScmSeq'(list) -> (List.fold_right (fun (l) -> List.append (find_reads name enclosing_lambda l bool) ) list [] )
    | ScmOr'(list) -> (List.fold_right (fun (l) -> List.append (find_reads name enclosing_lambda l bool) ) list [] )
    | ScmLambdaSimple'(params,body) -> (match (List.find_opt (fun (p)-> (String.equal name p)) params) with
                                        | None ->
                                            (match bool with
                                            | false -> find_reads name expr body true
                                            | true -> find_reads name enclosing_lambda body true
                                            )
                                        | _ -> []
                                        )              
    | ScmLambdaOpt'(params,param_s,body) -> let unit_params = params@[param_s] in
                                                (match (List.find_opt (fun (p)-> (String.equal name p)) unit_params) with  
                                                  | None ->(match bool with
                                                  | false -> find_reads name expr body true
                                                  | true -> find_reads name enclosing_lambda body true          
                                                   )
                                                  | _ -> []
                                                      )
    | ScmSet'(x,y) -> find_reads name enclosing_lambda y bool          
    | ScmApplic'(exp,list) -> (find_reads name enclosing_lambda exp bool)@
                               (List.fold_right (fun (l) -> List.append (find_reads name enclosing_lambda l bool)) list [] )
    | ScmApplicTP'(exp,list) -> (find_reads name enclosing_lambda exp bool)@
                                (List.fold_right (fun (l) -> List.append (find_reads name enclosing_lambda l bool)) list [] )                          
    | _ -> []

    let rec find_param element list =            
    match list with      
    | [] -> 0  
    | h :: t -> if element = h then 0 else 1 + find_param element t
 
   
   let rec box_set expr =   match expr with          
    | ScmLambdaSimple'(params,body) ->
                                box_set_2 params params body
    | ScmLambdaOpt'(params, param_s ,body) ->    
                           box_set_3 params param_s params param_s body        
    | ScmIf'(test,dit,dif) -> ScmIf'(box_set test,box_set dit,box_set dif)
    | ScmSeq'(list) -> ScmSeq'(List.map (fun (l) -> box_set l) list)
    | ScmOr'(list) -> ScmOr'(List.map (fun (l) -> box_set l) list)
    | ScmApplic'(exp,list) -> ScmApplic'(box_set exp,List.map (fun (l) -> box_set l) list)
    | ScmApplicTP'(exp,list) -> ScmApplicTP'(box_set exp,List.map (fun (l) -> box_set l) list)
    | ScmDef'(var,exp) -> ScmDef'(var,box_set exp)
    | ScmSet'(var,exp) -> ScmSet'(var,box_set exp)    

    | _ -> expr    

    and checking_number list1 list2 = match list1 with
    | [] -> 0  
    | hd::tl -> (match (List.exists (fun (l2)->  (not (hd = l2))) list2) with
                  | true -> 1 + checking_number tl list2        
                  | false -> 0 + checking_number tl list2              
    )                
                     
    and box_set_2 params params2 body =   (match params with              
                                      | [] -> ScmLambdaSimple'(params2,box_set body)                
                                      | _ -> let (new_params,last_param) = rdc_rac params in    
                                             let param_read = find_reads last_param (ScmLambdaSimple'(params2,body)) body false in
                                             let param_write = find_write last_param (ScmLambdaSimple'(params2,body)) body false in
                                                 let check_list = checking_number param_read param_write in
                                                 let check_list = (check_list>0) in              
                                             (match check_list with                    
                                                      | true -> box_set_2 new_params params2  
                                                       (do_seq_if_needed (param_template last_param params2) (scm_boxing last_param body))
                                                      | false -> (box_set_2 new_params params2 body)                                              
                                                                      ) )  
    and box_set_3 params p_last params2 p_last2 body =  let all_params =params@[p_last] in
                                                 (match all_params with  
                                      | [""] -> ScmLambdaOpt'(params2,p_last2,box_set body)                                                            
                                      | [] -> ScmLambdaOpt'(params2,p_last2,box_set body)                            
                                      | _ -> let (new_params,last_param) = rdc_rac all_params in    
                                             let param_read = find_reads last_param (ScmLambdaOpt'(params2,p_last2,body)) body false in
                                             let param_write = find_write last_param (ScmLambdaOpt'(params2,p_last2,body)) body false in
                                                 let check_list = checking_number param_read param_write in
                                                 let check_list = (check_list>0) in                    
                                             (match check_list with                                                            
                                                      | true ->    
                                                      (match new_params with
                                                      | [] ->  box_set_3 [] "" params2 p_last2    
                                                       (do_seq_if_needed (param_template last_param (params2@[p_last2])) (scm_boxing last_param body))        
                                                      | _ ->        
                                                       let (new_params_first,new_params_last) = rdc_rac new_params in
                                                      box_set_3 new_params_first new_params_last params2 p_last2    
                                                       (do_seq_if_needed (param_template last_param (params2@[p_last2])) (scm_boxing last_param body)) )
 
                                                      | false ->    
                                                       (match new_params with
                                                       | [] -> (box_set_3 [] "" params2 p_last2 body)  
                                                       | _->
                                                      let (new_params_first,new_params_last) = rdc_rac new_params in
                                                      (box_set_3 new_params_first new_params_last params2 p_last2 body)                                                  
                                                                      )))                                                                                      
    and do_seq_if_needed sets body= match body with                      
                            | ScmSeq'(rest)-> ScmSeq'([sets]@rest)  
                            | _ -> ScmSeq'([sets]@[body])                                                                            
    and param_template var list = let my_index = find_param var list in              
                                    ScmSet'(VarParam (var, my_index),ScmBox'(VarParam (var,my_index)))
   
           
    and scm_boxing name expr = match expr with                  
    | ScmSet'(VarParam (_, _),ScmBox'(VarParam (_,_))) -> expr                                
    | ScmVar'(VarParam (x,y)) -> if (String.equal name x) then ScmBoxGet'(VarParam (name,y)) else expr  
    | ScmVar'(VarBound (x, y, z)) -> if (String.equal name x) then ScmBoxGet'(VarBound (name, y, z)) else expr        
    | ScmSet'(VarParam (x,y),z) -> if (String.equal name x) then ScmBoxSet'(VarParam (name,y),scm_boxing name z)else ScmSet'(VarParam (x,y),scm_boxing name z)
    | ScmSet'(VarBound (x,y,z),w) -> if (String.equal name x) then ScmBoxSet'(VarBound (name,y,z),scm_boxing name w) else ScmSet'(VarBound (x,y,z),scm_boxing name w)
    | ScmSet'(VarFree (x),w) -> ScmSet'(VarFree (x) , scm_boxing name w)
    | ScmDef'(var,exp)-> ScmDef'(var,scm_boxing name exp)  
    | ScmLambdaSimple'(params,body) -> (match (List.find_opt (fun (p)-> (String.equal name p)) params) with
                                      | None -> ScmLambdaSimple'(params,scm_boxing name body)  
                                      | _ -> ScmLambdaSimple'(params,body))
    | ScmLambdaOpt'(params_first,p_last,body) -> let params = params_first@[p_last] in
                                          (match (List.find_opt (fun (p)-> (String.equal name p)) params) with
                                      | None -> ScmLambdaOpt'(params_first,p_last,scm_boxing name body)  
                                      | _ -> ScmLambdaOpt'(params_first,p_last,body))                                                  
    | ScmSeq'(list) -> ScmSeq'(List.map (fun (l)-> scm_boxing name l) list)                    
    | ScmOr'(list) -> ScmOr'(List.map (fun (l)-> scm_boxing name l) list)    
    | ScmIf'(test, dit, dif) -> ScmIf'(scm_boxing name test, scm_boxing name dit, scm_boxing name dif)
    | ScmApplic'(ex, list) -> ScmApplic'(scm_boxing name ex, (List.map (fun a -> scm_boxing name a) list))
    | ScmApplicTP' (ex, list) -> ScmApplicTP'(scm_boxing name ex, (List.map (fun a -> scm_boxing name a) list))
    | _ -> expr  
 
  let run_semantics expr =  
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr))

end;; (* end of module Semantic_Analysis *)