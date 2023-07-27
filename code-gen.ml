#use "semantic-analyser.ml";;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (sexpr * (int * string)) list
  val do_consts_tbl : expr' -> sexpr list
  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list
  val do_fvars_tbl : expr' -> string list  
  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second
     argument is the fvars table type, and the third is an expr' that has been annotated
     by the semantic analyser.
   *)
  val generate : (sexpr * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

  let rec rdc_rac s =  
    match s with
    | [e] -> ([], e)
    | e :: s ->  
       let (rdc, rac) = rdc_rac s
       in (e :: rdc, rac)
    | _ -> raise X_this_should_not_happen;;

  let rec do_consts_tbl ast = (match ast with
        | ScmConst'(x) -> (match x with  
                        | ScmPair(car,cdr) -> (match car with  
                                  | ScmSymbol(y) -> [ScmString(y)] @ [ScmSymbol(y)] @ (do_consts_tbl (ScmConst'(cdr)))@[x]
                                  | _-> (do_consts_tbl (ScmConst'(car)))@ (do_consts_tbl (ScmConst'(cdr)))@[x]
                        )                
                        | ScmVector(list) -> (List.fold_right (fun (l) -> List.append (do_consts_tbl (ScmConst'(l)))) list [] )@[x]
                        | ScmSymbol(y) -> [ScmString(y)]@[ScmSymbol(y)]
                        | _-> [x]    
        )  
        | ScmVar'(var) -> []  (** *)  
        | ScmBoxSet' (var,expr) -> do_consts_tbl expr
        | ScmIf'(test,dit,dif)-> do_consts_tbl test @ do_consts_tbl dit @ do_consts_tbl dif
        | ScmSeq'(list) -> (List.fold_right (fun (l) -> List.append (do_consts_tbl l) ) list [] )
        | ScmSet'(var,expr) -> do_consts_tbl expr
        | ScmDef'(var,expr) -> do_consts_tbl expr
        | ScmOr'(list) -> (List.fold_right (fun (l) -> List.append (do_consts_tbl l) ) list [] )
        | ScmLambdaSimple'(params,body) -> do_consts_tbl body
        | ScmLambdaOpt'(params,parm,body) -> do_consts_tbl body
        | ScmApplic'(exp,list) -> do_consts_tbl exp @ (List.fold_right (fun (l) -> List.append (do_consts_tbl l) ) list [] )
        | ScmApplicTP'(exp,list) -> do_consts_tbl exp @ (List.fold_right (fun (l) -> List.append (do_consts_tbl l) ) list [] )
        | _ -> []
  )

  let sexpr_to_type sexpr list = match sexpr with
        | ScmVoid -> "MAKE_VOID"
        | ScmNil -> "MAKE_NIL"
        | ScmBoolean(x) ->(match x with | false -> "MAKE_BOOL(0)" | true -> "MAKE_BOOL(1)")
        | ScmChar(x) -> Printf.sprintf "MAKE_LITERAL_CHAR(%s)" (string_of_int (Char.code x))    
        | ScmString(x) -> Printf.sprintf "MAKE_LITERAL_STRING(%S)" x
        | ScmSymbol(x) -> let findsym = List.assoc_opt (ScmString(x)) list in  
                          let findsym = (match findsym with
                              | None -> raise X_this_should_not_happen
                              | Some(c) -> c
                          ) in                  
        (Printf.sprintf "MAKE_LITERAL_SYMBOL(const_tbl + %d)" findsym)    
        | ScmNumber(x) -> (match x with    
                        | ScmRational(i,j) -> (Printf.sprintf "MAKE_LITERAL_RATIONAL(%d,%d) " i j)
                        | ScmReal(i) -> (Printf.sprintf "MAKE_LITERAL_REAL(%F)" i)     )
        | ScmVector(x) ->          
                                let y =List.map (fun (first) ->
                                  let find_car = List.assoc_opt first list in
                                let find_car = (match find_car with  
                                      | None -> raise X_this_should_not_happen
                                      | Some(c) -> c  
                                                  ) in
                                                  find_car
                                                  ) x in
                                let toapp = (List.fold_left (fun init first->  init ^ "(const_tbl+" ^ (string_of_int first) ^ "), ") "" y) in
                                let toapp = (match (String.length toapp) with
                                        | 0 -> ""
                                        | _->  String.sub toapp 0 ((String.length toapp) -2)
                                ) in
                        (Printf.sprintf "MAKE_LITERAL_VECTOR %s " toapp)  
        | ScmPair(car,cdr) ->  
                      let find_car = List.assoc_opt car list in
                      let find_car = (match find_car with  
                        | None -> raise X_this_should_not_happen
                        | Some(c) -> c
                      ) in
                      let find_cdr = List.assoc_opt cdr list in
                      let find_cdr = (match find_cdr with  
                        | None -> raise X_this_should_not_happen
                        | Some(c) -> c
                      ) in
                       (Printf.sprintf "MAKE_LITERAL_PAIR(const_tbl + %d,const_tbl + %d)" find_car find_cdr)

  let rec list_size list = match list with  
      | [] -> 0
      | hd::tl -> 1 + (list_size tl)

  let myindex sexpr = match sexpr with
        | ScmVoid -> 1  
        | ScmNil -> 1  
        | ScmBoolean(x) -> 1 + 1
        | ScmChar(x) -> 1 + 1
        | ScmString(x) -> 1 + 8 + (String.length x)
        | ScmSymbol(x) -> 1 +8
        | ScmNumber(x) -> (match x with
                        | ScmRational(i,j) ->  1 +8 +8
                        | ScmReal(i) -> 1 + 8)      
        | ScmVector(x) -> 1 + 8 + ((List.length x)*8)  
        | ScmPair(car,cdr) -> 1 + 8 + 8  

let rec filter_list_from_listlist list final_list =
            (match list with
              | [] -> final_list
              | hd::tl ->  let find_l = (List.find_opt (fun l -> hd = l )final_list) in
                            (match find_l with
                                    | None -> (filter_list_from_listlist (List.tl list) (final_list@[(List.hd list)]) )
                                    | _ -> (filter_list_from_listlist (List.tl list) final_list)
                              )
            )

let rec sum_indexs list index= match list with
| [] -> list  
| (e1,i1)::(e2,i2)::tl -> (e1,index)::(sum_indexs ((e2,i2)::tl) (i1+index))
| [(e,i)] -> [(e,index)]            

let make_consts_tbl asts =
                  let mylist= List.map (fun (ast)-> (do_consts_tbl ast)) asts in
                  let mylist = List.concat mylist in
                  let to_add = [ScmVoid;ScmNil;ScmBoolean(false);ScmBoolean(true)] in
                  let mylist = to_add@mylist in
                  let final_list = filter_list_from_listlist mylist [] in
                  let final_list = List.map (fun (l) -> (l,myindex l)) final_list in
                  let final_list = sum_indexs final_list 0 in      
                  let old_list = final_list in
                  let final_list1 = List.map (fun (l,index) -> (l,(index,sexpr_to_type l old_list))) final_list in
                  final_list1          

let make_var var' = match var' with  
        | VarFree(v) -> [v]
        | VarParam(v,i) -> []
        | VarBound(v,i,j) -> []
       
let rec do_fvars_tbl ast = (match ast with
        | ScmConst'(x) -> []
        | ScmVar'(var) -> (match var with  
                          | VarFree(x) -> [x]
                          | _ -> []    )
        | ScmBoxSet' (var,expr) -> do_fvars_tbl (ScmVar'(var)) @ do_fvars_tbl expr
        | ScmIf'(test,dit,dif)-> do_fvars_tbl test @ do_fvars_tbl dit @ do_fvars_tbl dif
        | ScmSeq'(list) -> (List.fold_right (fun (l) -> List.append (do_fvars_tbl l) ) list [] )
        | ScmSet'(var,expr) -> do_fvars_tbl (ScmVar'(var)) @ do_fvars_tbl expr
        | ScmDef'(var,expr) -> do_fvars_tbl (ScmVar'(var)) @ do_fvars_tbl expr
        | ScmOr'(list) -> (List.fold_right (fun (l) -> List.append (do_fvars_tbl l) ) list [] )
        | ScmLambdaSimple'(params,body) -> do_fvars_tbl body
        | ScmLambdaOpt'(params,parm,body) -> do_fvars_tbl body
        | ScmApplic'(exp,list) -> do_fvars_tbl exp @ (List.fold_right (fun (l) -> List.append (do_fvars_tbl l) ) list [] )
        | ScmApplicTP'(exp,list) -> do_fvars_tbl exp @ (List.fold_right (fun (l) -> List.append (do_fvars_tbl l) ) list [] )
        | _ -> []
  )

  let make_fvars_tbl asts =  
                  let mylist= List.map (fun (ast)-> (do_fvars_tbl ast)) asts in
                  let mylist = List.concat mylist in
                  let to_add = [
    (* Type queries  *)
    "boolean?"; "flonum?"; "rational?";
    "pair?"; "null?"; "char?"; "string?";
    "procedure?"; "symbol?";
    (* String procedures *)
    "string-length"; "string-ref"; "string-set!";
    "make-string"; "symbol->string";
    (* Type conversions *)
    "char->integer"; "integer->char"; "exact->inexact";
    (* Identity test *)
    "eq?";
    (* Arithmetic ops *)
    "+"; "*"; "/"; "="; "<";
    (* Additional rational numebr ops *)
    "numerator"; "denominator"; "gcd";
    "car";"cdr";"cons"; "set-car!"; "set-cdr!"; "apply" ;
    (* you can add yours here *)
  ]  in    
                  let mylist = to_add@mylist in
                  let final_list = filter_list_from_listlist mylist [] in
                  let x = List.map (fun (l) -> (l,1)) final_list in
                  let x = sum_indexs x 0 in
                     x  
 




  let rec insert_params index=  (** rcx is arg_count  , rbx is where to insert *)
  match index with
  | 0 -> ""
  | _->
Printf.sprintf
"mov rdx , qword [rbp + 8*(3+index)]
mov qword [rbx+((index-1)*8)],rdx  
" ^ insert_params (index-1)
 
  let counter = ref 0

  let generate consts_tbl fvars_tbl e =
  let rec run consts_tbl fvars_tbl e env_count =
  (match e with    
      | ScmConst'(c) ->        
              let const_row = List.find_opt (fun (const,( _, _)) -> sexpr_eq const c) consts_tbl in
              let const_row =  (match const_row with | None -> raise X_this_should_not_happen | Some(c)  -> c ) in
              let offset = (fun (_,(off, _)) -> off) const_row in
                  (Printf.sprintf "mov rax, const_tbl+%d \n" offset)  
 
      | ScmVar'(c) -> (match c with  
                        | VarFree(x) -> let fvar_row = List.find_opt (fun (fvar,_) -> String.equal fvar x) fvars_tbl in
                                         let fvar_row =  (match fvar_row with | None -> raise X_this_should_not_happen | Some(d)  -> d ) in
                                         let offset = (fun (_,off) -> off) fvar_row in
                                                (Printf.sprintf " mov rax, FVAR(%d) \n" (offset))
                        | VarParam(_,minor) -> (Printf.sprintf " mov rax, qword [rbp + 8 * (4 + %d)] \n" minor )
                        | VarBound(_ ,major ,minor) ->
(Printf.sprintf "mov rax, qword [rbp + 8 * 2]  
mov rax, qword [rax + 8 * %d]    
mov rax, qword [rax + 8 * %d] \n" major minor)  )  
                         
      | ScmSet'(VarFree(v),x) -> let x_string = run consts_tbl fvars_tbl x env_count in  
                                  let fvar_row = List.find_opt (fun (fvar,_) -> String.equal fvar v) fvars_tbl in
                                  let fvar_row =  (match fvar_row with | None -> raise X_this_should_not_happen | Some(d)  -> d ) in
                                  let offset = (fun (_,off) -> off) fvar_row in
                                        (Printf.sprintf "%s mov qword [fvar_tbl +%d], rax \n mov rax, SOB_VOID_ADDRESS\n" x_string (offset*8))
      | ScmSet'((VarParam(_,minor)),x) ->  
              run consts_tbl fvars_tbl x env_count ^ (Printf.sprintf " mov qword [rbp + 8 * (4 + %d)], rax \n mov rax, SOB_VOID_ADDRESS \n"  minor   )
      | ScmSet'(VarBound(_,major,minor),x) ->  
              run consts_tbl fvars_tbl x env_count ^ (Printf.sprintf " mov rbx, qword [rbp + 8 * 2] \n mov rbx, qword [rbx + 8 * %d] \n mov qword [rbx + 8 * %d], rax \n mov rax, SOB_VOID_ADDRESS \n" major minor   )    
       
      |ScmBox'(VarParam(v,minor)) ->
      (Printf.sprintf "%s
       MALLOC rbx, 8 \n
        mov qword [rbx], rax \n
        mov rax, rbx
        "
        (run consts_tbl fvars_tbl (ScmVar'(VarParam(v,minor))) env_count))

      | ScmBoxGet'(var) -> let var_string = (run consts_tbl fvars_tbl (ScmVar'(var)) env_count) in
                                   (Printf.sprintf " %s mov rax, qword [rax] \n" var_string)
      | ScmBoxSet'(var,exp) -> let exp_string = run consts_tbl fvars_tbl exp env_count in
                               let var_string = run consts_tbl fvars_tbl (ScmVar'(var)) env_count in
                               (Printf.sprintf " %s push rax \n %s pop qword [rax] \n mov rax, SOB_VOID_ADDRESS \n" exp_string var_string)
      | ScmDef'(VarFree(var),exp) ->    let x_string = run consts_tbl fvars_tbl exp env_count in  
                               let fvar_row = List.find_opt (fun (fvar,_) -> String.equal fvar var) fvars_tbl in
                               let fvar_row =  (match fvar_row with | None -> raise X_this_should_not_happen | Some(d)  -> d ) in
                               let offset = (fun (_,off) -> off) fvar_row in
                                        (Printf.sprintf "%s mov qword [fvar_tbl +%d], rax \n mov rax ,SOB_VOID_ADDRESS" x_string (offset*8))
               
      | ScmSeq'(list) -> List.fold_right (fun first init-> (run consts_tbl fvars_tbl first env_count) ^ init) list ""            
      | ScmOr'(list) ->  (counter := 1+ !counter);    
                          let mycounter = !counter in            
                     (match list with  
                                 | [] -> ""      
                                 | _ -> let jumps = (Printf.sprintf " cmp rax, SOB_FALSE_ADDRESS \n jne Lexit%d \n" mycounter)in    
                                       
                                        let (all,last) = rdc_rac list in    
                                        let toprint = List.fold_right (fun first init -> (run consts_tbl fvars_tbl first env_count) ^ jumps ^ init ) all ""  in
                                          (Printf.sprintf " %s %s Lexit%d:\n" toprint (run consts_tbl fvars_tbl last env_count) mycounter))

      | ScmIf'(test,dit,dif) -> (counter := 1+ !counter);  
                                let mycounter = !counter in
                                let test_string = run consts_tbl fvars_tbl test env_count in    
                                let dit_string =  run consts_tbl fvars_tbl dit env_count in
                                let dif_string =  run consts_tbl fvars_tbl dif env_count in  
                          (Printf.sprintf " %s cmp rax, SOB_FALSE_ADDRESS \n je Lelse%d \n %s jmp Lexit%d \n Lelse%d: \n %s Lexit%d: \n" test_string mycounter dit_string mycounter mycounter dif_string mycounter)
      | ScmLambdaSimple'(params,body) -> (counter := 1+ !counter);  
                                         let mycounter = !counter in
                                         let ext_env = env_count + 1 in  
                                          (Printf.sprintf  
"mov rcx, %d  ;;;mov rcx env_count
cmp rcx, 0    
jz my_continue%d    ;;; jump only if this is the first lambda  
mov rbx, %d* WORD_SIZE  ;;;mov rbx ext_env* 8
MALLOC rbx, rbx    ;;; creating new env with size as last env + 1   , pointer to new empty env with right size
push rbx       ;;;push rbx so we save him before jmp

mov rdx, qword [rbp +2*8]     ;;; rdx is pointer to last env
add rbx, 8          ;;; save that first place in the env for new params , to start from second position
copy_env%d:  
    mov rsi, qword [rdx]
    mov qword[rbx], rsi      ;;; copy the %d env
    add rbx, 8        ;;; increase the copy
    add rdx, 8    
    loop copy_env%d     ;;; do it rcx times , that is env_count times  
justcheck%d:  
pop rbx     ;;;pointer to the new_env
push rbx

mov rdx,qword [rbp + 8*3]
lea rdx, [rdx*8]     ;;; mov rdx,rdx*8
MALLOC rbx, rdx;;; allocate args_count from the last lambda
mov rcx, qword [rbp + 8*3] ;;; arg_count from last lambda
cmp rcx, 0
jz end_insert_params%d
insert_params%d:
mov rdx , qword [rbp + 8*(3+rcx)]
mov qword [rbx+((rcx-1)*8)],rdx  
  loop insert_params%d

end_insert_params%d:
mov rdx,rbx
pop rbx          
mov qword[rbx],rdx
jmp finished%d      
my_continue%d:
MALLOC rbx, 8 ;;; rbx point to new place
mov qword [rbx], SOB_NIL_ADDRESS
 
finished%d:  
"  env_count mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter )
^ (Printf.sprintf "
MAKE_CLOSURE (rax, rbx, Lcode%d)
jmp Lcont%d
Lcode%d:  
push rbp
mov rbp , rsp      
%s
leave
ret  
Lcont%d:
"   mycounter mycounter mycounter (run consts_tbl fvars_tbl body ext_env) mycounter
)


      | ScmLambdaOpt'(params,opt,body) -> (counter := 1+ !counter);    
                                         let mycounter = !counter in
                                         let ext_env = env_count + 1 in  
                                         let all_params_size = (List.length params) + 1   in
                                          (Printf.sprintf  
"        
mov rcx, %d  ;;;mov rcx env_count
cmp rcx, 0    
jz my_continue%d    ;;; jump only if this is the first lambda  
mov rbx, %d* WORD_SIZE  ;;;mov rbx ext_env* 8
MALLOC rbx, rbx    ;;; creating new env with size as last env + 1   , pointer to new empty env with right size
push rbx       ;;;push rbx so we save him before jmp
mov rdx, qword [rbp +2*8]     ;;; rdx is pointer to last env
add rbx, 8          ;;; save that first place in the env for new params , to start from second position
copy_env%d:  
    mov rsi, qword [rdx]
    mov qword[rbx], rsi      ;;; copy the %d env
    add rbx, 8        ;;; increase the copy
    add rdx, 8    
    loop copy_env%d     ;;; do it rcx times , that is env_count times  
justcheck%d:  
pop rbx     ;;;pointer to the new_env
push rbx
mov rdx,qword [rbp + 8*3]
lea rdx, [rdx*8]     ;;; mov rdx,rdx*8
MALLOC rbx, rdx;;; allocate args_count from the last lambda
mov rcx, qword [rbp + 8*3] ;;; arg_count from last lambda
cmp rcx, 0
jz end_insert_params%d
insert_params%d:
mov rdx , qword [rbp + 8*(3+rcx)]
mov qword [rbx+((rcx-1)*8)],rdx  
  loop insert_params%d
end_insert_params%d:
mov rdx,rbx
pop rbx          
mov qword[rbx],rdx
jmp finished%d      
my_continue%d:
MALLOC rbx, 8 ;;; rbx point to new place
mov qword [rbx], SOB_NIL_ADDRESS
finished%d:  
"  env_count mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter mycounter )
^ (Printf.sprintf "
MAKE_CLOSURE (rax, rbx, Lcode%d)
jmp Lcont%d
Lcode%d:  
push rbp  
mov rbp , rsp  
mov rcx, qword [rbp + 8*3]     ;;;arg_count 7
cmp rcx, %d      ;;; allparams_size 4
JNG finishing%d
big_frame_case%d:
mov rcx, qword [rbp + 8*3]     ;;;arg_count 7
sub rcx, %d   ;;;rcx=3    7-4=3 ;;;arg_count - allparamsize
mov rax, qword [rbp + 8*(%d+rcx +3)]           ;;cdr
start_listing%d:  
mov rbx, qword [rbp + 8*(%d-1+rcx +3)]         ;;; car    
mov rdx, rax
MAKE_PAIR (rax, rbx , rdx)
  loop start_listing%d  
mov qword [rbp + 8*(%d+3)] , rax        ;;; inserting the list
mov rdx , %d +1      ;;;idk if +1 OR not
 ;;;mov qword [rbp+ 8*3] , rdx   ;;;change arg_count to param_size
finishing%d:
%s
leave
ret  
Lcont%d:  
"   mycounter mycounter mycounter all_params_size  mycounter mycounter
all_params_size all_params_size mycounter all_params_size mycounter all_params_size all_params_size  mycounter (run consts_tbl fvars_tbl body ext_env) mycounter
)
 
      | ScmApplic'(proc,args) ->  let addMagic = Printf.sprintf "mov rax, SOB_NIL_ADDRESS \n push rax \n" in
                                  let rec make_arg args = (match args with
                                                        | [] -> ""
                                                        | hd::tl -> let (first, last) = rdc_rac args in
                                                      (Printf.sprintf "%s push rax\n"
                                                       (run consts_tbl fvars_tbl last env_count)  ^ make_arg first)) in
                                 let arg = make_arg args in  
                                 let num_of_args = (List.length args) +1 in
                                 let compare =
(*"cmp al, T_CLOSURE
jnz lambda_error_1"*)
" DO_THE_APPLIC  
"in

                      (addMagic ^ arg^     (Printf.sprintf "push %d\n %s %s" num_of_args (run consts_tbl fvars_tbl proc env_count) compare) )



       | ScmApplicTP'(proc,args) ->  let addMagic = Printf.sprintf "mov rax, SOB_NIL_ADDRESS \n push rax \n" in
                                  let rec make_arg args = (match args with
                                                        | [] -> ""
                                                        | hd::tl -> let (first, last) = rdc_rac args in
                                                      (Printf.sprintf "%s push rax\n   "
                                                       (run consts_tbl fvars_tbl last env_count)  ^ make_arg first)) in
                                 let arg = make_arg args in  
                                 let num_of_args = (List.length args) +1 in
                                 let compare = Printf.sprintf
"ScmApplicTP_start    ;;ScmApplicTP_start
 SHIFT_FRAME %d           ;;;copying to old frame 
 lea rsp, [rbp +8*(rdx - %d)]   ;;; adjust the rsp to point to start of frame 
 ScmApplicTP_end     ;;ScmApplicTP_end 
" (num_of_args+4) num_of_args in

                      (addMagic ^ arg^     (Printf.sprintf "push %d\n %s" num_of_args (run consts_tbl fvars_tbl proc env_count)) ^ compare )


      | _ ->"should_not_get_here!"    
                   
  ) in run consts_tbl fvars_tbl e 0
   


 
end;;
