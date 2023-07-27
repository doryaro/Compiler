(* reader.ml
 * A skeleton for the reader for the 2021-2022 course on compiler-construction
 *)

#use "pc.ml";;

let rec gcd a b =
  match (a, b) with
  | (0, b) -> b
  | (a, 0) -> a
  | (a, b) -> gcd b (a mod b);;

type scm_number =
  | ScmRational of (int * int)
  | ScmReal of float;;

type sexpr =
  | ScmVoid    
  | ScmNil
  | ScmBoolean of bool
  | ScmChar of char
  | ScmString of string
  | ScmSymbol of string
  | ScmNumber of scm_number
  | ScmVector of (sexpr list)
  | ScmPair of (sexpr * sexpr);;



module type READER = sig
    val nt_sexpr : sexpr PC.parser
end;; (* end of READER signature *)
   
module Reader : READER = struct
open PC;;

let unitify nt = pack nt (fun _ -> ());;
let digit = range '0' '9';;

let list_of_strings_to_string s =
  List.fold_left
    (fun str str2 -> str^str2)
    ""  
    s;;


let rec nt_whitespace str =
  const (fun ch -> ch <= ' ') str
and nt_end_of_line_or_file str =
   let nt1 = unitify (char '\n') in
   let nt2 = unitify nt_end_of_input in
   let nt1 = disj nt1 nt2 in
   nt1 str

and nt_line_comment str =
    let nt_end = disj (unitify (char('\n'))) (unitify (nt_end_of_input)) in
    let nt1 = char (';') in
    let nt2 = diff nt_any nt_end in
    let nt2 = star nt2 in
    let nt1 = caten nt1 (caten nt2 nt_end) in
    let nt1 = unitify nt1 in
    nt1 str

and nt_paired_comment str =
    let nt1 = unitify (caten (word "#;") nt_sexpr) in
    nt1 str

and nt_sexpr_comment str =
    let nt1 = char '{' in
    let nt2 = disj_list [
                  unitify nt_char;
                  unitify nt_string;
                  nt_comment
                ] in
    let nt2' = disj nt2 (unitify (one_of "{}")) in
    let nt3 = diff nt_any nt2' in  
    let nt3 = disj (unitify nt3) nt2 in  
    let nt3 = star nt3 in
    let nt4 = char '}' in
    let nt1 = unitify (caten nt1 (caten nt3 nt4)) in
    nt1 str  


and nt_comment str =  
  disj_list
    [nt_line_comment;
    nt_sexpr_comment;  
     nt_paired_comment  
     ] str  

and nt_skip_star str =
  let nt1 = disj (unitify nt_whitespace) nt_comment in
  let nt1 = unitify (star nt1) in
  nt1 str
and make_skipped_star (nt : 'a parser) =    
  let nt1 = caten nt_skip_star (caten nt nt_skip_star) in
  let nt1 = pack nt1 (fun (_, (e, _)) -> e) in
  nt1    

and nt_int str =  
    let nt_sign = disj (char '+') (char '-') in  
    let nt_0 = plus (char ('0')) in
    let nt_0 = pack nt_0 (fun(l) -> '0') in
    let nt_zeros = pack nt_0 (fun (l) -> 0) in
    let digits_ = (caten (range '1' '9') (star (range '0' '9'))) in
    let digits_ = pack digits_ (fun (l,p) ->  int_of_string((String.make 1 l)^(list_to_string p))) in
    let nt1 = caten (star nt_0) digits_ in
    let nt1 = pack nt1 (fun (z,l) -> l)in
    let res = disj nt1 nt_zeros in
    let res2 = caten nt_sign res in
    let res2 = pack res2 (fun (l,p) -> if (l=='-') then (-p) else (p)) in
    let res3 = disj res2 res in
    res3 str

and nt_integer_part str =
    let nt_0 = plus (char('0')) in
    let nt_0 = pack nt_0 (fun(l) -> '0') in
    let nt_zeros = pack nt_0 (fun (l) -> 0) in
    let digits_ = (caten (range '1' '9') (star (range '0' '9'))) in
    let digits_ = pack digits_ (fun (l,p) ->  int_of_string((String.make 1 l)^(list_to_string p))) in
    let nt1 = caten (star nt_0) digits_ in
    let nt1 = pack nt1 (fun (z,l) -> l)in
    let res = disj nt1 nt_zeros in  
    res str
 
and nt_notZero str =

    let digits_ = (caten (range '1' '9') (star (range '0' '9'))) in
    let digits_ = pack digits_ (fun (l,p) ->  int_of_string((String.make 1 l)^(list_to_string p))) in
    digits_ str    


and nt_frac str =  
    let nt3= caten (caten nt_int ((char('/')))) (only_if nt_integer_part (fun n -> n!= 0))   in
    let nt3 = pack nt3 (fun ((l,p),r)-> let x = gcd(abs (l))(r) in ScmRational(l/x,r/x)) in
    nt3 str


and nt_mantissa str = nt_integer_part str
   
and nt_exponent_token str =
    let nt1 = char_ci('e') in
    let nt1 = pack nt1 (fun (p) -> (String.make 1 p)) in
    let nt2 = word("*10^") in
    let nt2 = pack nt2 (fun (p) -> list_to_string p) in
    let nt3 = word("*10**") in
    let nt3 = pack nt3 (fun (p) -> list_to_string p) in
    let res = disj_list [nt1;nt2;nt3] in
    res str
   
   
and nt_exponent str =
    let nt1 = caten nt_exponent_token nt_int in
    let nt1 = pack nt1 (fun (l,p) -> p) in  
    nt1 str    
 
and nt_float_A str =
    let nt1 = nt_integer_part in
    let nt1 = pack nt1 (fun (p) -> string_of_int p) in
    let point = char('.') in    
    let point = pack point (fun (p) -> String.make 1 p) in
    let nt2 = maybe nt_mantissa in
    let nt2 = pack nt2 (fun (p) -> match p with  |None -> "" |Some(c)-> string_of_int c) in
    let nt3 = maybe nt_exponent in
    let nt3 = pack nt3 (fun (p) -> match p with  |None -> "" |Some(c)-> "e"^string_of_int c) in
    let res = caten (caten (caten nt1 point)nt2) nt3 in
    let res = pack res (fun (((l,t),p),r) -> Float.of_string (l^t^p^r)) in
    let res = pack res (fun (l) -> l) in
    res str
     
and nt_float_B str =          
    let point = char ('.') in
    let point = pack point (fun (p) -> String.make 1 p) in
    let nt1 = pack nt_mantissa (fun (p) -> string_of_int p) in  
    let nt2 = maybe nt_exponent in
    let nt2 = pack nt2 (fun (p) -> match p with | None -> "" |Some(c)  -> (string_of_int c)) in
    let res = caten (caten point nt1 ) nt2 in  
    let res = pack res (fun ((p,l),r) -> match r with | "" -> Float.of_string (p^l)  | r  -> Float.of_string (p^l^"e"^r)) in
    let res = pack res (fun (l) -> l) in
    res str
and nt_float_C str =  
    let nt1 = pack nt_integer_part (fun (p) -> string_of_int p) in
    let res = caten nt1 nt_exponent in
    let res = pack res (fun (l,p) -> Float.of_string (l^"e" ^(string_of_int p))) in
    let res = pack res (fun (l) -> l) in
    res str    


and nt_floatS str =
    let nt_sign = disj (char '+') (char '-') in  
    let nt1 = disj_list [nt_float_A ; nt_float_B; nt_float_C] in
    let res = caten nt_sign nt1 in
    let res = pack res (fun (l,p) -> if(l=='-') then ScmReal(Float.neg(p)) else ScmReal(p)) in
    res str    

and nt_floatNS str =
    let nt1 = disj_list [nt_float_A ; nt_float_B; nt_float_C] in
    let nt1 = pack nt1 (fun p ->(ScmReal(p))) in
    nt1 str      

and nt_float str =            
  let nt1 = disj nt_floatS nt_floatNS in
  let nt1 = pack nt1 (fun n -> n)in
  nt1 str
   
and nt_number str =
  let nt1 = nt_float in                        
  let nt2 = nt_frac in
  let nt3 = pack nt_int (fun n -> ScmRational(n, 1)) in
  let nt1 = disj nt1 (disj nt2 nt3) in
  let nt1 = pack nt1 (fun r -> ScmNumber r) in
  let nt1 = not_followed_by nt1 nt_symbol_char in
  nt1 str  

and nt_boolean str =
    let nt1 = word_ci "#f" in
    let nt1 = pack nt1 (fun (_) -> false) in
    let nt2 = word_ci "#t" in
    let nt2 = pack nt2 (fun (_) -> true) in
    let res = disj nt1 nt2 in
    let res = pack res (fun (b) -> ScmBoolean b) in
    res str

and nt_char_simple str =
  let nt1 = range '!' '~' in  
  let nt1 = caten (word "#\\") nt1 in
  let nt1 = pack nt1 (fun (p,c) -> c)in
  nt1 str
     
and nt_char_named str =     
  let nt1 = disj_list
         [pack (word_ci "#\\space") (fun _ -> ' ');
          pack (word_ci "#\\tab") (fun _ -> '\t');
          pack (word_ci "#\\newline") (fun _ -> '\n');  
          pack (word_ci "#\\page") (fun _ -> '\012');
          pack (word_ci "#\\return") (fun _ -> '\r');
          pack (word_ci "#\\nul") (fun _ -> '\x00')]in         
          nt1 str

and nt_char_hex str =
  let nt2 = range '0' '9' in
  let nt2 = pack nt2 (fun (ch) -> let delta = int_of_char '0' in
                                  (int_of_char ch) - delta ) in
  let nt3 = range 'a' 'f' in
  let nt3 = pack nt3 (fun (ch) -> let delta = (int_of_char 'a') - 10 in
                                  (int_of_char ch) - delta) in
  let nt2 = disj nt2 nt3 in
  let nt2 = plus nt2 in
  let nt2 = pack nt2 (fun (digit) -> List.fold_left
                                  (fun (a)(b) -> 16*a+b) 0 digit) in
  nt2 str

and nt_hexadecimalChar str =
  let nt1 = word ("#\\x") in
  let nt2 = (only_if nt_char_hex (fun n -> n > 0)) in
  let nt2 = (only_if nt2 (fun n -> n < 256)) in
  let nt2 = pack nt2 (fun n -> Char.chr n )in                  
  let res = caten nt1 nt2 in
  let res = pack res (fun (p,l) -> l) in
  res str
   
and nt_char str =
  let nt1 = disj nt_hexadecimalChar nt_char_named in
  let nt1 = disj nt1 nt_char_simple in
  let nt_rest = maybe (char ' ') in
  let nt1 = caten nt1 nt_rest in
  let nt1 = pack nt1 (fun (a,b) -> a ) in
  let nt1 = pack nt1 (fun (p) -> ScmChar(p))  in
  nt1 str        
 
and nt_symbol_char str =
    let nt_capital_letter_to_lower_letter =  (range 'A' 'Z') in  
    let nt_capital_letter_to_lower_letter = pack nt_capital_letter_to_lower_letter (fun (a) -> lowercase_ascii a) in
    let nt1 =  
      disj_list [(range '0' '9');  
                (range 'a' 'z');
                nt_capital_letter_to_lower_letter;
                (char ('!'));  
                (char ('$'));
                (char ('^'));
                (char ('*'));
                (char ('-'));
                (char ('_'));
                (char ('='));
                (char ('+'));
                (char ('<'));
                (char ('>'));  
                (char ('?'));
                (char ('/'));
                (char (':'))] in
      let nt1 = pack nt1 (fun (p) -> p) in          
    nt1 str            
and nt_symbol str =
  let nt1 = plus nt_symbol_char in
  let nt1 = pack nt1 list_to_string in
  let nt1 = pack nt1 (fun name -> ScmSymbol name) in
  let nt1 = diff nt1 nt_number in
  nt1 str



and nt_stringMetaChar str =
  let nt1 =
    disj_list [word "\\\\";
               word "\\\"";
               word "\\t";
               word "\\f";    
               word "\\n";  
               word "\\r" ] in
  let nt1 = pack nt1 (fun (p) -> list_to_string p) in            
  let nt1 = pack nt1 (fun (p) -> match p with
                        | "\\\\" ->  "\\"
                        | "\\\"" -> "\""
                        | "\\t" -> "\t"
                        | "\\f" -> "\012"
                        | "\\n" -> "\n"
                        | "\\r" -> "\r"
                        |  _ -> raise X_no_match
                        ) in  
                               
  nt1 str
 
and nt_StringLiteralChar str =
  let nt_no_good = disj_list [char('\\'); char('\"'); char('~')] in
  let nt1 = diff nt_any nt_no_good in
  let nt1 = pack nt1 (fun (p) -> (String.make 1 p))in
  nt1 str

and nt_StringHexChar str =
  let nt1 = word ("\\x") in
  let nt2 = (only_if nt_char_hex (fun n -> n > 0)) in
  let nt2 = (only_if nt2 (fun n -> n < 256)) in
  let nt2 = pack nt2 (fun n -> Char.chr n )in
  let nt3 = caten nt2 (char ';') in
  let nt3 = pack nt3 (fun (l,p) -> l) in  
  let res = caten nt1 nt3 in
  let res = pack res (fun (p,l) -> String.make 1 l) in
  res str
       
and nt_StringChar str =
  let nt1 = disj_list [
            nt_StringLiteralChar;
            nt_StringHexChar;
            nt_stringMetaChar] in
  nt1 str    
     
and nt_interpolated_strings str =
  let nt1 = word "~{" in  
  let nt2 = nt_sexpr in  
  let nt3 = word "}" in          
  let res = caten (caten nt1 nt2) nt3 in
  let res = pack res (fun ((l,p),r) -> ScmPair(ScmSymbol("format"),ScmPair(ScmString("~a"),ScmPair (p, ScmNil)))) in
  res str        
             
and nt_string str =
  let start = word "\"" in          
  let nt0  = nt_StringChar in  
  let nt0_plus = plus nt0 in        
  let nt0_plus = pack nt0_plus (fun (p)->ScmString(list_of_strings_to_string p)) in    
  let nt1 = nt_interpolated_strings in    
  let nt_res2_1 = disj nt1 nt0_plus in                  
  let nt_res2_1 = plus nt_res2_1 in      
  let nt_res2_1 = pack nt_res2_1 (fun (p) ->          
    List.fold_right          
       (fun list exp -> ScmPair(list,exp))        
       p              
       ScmNil                
        ) in                      
  let nt_res2_1 = pack nt_res2_1 (fun (p) -> ScmPair (ScmSymbol ("string-append"),p)) in
  let nt_res1 = disj nt1 nt0_plus in
  let finish = word "\"" in    
  let nt_res2_1 = caten nt_res2_1 finish in
  let nt_res2_1 = pack nt_res2_1 (fun (p,l) -> p) in
  let nt_res1  = caten nt_res1 finish in        
  let nt_res1 = pack nt_res1 (fun (p,l) -> p) in
  let nt_res_final = disj nt_res1 nt_res2_1 in    
  let nothing = word "\"\"" in                
  let nothing = pack nothing (fun (p) -> ScmString("")) in
  let finish = caten start nt_res_final  in      
  let finish = pack finish (fun (p,l)->l) in  
  let nt_res_final2 = disj nothing finish in
  nt_res_final2 str
 

and nt_literal_char str = raise X_not_yet_implemented
 
and nt_vector str =
  let nt1 = word "#(" in
  let nt2 = caten nt_skip_star (char ')') in
  let nt2 = pack nt2 (fun _ -> ScmVector []) in    
  let nt3 = plus nt_sexpr in
  let nt4 = char ')' in
  let nt3 = caten nt3 nt4 in
  let nt3 = pack nt3 (fun (sexprs, _) -> ScmVector sexprs) in
  let nt2 = disj nt2 nt3 in
  let nt1 = caten nt1 nt2 in
  let nt1 = pack nt1 (fun (_, sexpr) -> sexpr) in
  nt1 str
and nt_proper_list str =
    let nt1 = star nt_sexpr in
    let nt1 = pack nt1 (fun (p) ->
    List.fold_right
       (fun list exp -> ScmPair(list,exp))
       p
       ScmNil
        ) in
    nt1 str  

and nt_inproper_list str =        
    let nt1 = star nt_sexpr in
    let nt2 = char '.' in
    let nt3 = nt_sexpr in
    let nt3 = caten (caten nt1 nt2) nt3 in
    let nt3 = pack nt3 (fun ((p,l),r) ->  
        List.fold_right
       (fun list exp -> ScmPair(list,exp))
       p
       r) in
    nt3 str      
   
and nt_list str =  
    let nt0 = char '(' in
    let nt1 = (disj nt_inproper_list nt_proper_list) in
    let nt2 = caten nt_skip_star (char ')') in
    let nt2 = pack nt2 (fun (p,l)->l) in
    let res = caten (caten nt0 nt1) nt2 in    
    let res = pack res (fun ((p,l),r) -> l) in
    res str
   
and nt_quote str =  
    let nt1 = char '\'' in
    let nt2 = nt_sexpr in  
    let nt2 = caten nt1 nt2 in
    let nt2 = pack nt2 (fun (p,l) -> ScmPair(ScmSymbol("quote"),ScmPair(l,ScmNil))) in
    nt2 str

and nt_unquote str =  
    let nt1 = char ',' in
    let nt2 = nt_sexpr in    
    let nt2 = caten nt1 nt2 in
    let nt2 = pack nt2 (fun (p,l) -> ScmPair(ScmSymbol("unquote"),ScmPair(l,ScmNil))) in
    nt2 str    

and nt_quasiquote str =
    let nt1 = char '`' in
    let nt2 = nt_sexpr in  
    let nt2 = caten nt1 nt2 in
    let nt2 = pack nt2 (fun (p,l) -> ScmPair(ScmSymbol("quasiquote"),ScmPair(l,ScmNil))) in
    nt2 str
   
and nt_unquote_splicing str =
    let nt1 = word ",@" in
    let nt2 = nt_sexpr in  
    let nt2 = caten nt1 nt2 in
    let nt2 = pack nt2 (fun (p,l) -> ScmPair(ScmSymbol("unquote-splicing"),ScmPair(l,ScmNil))) in
    nt2 str

and nt_quoted_forms str =  
    let nt1 = disj_list [nt_quote;nt_unquote_splicing;nt_unquote;nt_quasiquote] in
    nt1 str

and nt_sexpr str =      
  let nt1 =
    disj_list [nt_number; nt_boolean; nt_char; nt_symbol;  
               nt_string; nt_vector; nt_list; nt_quoted_forms
               ] in
             
  let nt1 = make_skipped_star nt1 in
  nt1 str

end;; (* end of struct Reader *)

let rec string_of_sexpr = function
  | ScmVoid -> "#<void>"
  | ScmNil -> "()"
  | ScmBoolean(false) -> "#f"
  | ScmBoolean(true) -> "#t"  
  | ScmChar('\n') -> "#\\newline"
  | ScmChar('\r') -> "#\\return"
  | ScmChar('\012') -> "#\\page"
  | ScmChar('\t') -> "#\\tab"
  | ScmChar(' ') -> "#\\space"
  | ScmChar(ch) ->
     if (ch < ' ')
     then let n = int_of_char ch in
          Printf.sprintf "#\\x%x" n
     else Printf.sprintf "#\\%c" ch
  | ScmString(str) ->
     Printf.sprintf "\"%s\""
       (String.concat ""
          (List.map
             (function
              | '\n' -> "\\n"
              | '\012' -> "\\f"
              | '\r' -> "\\r"
              | '\t' -> "\\t"
              | ch ->
                 if (ch < ' ')
                 then Printf.sprintf "\\x%x;" (int_of_char ch)
                 else Printf.sprintf "%c" ch)
             (string_to_list str)))
  | ScmSymbol(sym) -> sym
  | ScmNumber(ScmRational(0, _)) -> "0"
  | ScmNumber(ScmRational(num, 1)) -> Printf.sprintf "%d" num
  | ScmNumber(ScmRational(num, -1)) -> Printf.sprintf "%d" (- num)
  | ScmNumber(ScmRational(num, den)) -> Printf.sprintf "%d/%d" num den
  | ScmNumber(ScmReal(x)) -> Printf.sprintf "%f" x
  | ScmVector(sexprs) ->
     let strings = List.map string_of_sexpr sexprs in
     let inner_string = String.concat " " strings in
     Printf.sprintf "#(%s)" inner_string
  | ScmPair(ScmSymbol "quote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "'%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "quasiquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "`%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote-splicing",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",@%s" (string_of_sexpr sexpr)
  | ScmPair(car, cdr) ->  
     string_of_sexpr' (string_of_sexpr car) cdr
and string_of_sexpr' car_string = function
  | ScmNil -> Printf.sprintf "(%s)" car_string
  | ScmPair(cadr, cddr) ->
     let new_car_string =
       Printf.sprintf "%s %s" car_string (string_of_sexpr cadr) in
     string_of_sexpr' new_car_string cddr
  | cdr ->
     let cdr_string = (string_of_sexpr cdr) in    
     Printf.sprintf "(%s . %s)" car_string cdr_string;;