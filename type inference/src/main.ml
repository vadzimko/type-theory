open Tree;;
open Buffer;;
open Pervasives;;
open Printf;;

module Ht = Hashtbl;;
module S = Set.Make(String);;

let cout = stdout;;
let cin  = stdin;;

type exprType = Type of string
              | Impl of exprType * exprType
              | Equal of exprType * exprType;;

let rec type_to_string t = match t with
          Type var -> var
        | Impl (left, right) -> "(" ^ (type_to_string left) ^ " -> " ^ (type_to_string right) ^ ")"
        | Equal (left, right) -> "(" ^ (type_to_string left) ^ " = " ^ (type_to_string right) ^ ")"

let printSet s = Printf.printf "Size: %d, Elements: " (S.cardinal s);S.iter (fun newTypeName -> Printf.printf "%s " newTypeName) s; Printf.printf "\n";;
let printTypesMap typesMap = Ht.iter (fun x y -> Printf.printf "%s -> %s\n" (type_to_string x) (type_to_string y)) typesMap;;

let rec to_string tree str = match tree with
      Var var                   -> add_string str var
    | Apply (left,right)        -> add_char str '(';to_string left str;add_char str ' ';to_string right str;add_char str ')'
    | Abstract (var,arg)        -> add_string str "(\\";add_string str var;add_string str ". ";to_string arg str;add_char str ')';;
let tree_to_string tree =
    let str = Buffer.create 100 in to_string tree str; contents str;;
let read_input () = begin
  let lines = ref [] in
  let read_lines () =
    try
      while true; do
        let line = input_line cin in
        lines := line :: !lines;
        done; !lines
      with _ -> List.rev !lines in
  let str = Buffer.create 10000 in
  let rec join lines = match lines with
  | h::t ->  add_string str h;
             add_char str '\n';
             join t

  | [] -> contents str in
  let input = ref (join (read_lines())) in
  input := Str.global_replace (Str.regexp "\\\\") " \\\\" !input;
  input := Str.global_replace (Str.regexp "(") " ( " !input;
  input := Str.global_replace (Str.regexp ")") " ) " !input;
  input := String.trim !input;
  !input
end;;

let input = read_input();;
let tree = (Parser.main Lexer.main (Lexing.from_string input));;

let typeIndex = ref 0;;
let getNewTypeIndex () = begin
  let newIndex = !typeIndex in
  typeIndex := !typeIndex + 1;
  string_of_int newIndex
end;;

let getExprType context expr = begin
  let newType = try Ht.find context expr with Not_found -> begin
    let newType = Type("t" ^ (getNewTypeIndex())) in
    Ht.add context expr newType;
    newType
  end in newType
end;;

let init tree = begin
  typeIndex := 0;
  let rec findFreeVars tree freeVars = begin
    match tree with
    | Var var                  -> S.add var freeVars
    | Apply(left, right)       -> findFreeVars right (findFreeVars left freeVars)
    | Abstract (var, abstExpr) -> S.remove var (findFreeVars abstExpr freeVars)
  end in
  let freeVars = findFreeVars tree S.empty in
  let freeContext = Ht.create 128 in
  let addToContext context var = Ht.add context (Var var) (Type("t" ^ (getNewTypeIndex()))) in
  S.iter (addToContext freeContext) freeVars;
  (*printSet freeVars;*)
  (*Ht.iter (fun x y -> Printf.printf "%s -> %s\n" (tree_to_string x) (type_to_string y)) freeContext;*)
  (freeContext, freeVars)
end;;

let (freeContext, freeVars) = init tree;;

let rec buildSystem tree context = begin
  match tree with
  | Var var -> []
  | Apply(left, right) -> begin
    (*Ht.iter (fun x y -> Printf.printf "%s -> %s\n" (tree_to_string x) (type_to_string y)) freeContext;*)
    let currType = getExprType context tree in
    let leftContext = Ht.copy context in
    let leftType = getExprType leftContext left in
    let leftEquations = buildSystem left leftContext in
    (*Ht.remove context left;*)
    let rightContext = Ht.copy context in
    let rightType = getExprType rightContext right in
    let rightEquations = buildSystem right rightContext in
    (*Ht.remove context right;*)
    let allEquations = List.append leftEquations rightEquations in
    let newEquation = Equal(leftType, Impl(rightType, currType)) in
    newEquation::allEquations
  end
  | Abstract (var, expr) -> begin
    let currType = getExprType context tree in
    let varContext = Ht.copy context in
    let varType = getExprType varContext (Var var) in
    let exprType = getExprType varContext expr in
    let exprEquations = buildSystem expr varContext in
    (*Ht.remove context (Var var);*)
    let newEquation = Equal(currType, Impl(varType, exprType)) in
    newEquation::exprEquations
  end
end;;

let equations = buildSystem tree (Ht.copy freeContext);;

let rec print_list myList = match myList with
	| [] -> ()
	| h::t ->
begin
  print_endline (type_to_string h);
  print_list t
end;;

let getTypesMap equations = begin

	let remove_dups l =
	  let sl = List.sort compare l in
	  let rec go l acc = match l with
	    | [] -> List.rev acc
	    | [x] -> List.rev (x::acc)
	    | (x1::x2::xs) ->
	      if x1 = x2
	      then go (x2::xs) acc
	      else go (x2::xs) (x1::acc)
	  in go sl [] in

	let rec contains equation searchType  = begin
	  match equation with
	  | Impl (left, right) -> if (contains left searchType) then true else contains right searchType
	  | Type t             -> t = searchType
	  | _ -> false
	end in

	let makeSubstitution equations oldType newType = begin
    let rec substitute expr = match expr with
      | Impl(left, right) -> Impl(substitute left, substitute right)
      | Type(t)           -> if (t <> oldType) then Type(t) else newType
      | _                 -> expr
    in
    let rec apply equations = match equations with
      | [] -> []
      | h::t -> match h with
        | Equal(left, right) -> Equal((substitute left), (substitute right))::(apply t)
        | _                  -> h::(apply t)
    in apply equations
  end in

	let rec unification equations result = begin
	  match equations with
	  | [] -> result
	  | head::tail -> begin
	    let filtered = (List.filter ((<>) head) result) in
	    match head with
	    | Equal(Type t1, Type t2) when (t1 = t2) -> unification tail filtered
	    | Equal(Type t, expr) -> if (contains expr t) then (print_endline "Expression has no type"; exit 0)
				else begin
	        let reduced = makeSubstitution filtered t expr in
	        remove_dups (unification (makeSubstitution tail t expr) (remove_dups (head::reduced)))
	      end
	    | Equal(expr, Type t) -> if (contains expr t) then (print_endline "Expression has no type"; exit 0)
        else begin
          let reduced = makeSubstitution filtered t expr in
          remove_dups (unification (makeSubstitution tail t expr) (remove_dups (Equal(Type t, expr)::reduced)))
        end
	    | Equal(Impl (left_from, left_to), Impl(right_from, right_to)) -> remove_dups (unification tail (Equal(left_from, right_from)::(Equal(left_to, right_to)::filtered)))
			| _ -> []
	  end
	end in

	let needUnifyMore list1 list2 = (List.sort compare list1 <> List.sort compare list2) in
	let rec repeatUnification equations = begin
	  let newEquations = unification equations equations in
	  if needUnifyMore equations newEquations then repeatUnification newEquations else newEquations
	end in

	let rec buildTypesMap equations = begin
    match equations with
    | [] -> Ht.create 1024
    | head::tail -> begin
      let typesMap = buildTypesMap tail in
      match head with
      | Equal(left, right) -> begin
  	      match left with
  	      | Impl(t1, t2) -> (print_endline "Expression has no type"; exit 0)
          | Type t -> Ht.add typesMap left right; typesMap
  	      | _ -> typesMap
      end
      | _ -> typesMap
    end
  end in
  let unified = repeatUnification equations in
  buildTypesMap unified
end;;

let typesMap = getTypesMap equations;;
(*exit(0);;*)
(*let _ = printTypesMap typesMap;;*)

let expr_to_string expr context = begin
	let getType expr = begin
	  try
	    let freeType = Ht.find context expr in
	    let t = try Ht.find typesMap freeType with Not_found -> freeType in t
	  with Not_found -> begin
	    match expr with
	    | Var var -> let t = try Ht.find typesMap (Type var) with Not_found -> (print_endline "Expression has no type"; exit 0) in t
	    | _ -> ("Expression has no type"; exit 0)
	  end
	end in
  ((tree_to_string expr) ^ " : " ^ (type_to_string (getType expr)))
end;;

let context_to_string context = begin
	let str = Buffer.create 128 in
	let rec join lines = match lines with
	  | h::[] -> add_string str h; add_string str " ";  join []
	  | h::t ->  add_string str h; add_string str ", "; join t
    | [] -> contents str in
	let res = ref [] in
	let transform context = Ht.iter (fun x y ->
	match x with
		| Var var -> res := (expr_to_string x context)::!res
		| _ -> ()
	) context in
	let _ = transform context in
	join !res
end;;

let (context, _) = init tree;;
let printSolution tree context = begin
	let rec findSolution tree context offset = begin
	  match tree with
	  | Var var -> begin
	    [offset ^ (context_to_string context) ^ "|- " ^ (expr_to_string tree context) ^ " [rule #1]"]
	  end
	  | Apply(left, right) -> begin
	    let newContext = Ht.copy context in
	    let _ = getExprType context tree in

	    let leftContext = Ht.copy newContext in
	    let _ = getExprType leftContext left in
	    let leftSolution = findSolution left leftContext (offset ^ "*   ") in

	    let rightContext = Ht.copy newContext in
	    let _ = getExprType rightContext right in
	    let rightSolution = findSolution right rightContext (offset ^ "*   ") in

	    let allSolutions = List.append leftSolution rightSolution in
	    let line = offset ^ (context_to_string context) ^ "|- " ^ (expr_to_string tree context) ^ " [rule #2]" in
			line::allSolutions
	  end
	  | Abstract (var, expr) -> begin
	    let _ = getExprType context tree in

	    let varContext = Ht.copy context in
	    let _ = getExprType varContext (Var var) in
	    let _ = getExprType varContext expr in

	    let line = offset ^ (context_to_string context) ^ "|- " ^ (expr_to_string tree context) ^ " [rule #3]" in
	    (line)::(findSolution expr varContext (offset ^ "*   "));
	  end
	end in

  let rec print lines = match lines with
	  | h::t -> print_endline h; print t
	  | []   -> () in
	print (findSolution tree context "")
end;;

printSolution tree context;;