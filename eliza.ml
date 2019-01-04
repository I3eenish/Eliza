#use "eliza_rules.ml";;

(* extract
 * word: string, either a single word (perhaps with hyphens or apostrophes, 
 * but no blanks or other whitespace) or a single punctuation mark 
 * (with no whitespace) (e.g. "yay", "?")
 * phrase: a list of words (e.g. ["Hi" ; "," ; "I'm" ; "feeling" ; "blue"])
 * phrase list: a list of phrases (e.g. [], [["hello" ; "Bob"] ; ["bye"]])
 * pattern_element: Lit (word), One, or Any
 * pattern: a list of pattern_elements 
 * (e.g. [Lit("Hi") ; Lit(",") ; Any ; Lit("feeling") ; One])
 
 * Input: a phrase * pattern tupule
 * Output: phrase list option, either Some, followed by the portions 
 * of the input that match each wild card, if the input matches the pattern, 
 * or None, otherwise *)

let rec extract : phrase * pattern -> phrase list option = function
  | [], [] -> Some []
  | head :: tail, [] -> None
  | [], Lit word :: rest -> None
  | [], One :: rest -> None
  | [], Any :: rest ->
  	(match rest with 
  	| [] -> Some [[]]
  	| head :: tail -> extract ([], head :: tail))
  | hd :: tl, Lit word :: rest ->
  	if hd = word then extract (tl, rest) else None
  | hd :: tl, One :: rest ->
  	(match extract (tl, rest) with
  	| None -> None
  	| Some x -> Some([hd] :: x))
  | hd :: tl, Any :: rest ->
  	match extract ((hd :: tl), rest) with 
  	| Some y -> Some([] :: y)
  	| None -> 
  		match extract (tl, (Any :: rest)) with
  		| Some z ->
  		  Some((hd :: (List.hd z)) :: (List.tl z))
  		| None -> None ;;



(* make_response
 * word: string, either a single word (perhaps with hyphens or apostrophes, 
 * but no blanks or other whitespace) or a single punctuation mark 
 * (with no whitespace) (e.g. "yay", "?")
 * phrase: a list of words (e.g. ["Hi" ; "," ; "I'm" ; "feeling" ; "blue"])
 * phrase list: a list of phrases (e.g. [], [["hello" ; "Bob"] ; ["bye"]])
 * response_element: Text of word list or Place of int (e.g. Text["Hi!"],
 * Place(1))
 * response_template: a list of response_elements 
 * (e.g. [Text["Hi!"] ; Place(1)])


 * Input: a phrase list * response_template tupule (extraction, temp) 
 * Output: a response (i.e. phrase), constructed by substituting 
 * the extracted literals from extraction into their positions 
 * in the template *)

let rec make_response: phrase list * response_template -> phrase = function
  (extraction, temp) -> match temp with
    | Text str ::rest ->
	  str @ make_response (extraction, rest)
	| Place num ::rest ->
	  List.nth extraction (num - 1) @ make_response (extraction, rest)
    | _ -> [] ;;



(* eliza_respond
 * word: string, either a single word (perhaps with hyphens or apostrophes, 
 * but no blanks or other whitespace) or a single punctuation mark 
 * (with no whitespace) (e.g. "yay", "?")
 * phrase: a list of words (e.g. ["Hi" ; "," ; "I'm" ; "feeling" ; "blue"])
 * rule: Rule of pattern * response_template (e.g. Rule([Any], 
 * [Text(["Why" ; "do" ; "you"]) ; Place(1) ; Text(["your"]) ; Place(2) ;
 * Text(["?"])]))
 * rule list: a list of rules

 * Input: a phrase * rule list tupule (input, rules)
 * Output: phrase, representing ELIZA's response to the input phrase *)
 
let rec eliza_respond: phrase * rule list -> phrase = function
  (input, rules) -> match rules with
    | Rule(rule_pattern, rule_template)::tail_rules -> 
	  (match (extract (input, rule_pattern)) with
	  	| None -> eliza_respond (input, tail_rules)
      	| Some(extract_result) -> 
      		make_response (extract_result, rule_template))
	| _ -> [] ;;

(* eliza
 * I/P: a string * rule list tuple (input, rules) 
 *	consisting of a string of user input and a rule list
 * O/P: ELIZA's response to input based on the patterns contained
 *      in rules, converted to a string. *)
let eliza: string * rule list -> string = function
  (input, rules) -> from_phrase (eliza_respond (to_phrase input, rules)) ;;