#use "CS17setup.ml" ;;
#use "eliza_rules.ml";;


(* extract
 * phrase: a list of words (e.g. ["Hi" ; "," ; "I'm" ; "feeling" ; "blue"])
 * word: string, either a single word (perhaps with hyphens or apostrophes, 
 * but no blanks or other whitespace) or a single punctuation mark 
 * (with no whitespace) (e.g. "yay", "?")
 * phrase list: a list of phrases (e.g. [], [["hello" ; "Bob"] ; ["bye"]])
 * pattern: a list of pattern_elements 
 * (e.g. [Lit("Hi") ; Lit(",") ; Any ; Lit("feeling") ; One])
 * pattern_element: Lit (word), One, or Any

 * input: a phrase * pattern tupule
 * output: phrase list option, either Some, followed by the portions 
 * of the input that match each wild card, if the input matches the pattern, 
 * or None, otherwise *)

let rec extract : phrase * pattern -> phrase list option = function
  | [], [] -> Some [] (* phrase: empty, pattern: empty *)
  | head :: tail, [] -> None (* phrase: not empty, pattern: empty *)
  | [], Lit word :: rest -> None (* phrase: empty, pattern: car: Lit *)
  | [], One :: rest -> None (* phrase: empty, pattern: car: One *)
  | [], Any :: rest -> (* phrase: empty, pattern: car: Any *)
  	(match rest with 
  	| [] -> Some [[]] (* result when [Any] or [Any ; Any ; Any ; ...] *)
  	| head :: tail -> extract ([], head :: tail)) (* check rest of pat *)
  | hd :: tl, Lit word :: rest -> (* phrase: not empty, pattern: car: Lit *)
  	if hd = word then extract (tl, rest) else None (* None, when cars not = *)
  | hd :: tl, One :: rest -> (* phrase: not empty, pattern: car: One *)
  	(match extract (tl, rest) with
  	| None -> None (* None, when cdr of phrase doesn't match cdr of pat *)
  	| Some x -> Some([hd] :: x)) (* otherwise, cons car of phrase *)
  | hd :: tl, Any :: rest -> (* phrase: not empty, pattern: car: Any *)
  	match extract ((hd :: tl), rest) with 
  	| Some y -> Some([] :: y) (* when Any matches with zero elements *)
  	| None -> 
  		match extract (tl, (Any :: rest)) with
  		| Some z -> (* when Any matches with one or more elements *)
  		  Some((hd :: (List.hd z)) :: (List.tl z))
  		| None -> None ;; (* when phrase and pattern don't match *)

(* test cases for extract *)
check_expect (extract ([], [])) (Some []) ;;
check_expect (extract ([], [Any])) (Some [[]]) ;;
check_expect (extract ([], [Any; Any])) (Some [[]]) ;;
check_expect (extract ([], [Any; Lit("")])) None ;;
check_expect (extract ([], [Any; Lit("blargh")])) None ;;
check_expect (extract ([], [Any; One])) None ;;
check_expect (extract ([], [Any; Any; One])) None ;;
check_expect (extract ([], [Any; Any; Any])) (Some [[]]) ;;
check_expect (extract ([], [Any; Lit ""; Any])) None ;;
check_expect (extract ([], [Any; Any; Lit ""])) None ;;
check_expect (extract ([], [Any; Lit ""; Lit ""])) None ;;
check_expect (extract ([], [Any; One; Lit ""])) None ;;
check_expect (extract ([], [Any; One; One])) None ;;
check_expect (extract ([], [Any; Lit ""; One])) None ;;
check_expect (extract ([], [One])) None ;;
check_expect (extract ([], [One; One])) None ;;
check_expect (extract ([], [One; Any])) None ;;
check_expect (extract ([], [One; Lit ""])) None ;;
check_expect (extract ([], [One; Lit ""; Any])) None ;;
check_expect (extract ([], [Lit ""])) None ;;
check_expect (extract ([], [Lit "blargh"])) None ;;
check_expect (extract ([], [Lit ""; Any])) None ;;
check_expect (extract ([], [Lit ""; One])) None ;;
check_expect (extract ([], [Any ; Any ; Any ; Any ; One])) None ;;
check_expect (extract (["bye"], [One])) (Some [["bye"]]) ;;
check_expect (extract (["bye"], [One; One])) None ;;
check_expect (extract (["bye"], [One; Any])) (Some [["bye"]; []]) ;;
check_expect (extract (["bye"], [One; Any; Any])) (Some [["bye"]; []]) ;;
check_expect (extract (["bye"], [One; Any; Lit ""])) None ;;
check_expect (extract (["bye"], [One; Any; One])) None ;;
check_expect (extract (["bye"], [One; Lit ""])) None ;;
check_expect (extract (["bye"], [One; Lit ""; One])) None ;;
check_expect (extract (["bye"], [One; Lit ""; Any])) None ;;
check_expect (extract (["bye"], [Any])) (Some [["bye"]]) ;;
check_expect (extract (["bye"], [Any; Any])) (Some [[]; ["bye"]]) ;;
check_expect (extract (["bye"], [Any; Any; Lit("bye")])) (Some [[]; []]) ;;
check_expect (extract (["bye"], [Any; Any; Any])) (Some [[]; []; ["bye"]]) ;;
check_expect (extract (["bye"], [Any; One])) (Some [[]; ["bye"]]);;
check_expect (extract (["bye"], [Any; One; Any])) (Some [[]; ["bye"]; []]) ;;
check_expect (extract (["bye"], [Any; Lit ""])) None ;;
check_expect (extract (["bye"], [Any; Lit "bye"])) (Some [[]]) ;;
check_expect (extract (["bye"], [Any; Lit "bye"])) (Some [[]]) ;;
check_expect (extract (["bye"], [Any; Lit "bye"; Any])) (Some [[]; []]) ;;
check_expect (extract (["bye"], [Any; Lit "bye"; One])) None ;;
check_expect (extract (["hello"], [Lit "hello"])) (Some []) ;;
check_expect (extract (["Hi"], [Lit "hello"])) None ;;
check_expect (extract (["hello"], [Lit "hello"; Any])) (Some [[]]) ;;
check_expect (extract (["hello"], [Lit "hello"; One])) None ;;
check_expect (extract (["hello"], [Lit "hello"; One; One])) None ;;
check_expect (extract (["hello"], [Lit "hello"; Lit ""])) None ;;
check_expect (extract (["hello"], [Lit "hello"; Lit "b"])) None ;;
check_expect 
	(extract (["hello"], [Lit "hello"; Any; Any; Any])) (Some [[]]) ;;
check_expect (extract (["hello"], [Lit "hello"; Any; Any; One])) None ;;
check_expect (extract (["hello"], [Lit "hello"; Any; One; Any])) None ;;
check_expect 
	(extract (["hello"; "it's"; "me"], [Lit "hello"; Lit "it's"; Lit "me"])) 
	(Some []) ;;
check_expect 
  	(extract (["hi"; "it's"; "me"], [Lit "hi"; Lit "it's"; Lit "you"])) 
  	None ;;
check_expect 
  	(extract 
  		(["hi"; "it's"; "you"], [Lit "hello"; Lit "it's"; Lit "you"]))
  	None ;;
check_expect 
  	(extract 
  		(["hello"; "it's"; "me"], 
		[Lit "hello"; Lit "it"; Lit "is"; Lit "me"])) 
  	None ;;
check_expect 
  	(extract 
  		(["hello"; "it"; "is"; "me"], 
  		[Lit "hello"; Lit "it's"; Lit "me"])) 
  	None ;;
check_expect 
  	(extract 
  		(["hello"; "it's"; "me"], 
  		[Lit "hello"; Lit "it's"; Lit "me"; Lit "too"])) 
  	None ;;
check_expect 
  	(extract 
  		(["hello" ; "it's" ; "me"; "too"], 
  		[Lit "hello"; Lit "it's"; Lit "me"])) 
  	None ;;
check_expect 
  	(extract 
  		(["hello"; "hi"; "bye"], 
  		[Any; Any; Lit "hello"; Lit "hi"; Lit "bye"])) 
  	(Some [[] ; []]) ;;
check_expect 
  	(extract 
  		(["hello"; "hi"; "bye"], 
  		[Any; Any; Lit "hello"; Lit "hi"; One])) 
  	(Some [[] ; [] ; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any; Lit "hi"; Lit "bye"])) 
  	(Some [["hello"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any; One; Lit "bye"])) 
  	(Some [["hello"] ; ["hi"]]) ;;
check_expect 
  	(extract (["hello"; ","; "bye"], [Any; One; One; Lit "bye"])) 
  	(Some [[]; ["hello"] ; [","]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any; One])) 
  	(Some [["hello"; "hi"]; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any; Any; Any; Any; One])) 
  	(Some [[]; []; []; ["hello"; "hi"]; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any ; One; Any; One])) 
  	(Some [[]; ["hello"]; ["hi"]; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any; Lit "hi"; Lit "byee"]))
  	None ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any; Lit "hi"; Lit "bye"]))
  	(Some [["hello"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any; One])) 
  	(Some [["hello"; "hi"] ; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [Any; Any; One])) 
  	(Some [[]; ["hello"; "hi"] ; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [One; One])) None ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [One; One; One])) 
  	(Some [["hello"]; ["hi"]; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [One; Any; One])) 
  	(Some [["hello"]; ["hi"] ; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [One; Any; One; One])) 
  	(Some [["hello"]; []; ["hi"]; ["bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [One; Any])) 
  	(Some [["hello"]; ["hi"; "bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [One; Any; Any])) 
  	(Some [["hello"]; []; ["hi"; "bye"]]) ;;
check_expect 
  	(extract (["hello"; "hi"; "bye"], [One; One; One; Any])) 
  	(Some [["hello"]; ["hi"]; ["bye"]; []]) ;;
check_expect 
  	(extract (["hello" ; "hi" ; "bye"], [Any ; One ; Any ; Any]))
  	(Some [[] ; ["hello"] ; [] ; ["hi" ; "bye"]]) ;;
check_expect 
  	(extract 
  		(["I" ; "hate" ; "to" ; "hate"; "you"], 
  		[Lit "I"; Any; Lit "hate"; Lit "you"])) 
  	(Some[["hate" ; "to"]]) ;;


(* make_response
 * phrase: a list of words (e.g. ["Hi" ; "," ; "I'm" ; "feeling" ; "blue"])
 * word: string, either a single word (perhaps with hyphens or apostrophes, 
 * but no blanks or other whitespace) or a single punctuation mark 
 * (with no whitespace) (e.g. "yay", "?")
 * phrase list: a list of phrases (e.g. [], [["hello" ; "Bob"] ; ["bye"]])
 * response_template: a list of response_elements 
 * (e.g. [Text["Hi!"] ; Place(1)])
 * response_element: Text of word list or Place of int (e.g. Text["Hi!"],
 * Place(1))

 * input: a phrase list * response_template tupule (extraction, temp) 
 * output: a response (i.e. phrase), constructed by substituting 
 * the extracted literals from extraction into their positions 
 * in the template *)

let rec make_response: phrase list * response_template -> phrase = function
  (extraction, temp) -> match temp with
    | Text str ::rest -> (* append string from Text *)
	  str @ make_response (extraction, rest)
	| Place num ::rest -> (* get phrase using index from Place and append *)
	  List.nth extraction (num - 1) @ make_response (extraction, rest)
    | _ -> [] ;;

(* test cases for make_response *)
check_expect (make_response ([], [])) [] ;;
check_expect (make_response ([], [Text["Hi!"]])) ["Hi!"] ;;
check_expect (make_response ([[]], [Text["Hi!"]])) ["Hi!"] ;;
check_expect (make_response ([[""]], [Text["Hi!"]])) ["Hi!"] ;;
check_expect (make_response ([[""]], [Place 1])) [""] ;;
check_expect (make_response ([["Eric"]], [Place 1])) ["Eric"] ;;
check_expect (make_response ([["Eric"]], [Text ["CS"]])) ["CS"] ;;
check_expect (make_response ([["Eric"]], [Text ["CS"]; Place 1])) 
	["CS"; "Eric"] ;;
check_expect (make_response ([["Pizzas"; "are"; "Great"]], [Place 1]))
	["Pizzas"; "are"; "Great"] ;;
check_expect (make_response ([["Pizzas"; "are"; "Great"]], [Place 1; 
	Text ["and"; "go"; "with"; "pineapple"]])) 
    ["Pizzas"; "are"; "Great"; "and"; "go"; "with"; "pineapple"] ;;
check_expect (make_response ([["Pizzas"]; ["cheese"]; ["tomatoes"]], [Place 1; 
	Text["are"; "made"; "with"]; Place 2; Place 3])) 
    ["Pizzas"; "are"; "made"; "with"; "cheese"; "tomatoes"] ;;
check_expect (make_response ([["Pizzas"]; ["cheese"]; ["tomatoes"]], [Place 2; 
	Text["are"; "made"; "from"]; Place 1; Place 3])) 
    ["cheese"; "are"; "made"; "from"; "Pizzas"; "tomatoes"] ;;
check_expect (make_response ([["Pizzas"]; ["cheese"]; ["tomatoes"]], [Place 2; 
	Place 3; Place 1])) 
    ["cheese"; "tomatoes"; "Pizzas"] ;;
check_expect (make_response ([["Pizzas"]; ["cheese"]; ["tomatoes"]], [Place 2; 
	Place 3])) 
    ["cheese"; "tomatoes"] ;;
check_expect (make_response ([["Pizzas"]; ["cheese"]; ["tomatoes"]], 
	[Place 3])) ["tomatoes"] ;;
check_expect (make_response ([["Pizzas"]; ["cheese"]; ["tomatoes"]], [Place 2; 
	Place 2])) ["cheese"; "cheese"] ;;

(* eliza_respond
 * phrase: a list of words (e.g. ["Hi" ; "," ; "I'm" ; "feeling" ; "blue"])
 * word: string, either a single word (perhaps with hyphens or apostrophes, 
 * but no blanks or other whitespace) or a single punctuation mark 
 * (with no whitespace) (e.g. "yay", "?")
 * rule list: a list of rules
 * rule: Rule of pattern * response_template (e.g. Rule([Any], 
 * [Text(["Why" ; "do" ; "you"]) ; Place(1) ; Text(["your"]) ; Place(2) ;
 * Text(["?"])]))

 * input: a phrase * rule list tupule (input, rules)
 * output: phrase, representing ELIZA's response to the input phrase *)
 
let rec eliza_respond: phrase * rule list -> phrase = function
  (input, rules) -> match rules with
    | Rule(rule_pattern, rule_template)::tail_rules ->
	  (* attempt to pattern match input using extract *) 
	  (match (extract (input, rule_pattern)) with 
	  | None ->  (* if phrase doesn't match, call again on rest of rules *)
	    eliza_respond (input, tail_rules) 
      | Some(extract_result) -> 
	  	(* if phrase matches, call make_response with wildcard values and 
		rule's response template  *)
	    make_response (extract_result, rule_template))
	  | _ -> [];; (* if no rules, return empty list *)

check_expect (eliza_respond ([], [])) [] ;;
check_expect (eliza_respond (["Hakuna"; "Matata"], [])) [] ;;
check_expect (eliza_respond (["Hakuna"; "Matata"], 
	[Rule([Lit "Hi"], [Text ["Hi"]]);])) [] ;;
check_expect (eliza_respond (["Hi"], my_rules)) ["Hi"] ;;
check_expect (eliza_respond (["Hello"], my_rules)) ["Hi"] ;;
check_expect (eliza_respond (["Hi"; "Eliza"], my_rules)) ["Hi"] ;;
check_expect (eliza_respond (["Hi"; "Bob"; "the"; "builder"], my_rules)) 
	["I"; "am"; "Bob"; "the"; "builder"] ;;
check_expect (eliza_respond (["Gibberish"], my_rules)) 
	["Gibberish"; "well"; "then"] ;;
check_expect (eliza_respond (["The"; "sky"; "is"; "red"], my_rules)) 
	["Do"; "you"; "like"; "red"] ;;
check_expect (eliza_respond (["The"; "store"; "is"; "closed"; "tomorrow"; 
	"for"; "Thanksgiving"; "break"], my_rules)) 
	["What"; "are"; "you"; "doing"; "when"; "it's"; "closed"; "tomorrow"; 
	"for"; "Thanksgiving"; "break"] ;;
check_expect 
	(eliza_respond (["Are"; "the"; "green"; "farm"; "tomatoes"; "good"],
	my_rules)) 
	["I"; "don't"; "like"; "green"; "farm"; "tomatoes"] ;;
check_expect (eliza_respond (["Is"; "my"; "guard"; "dog"; "sick"], my_rules)) 
	["I"; "don't"; "know"; "if"; "your"; "guard"; "dog"; "is"; "sick"] ;;
check_expect (eliza_respond (["Can"; "my"; "pig"; "fly"], my_rules)) 
	["Only"; "if"; "you"; "want"; "your"; "pig"; "to"; "fly"] ;;
check_expect (eliza_respond (["It"; "is"; "pink"], my_rules)) 
	["Why"; "is"; "it"; "pink"] ;;
check_expect (eliza_respond (["He"; "blinks"; "pink"], my_rules)) 
	["Why"; "he"; "blinks"] ;;
check_expect 
	(eliza_respond (["She"; "favors"; "her"; "potatoes"; "in"; "pizza"],   
	my_rules)) 
	["What's"; "with"; "potatoes"; "in"; "pizza"] ;;
check_expect (eliza_respond (["Her"; "cat's"; "giving"; "candy"], my_rules)) 
	["Maybe"; "giving"; "candy"; "is"; "meant"; "for"; "cat's"] ;;
check_expect 
	(eliza_respond (["The"; "first"; "was"; "made"; "in"; "China"], my_rules)) 
	["Are"; "the"; "rest"; "made"; "in"; "China"] ;;
check_expect (eliza_respond (["My"; "pet"; "frog"; "eats"], my_rules)) 
	["How"; "does"; "your"; "frog"; "eats"] ;;
check_expect (eliza_respond (["My"; "toy"; "frog"; "eats"], my_rules)) 
	["How"; "does"; "your"; "frog"; "eats"] ;;
check_expect 
	(eliza_respond (["My"; "favorite"; "drinks"; "are"; "water"; "coke"; "and"; 
	"lemonade"], my_rules)) 
	["Why"; "coke"; "and"; "lemonade"; "water"] ;;
check_expect 
	(eliza_respond (["My"; "favorite"; "foods"; "are"; "chips"; "fries"; 
	"cookies"], my_rules)) 
	["Why"; "chips"; "fries"; "cookies"] ;;
check_expect 
	(eliza_respond (["I"; "like"; "Sci-Fi"; "Fiction"; "Non-Fiction"], 
	my_rules)) 
	["I"; "don't"; "like"; "Sci-Fi"; "Fiction"; "Non-Fiction"] ;;
check_expect 
	(eliza_respond (["The"; "mall"; "has"; "violet"; "paintings"; "in"; "art"; 
	"stores"], my_rules)) 
	["I"; "can't"; "find"; "the"; "violet"; "paintings"; "in"; "the"; "in"; 
	"art"; "stores"] ;;
check_expect 
	(eliza_respond (["Leaves"; "turn"; "red"; "in"; "Fall"], my_rules)) 
	["What"; "does"; "turn"; "red"; "in"; "have"; "to"; "do"; "with"; 
	"Fall"] ;;
check_expect (eliza_respond (["Do"; "bats"; "need"; "light"; "to"; "eat"], 
	my_rules)) 
	["Yes"; "bats"; "do"; "need"; "light"] ;;
check_expect 
	(eliza_respond (["Should"; "sick"; "dogs"; "go"; "for"; "runs"], my_rules)) 
	["Why"; "not"; "let"; "go"; "even"; "if"; "they"; "are"; "sick"; "dogs"] ;;
check_expect 
	(eliza_respond (["They"; "jog"; "through"; "parks"; "on"; "mornings"], 
	my_rules)) 
	["Is"; "it"; "not"; "too"; "early"; "for"; "jog"; "through"; "parks"] ;;
check_expect 
	(eliza_respond (["They"; "jog"; "through"; "parks"; "on"; "nights"], 
	my_rules)) 
	["Is"; "it"; "not"; "too"; "late"; "for"; "jog"; "through"; "parks"] ;;
check_expect 
	(eliza_respond (["He"; "treats"; "others"; "with"; "cruelty"], my_rules)) 
	["Why"; "is"; "he"; "cruel"; "to"; "others"] ;;
check_expect 
	(eliza_respond (["The"; "Green"; "Bananas"; "love"; "your"; "talent"], 
	my_rules)) 
	["Why"; "can't"; "everyone"; "be"; "like"; "The"; "Green"; "Bananas";
    "and"; "love"; "your"; "talent"] ;;
check_expect (eliza_respond (["I"; "enjoy"; "driving"; "cars"], my_rules)) 
	["What"; "kind"; "of"; "cars"] ;;
check_expect (eliza_respond (["cows"; "walk"; "fields"], my_rules)) 
	["Can"; "fields"; "walk"; "cows";] ;;
check_expect 
	(eliza_respond (["Farms"; "feed"; "Americans"; "meals"], my_rules))
	["To"; "feed"; "Americans"; "daily"; "meals"; "there"; "are"; "Farms"] ;;
check_expect (eliza_respond ([""], my_rules)) [""; "well"; "then"] ;;
check_expect (eliza_respond (["sdasd"], my_rules)) ["sdasd"; "well"; "then"] ;;
check_expect (eliza_respond ([], my_rules)) ["I"; "don't"; "get"; "you"] ;;

(* eliza
 * I/P: a string * rule list tuple (input, rules) 
 *	consisting of a string of user input and a rule list
 * O/P: ELIZA's response to input based on the patterns contained
 *      in rules, converted to a string. 
 * You need not test eliza, as it is already implemented. *)
let eliza: string * rule list -> string = function
  (input, rules) -> from_phrase (eliza_respond (to_phrase input, rules)) ;;