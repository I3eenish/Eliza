#use "eliza_support.ml";;

let my_rules = [
  Rule([Lit "Hi"],
    [Text ["Hi"]]);
    
  Rule([Lit "Hello"],
    [Text ["Hi"]]);

  Rule([Lit "Hi"; Lit "Eliza"],
    [Text ["Hi"]]);

  Rule([Lit "Hi"; Any],
    [Text ["I"; "am"]; Place 1]);

  Rule([One],
    [Place 1; Text ["well"; "then"]]);

  Rule([Lit "The"; Lit "sky"; Lit "is"; One],
    [Text ["Do"; "you"; "like"]; Place 1]);

  Rule([Lit "The"; Lit "store"; Lit "is"; Any; Any],
    [Text ["What"; "are"; "you"; "doing"]; Place 1; 
      Text ["when"; "it's"]; Place 2]);
    
  Rule([Lit "Are"; Lit "the"; Any; One],
    [Text ["I"; "don't"; "like"]; Place 1]);

  Rule([Lit "Is"; Lit "my"; Any; One],
    [Text["I"; "don't"; "know"; "if"; "your"]; Place 1; Text ["is"]; 
    Place 2]);

  Rule([Lit "Can"; Lit "my"; One; Any],
    [Text ["Only"; "if"; "you"; "want"; "your"]; Place 1; 
    Text ["to"]; Place 2]);

  Rule([Lit "It"; One; Any],
    [Text ["Why"; "is"; "it"]; Place 2]);

  Rule([Lit "He"; One; One; Lit "with"; Lit "cruelty"],
    [Text ["Why"; "is"; "he"; "cruel"; "to"]; Place 2]);

  Rule([Lit "He"; One; Any],
    [Text ["Why"; "he"]; Place 1]);

  Rule([Lit "She"; Lit "favors"; One; Any],
    [Text ["What's"; "with"]; Place 2]);

  Rule([Lit "Her"; One; Any],
    [Text ["Maybe"]; Place 2; Text ["is"; "meant"; "for"]; Place 1]);

  Rule([Lit "The"; Lit "first"; One; Any],
    [Text ["Are"; "the"; "rest"]; Place 2]);

  Rule([Lit "My"; Lit "pet"; One; One],
    [Text ["How"; "does"; "your"]; Place 1; Place 2]);

  Rule([Lit "My"; Lit "toy"; One; One],
    [Text ["How"; "does"; "your"]; Place 1; Place 2]);

  Rule([Lit "My"; Lit "favorite"; Lit "drinks"; Lit "are"; Any; One; Any],
    [Text ["Why"]; Place 3; Place 2; Place 1]);

  Rule([Lit "My"; Lit "favorite"; Lit "foods"; Lit "are"; Any; One; Any],
    [Text ["Why"]; Place 1; Place 2; Place 3]);

  Rule([Lit "I"; Lit "like"; Any; Any; One],
    [Text ["I"; "don't"; "like"]; Place 2; Place 1; Place 3]);

  Rule([Lit "The"; Lit "mall"; Lit "has"; One; One; Any],
    [Text ["I"; "can't"; "find"; "the"]; Place 1; Place 2; 
    Text ["in"; "the"]; Place 3]);

  Rule([Lit "Leaves"; Any; One],
    [Text ["What"; "does"]; Place 1; Text ["have"; "to"; "do"; "with"]; 
    Place 2]);

  Rule([Lit "Do"; One; Any;  Lit "to"; Lit "eat"],
    [Text ["Yes"]; Place 1; Text ["do"]; Place 2]);

  Rule([Lit "Should"; Any; Any; Lit "go"; Lit "for"; Lit "runs"],
    [Text ["Why"; "not"; "let"]; Place 1; 
    Text ["go"; "even"; "if"; "they"; "are"]; Place 2]);

  Rule([Lit "They"; Any; Lit "on"; Lit "mornings"],
    [Text ["Is"; "it"; "not"; "too"; "early"; "for"]; Place 1]);

  Rule([Lit "They"; Any; Lit "on"; Lit "nights"],
    [Text ["Is"; "it"; "not"; "too"; "late"; "for"]; Place 1]);

  Rule([Any; Lit "love"; Any],
    [Text ["Why"; "can't"; "everyone"; "be"; "like"]; Place 1; 
    Text ["and"; "love"]; Place 2]);

  Rule([Any; Lit "driving"; One],
    [Text ["What"; "kind"; "of"]; Place 2]);

  Rule([One; Lit "walk"; One], 
    [Text ["Can"]; Place 2; Text ["walk"]; Place 1]);

  Rule([One; Any; One], 
    [Text ["To"]; Place 2; Text ["daily"]; Place 3; Text ["there"; "are"]; 
    Place 1]);

  Rule([Any],
    [Text ["I"; "don't"; "get"; "you"]]);
];;