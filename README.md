# Eliza
Elementary Natural Language Processing program.

The user provides a string as input to the eliza procedure. Note that the eliza procedure also requires a list of rules as part of the input; however, we are assuming here that the list of rules is not to be provided by the individual interacting with the program but rather by the individual coding the program. The output is a string that “intelligently” responds to the user’s input. For example, if the user inputs “I am feeling rather blue today”, the eliza procedure would then output “Why are you feeling rather blue today?”

Below is an outline of Eliza:

eliza -> to_phrase -> eliza_respond -> extract -> make_response -> from_phrase
