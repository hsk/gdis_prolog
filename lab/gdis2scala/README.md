# GDIS Prolog Interpretor

Implemented Goals Database Index Stack Prolog in Scala.

## Future

- :- prefix operator : 
- =/2 infix operator : unification
- ,/2 infix operator : conjunction
- ;/2 infix operator : 'or' predicate
- !/0 infix operator : cut
- is/2 infix operator : expression evalutes 
- assert/1 predicate : add term to database
- consult/1 predicate : load of file
- halt/0 predicate : exit program
- write/1 preicate : stdout output

## Constitution

- syntax.ml  abstract syntax tree
- parser.mly grammer
- lexer.mll  lexical grammer
- prolog.ml  main logic
- main.ml    main program

