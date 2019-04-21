Nonterminals
expression.

Terminals
num id 'send' 'recv' dot 'null' oParen cParen oBrace cBrace 'define' name.

Rootsymbol expression.

expression ->
    define name oBrace expression cBrace : { define, '$2', '$4' }.
expression ->
    num : { numExp, '$1' }.
expression ->
    id : { idExp, '$1' }.
expression ->
    null : { null }.
expression ->
    send id oParen expression cParen dot expression : { send, '$2', '$4', '$7' }.
expression ->
    recv id oParen id cParen dot expression : { recv, '$2', '$4', '$7' }.
expression ->
    oParen expression cParen : '$2'.
