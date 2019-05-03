Nonterminals
channel expression program.

Terminals
num id 'send' 'recv' dot 'null' oParen cParen oBrace cBrace 'define' name semicolon new.

Rootsymbol program.

expression ->
    define name oBrace expression cBrace : { define, '$2', '$4' }.
expression ->
    num : '$1'.
expression ->
    id : '$1'.
expression ->
    null : { null }.
expression ->
    send id oParen expression cParen dot expression : { send, '$2', '$4', '$7' }.
expression ->
    recv id oParen id cParen dot expression : { recv, '$2', '$4', '$7' }.
expression ->
    oParen expression cParen : '$2'.

channel ->
    new id semicolon : ['$2'].
channel ->
    new id semicolon channel : ['$2'] ++ '$4'.

program -> channel semicolon program : [{chans, '$1'}, '$3'].
program -> expression : ['$1'].
program -> expression semicolon program : ['$1'] ++ '$3'.
