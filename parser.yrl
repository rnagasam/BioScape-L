Nonterminals
channel definitions expression expressions program
location namelist entity commands.

Terminals
num id 'send' 'recv' 'null' 'spawn' 'this' 'define' 'new' 'run' 'step'
name oParen cParen oBrace cBrace dot semicolon comma at.

Rootsymbol program.

entity ->
    name dot oBrace at id cBrace : {'$1', '$5'}.
entity ->
    name dot oBrace location cBrace : {'$1', '$4'}.

expression ->
    define name dot oBrace location cBrace oParen expression cParen
    : {define, '$2', '$5', '$8'}.
expression ->
    num : '$1'.
expression ->
    id : '$1'.
expression ->
    null : {null}.
expression ->
    send id oParen expression cParen dot expression : {send, '$2', '$4', '$7'}.
expression ->
    recv id oParen id cParen dot expression : {recv, '$2', '$4', '$7'}.
expression ->
    spawn oParen namelist cParen dot expression : {spawn, '$3', '$6'}.
expression ->
    expressions : {choice, '$1'}.
expression ->
    oParen expression cParen : '$2'.

expressions ->
    oBrace expression cBrace: ['$2'].
expressions ->
    oBrace expression cBrace comma expressions: ['$2'] ++ '$5'.

namelist ->
    entity : ['$1'].
namelist ->
    entity comma namelist : ['$1'] ++ '$3'.

channel ->
    new id num semicolon : [{'$2', '$3'}].
channel ->
    new id num semicolon channel : [{'$2', '$3'}] ++ '$5'.

commands ->
    step num semicolon commands : [{step, '$2'}|'$4'].
commands ->
    run name num semicolon : [{run, '$2', '$3'}].
commands ->
    run name num semicolon commands : [{run, '$2', '$3'}] ++ '$5'.

location ->
    at num comma num : {'$2', '$4'}.
location ->
    at num comma num comma num: {'$2', '$4', '$6'}.
location ->
    this : {this}.

definitions -> expression semicolon : ['$1'].
definitions -> channel semicolon definitions : [{chans, '$1'}, '$3'].
definitions -> expression semicolon definitions : ['$1'] ++ '$3'.

program ->
    definitions commands : ['$1', '$2'].
