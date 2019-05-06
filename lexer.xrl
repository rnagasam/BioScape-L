Definitions.

Id = [a-z][0-9a-zA-Z_]*

Name = [A-Z][0-9a-zA-Z_]*

Str = \'[0-9a-zA-Z_]*\'

Num = [0-9][0-9]*

WhiteSpace = [\s\t\n\r]+

Null = null

Define = define

Send = send

Recv = recv

Spawn = spawn

New = new

This = @this

Run = -run

Step = -step

Rules.

{Num} :
  {token,{num,float(list_to_integer(TokenChars))}}.

{WhiteSpace} : skip_token.

%% {Name} :
%%   {token,{name,TokenLine,list_to_atom(TokenChars)}}.

{Spawn} :
  {token,{'spawn',TokenLine,TokenChars}}.

{Name} :
  {token,{name,list_to_atom(TokenChars)}}.

{Str} :
  {token,{str,TokenChars}}.

{Define} :
  {token,{'define',TokenLine,TokenChars}}.

{Send} :
  {token,{'send',TokenLine,TokenChars}}.

{Recv} :
  {token,{'recv',TokenLine,TokenChars}}.

{Null} :
  {token,{null,TokenLine,TokenChars}}.

{This} :
  {token,{'this',TokenLine,TokenChars}}.

{Run} :
  {token,{'run',TokenLine,TokenChars}}.

{Step} :
  {token,{'step',TokenLine,TokenChars}}.

{New} :
  {token,{'new',TokenLine,TokenChars}}.

{Id} :
  {token,{id,list_to_atom(TokenChars)}}.

\! :
  {token,{send,TokenLine,TokenChars}}.

\? :
  {token,{recv,TokenLine,TokenChars}}.

\( :
  {token,{oParen,TokenLine,TokenChars}}.

\) :
  {token,{cParen,TokenLine,TokenChars}}.

\{ :
  {token,{oBrace,TokenLine,TokenChars}}.

\} :
  {token,{cBrace,TokenLine,TokenChars}}.

\, :
  {token,{comma,TokenLine,TokenChars}}.

\. :
  {token,{dot,TokenLine,TokenChars}}.

\; :
  {token,{semicolon,TokenLine,TokenChars}}.

\@ :
  {token,{at,TokenChars}}.


Erlang code.
