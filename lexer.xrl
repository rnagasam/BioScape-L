Definitions.

Id = [a-z][0-9a-zA-Z_]*

Name = [A-Z][0-9a-zA-Z_]*

Str = \'[0-9a-zA-Z_]*\'

Num = [0-9][0-9]*

WhiteSpace = [\s\t\n\r]+

Null = null

Define = define

Rules.

{Num} :
  {token,{num,TokenLine,list_to_integer(TokenChars)}}.

{WhiteSpace} : skip_token.

{Name} :
  {token,{name,TokenLine,list_to_atom(TokenChars)}}.

{Str} :
  {token,{str,TokenLine,TokenChars}}.

{Define} :
  {token,{'define',TokenLine,TokenChars}}.

{Null} :
  {token,{null,TokenLine,TokenChars}}.

{Id} :
  {token,{id,TokenLine,list_to_atom(TokenChars)}}.

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

\. :
  {token,{dot,TokenLine,TokenChars}}.


Erlang code.
