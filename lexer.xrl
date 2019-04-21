Definitions.

Id = [a-z][0-9a-zA-Z_]*

Str = \'[0-9a-zA-Z_]*\'

Num = [0-9][0-9]*

WhiteSpace = [\s\t\n\r]+

Null = null

Rules.

{Num} :
  {token,{num,TokenLine,list_to_integer(TokenChars)}}.

{Null} :
  {token,{null,TokenLine,TokenChars}}.

{WhiteSpace} : skip_token.

{Id} :
  {token,{id,TokenLine,list_to_atom(TokenChars)}}.

{Str} :
  {token,{str,TokenLine,TokenChars}}.

\! :
  {token,{send,TokenLine,TokenChars}}.

\? :
  {token,{recv,TokenLine,TokenChars}}.

\( :
  {token,{oParen,TokenLine,TokenChars}}.

\) :
  {token,{cParen,TokenLine,TokenChars}}.

\. :
  {token,{dot,TokenLine,TokenChars}}.


Erlang code.
