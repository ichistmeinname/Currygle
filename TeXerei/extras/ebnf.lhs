\begin{code}
Query                     ::= { Expr } | Expr Bool Expr { Bool Expr } | "(" Query ")";
Expr                      ::= ":" Specifier | Signature | String | "(" Expr ")";
Bool                      ::= "AND" | "OR" | "NOT";
String                    ::= IdentStart { IdentLetter } | { Operator };
IdentStart                ::= "_" | lower;
IdentLetter               ::= alphaNum | "'";
Operator                  ::= ":" | "!" | "#" | "$" | "%" | "&" | "*" | "+" | "." 
                            | "/" | "<" | "/" | "<" | "=" | ">" | "?" | "@" | "\\" 
                            | "^" | "|" | "-" | "~" | "_";
Signature                 ::= ConstructorType | ConsArgumentType;

ConstructorType           ::= Identifier ConsArgumentType | Signature "->" Signature;
ConsArgumentType          ::= PrimitiveType | TypeVariable | ListType | TupleType 
                            | "(" ConstructorType ")" | "(" ConsArgumentType ")";
ListType                  ::= "[" Signature "]";
TupleType                 ::= "(" Signature "," Signature { "," Signature } ")";
PrimitiveType             ::= Identifier | "()";
TypeVariable              ::= lowerCase;
Identifier                ::= Upper { alphaNum };
\end{code}

\begin{code}
Specifier                 ::= SignatureSpecifier | ModuleSpecifier | FunctionSpecifier 
                            | TypeSpecifier |AuthorSpecifier | InModuleSpecifier 
                            | FlexibleSpecifier | RigidSpecifier 
                            | NonDetSpecifier | DetSpecifier;
SignatureSpecifier        ::= "signature" [ Signature ] | "s" [ Signature ];
ModuleSpecifier           ::= "module" [ alphaNum ] | "m" [ alphaNum ];
FunctionSpecifier         ::= "function" [ alphaNum ] | "f" [ alphaNum ];
TypeSpecifier             ::= "type" [ alphaNum ] | "t" [ alphaNum ];
AuthorSpecifier           ::= "author" [ alphaNum ] | "a" [ alphaNum ];
InModuleSpecifier         ::= "inModule" [alphaNum] | "in" [ alphaNum ];
FlexibleSpecifier         ::= "flexible" | "fl";
RigidSpecifier            ::= "rigid" | "r";
NonDeterSpecifier         ::= "nondet" | "nd";
DetSpecifier              ::= "det" | "d";
\end{code}
