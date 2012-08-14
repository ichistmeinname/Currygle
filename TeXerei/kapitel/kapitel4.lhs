\chapter{Implementation}\label{implementation}
% Mention that the implementation is done in Haskell or Curry.\\
% Give a short summary of the next sections.

This chapter presents the implementation of the search engine on the
basis of some code examples and the corresponding design ideas and
decisions. At first we take a look at the extension for the current
version of CurryDoc. In this context we illustrate the interaction
between this extension and the index creation. The latter determines
the topic of the second section. We specify the index and document
data in more detail and state some difficulties that arose due to the
usage of the Holumbus framework. The third section addresses the
implemented parser to convert the user input into a query that can be
processed by Holumbus. We illustrate the general idea and
implementation approach of a parser to introduce into the subject. The
last section covers features and implementations of the web
application for the search engine.

\section{CurryDoc extension}\label{implementation:currydoc}

Present the general structure of the |CurryInfo| data and the
sub-structures |ModuleInfo|, |FunctionInfo| and |TypeInfo|.

\begin{code}
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
\end{code}

The name, author and description is the interesting information that
is exported. 
\begin{code}
data ModuleInfo = ModuleInfo String String String [String] String
\end{code}

The model contains the name, signature, module, and description of a function
and extra information about the non-/determinism and flexible/rigid status.

\begin{code} 
data FunctionInfo = FunctionInfo String (QName, TypeExpr) String 
  String Bool FlexRigidResult
\end{code}

The TypeInfo includes the name, signature, list of type variables,
module, and description.
\begin{code}
data TypeInfo = TypeInfo String [(QName, [TypeExpr])] [TVarIndex]
  String String
\end{code}

Explain how this extension is used in the CurryDoc tool. 

The same data structure is used on the Haskell side that implements
the search engine.

\section{Indexing}\label{implementation:index}

First mention that the interessting parts (that we want to add to the
index) are already filtered by the CurryDoc part. So the indexer only
processes the information to the structure provided by the Holumbus
framework (HolumbusState).

Present the output (files) the indexer produces. After that introduce
the concept of documents and index. In addition to that, say something
about refreshing and checking the list of modules.

Mention the three different kinds of documents for the three
structures: module, function, type.
\begin{code}
-- || Pair of index and documents of the type ModuleInfo
type CurryModIndexerState         = HolumbusState ModuleInfo

-- || Pair of index and documents of the type FunctionInfo
type CurryFctIndexerState         = HolumbusState FunctionInfo

-- || Pair of index and documents of the type TypeInfo
type CurryTypeIndexerState        = HolumbusState TypeInfo
\end{code}

Explain how each kind of information (description, module name,
function signature etc) is combined with its context, and that these
are stored in the index. Note that information can be extracted by the
context again.

Note the difficulties of updating the index, because the data
structure of the loaded pair of index and document differs from
HolumbusState a. Conversion of |Inverted| to |CompactInverted| and
|Documents a| to |SmallDocuments a|.
\begin{code}
type LoadedIndexerState a = (CompactInverted, SmallDocuments a)
\end{code}

Refer to the appendix, where the usage of the curryIndexer is
explained.

\section{Parsing user queries}\label{implementation:parser}

% First describe the idea, that the use of a specific language increases
% the usability. But it also restricts the user in her usage of the
% search engine, if this language gets more complex. So this results in
% a compromise between a simply to use language and a language that can
% be parsed.  Show the example of searching IO, where the restriction to
% modules minimizes the result.

% After that list all language components that describe the restriction
% to contexts (module, function, type, signature and  non-/deterministic,
% flexible, rigid functions.

% Set the focus on signatures. Because Hayoo does not find signatures with
% redundant parenthesis, Curr(y)gle supports parenthesized signatures
% and parenthesized query parts in general.  

Explain the general idea of parsers: a parser is used to analyze the
user query with these different kinds of information. While parsing
the string, a new data structure is composed for further use. This
data structure is provided by the Holumbus framework and is used to
starte the processs that returns the search results.

Introduce the parser type that is parametrized with the type to parse and the
resulting type.

\begin{code}
type Parser sigma alpha = [sigma] -> [(alpha, [sigma])]
\end{code}

Explain the operator |(<||>)| that applies two parsers and concats the
parsing results.  
\begin{code}
(<|>) :: Parser s a -> Parser s a -> Parser s a 
p <|> q = (\ts -> p ts ++ q ts)
\end{code}

Introduce the combination of two parsers that is similar to a monadic
bind operator. 
\begin{code}
(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b 
p <*> q = \ts -> [ (f x, ts2) | (f, ts1) <- p ts, (x, ts2) <- q ts1 ]
\end{code}

Now show that a function has to be applied, to use the binding operator.
\begin{code}
(<$>) :: (a -> b) -> Parser s a -> Parser s b
f <$> p =  \ts -> [ (f x, ts1) | (x, ts1) <- p ts]
\end{code}

Present a simple example that can parse a given character.
\begin{code}
parsePredicate :: (Char -> Bool) -> Parser Char String
parsePredicate predicate (t:ts) 
  | predicate t = [( [], ts )]
  | otherwise   = [( [], t:ts )]
parsePredicate _ [] = [] 
\end{code}

Present the concept by writing a parser for expressions with or without
parentheses using |<||>| and |<*>|. 

\begin{code}
-- Parses an opening parenthesis by using the predicate parser.
parseOpenParenthesis :: Parser Char String
parseOpenParenthesis =  
  parsePredicate ('(' ==)

-- Parses an closing parenthesis.
parseCloseParenthesis :: Parser Char String
parseCloseParenthesis =  
  parsePredicate (')' ==)

-- Parses a sequence of alpha numeric characters. 
parseExpression :: Parser Char String
parseExpression (t:ts) = 
  if isAlphaNum t then parseExpression ts else [( [], t:ts )]
parseExpression [] = []

-- Parses an expression or a parenthesized expression.
parenthesizedExpression :: Parser Char String
parenthesizedExpression = 
    ((\_ expr _ -> expr) 
      <$> parseOpenParenthesis <*> parseExpression <*> parseCloseParenthesis)
    <|> parseExpression
\end{code}

Mention that the used type in the implementation correlates to |Parser
String TypeExpr| (which is already used for the data exchange) for
signatures and |Parser String Query| (which is provided by the Holumbus framework) for the end
result. 

% Explain that in most cases, a combination of more search words is
% desirable, because first popular search engines like Google\texttrademark~use this
% feature so it's common knowledge (the user expects this features) and
% second it's easier to search for more search words, if the desired
% result is still vague.

Give a definition of the language (EBNF(?) / appendix). 

\begin{code} 
 query := expr [ expr ] | expr bool expr | (expr)
 expr := (expr) | specifier | signature | string
 bool := "AND" | "OR" | "NOT" 
 specifier :=  ":module" [ alphaNum ] | ":signature"  [ signature ]
 |  ":function" [ alphaNum ] | ":flexible" | ":rigid" |  ":nondet" | ":det" 
 signature := Upper alphaNum | function | constructor 
 function := signature "->" signature  | lower "->" signature |
 signature "->" lower
 constructor := Upper alphaNum signature | Upper alphaNum lower
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The actual use case of the search engine is to search for a given query.

% For the user, it's important that the search engine understands her
% query. For this cause, the user is obliged to use a specific language
% that can be interpreted unambiguously.

% To simplify the usage of the search engine, Curr(y)gle\texttrademark
% provides a language to restrict the search to a specific
% context. This feature not only simplifies the use, but results in a better
% user-experience.

% (Example) Let's assume you want to search for the module Map. Without
% the restriction to modules, there are numerous results, because map is
% a very common name (as it is associated with higher-order functions)
% in functional languages. Thanks to the feature to search for specific
% contexts, you can search for the query \emph{:module map} instead,
% where \emph{:module} indicates the context of modules.

% Curr(y)gle \texttrademark supports to search explicitly for modules,
% functions, types and signatures. In addition to that, it allows
% to search for functions with a given characteristic, like
% non-deterministic, flexible and rigid functions.

% As the number of the supported features increases, the query gets more
% complex to parse. 

\section{Web application}