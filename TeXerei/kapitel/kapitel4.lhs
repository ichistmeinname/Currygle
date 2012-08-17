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
application for the search engine.  The implementations are mostly
written in the functional programming language Haskell; we use the
functional logic programming language Curry for the CurryDoc
extension only.\\

\todo[inline]{general section about the underlying environment?}

\section{CurryDoc extension}\label{implementation:currydoc}

In the previous chapter we discussed the general idea of an extension
for CurryDoc to generate a data structure. Later this data structure serves
as source for the index creation. In this section we take a look at
the implementation of this extension.

% Present the general structure of the |CurryInfo| data and the
% sub-structures |ModuleInfo|, |FunctionInfo| and |TypeInfo|.
Since CurryDoc is written in Curry, we implemented our extension in
Curry as well. With this decision we stand to benefit from already
implemented functionalities and on the other hand, using the same
programming language simplifies the integration of our
implementation with the current CurryDoc version.\\

CurryDoc uses the meta-programming language FlatCurry to gain an
intermediate data structure. We can use this data structure for our
purposes. Additionally, we can use other functions provided by
CurryDoc, that are already implemented. For example, CurryDoc supports
a special comment syntax to annotate the author and version of a
module. Furthermore the arguments and the return value of a function
can be described as well as general descriptions.

But at first, we discuss which information we want to provide in our
data structures. We already introduced \emph{CurryInfo} as structure
for a Curry program in the
\hyperref[preliminaries:currydoc:curryInfo]{second chapter}. As next
step we want to describe |ModuleInfo|, |FunctionInfo| and |TypeInfo|,
since they are a part of the |CurryInfo| data structure. You can take
a look at these data structures and their definitions in advance in
\hyperref[fig:curryInfo]{Figure \ref{fig:curryInfo}}.

\begin{figure}[h]
\begin{code}
-- || The CurryInfo data holds information about the module, and
--  corresponding  functions, data and type declaration of a given 
--  Curry module.
data CurryInfo = 
  CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
%//%
-- || ModuleInfo holds information about the name, author,
--    and the description of a given module.
data ModuleInfo = 
  ModuleInfo String String String
%//%
-- || FunctionInfo holds information about the name, signature, corresponding 
-- module, description and flexible/rigid status of a function and 
-- its non-/deterministic behaviour.
data FunctionInfo = 
  FunctionInfo String (QName, TypeExpr) String String Bool
  FlexRigidResult
%//%
-- || TypeInfo holds information about the name, signature, type variables,
-- corresponding module, and description of a given type.
data TypeInfo = 
  TypeInfo String [(QName, [TypeExpr])] [Int] String String
\end{code}
\caption{The data structures representing a Curry program}
\label{fig:curryInfo}
\end{figure}

Like the name suggests, |ModuleInfo| represents the data corresponding
to a Curry module. The main information about a module consists of its
name, author and description. We can also provide the version number
of the implementation or the imported modules, but we decided against
it. The latter seems to be useless information for the search engine,
since the Curry modules are highly interrelated. Thus, searching for a
module results in a great amount of hits, since every correlating
module will be shown as well. Furthermore we think the version number
is not a significant characteristic for a module. Therefore we decided
to focus on the three mentioned proporties only.

|FunctionInfo| consists of characteristics for a given function like
the function's name and description. Additionally we decide to add the
corresponding module to provide a connection between the function and
its module. This decision is based on the cause that we do not keep
the |CurryInfo| data structure as whole for the index construction,
but the three arguments consisting of the list of functions, the list
of types and the module information. Thanks to FlatCurry, we can also
access function characteristics like nondeterminism and determinism,
along with the information if a given function is rigid or
flexible. Since these are important characteristics to differ between
Curry functions, |FunctionInfo| stores these information as
property. In addition FlatCurry provides a data structure |TypeExpr|
to describe type signatures (see \hyperref[fig:typeExpr]{last
  chapter}), furthermore we get the actual definition of a
function. We use a function's type signature as part of the
|FunctionInfo| data structure, but decided against the usage of a
function's definition since we could not think of a relevant use-case
for our search engine.

The data structure for types looks quite similar to
|FunctionInfo|. |TypeInfo| consists of a type's name, description and
corresponding module. Since FlatCurry provides type signatures for
functions, we also get information about constructors for a given
type. Therefore we store a list of |TypeExpr| representing the type's
constructors. Additionally |TypeInfo| holds a list of integer to
represent possible type variables. The decision to use integer corresponds to
the definition of |TypeExpr|, where type variables are represented as
integer as well. \\

In the end, we feed the |CurryInfo| data structure with the specific
module, function and type information of a given Curry program and our
CurryDoc extension writes the data structure into a
\emph{.cdoc}-file. The final CurryDoc version allows two mechanisms to
generate the |CurryInfo| structure. You can generate the
\emph{.cdoc}-file only or you initiate the HTML generation, where the
\emph{.cdoc}-file is also part of the output. In
\hyperref[a:currydoc]{Appendix \ref{a:currydoc}} we provide further
instruction for the usage of CurryDoc.

Due to the similar syntax, we can use the same data structures in
Haskell as in Curry to exchange those structures. More precisely, we
can read the \emph{.cdoc}-file within our Haskell implementation and
work with the data structure. In order to do that, we need a Haskell
program that defines all the data structures used in |CurryInfo|
including the nested structures. Thus, we generate |CurryInfo| to use
it as data structure in the process of the index creation.
% The same data structure is used on the Haskell side that implements
% the search engine.

\section{Indexer}\label{implementation:index}

This section illustrates how to create the index for our search
engine. In our analysis, we discussed some data structures to handle
the index creation and the information we want to store. In the
following, we talk the advantages and disadvantages of using the
Holumbus framework and describe our implementation of the index
creation in more detail.
The following implementation is done in the functional logic
programming language Haskell. This decision allows us to use the
Holumbus framework for our purpose and a good number of functional,
pure and strict features.\\

In the previous section we already mentioned the required preparation
to create the index. On the Haskell side, we need to define the data
structures we use to build the |CurryInfo| structure. Then we can read
the file produced by the CurryDoc extension for further usage. Since
|CurryInfo| and its substructures |ModuleInfo|, |FunctionInfo| and
|TypeInfo| already provide the information we want to store in our
index, it is not necessary to filter these structures. Instead, we
only have to process these data to fit the data structures provided by
Holumbus. In the end we can either create a new index by writing each
structure to a file to store our information or update an existing
index with additional Curry modules. In order to do that, we load the
index and document files and merge them with new data. Due to lazy
evaluation, we cannot read and write to the same file; it is
not assured that we finish reading before we start to rewrite the
file. Therefore we have to write temporary files and rename these
files afterwards to guarantee a clean outcome.

During the testing phase of the indexer, we noticed problems regarding
duplicate data. In particular, when we add a Curry module to the
index twice, there is no mechanism to detect the duplicated data. For
that reason only we added a list of the modules that are stored in the
index as output file. So every time we update the index with a given
module, we check if it already exists in the saved list. We only start
the processing of the data, if the module does not occur in our list
and on the other hand, we add the module's name to the list.\\

In the previous chapter, we introduced the idea of storing two
structures: an index |Inverted| and a document |Documents
a|. \todo{index would be better} In the following, we present this
idea in more detail, beginning with the document. At first, we take
closer look at the data structure itself. Holumbus provides us with
|Documents a| can be described as mapping of yet another data
structure |Document a| and an unique identifier (see
\hyperref[fig:documents]{Figure \ref{fig:documents}}). Each |Document
a| consists of a title, an URI and customizable information. The
latter has the type |a| and determines the type for document. In order
to have a faster access to a specific document, |Documents a| also
stores the highest document id used in the mapping as well as a
mapping from the document's uri to the document's identifier.\\

\begin{figure}[h]
\begin{code}
data Document a = Document
                  { title  :: Title
                  , uri    :: URI
                  , custom :: (Maybe a)
                  }
%//%
data Documents a = Documents
                   { idToDoc   :: (DocMap a)   
                   , docToId   :: URIMap
                   , lastDocId :: DocId     
                   }
\end{code}
\caption{Holumbus' data structure for a document and a collection of documents}
\label{fig:documents}
\end{figure}

For the creation of the index, we have to feed |Documents a| with
actual data. As mentioned before, we can read the |CurryInfo|
structure and use it in the process. The first idea is to construct a
document with |CurryInfo| as data structure for the custom
information. This idea is easy to implement since we just use the
unmodified data structure that CurryDoc produces. As consequence, all
the information in the corresponding index map to this one
document. When we search our index for an information, we can relate a
given search result only to the corresponding |CurryInfo|; we cannot
distinguish if the search result is associated to the module, function
or type information of the given |CurryInfo| structure. In order to
provide a more differentiated representation of a Curry module in the
index, we choose not to use |CurryInfo| but its substructures
|ModuleInfo|, |FunctionInfo| and |TypeInfo| as document types,
i.e. the custom information. However, if we want to distinguish
between these three sources of information, we need to store three
types of documents. Nevertheless this decision allows us to relate the
information of a Curry module to its functions, types or information
about the module itself.\todo{Not specific enough: each function and
  data structure of the module has a document, all functions are
  collected in documents as well as types} This design already determines the decision
regarding the title and the uri. The title corresponds to the name of
the function, type or module. The value of the URI is an argument to
fill by the user when generating the index; the URI can point to a
local or online source for documentation. We designed the URI
representation for the HTML-documentation provided by CurryDoc, since
the current main source for Curry documentation, that can be accessed
online\footnote{\url{http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC}},
is generated via CurryDoc. Since this HTML-structure of a Curry module
documentation provides anchors to the module's defined functions and
data structures, we use this link mechanism for our URIs as well. In
\hyperref[fig:uri]{Figure \ref{fig:uri}} you can see that modules use
the base URI given by the user, whereas functions and data structures
are build by combining the base URI, an anchor symbol and the
function's or data structure's name.\\

\begin{figure}[h]
\begin{code}

-- Function to build a document.
doc :: String -> (a -> String) -> Uri a -> a -> Document a
doc uriPath fiName uriType info = Document 
  { title  = fiName info
  , uri    = uriP
  , custom = Just info
  }
 where uriP = 
   case uriType of
     ModuleUri baseUri -> 
       uriPath ++ baseUri info ++ ".html"
     FctOrTypeUri baseUri name -> 
       uriPath ++ baseUri info ++ ".html" ++ "#" ++ name info

%//%
-- Data to represent uri,
-- functions and types use anchors, i.e. moduleName.html$$functionName.
data Uri a = 
  ModuleUri (a -> String) | 
  FctOrTypeUri (a -> String) (a -> String)
\end{code}
\caption{Function to build |Document a| and data structure to
  distinguish between an URI with or without anchors}
\label{fig:uri}
\end{figure}

% First mention that the interesting parts (that we want to add to the
% index) are already filtered by the CurryDoc part. So the indexer only
% processes the information to the structure provided by the Holumbus
% framework (HolumbusState). First the .cdoc file is read and resolved
% into the index and document structure.

% Present the output (files) the indexer produces. In addition to that, say something
% about refreshing and checking the list of modules.

% After that introduce the concept of documents and index. Mention the
% three different kinds of documents for the three structures: module,
% function, type.
% \begin{code}
% -- || Pair of index and documents of the type ModuleInfo
% type CurryModIndexerState         = HolumbusState ModuleInfo

% -- || Pair of index and documents of the type FunctionInfo
% type CurryFctIndexerState         = HolumbusState FunctionInfo

% -- || Pair of index and documents of the type TypeInfo
% type CurryTypeIndexerState        = HolumbusState TypeInfo
% \end{code}

Note the difficulties of updating the index, because the data
structure of the loaded pair of index and document differs from
]HolumbusState a. Conversion of |Inverted| to |CompactInverted| and
|Documents a| to |SmallDocuments a|.
\begin{code}
type LoadedIndexerState a = (CompactInverted, SmallDocuments a)
\end{code}

Explain how each kind of information (description, module name,
function signature etc) is combined with its context, and that these
are stored in the index. Note that these information can be extracted by the
context again. Focus on signatures and the problem of prefix search.

Refer to the appendix, where the usage of the curryIndexer is
explained.

\section{Parser}\label{implementation:parser}

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
start the process that returns the search results.

Introduce the parser type that is parametrized with the type to parse and the
resulting type.

\begin{code}
type Parser sigma alpha = [sigma] -> [(alpha, [sigma])]
\end{code}

Explain the operator |(<||>)| that applies two parsers and concats the
parsing results.  
\begin{code}
(<|>) :: Parser s a -> Parser s a -> Parser s a 
p ~<|>~ q = (\ts -> p ts ++ q ts)
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
f ~<$>~ p =  \ts -> [ (f x, ts1) | (x, ts1) <- p ts]
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
      ~<$>~ parseOpenParenthesis <*> parseExpression <*> parseCloseParenthesis)
    ~<|>~ parseExpression
\end{code}

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

\section{Web application}