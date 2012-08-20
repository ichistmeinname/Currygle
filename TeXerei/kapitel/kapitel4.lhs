\chapter{Implementation}\label{implementation}
% Mention that the implementation is done in Haskell or Curry.\\
% Give a short summary of the next sections.

This chapter presents the implementation of the search engine on the
basis of some code examples and the corresponding design ideas and
decisions. %
At first we take a look at the extension for the current version of
CurryDoc. %
In this context we illustrate the interaction between this extension
and the index creation. %
The latter determines the topic of the second section. %
We specify the index and document data in more detail and state some
difficulties that arose due to the usage of the Holumbus framework. %
The third section addresses the implemented parser to convert the user
input into a query that can be processed by Holumbus. %
We illustrate the general idea and implementation approach of a parser
to introduce into the subject. %
The last section covers features and implementations of the web
application for the search engine. %
The implementations are mostly written in the functional programming
language Haskell; we use the functional logic programming language
Curry for the CurryDoc extension only.\\

\todo[inline]{general section about the underlying environment?}

\section{CurryDoc extension}\label{implementation:currydoc}

In the previous chapter we discussed the general idea of an extension
for CurryDoc to generate a data structure. %
Later this data structure serves as source for the index creation.%
In this section we take a look at the implementation of this
extension.

% Present the general structure of the |CurryInfo| data and the
% sub-structures |ModuleInfo|, |FunctionInfo| and |TypeInfo|.
Since CurryDoc is written in Curry, we implemented our extension in
Curry as well. %
With this decision we stand to benefit from already implemented
functionalities and on the other hand, using the same programming
language simplifies the integration of our implementation with the
current CurryDoc version.\\

CurryDoc uses the meta-programming language FlatCurry to gain an
intermediate data structure. %
We can use this data structure for our purposes. %
Additionally, we can use other functions provided by CurryDoc, that
are already implemented. %
For example, CurryDoc supports a special comment syntax to annotate
the author and version of a module. %
Furthermore the arguments and the return value of a function can be
described as well as general descriptions.

But at first, we discuss which information we want to provide in our
data structures. %
We already introduced \emph{CurryInfo} as structure for a Curry
program in the \hyperref[preliminaries:currydoc:curryInfo]{second
  chapter}. %
As next step we want to describe |ModuleInfo|, |FunctionInfo| and
|TypeInfo|, since they are a part of the |CurryInfo| data structure. %
You can take a look at these data structures and their definitions in
advance in \hyperref[fig:curryInfo]{Figure \ref{fig:curryInfo}}.

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
to a Curry module. %
The main information about a module consists of its name, author and
description. %
We can also provide the version number of the implementation or the
imported modules, but we decided against it. %
The latter seems to be useless information for the search engine,
since the Curry modules are highly interrelated. %
Thus, searching for a module results in a great amount of hits, since
every correlating module will be shown as well. %
Furthermore we think the version number is not a significant
characteristic for a module. %
Therefore we decided to focus on the three mentioned proporties only.

|FunctionInfo| consists of characteristics for a given function like
the function's name and description. %
Additionally we decide to add the corresponding module to provide a
connection between the function and its module. %
This decision is based on the cause that we do not keep the
|CurryInfo| data structure as whole for the index construction, but
the three arguments consisting of the list of functions, the list of
types and the module information. %
Thanks to FlatCurry, we can also access function characteristics like
nondeterminism and determinism, along with the information if a given
function is rigid or flexible. %
Since these are important characteristics to differ between Curry
functions, |FunctionInfo| stores these information as property. %
In addition FlatCurry provides a data structure |TypeExpr| to describe
type signatures (see \hyperref[fig:typeExpr]{last chapter}),
furthermore we get the actual definition of a function. %
We use a function's type signature as part of the |FunctionInfo| data
structure, but decided against the usage of a function's definition
since we could not think of a relevant use-case for our search engine.

The data structure for types looks quite similar to |FunctionInfo|. %
|TypeInfo| consists of a type's name, description and corresponding
module. %
Since FlatCurry provides type signatures for functions, we also get
information about constructors for a given type. %
Therefore we store a list of |TypeExpr| representing the type's
constructors. %
Additionally |TypeInfo| holds a list of integer to represent possible
type variables. %
The decision to use integer corresponds to the definition of
|TypeExpr|, where type variables are represented as integer as well.\\

In the end, we feed the |CurryInfo| data structure with the specific
module, function and type information of a given Curry program and our
CurryDoc extension writes the data structure into a
\emph{.cdoc}-file. %
The final CurryDoc version allows two mechanisms to generate the
|CurryInfo| structure. %
You can generate the \emph{.cdoc}-file only or you initiate the HTML
generation, where the \emph{.cdoc}-file is also part of the output. %
In \hyperref[a:currydoc]{Appendix \ref{a:currydoc}} we provide further
instruction for the usage of CurryDoc.

Due to the similar syntax, we can use the same data structures in
Haskell as in Curry to exchange those structures. %
More precisely, we can read the \emph{.cdoc}-file within our Haskell
implementation and work with the data structure. %
In order to do that, we need a Haskell program that defines all the
data structures used in |CurryInfo| including the nested structures. %
Thus, we generate |CurryInfo| to use it as data structure in the
process of the index creation.
% The same data structure is used on the Haskell side that implements
% the search engine.

\section{Indexer}\label{implementation:index}

This section illustrates how to create the index for our search
engine. In the following we refer to this process as indexer. %
In our analysis, we discussed some data structures to handle the index
creation and the information we want to store. %
In the following, we talk about the advantages and disadvantages of
using the Holumbus framework and describe our implementation of the
index creation in more detail. %
The following implementation is done in the functional logic
programming language Haskell. %
This decision allows us to use the Holumbus framework for our purpose
and a good number of functional, pure and strict features.\\

In the previous section we already mentioned the required preparation
to create the index. %
On the Haskell side, we need to define the data structures we use to
build the |CurryInfo| structure. %
Then we can read the file produced by the CurryDoc extension for
further usage. %
Since |CurryInfo| and its substructures |ModuleInfo|, |FunctionInfo|
and |TypeInfo| already provide the information we want to store in our
index, it is not necessary to filter these structures. %
Instead, we only have to process these data to fit the data structures
provided by Holumbus. %
In the end we can either create a new index by writing each structure
to a file to store our information or update an existing index with
additional Curry modules. %
In order to do that, we load the index and document files and merge
them with new data. %
Due to lazy evaluation, we cannot read and write to the same file; it
is not assured that we finish reading before we start to rewrite the
file. %
Therefore we have to write temporary files and rename these files
afterwards to guarantee a clean outcome.

During the testing phase of the indexer, we noticed problems regarding
duplicate data. %
In particular, when we add a Curry module to the index twice, there is
no mechanism to detect the duplicated data. %
For that reason only we added a list of the modules that are stored in
the index as output file. %
So every time we update the index with a given module, we check if it
already exists in the saved list. %
We only start the processing of the data, if the module does not occur
in our list and on the other hand, we add the module's name to the
list.\\

In the previous chapter, we introduced the idea of storing two
structures: an index |Inverted| and a document |Documents a|. %
In the following, we present this idea in more detail, beginning with
the index. %

As mentioned before, the main idea behind the index data structure is
to manage pairs of |String|. %
The first entry describes the context and the second entry stands for
the actual word, we want to store in the index. %
In the background, Holumbus data structure |Inverted| maps the words
to their location, i.e. the document. %
Furthermore the words are stores in prefix tree, which only allows
prefix search like we mentioned before. %
Due to the prefix search, we came across some difficulties which we
discuss later. %
Thus, when we implement the indexing, we can make use of a function
provided by Holumbus that allows to create the index structure from a
list. %
Simply put, this list consists of a triple: the pair of |String|s and
a reference to the document we are indexing. %
When we index the |CurryInfo| structure, we examine its substructures
|ModuleInfo|, |FunctionInfo| and |TypeInfo| to gain the characteristic
information. %
In this process, all information are paired with a context and the
contexts' names correspond to the information in these
substructures. %
For instance, in |ModuleInfo| we keep the module's name, author and
description; therefore we have our first contexts, named \emph{\textss{module}} for
the name, \emph{\textss{author}} and \emph{\textss{description}} (see
\hyperref[t:modcontext]{Table \ref{t:modcontext}}). %
We do not have much to do for a module's name, but since a module can
be written by several authors, we have to add a context for each
author stored in |ModuleInfo|. %

\begin{table}[h]
\begin{tabular}{||l||l||l||}
\hline \multicolumn{2}{||c||}{ModuleInfo} \\
\hline property & context name \\
name & "module''\\
author & "author''\\
description & "description"\\
\hline
\end{tabular}
\caption{The contexts for a |ModuleInfo| data structure}
\label{t:modcontext}
\end{table}

Since we only have a representation as |String|, we need to process
the string containing the author information. %
In fact, the representation is not the main problem; the prefix search
frustrates the usage of the whole string as word for our index. %
If we search for an author named \emph{\textss{Duck}}, we will not
find \emph{\textss{Donald Duck}} since the search word
\emph{\textss{Duck}} is not a prefix. %
Therefore we have to spilt the |String| on whitespace to gain and add
all parts individually. %
A similar problem applies to the description, so we split the
description on whitespace too. %
In addition, we filter biased words that are shorter than two
characters to minimize redundant or, more precisely, useless data in
the index. %
The same preparation and context names hold for the name of a function
and its description in |FunctionInfo| and the type's name and
description in |TypeInfo|. %
Both structures hold their corresponding module's name that we want to
add to the index. %
At first we used the same context as for the name in |ModuleInfo|. %
However, we want to distinguish between a search for a module's name
and a function or data structure in a specific module. %
Therefore we decide to use two different contexts: one for
|ModuleInfo| that we already introduced and the second one in the
context of a corresponding module for a function or data structure;
for the latter we use \emph{\textss{inModule}}. %
Next we take a look at signatures, where we have the same problems due
to the prefix search. %
As first step, we convert the |TypeExpr| we store in |FunctionInfo|
and |TypeInfo| into a valid type signature that is consistent to the
Curry syntax. %
This conversion yields a |String| for further processing. %
Now let's assume we want to search for a function that yields the type
|HTMLExp|. %
The first idea is to search for \emph{\textss{:type HTMLExp}} and look
for further information in the documentation. %
The problem is, we do not want the user to look for information in the
documentation, we rather want to provide a mechanism to cover this
scenario. %
Since we provide type signatures, the user can search for |HTMLExp| in
signatures to get information about all the functions (and data
structures) that consist of the type |HTMLExp|. %
Unfortunately, in order to provide this behaviour we have to modify
the signatures too. %
Due to the prefix search we can only search type signatures that begin
with |HTMLExp|, but a function's type signature that yields the type
|HTMLExp| ends with |HTMLExp|. %
The first idea is to split a given type string like |String -> String
-> HTMLExp| on |->| add each part to the context. As first
consequence, we lose function types like in |(Int -> String) -> String
-> HTMLExp|, because we get the partitions |(Int, String), String,
HTMLExp|. %
Secondly, we cannot only add primitive or constructor types (like
|String| or |Maybe Int|) to the index, if we want to provide the
search for type signatures with at least one function arrow. %
Thus, we decided not to convert the |TypeExpr| to the corresponding
|String|, but to decompose the signature into all its valid suffixes
first.%
In this way,\todo{listToSignature?} each suffix is paired with the
context \emph{\textss{signature}}, converted into a type signature
represented as |String| and added to the index. %
Since |TypeInfo| stores a list of |TypeExpr| representing its
constructors' type signatures, we have to apply the mentioned
mechanism to all |TypeExpr|. %
Additionally we have to add the constructor name to the index
manually, because it is not part of the |TypeExpr|, since its usage is
more similar to a function than to a type. %
\hyperref[t:typcontext]{Table \ref{t:typcontext}} summarizes the
contexts of |TypeInfo|, whereas there a still contexts of
|FunctionInfo| left to be discussed. %
We distinguish between non/-deterministic and flexible or rigid
functions. %
For each characteristic that applies to a function, we add the given
context to the index. %
For example, in case of a nondeterministic and flexible function, we
add |("nondet"," ")| and |"(flexible","")|. %
The summary of all contexts


\begin{table}[h]
\begin{tabular}{||l||l||}
\hline \multicolumn{2}{||c||}{FunctionInfo} \\
\hline  property & context name \\
\hline name & "function''\\
 corres. module & "inModule''\\
 signature & "signature"\\
 description & "description"\\
 \multirow{2}{*}{flexible/rigid} & "flexible'' \\  & "rigid' \\
\multirow{2}{*}{non/-deterministic} &  "nondet'' \\ & "det'' \\
\hline
\end{tabular}
\caption{The contexts for a |FunctionInfo| data structure}
\label{t:funcontext}
\end{table}

\begin{table}[h]
\begin{tabular}{||l||l||}
\hline \multicolumn{2}{||c||}{TypeInfo} \\
\hline property & context name \\
\hline name & "type''\\
 corres. module & "inModule''\\
 signature & "signature"\\
 description & "description"\\
\hline
\end{tabular}
\caption{The contexts for a |TypeInfo| data structure}
\label{t:typcontext}
\end{table}

% \begin{figure}[h]
% \begin{code}
% contextsMod :: ModuleInfo -> DocId -> [(String, String, Occurrences)]
% contextsMod moduleI i = 
%   map (addOcc  (occ i 1))$ [(":module", mName moduleI)] 
%    ++ (author $ mAuthor moduleI)
%    ++ (description $ mDescription moduleI)
% \end{code}
% \caption{Function to create the context list for a module}
% \end{figure}

% \begin{figure}[h]
% \begin{code}
% contextsF :: FunctionInfo -> DocId -> [(String, String, Occurrences)]
% contextsF functionI i =
%   map (addOcc  (occ i 2)) $ [(":function", fName functionI)] 
%    ++ [(":inModule", fModule functionI)]
%    ++ (signature $ signatureComponents $ fSignature functionI)
%    ++ (flexRigid $ fFlexRigid functionI)
%    ++ (nonDet $ fNonDet functionI)
%    ++ (description $ fDescription functionI)
%  where flexRigid fr = case fr of
%                       KnownFlex  -> [(":flexible", "")]     
%                       KnownRigid -> [(":rigid", "")]
%                       ConflictFR -> [(":flexible", ""), (":rigid", "")]
%                       _          -> []
%        nonDet nd    = if nd then [(":nondet", "")] else [(":det", "")]
% \end{code}
% \caption{Function to create the context list for a function}
% \end{figure}

% \begin{figure}[h]
% \begin{code}
% contextsT :: TypeInfo -> DocId -> [(String, String, Occurrences)]
% contextsT typeI i = 
%   let sigPair = map (showTypeList (tName typeI 
%     ++ (varIndex $ tVarIndex typeI))) $ tSignature typeI
%   in map (addOcc  (occ i 1)) $ [(":type", tName typeI)] 
%       ++ [(":inModule", tModule typeI)]
%       ++ (concatMap signature $ map fst sigPair)
%       ++ (signature $ map snd sigPair)
%       ++ (description $ tDescription typeI)   
% \end{code}
% \caption{Function to create the context list for a type}
% \end{figure}
  
Furthermore we examine the design of a document. %
At first, we take closer look at the data structure itself. %
Holumbus provides us with |Documents a| can be described as mapping of
yet another data structure |Document a| and an unique identifier (see
\hyperref[fig:documents]{Figure \ref{fig:documents}}). %
Each |Document a| consists of a title, an URI and customizable
information. %
The latter has the type |a| and determines the type for document. %
In order to have a faster access to a specific document, |Documents a|
also stores the highest document id used in the mapping as well as a
mapping from the document's uri to the document's identifier.

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
actual data. %
As mentioned before, we can read the |CurryInfo| structure and use it
in the process. %
The first idea is to construct a document with |CurryInfo| as data
structure for the custom information. %
This idea is easy to implement since we just use the unmodified data
structure that CurryDoc produces. %
As consequence, all the information in the corresponding index map to
this one document. %
When we search our index for an information, we can relate a given
search result only to the corresponding |CurryInfo|; we cannot
distinguish if the search result is associated to the module, function
or type information of the given |CurryInfo| structure. %
In order to provide a more differentiated representation of a Curry
module in the index, we choose not to use |CurryInfo| but its
substructures |ModuleInfo|, |FunctionInfo| and |TypeInfo| as document
types, i.e. the custom information. %
However, if we want to distinguish between these three sources of
information, we need to store three types of documents. %
Nevertheless this decision allows us to relate the information of a
Curry module to its functions, types or information about the module
itself. %
Furthermore, we decide that a Curry module is converted into more than
this three documents. %
We rather want to create a document for each function and data
structure of a given Curry module plus the module's general
information. %
In the end, we trace back a search result to the exact function, data
structure or module information, since we can take advantage of the
devision of the |CurryInfo| data structure. %

This design already determines the decision regarding the title and
the URI. %
The title corresponds to the name of the function, type or module. %
The value of the URI is an argument to fill by the user when
generating the index; the URI can point to a local or online source
for documentation. %
We designed the URI representation for the HTML-documentation provided
by CurryDoc, since the current main source for Curry documentation,
that can be accessed
online\footnote{\url{http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC}},
is generated via CurryDoc. %
Since this HTML-structure of a Curry module documentation provides
anchors to the module's defined functions and data structures, we use
this link mechanism for our URIs as well. %
In \hyperref[fig:uri]{Figure \ref{fig:uri}} you can see that modules
use the base URI given by the user, whereas functions and data
structures are build by combining the base URI, an anchor symbol and
the function's or data structure's name.\\

\begin{figure}[h]
\begin{code}
doc :: String -> (a -> String) -> URI a -> a -> Document a
doc uriPath fiName uriType info = Document 
  { title  = fiName info
  , uri    = uriP
  , custom = Just info
  }
 where uriP = 
   case uriType of
     ModuleURI baseURI -> 
       uriPath ++ baseURI info ++ ".html"
     FctOrTypeURI baseURI name -> 
       uriPath ++ baseURI info ++ ".html" ++ "#" ++ name info

%//%
data URI a = 
  ModuleURI (a -> String) | 
  FctOrTypeURI (a -> String) (a -> String)
\end{code}
\caption{A function to build |Document a| and the data structure to
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
|HolumbusState a|. Conversion of |Inverted| to |CompactInverted| and
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
(<$$>) :: (a -> b) -> Parser s a -> Parser s b
f ~<$$>~ p =  \ts -> [ (f x, ts1) | (x, ts1) <- p ts]
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
      ~<$$>~ parseOpenParenthesis <*> parseExpression <*> parseCloseParenthesis)
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