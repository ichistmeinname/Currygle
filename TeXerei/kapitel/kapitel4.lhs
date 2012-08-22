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

\section{CurryDoc Extension}\label{implementation:currydoc}

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

\begin{code}
data CurryInfo = 
  CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
\end{code}

We already introduced \emph{CurryInfo} as structure for a Curry
program in the \hyperref[preliminaries:currydoc:curryInfo]{second
  chapter}. %
As next step we want to describe |ModuleInfo|, |FunctionInfo| and
|TypeInfo|, since they are a part of the |CurryInfo| data structure. %
% You can take a look at these data structures and their definitions in
% advance in \hyperref[fig:curryInfo]{Figure \ref{fig:curryInfo}}.

% \begin{figure}[h]
% \begin{code}
% -- || The CurryInfo data holds information about the module, and
% --  corresponding  functions, data and type declaration of a given 
% --  Curry module.
% data CurryInfo = 
%   CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
% %//%
% -- || ModuleInfo holds information about the name, author,
% --    and the description of a given module.
% data ModuleInfo = 
%   ModuleInfo String String String
% %//%
% -- || FunctionInfo holds information about the name, signature, corresponding 
% -- module, description and flexible/rigid status of a function and 
% -- its non-/deterministic behaviour.
% data FunctionInfo = 
%   FunctionInfo String (QName, TypeExpr) String String Bool
%   FlexRigidResult
% %//%
% -- || TypeInfo holds information about the name, signature, type variables,
% -- corresponding module, and description of a given type.
% data TypeInfo = 
%   TypeInfo String [(QName, [TypeExpr])] [Int] String String
% \end{code}
% \caption{The data structures representing a Curry program}
% \label{fig:curryInfo}
% \end{figure}

\begin{code}
data ModuleInfo = 
  ModuleInfo String String String
\end{code}

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

\begin{code}
data FunctionInfo = 
  FunctionInfo String (QName, TypeExpr) String String Bool
  FlexRigidResult
\end{code}

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

\begin{code}
data TypeInfo = 
  TypeInfo String [(QName, [TypeExpr])] [Int] String String
\end{code}

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

\section{CurryIndexer Implementation}\label{implementation:index}

This section illustrates how to create the index for our search
engine. % In the following we refer to this process as indexer. %
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

\subsection{Index Construction}
As mentioned before, the main idea behind the index data structure is
to manage pairs of |String|. %

\begin{code}
type Pair     = (Context, Word)
type Word     = String
type Context  = String
\end{code}

The first entry describes the context and the second entry stands for
the actual word, we want to store in the index. %
Both are represented as |String|.%
In the background, Holumbus data structure |Inverted| maps the words
to their location, i.e. the document. %

\begin{code}
type Inverted    = Map Pair Occurrence
type Occurrence  = Map DocId Positions
type Positions   = IntSet
type DocId       = Int
\end{code}

More precisely, |Inverted| maps the word and its context to an
|Occurrence|, which on the other hand maps a reference to a document
(|DocId|) and its |Positions|. %
With these |Positions| that consist of a set of |Int|, we can
reconstruct a phrase of document. %
However, we only need these |Positions|, if we want to provide phrase
queries. %
Since our main goal is to provide the search signature or function,
module and types name, we decided against the support of phrase
queries. %
Therefore, we do not use the |Position| when indexing our data. %
The code examples just illustrate a sketch of the idea behind the
|Inverted| structure, the actual implementation does not matter for
our implementation, since we mostly use the provided interface. %
Furthermore the words are stores in prefix tree, which only allows
prefix search like we mentioned before. %

% \begin{figure}[h]
% \begin{code}
% type Inverted   = Map Pair Occurrence
% type Occurrence = Map DocId Positions
% type Positions  = IntSet
% type DocId      = Int
% type Pair       = (Context, Word)
% type Word       = String
% type Context    = String
% \end{code}
% \caption{Sketch for |Inverted| in Haskell syntax}
% \label{fig:inverted}
% \end{figure}

Due to the prefix search, we came across some difficulties which we
discuss later. %
Nevertheless, when we implement the indexing, we can make use of a function
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
description; therefore we have our first contexts, named
\emph{\textss{module}} for the name, \emph{\textss{author}} and
\emph{\textss{description}} (see \hyperref[t:modcontext]{Table
  \ref{t:modcontext}}). %
We do not have much to do for a module's name, but since a module can
be written by several authors, we have to add a context for each
author stored in |ModuleInfo|. %

\begin{table}[h]
\centering{
\begin{tabular}{||l||l||l||}
\hline \multicolumn{2}{||c||}{ModuleInfo} \\
\hline property & context name \\
name & "module''\\
author & "author''\\
description & "description"\\
\hline
\end{tabular}
}
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
In this way, each suffix is paired with the context
\emph{\textss{signature}}, converted into a type signature represented
as |String| and added to the index. The conversion function takes a
list of types represented as |String|s, concatenate this list with the
function arrow |->| between each element of the list and yields a
|String|. %
Since |TypeInfo| stores a list of |TypeExpr| representing its
constructors' type signatures, we have to apply the mentioned
mechanism to all |TypeExpr|. %
Additionally we have to add the constructor name to the index
manually, because it is not part of the |TypeExpr|, since its usage is
more similar to a function than to a type. %
\hyperref[t:typcontext]{Table \ref{t:typcontext}} summarizes the
contexts of |TypeInfo|, whereas there a still contexts of
|FunctionInfo| left to be discussed. %

\begin{table}[h]
\centering{
\begin{tabular}{||l||l||}
\hline \multicolumn{2}{||c||}{TypeInfo} \\
\hline property & context name \\
\hline name & "type''\\
 corres. module & "inModule''\\
 signature & "signature"\\
 description & "description"\\
\hline
\end{tabular}
}
\caption{The contexts for a |TypeInfo| data structure}
\label{t:typcontext}
\end{table}

We distinguish between non/-deterministic and flexible or rigid
functions. %
For each characteristic that applies to a function, we add the given
context to the index. %
In case of a deterministic and flexible function, we
add |("flexible"," ")| and |("det"," ")|. %
The summary of all contexts concerning a function is shown in
\hyperref[t:funcontext]{Table \ref{t:funcontext}}.

\begin{table}[h]
\centering{
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
}
\caption{The contexts for a |FunctionInfo| data structure}
\label{t:funcontext}
\end{table}

Summing up, we process |CurryInfo|'s substructures |ModuleInfo|,
|FunctionInfo| and |TypeInfo| to pair their information with a
context. %
The last \emph{unknown value} of the indexing process is the third
entry of the triple: the reference to the document. %
Thus, as next step we examine the design of a document and in this
context, we discuss the document's role and value as part of the
triple.

\subsection{Document Construction}

% At first, we take closer look at the data structure itself. %
% Holumbus provides us with |Documents a| can be described as mapping of
% yet another data structure |Document a| and an unique identifier (see
% \hyperref[fig:documents]{Figure \ref{fig:documents}}). %
% Each |Document a| consists of a title, an URI and customizable
% information. %
% The latter has the type |a| and determines the type for a document. %
% In order to have a faster access to a specific document, |Documents a|
% also stores the highest document id used in the mapping as well as a
% mapping from the document's uri to the document's identifier.

% \begin{figure}[h!]
% \begin{code}
% data Document a = Document
%                   { title  :: Title
%                   , uri    :: URI
%                   , custom :: (Maybe a)
%                   }
% %//%
% data Documents a = Documents
%                    { idToDoc   :: (DocMap a)   
%                    , docToId   :: URIMap
%                    , lastDocId :: DocId     
%                    }
% \end{code}
% \caption{Holumbus' data structure for a document and a collection of documents}
% \label{fig:documents}
% \end{figure}

At first, we take closer look at the data structure itself. %

\begin{code}
data Documents a = Documents
                   { idToDoc   :: (DocMap a)   
                   , docToId   :: URIMap
                   , lastDocId :: DocId     
                   }
\end{code}

Holumbus provides us with |Documents a| that can be described as
mapping of yet another data structure |Document a| and an unique
identifier. %
Each |Document a| consists of a title, an URI and customizable
information. %
The latter has the type |a| and determines the type for a document. %

\begin{code}
data Document a = Document
                  { title  :: Title
                  , uri    :: URI
                  , custom :: (Maybe a)
                  }
\end{code}
                
In order to have a faster access to a specific document, |Documents a|
also stores the highest document id used in the mapping as well as a
mapping from the document's uri to the document's identifier. %

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

\begin{code}
data URI a = 
  ModuleURI (a -> String) | 
  FctOrTypeURI (a -> String) (a -> String)
\end{code}

The data structure |URI a| can describe an URI for a module and a type
or function. %
The first URI constructor takes a function that yields the module's
name for a |ModuleInfo| structure. %
The latter constructor takes two functions, the first to yield the
name of a function's or type's corresponding module and the second to
yield the function's or type's name. %
The function |uriTypeToUriPath :: String -> URI a -> a -> String| distinguishes
between the two constructors to build the URI and combines it with the URI
path given by the user.

\begin{code}
ModuleURI baseURI -> 
       uriPath ++ baseURI info ++ ".html"
\end{code}

In the case of an URI for a module, we just combine the URI path with
the module's name and add the \emph{.html} extension.

\begin{code}
FctOrTypeURI baseURI name -> 
       uriPath ++ baseURI info ++ ".html" ++ "#" ++ name info
\end{code}

On other hand, we construct the URI as above, add the anchor symbol
and the function's and types' name to generate the URI linking to the
documentation.
     
% \begin{figure}[h]
% \begin{code}
% doc :: String -> (a -> String) -> URI a -> a -> Document a
% doc uriPath fiName uriType info = Document 
%   { title  = fiName info
%   , uri    = uriP
%   , custom = Just info
%   }
%  where uriP = 
%    case uriType of
%      ModuleURI baseURI -> 
%        uriPath ++ baseURI info ++ ".html"
%      FctOrTypeURI baseURI name -> 
%        uriPath ++ baseURI info ++ ".html" ++ "#" ++ name info

% %//%
% data URI a = 
%   ModuleURI (a -> String) | 
%   FctOrTypeURI (a -> String) (a -> String)
% \end{code}
% \caption{A function to build |Document a| and the data structure to
%   distinguish between an URI with or without anchors}
% \label{fig:uri}
% \end{figure}


\subsection{Conclusion and Example}
All in all, we have a mechanism to create an index data structure that
is traversed, when we perform a search query and one mechanism to
store the corresponding document, that is linked to the index
structure again. %
We create these structures for the three substructures of |CurryInfo|
and combine them into a |HolumbusState a|, where |a| defines the type
of the document. %
In the end, each of the three |HolumbusState a| data structures are
written into a two files (separating index and document structure
again); these files serve aus our index. \\

Since we do not want or search engine to be build on just one Curry
program, we update the index with new data. %
We already adressed the problem concerning lazy evaluation, but we ran
into another problems as well. %
When we write the |HolumbusState a| structure into a file, the data is
compressed into structures named |CompactInverted| and |SmallDocuments
a|. %
This means, when we load our files again, the index as well as the
document structures does not harmonize with |Inverted| and |Documents
a| anymore. %
We solve this problem by creating the |Inverted| and |Documents a| as
usually and as second step, we convert |Inverted| into
|CompactInverted| and |Documents a| into |SmallDocuments a|. %
We cannot merge the structs just yet, since we created new documents
with identifiers starting with |1|. %
We need to adapt the |DocId|s such that the minimum of the new
structure is the maximum of the old one. %
After that, we can merge the documents and index structure and write
the new file. \\

The following code serves as showcase for an index construction with
the information of a function. %
In the end we build a |HolumbusState FunctionInfo| structure. %


\begin{code}
fctState = ixDoc 
                  contextsF                     
                  (functionInfos curryDoc) 
                  (map (doc uriPath fName (FctOrTypeURI fModule fName)) 
                    $ functionInfos curryDoc)
                  cFct
\end{code}

We have a |CurryInfo| structure named |curryDoc| and extract the
|FunctionInfo| structure through |functionInfos curryDoc|.

Next, we construct the pairs of contexts and words that we need
for the |Inverted| index structure; this behaviour is described by the
function |contextsF|.

\begin{code}
contextsF functionInfo =
  [(":function", fName functionInfo)] 
   ++ [(":inModule", fModule functionInfo)]
   ++ (signature $ signatureComponents $ fSignature functionInfo)
   ++ (flexRigid $ fFlexRigid functionInfo)
   ++ (nonDet $ fNonDet functionInfo)
   ++ (description $ fDescription functionInfo)
\end{code}

The function |description| just pairs the
context with the given the data of |FunctionInfo|; %
|flexRigid| and |nonDet| do the same with the addition that they first
check, which context to apply; %
|signatureComponents| is the function that deconstructs a type
signature into valid suffixes and |signature| pairs all these suffixes
with the context. %

Since we are just building tuples, we add the reference to the
document as |Occurrences| to construct the required triple. %
We gain the |DocId| to build |Occurrences| when we insert a document
into a new |Documents a| structure. %
Therefore, we take a look at the document construction first. %

\begin{code}
doc uriPath fiName uriType info = 
  Document 
  { title  = fiName info
  , uri    = uriTypeToUriPath uriPath uriType info
  , custom  = Just 
  }
\end{code}

The URI path is given by the user, the title of the document is the
function's name, which can be extracted by the provided function and
the |URI a| data structure determines the final URI. %

\begin{code}
(map (doc uriPath fName (FctOrTypeURI fModule fName)) 
  $ functionInfos curryDoc)
\end{code}

In our code, we create a document structure for all the functions in
|CurryInfo| and work a list of |Document FunctionInfo| in the further
process. %
Now we insert each document to a new |Documents FunctionInfo|
structure.

\begin{code}
insertDoc documents document :: (DocId, Documents a)
\end{code}

We use the |DocId| to add the |Occurrences| to each pair of our
contexts to gain the list of triples.

\begin{code}
contexts info docId = map (\(c, w) -> (c, w, occ docId)) (contextsF info)
\end{code}

Luckily, the |HolIndex| interface provides a function to build an
|Inverted| structure from a list of these triples. %
In this example, we create a new index, therefore we provide the empty
index as basis. %

\begin{code}
idx = fromList emptyInverted contextsF
\end{code}

In the end, for each function we construct the corresponding |Document
FunctionInfo|, insert it into a new |Documents Function| structure,
pair the contexts with the information about the function and add the
obtained |DocId| to gain the |Inverted| index. %
Then we merge one index after another and also insert new
corresponding |Document FunctionInfo| structures to |Documents
FunctionInfo|. %

\begin{code}
ixDoc contextList (info:infos) (doc1:docs) (IndexerState ix dc) = 
  let (docId, docs') = insertDoc dc doc1
      idx'           = mergeIndexes ix $ idx contextList info docId    
  in ixDoc contextList infos docs (makeIndexerState idx' docs')
\end{code}

Since we can also merge the new created |HolumbusState FunctionInfo|
with existing data, we pass an empty |HolumbusState FunctionInfo| when
constructing a new index. %
In our example, we replace the parameter |contextList| with
|contexts|, |(info:infos)| with the given |FunctionInfo| structures,
|(doc1:docs)| are the corresponding |Document FunctionInfo| structures
and |IndexerState ix dc| is an empty |HolumbusState FunctionInfo|. %

\begin{code}
ixDoc _ _ _ hs = hs
\end{code}

If there are no more functions left to process, we return the
constructed |HolumbusState| structure. \\

In the end there are one requirement and two ways to build an index. %
The requirement is the generation of a \emph{.cdoc}-file for at least
one Curry program. %
Then you can start the indexing for the given file and the URI to the
corresponding documentation to either create a new index or to update
an existing one. %
We also provide a mechanism to read out a \emph{.txt}-file consisting
of pairs of paths to \emph{.cdoc}-files and the corresponding URI. %
Further information about the usage is provided in
\hyperref[a:currysearch]{Appendix \ref{a:currysearch}}. %

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

% Refer to the appendix, where the usage of the curryIndexer is
% explained.
 
\section{Parser}\label{implementation:parser}

In this section we discuss the general idea of a parser, connect this
idea with our search engine and develop a simple parser as example. %
The example addresses the problem of parsing expressions that can
occur with and without parentheses. %
We choose this example, because we need such a parser for the user
queries in our search engine, too. %
Furthermore, we introduce
\emph{Parsec}\footnote{\url{http://hackage.haskell.org/package/parsec}}\footnote{\url{http://legacy.cs.uu.nl/daan/parsec.html}},
a Haskell library to build a strong and fast parser. %
In this context, we present some code examples of our implementation
and outline the advantages of using Parsec as well as some
problems that occurred during the development. %

\subsection{General Idea and Usage}

A parser is used to analyze a given input and compose a new data
structure depending on the information of the input. %
Parsing is an important topic among functional programmers and hence
many papers discuss the development of parsers. %
The most popular approaches use monadic parsers \cite{monpars} since
they can parse context-sensitive grammers, but there also alternatives
using higher-order functions \cite{funcpar}. %

In general, we want the parser to take an input value and transform it
into another structure. Thus, a parser can be described as
follows:

\begin{code}
type Parser sigma alpha = sigma -> alpha
\end{code}

With the type definition above, we can run one parser on a given
input. %
The main idea behind parsing is to apply several parsers and combine
the results. %
In order to do that, the parser type needs a pair consisting of the
parsing result and the rest of the input, which was not parsed. %
Additionally, we need to consider that the parsing of an input is
ambiguous. %
This means, there can be more than one way to parse the input and it
is also possible that the input can be parsed at all. %
Thus, we extend the result type to a be a list of pairs, representing
the different parsing combinations or the empty list, if the parser
fails. %
This approach of lists as success values was introduce by Philip
Wadler\cite{successlist}.

\begin{code}
type Parser sigma alpha = [sigma] -> [(alpha, [sigma])]
\end{code} 

Summing up, a parser is parametrized with the type to parse, in the
following used as |sigma|, and the resulting type |alpha|. %
The parser takes a list of |sigma| and returns a list of
possible parsed structures and rest strings. %
These structures are pairs, where the first entry is the composed
result and the second entry represents the remaining input. %
An empty list denotes failure, whereas a non-empty list stands for
success. %

% Explain the general idea of parsers: a parser is used to analyze the
% user query with these different kinds of information. While parsing
% the string, a new data structure is composed for further use. This
% data structure is provided by the Holumbus framework and is used to
% start the process that returns the search results.

% Introduce the parser type that is parametrized with the type to parse and the
% resulting type.

In order to make use of more than one parser, we need parser
combinators. %
At first, we take a look at combinator for alternatives (also: choice combinator). %

% Explain the operator |(<||>)| that applies two parsers and concatenates the
% parsing results.  

\begin{code}
(<|>) :: Parser sigma alpha -> Parser sigma alpha -> Parser sigma alpha 
p ~<|>~ q = (\ts -> p ts ++ q ts)
\end{code}

For a given input, we apply parser |p| first and concatenate the
resulting list with the result of the application of parser |q| to
the input. %
We get all possible combinations that can be parsed by parser |p| and
parser |q|. %

Additionally, we want to combine a sequence of parsers. %
There are three possibilities to combine two parses, the first two are
similar to the monadic bind operator |>>| and the third one follows
the same idea as |>>=| . %
We start with the latter combinator. The operator allows us to
sequence the parser and combine their results.

% Introduce the combination of two parsers that is similar to a monadic
% bind operator. 
\begin{code}
(<*>) :: Parser sigma (alpha -> beta) -> Parser sigma alpha -> Parser sigma beta
p <*> q = \ts -> [ (f x, ts2) | (f, ts1) <- p ts, (x, ts2) <- q ts1 ]
\end{code}

At first, we apply the input to parser |p| and gain a tuple consisting
of |f :: alpha -> beta| and the rest of the input |ts1 :: sigma|. %
Then we apply parser |q| on the rest input, which results to a further
tuple |(x, ts2) :: (alpha, sigma)|. %
Finally, we apply the gained function |f x :: beta| and return it as
pair with the remaining input |ts2|.

In order to actually use the binding operator, we have to apply a
function that determines the conversion. %

% Now show that a function has to be applied, to use the binding
% operator.

\begin{code}
(<$$>) :: (alpha -> beta) -> Parser sigma alpha -> Parser sigma beta
f ~<$$>~ p =  \ts -> [ (f x, ts1) | (x, ts1) <- p ts]
\end{code}

This function runs the parser |p| for a given input, applies the given
function to the first entry of the resulting pair and returns the
modified result. %

We define the other two sequence combinators to show the interaction
of the two functions |<*>| and |<$$>|. %
The other twos equence combinators also take two parsers, but discard
one parser's result and return the other. %

\begin{code}
(*>) :: Parser sigma alpha -> Parser sigma beta -> Parser sigma beta
p ~*>~ q = (\_ qResult -> qResult) ~<$$>~ (\ts -> (p <*> q) ts)
\end{code}

This function discards the result of the first parser and returns the
result of the second. %
Whereas the next function works the other way around: the second
result is discarded and we return the result of the first parser.

\begin{code}
(<*) :: Parser sigma alpha -> Parser sigma beta -> Parser sigma alpha
p ~<*~ q = (\pResult _ -> pResult) ~<$$>~ (\ts -> (p <*> q) ts) 
\end{code}


These function illustrate the fundamentals to build effictive
parsers. %
In order to get a better image of the usage, we present a small
example. %
In most cases you parse |String| as input, so we get the following type for our
parser. %

\begin{code}
type Parser alpha = String -> [(alpha, String)]
\end{code}

At first, we want to construct a parser to read a character and return
it in the resulting pair. %
Otherwise the parser returns an empty list for an empty |String| as
input. %

\begin{code}
parsePredicate :: (Char -> Bool) -> Parser Char
parsePredicate predicate =
  (\(t:ts) -> if predicate t then [(t, ts)] else [([], t:ts)])
parsePredicate _ = \ts -> []
\end{code}

With this function we can define simple parser like |parseAny| that
parses any alphanumeric characters, %

\begin{code}
parseDigit :: Parser Char
parseDigit = parseSymbol isAlphaNum
\end{code}

or |parseT| that parses the character |t|. %

\begin{code}
parseT :: Parser Char
parseT = parseSymbol (t ==)
\end{code}

One of the reasons we need to parse the user queries for our search
engine are signatures. %
Since Curry is a functional languages, the most important search terms
are the functions' names and functions' type signatures. %
Like we mentioned before, we especially want to parse redundant
parenthesized signatures or partial queries. %
The following code illustrates how to parse expressions that can occur
with or with parentheses. %
In this example, an expression a sequence of alphanumeric characters. %

% Present the concept by writing a parser for expressions with or without
% parentheses using |<||>| and |<*>|. `

With the same schema as above, we can construct a parser that reads an
open parenthesis and a parser that reads a close parenthesis. %

\begin{code}
parseOpen :: Parser Char
parseOpen =  parsePredicate ('(' ==)

parseClose :: Parser Char
parseClose =  parsePredicate (')' ==)
\end{code}

Next, we need to defined a parser for the expressions. %
Since we already have a parser for characters and we want to parse the
a |String|, the main idea is to parse a sequence of characters,
i.e. to combine a sequence of |parsePredicate|. %

\begin{code} 
parseExpression :: Parser String
parseExpression = 
  \ts -> (\(c,cs) -> c:cs) ~<$$>~  (parseAny <*> parseExpression) ts
\end{code}

For a given input |String| we run |parseAny| that reads the first
character. %
Next, |parseExpression| is executed with the remaining input, hence we
read one character after another until the parsing fails or the input
is read entirely. %
The parsed characters are then composed to a list, so all in all, the
parser returns the parsed |String|, i.e. the expression. %

Finally, we want a parser to read a parenthesized expression, but
discard the parentheses. %
\begin{code}
parseParenExpression :: Parser String
parseParenExpression = 
  \ts -> (\_ expr _ -> expr) ~<$$>~ (parseOpen <*> parseExpression <*> parseClose) ts
\end{code}

With these parser combinators, we read the open parenthesis first,
then the expression and finally the close parenthesis. %
The function to combine the three results just returns the result of
the expression. %
If we are more precisly, the parser above does not parse an expression
with \emph{or} without parentheses like we promised before. %
In order to fix the parser, we need to add the choice combinator. %
Additionally, we want to present the second implementation to discard
the read parenthesis. % 

\begin{code}
parseParenExpression2 :: Parser String
parseParenExpression2 = 
  \ts -> (parseOpen ~*>~ parseExpression ~<*~ parseClose)
   ~<|>~ parseExpression
 \end{code}

Summing up, we introduced the basic idea of parsers and implemented a
small parser that can read an expression with or without parentheses. %
In the following, we have to keep in mind that we build a strong parser
by combining several parses that parse substructures. %

\subsection{Query Parser}

In our implementation we use the Haskell library Parsec to
build the parser to read and analyze the user queries. %
The main idea of Parsec is to use parser combinators instead of parser
generators. % 
The latter can generate a parser for a given grammer, whereas the
combination of several parses correlates to the idea we presented
above. %

The Parsec library provides a set of parser combinators that look
similar to the operators we introduced above. %
Unfortunately, they do not perform the exact same task. %
Thus, we take a look at the functionality Parsec provides first. %

The choice combinator follows a deterministic predictive approach with
limited lookahead \cite{parsec1}, i.e. the parser |p ~<||>~ q| only tries the second
parser |q|, if the first parser did not consumed any input. %
In order to gain the functionality that both parsers a applied to the
input, Parsec provides the function |try| that discards the read input
a parser, if it fails during parsing. %

\begin{code}
parsePorQ :: Parser String
parsePorQ = (try p) ~<|>~ q
\end{code} 

Thus, the parser tries |p| first; if it succeeds, parser |q| is not
applied anymore; if it fails, the consumed input is discarded and
parser |q| is applied to the unmodified input. %

The sequence combinators |<*>|, |<*| and |*>| as well as the operator
|<$$>| are not part of the Parsec library. %
These functions apply to general concepts of sequening (also:
application), that are provided by the \emph{Applicative} interface
\footnote{\url{http://hackage.haskell.org/packages/archive/base/4.5.0.0/doc/html/Control-Applicative.html}}. %
The concept of applicative programming is an idea introduced by Conor
McBride and Ross Patterson \cite{applicative}. %
However, the parser type that Parsec implements is an instance of
Applicative, which means, without limitation, we can use these functions in our
implementation. \\ %

The Parsec library called our attention, since the Holumbus framework
uses the library to provide a simple parser. %
The parser handles binary operations like |AND|, |OR| and |NOT| for
user queries. %
In the \hyperref[analysis:parser]{previous chapter} we already
mentioned our requirements on a parser for user queries, therefore we
do not use Holumbus' parser, but implement our own parser. %
In \hyperref[a:syntax]{Appendix \ref{a:syntax}} we give an overview of
the language we provide for the user queries. %

The following code presentation focuses on our implementation of the
parser for signatures, since the signatures are the main reason we
decided to build a new parser. %
Furthermore, the implementation uses some special feature Parsec
provides that we want to share. \\ %

At first, we take a look at all kinds of type expression that can
compose a type signature. %

\begin{itemize}
\item type variables, i.e. |a|
\item primitive types, i.e. |Int|, |Bool|
\item type constructors, i.e. |Maybe Int|, |IO ()|
\item function types, i.e. |Int -> Bool|
\item lists, i.e. |[Int]|
\item tuples, i.e. |(String, Int)|
\end{itemize}

We build our parser for type signatures using these substructures. %
Each item on the list needs to be analyzed by its own parser and step
by step, we combine all parsers according to valid Curry syntax. \\

If you take a closer look at the our list of type expression, you see
that all identifiers that we have to parse, start with an upper-case
letter, except for type variables. %
Thus, at first, we need a parser that reads all valid identifiers. %
Luckily, Parsec provides a feature to define the tokens that are used
by the language and compose a parser for these tokens. %
Tokens are constructs like white space, comments, identifiers,
reserved words, numbers or strings. %

The module |ParsecToken| exports a function |makeTokenParser| that
takes such an language definition as returns a record with a set of
lexical parsers. %
Every lexical parser already considers surrounding white spaces, hence
we do not need to consider white spaces when we use a field of the
language record. %
The following code shows our language definition for type signatures:

\begin{code}
signatureDef = emptyDef 
  { identStart      = upper,
    identLetter     = alphaNum,
    reservedNames   =  ["AND", "NOT", "OR"]
   }
\end{code}

In order to define such a language, we take an empty definition record
and rewrite the fields we want to use. %
Identifier start with an upper case (we handle type variables
separately) and the consists of alphanumeric characters. %
The reserved names are words that are not allows to occur as name of
an type expression. %
Our parser also knows the binary operations |AND|, |NOT| and |OR| and
since they are starting with an upper character, they are potential
type expressions. %
Therefore we need the parser to fail on these reserved operations when
parsing type signatures. %

Before we examine the first substructure, we need to consider the type
of our parser. %
We are parsing a |String| into a |TypeExpr|, thus, in the definitions
we used above, the type of our parser corresponds to: %

\begin{code}
TypeExprParser = Parser String TypeExpr
\end{code}


The first substructure we want to parse is a primitive type.

\begin{code}
primParser :: TypeExprParser
primParser = prim ~<$$>~ sigIdentifier
\end{code}

For primitive types we only need to consider one identifier and |prim|
wraps this identifier into a |TCons|-structure with no type
arguments\footnote{A little reminder: |Int| => |TCons ("Prelude",
  "Int") []|}. %
In case of an n-ary type constructor, we parse at least one identifier
a white space and another type expression. %

\begin{code}
consParser :: TypeExprParser
consParser = 
  ((\constr _ expr -> cons constr expr) 
   ~<$$>~ sigIdentifier
   <*> whitespace 
   <*> sepBy1 (signatureTerm False False) whitespace)
  ~<|>~ specialConstructors
 where specialConstructors =
  (\constr -> cons constr []) ~<$$>~ (string ":" <|> string "[]" <|> string "()")
\end{code}

At first, we parse an identifier like we do for primitive types and
additionally, we need a white space and another type expression to
follow. %
The function |sebBy1| takes two parsers as arguments, where the second
parser is a separator that occurs between two tokens of the same
kind. %
At least one type signature (but no white space) needs to occur,
otherwise the parser |sepBy1| fails and altogether |consParser|
fails. %
Thus, In our case we parse at least two type expression separated by a
white space or just one type expression. %
The parser |signatureTerm| handles all the substructures we listed
above. %
The first boolean value indicates, if a constructor type may occur
without parentheses; the second |Bool| is |True|, if we are in the
process of parsing a list or tuple and |False| otherwise. %
At last, we need to consider special constructors like the list
constructors |:| and |[]| that are defined in the Prelude as well as
the unit type |()|. %


% the parser does not check if the whole string was parsed -> better user-experience

% Give a definition of the language (EBNF(?) / appendix). 

% \begin{code} 
%  query := expr [ expr ] | expr bool expr | (expr)
%  expr := (expr) | specifier | signature | string
%  bool := "AND" | "OR" | "NOT" 
%  specifier :=  ":module" [ alphaNum ] | ":signature"  [ signature ]
%  |  ":function" [ alphaNum ] | ":flexible" | ":rigid" |  ":nondet" | ":det" 
%  signature := Upper alphaNum | function | constructor 
%  function := signature "->" signature  | lower "->" signature |
%  signature "->" lower
%  constructor := Upper alphaNum signature | Upper alphaNum lower
% \end{code}

\section{Web Application}