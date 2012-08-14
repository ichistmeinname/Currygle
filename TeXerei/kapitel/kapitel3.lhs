\chapter{Analysis}\label{analysis}
This chapter looks into the requirements to build and run Curr(y)gle,
an API search engine for Curry. The first section deals with the
creation of an index, whereas the second and third section addresses
the process of searching for a query. In this context, we take a
closer look of the Holumbus framework and its features related to
searching and evaluate the criteria to accomplish a user-friendly
search mechanism. Furthermore we discuss the features Curr(y)gle
should provide to specify a search query. % Above all we need a web
% application to handle user queries, which is analyzed in the last
% section.

% Give a short summary about the following sections.\\

% What do we need to create this search engine? 
% The possibility to generate and update the index.\\
% The possibility to search for these informations in the index. At
% best: user-friendly \\

% What do we want above all?
% A web application for queries.

% \section{Currydoc}
\section{Creating the Index}
At first, we have to decide about the information we want to store in
the index. Secondly the data structure of the index and documents
provided by the Holumbus framework, that hold
these information, is presented.\\

The data we want to extract from a given Curry module includes at least
the list of defined functions and data structures. We also want to
consider the name, corresponding module and description of these
functions and data structures. Additionally general information about
the module like its name and author needs to be stored. Usually a web
crawler is applied to browse the world wide web for our data. But since Curry is
currently organized by the module documentation generated by CurryDoc,
we already have a mechanism to gain these information about a Curry
module. In fact, we even have more function related information as we
stated in the \hyperref[preliminaries:curryInfo]{previous chapter}. We
know if a function definition is non-deterministic or deterministic
and if a given function is flexible or rigid.

CurryDoc processes Curry modules and generates documentation as HTML
or \LaTeX{} output. For our index, we are not interested in any
document markup language, but the pure information about the Curry
module. This leads to the idea of generating a new readable data
structure as an extension to CurryDoc. In the process we take
advantage of the FlatCurry representation of a Curry module to access
these information we mentioned above. We discuss the actual
implementation in \hyperref[implementation:currydoc]{Section \ref{implementation:currydoc}}. But in
preparation of the next chapter, we introduce the data structure
|TypeExpr| that is provided by the Flat module (see
\hyperref[fig:typeExpr]{Figure \ref{fig:typeExpr}}).

\begin{figure}[h!]
\begin{code}
data TypeExpr =
     TVar TVarIndex                 -- type variable
   | FuncType TypeExpr TypeExpr     -- function type t1->t2
   | TCons QName [TypeExpr]         -- type constructor application
                                    -- TCons (module, name) typeargs

type QName = (String, String)     -- (module, type or constructor  name)
type TVarIndex = Int
\end{code}
\caption{FlatCurry data structure for type signatures}
\label{fig:typeExpr}
\end{figure}

|TypeExpr| represents type and function signatures. It consists of
three constructors to distinguish between a function type (|FuncType
TypeExpr TypeExpr|), a type variable (|TVar TVarIndex|), and a type
constructor application (|TCons QName [TypeExpr]|). An unary type like
|Bool| is represented as a type constructor with an empty list,
i.e. without an application to type arguments. The following code
shows some signatures and their representation in the |TypeExpr| data
structure.

\begin{figure}[h!]
\begin{code}
TCons (Prelude, Maybe) [(TCons (Prelude, Int) []), 
            (TCons (Prelude, [ ]) [(TCons (Prelude, Char))])]
\end{code}
\caption{The representation of \emph{Maybe Int String} as |TypeExpr|}
\end{figure}
\begin{figure}[h!]
\begin{code}
FuncType (TCons (Prelude, IO) [TVar 97]) (TCons (Prelude, IO)
\end{code}
\caption{The representation of \emph{IO a -> IO a} as |TypeExpr|}
\end{figure}
\begin{figure}[h!]
\begin{code}
FuncType (TCons (Prelude, Bool) []) (FuncType (TCons (Prelude, Int) []) 
  (TCons (Prelude, Int) []))
\end{code}
\caption{The representation of \emph{Bool -> Int -> Int} as |TypeExpr|}
\end{figure}

% CurryDoc uses the meta-programming language FlatCurry to gain an
% intermediate data structure. We can use this data structure for our
% purposes. But at first, we have to discuss the which information to
% provide in our data structures. We already introduced \emph{CurryInfo}
% as structure for a Curry module in the
% \hyperref[preliminaries:currydoc:curryInfo]{previous chapter}.
% As next step we want to describe the information this data
% structure holds. The information about a module covers its name,
% author and description.

% Thanks to FlatCurry, we get quite more details about functions and
% types. Besides basic information like the name, description and the
% corresponding module, we have access to function characteristics like
% its flexible or rigid status and non- or deterministic property. In addition
% FlatCurry provides a data structure |TypeExpr| to describe type
% signatures (see \hyperref[fig:typeExpr]{Figure \ref{fig:typeExpr}}),
% which we use in both data structures. All described data structures to
% provide a representation for a Curry module can be seen in
% \hyperref[fig:curryInfo]{Figure \ref{fig:curryInfo}}.

% \begin{figure}[h]
% \begin{code}
% -- || The CurryInfo data holds information about the module, and
% --  corresponding  functions, data and type declaration of a given 
% --  Curry module.
% data CurryInfo = 
%   CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]


% -- || ModuleInfo holds information about the name, author,
% --    and the description of a given module.
% data ModuleInfo = 
%   ModuleInfo String String String

% -- || FunctionInfo holds information about the name, signature, corresponding 
% -- module, description and flexible/rigid status of a function and 
% -- its non-/deterministic behaviour.
% data FunctionInfo = 
%   FunctionInfo String (QName, TypeExpr) String String Bool
%   FlexRigidResult

% -- || TypeInfo holds information about the name, signature, type variables,
% -- corresponding module, and description of a given type.
% data TypeInfo = 
%   TypeInfo String [(QName, [TypeExpr])] [Int] String String
% \end{code}
% \caption{The data structures for a Curry module}
% \label{fig:curryInfo}
% \end{figure}


% What do we need to create an index that can be used for the Curry
% search engine?\\

% First: Curry specific information (CurryDoc)\\

% First start with the idea of the extension: instead of generating a
% document markup language, generate a readable data structure.\\

% Introduce the TypeExpr data structure that is part of the FlatCurry
% feature.\\

% Second: a data structure to hold the information (index and document
% structure from the Holumbus framework)\\
After we decide about the contents of the index, we need to discuss
the storage of these information. We make use of the Holumbus
framework that provides data structures to manage the collected
data. The main idea is to use two structures to score the data.
|HolDocuments a| stores the collected data, where |a| is the type of
the document. Secondly the framework provides a data structure
|HolIndex| for the actual index that is traversed in the search
process. Simply put, the |HolIndex| stores pairs |(String, String)|,
where the second entry is the word that can be searched and the first
entry is the context of this word. An example for our API search
engine is a context \emph{description}, whose corresponding |String|
is the description text of a Curry module. This design allows the
association between the information in the index and the corresponding
document through a mapping. A further structure provided by the
framework is |HolumbusState a|: a combination of index and document,
polymorph by the data |HolDocuments a| holds. In
\hyperref[implementation:index]{Section
  \ref{implementation:index}} we illustrate the use of these data
structures in our implementation.\\

To sum up, we want to extend the current CurryDoc implementation to
generate a new readable data structure about a given Curry
module. These information cover, among other things, function and data
structure definitions, user comments and description in the source
code, and general information about the module. This new data
structure determines the type of |HolDocuments a| and forms one part
of the index-document-duo that Holumbus provides. 
% \begin{itemize}
% \item HolDocuments - Stores the documents that correspond to the
%   index. A mapping is provided.
% \item HolIndex - Data structure to store the information, that is
%   traversed in the search process.
% \item HolumbusState a - the combination of index and document,
%   polymorph by the data the HolDocuments holds.
% \end{itemize}

\section{Searching}
% How do we search for the information in the index?
After creating the skeleton for the index and its storage, we want to
actually use these information in a search query. Thus the first step
is to think about the structure of a query, in the second
step we process the query and last we need a representation of the
results of the processed query for further use.\\

Thankfully these are all features the Holumbus framework provides. At
first we take a look at the search mechanism. The data structure
|Query| (see \hyperref[analysis:query]{Figure \ref{analysis:query}})
allows to search for a word and a phrase, both case-insensitive and
case-sensitive. Since the search depends on user-input, the framework
also allows something called \emph{fuzzy search} to scan for results
with spelling errors like transposed letters. Since the index data
structure of Holumbus uses pairs of words and contexts, a special
mechanism to search for these contexts is given. Furthermore the
structure provides binary operators like |AND|, |OR| and |NOT| to
combine multiple queries.
% Holumbus provides search mechanism with a special syntax.
\begin{figure}
\label{analysis:query}
\begin{code}
data Query = Word String
           | Phrase String
           | CaseWord String
           | CasePhrase String
           | FuzzyWord String
           | Specifier [Context] Query
           | Negation Query
           | BinQuery BinOp Query Query
data BinOp = And | Or | But
\end{code}
\caption{The |Query| data structure provided by the Holumbus
  framework}
\end{figure}

% And this data structure can be processed by processQuery (Holumbus.Query.Processor).
As next step, we pass the index, document and query to the function
\emph{processQuery} that, as the name suggests, processes the
query. When processing the query, Holumbus only matches for prefixes
of the given word or phrase in a query, we need to consider this
restriction when creating our index in
\hyperref[implementation:index]{Section
  \ref{implementation:index}}. The data structure we get as return
value is shown in \hyperref[analysis:result]{Figure
  \ref{analysis:result}}. Since we make use of this structure in our
implementation, let's take a closer look of the code. |Result a|
consists of the matching documents with type |a| as well as possible
word completions. The first is represented by |DocHits a| that is a
mapping of |DocInfo a| and the contexts. On the other hand |DocInfo a|
consists of the matching document and a score. By default this score
is calculated by the number of occurrences of the search query in the
document. But Holumbus also provides a mechanism to apply a customized
ranking function to calculate the score. |WordHits| illustrates the
word completions and is represented by a mapping of the possible completions
of the given prefix in combination with its score, i.e. |WordInfo|, and the contexts.\\
% Holumbus also provides a data structure that is returned after a query

\begin{figure}[h!]
\begin{code}
data Result a  = Result        
                { docHits  :: (DocHits a)
                , wordHits :: WordHits
                }

data DocInfo a = DocInfo 
                { document :: (Document a)
                , docScore :: Score
                }

data WordInfo  = WordInfo 
                { terms     :: Terms
                , wordScore :: Score                
                }

type DocHits a         = DocIdMap (DocInfo a, DocContextHits)

type DocContextHits    = Map Context DocWordHits

type DocWordHits       = Map Word Positions

type WordHits          = Map Word (WordInfo, WordContextHits)

type WordContextHits   = Map Context WordDocHits

type WordDocHits       = Occurrences

type Score           = Float
type Terms           = [String]
\end{code}
\caption{The |Result| data structure provided by the Holumbus
  framework}
\label{analysis:result}
\end{figure}

Summing up, we have discussed the mechanism to evaluate a query with
the Holumbus framework. This includes the data structures to represent a
query, which can be processed to a data structure consisting of the
matching documents and possible word completions.
% But first the user input has to be parsed into the query structure to
% start the processing.

\section{Parsing User-Queries}
% Which criteria do we want to search for? Modules, functions, types,
% signatures, and  det./non-det., flexible/rigid functions. \\
The next question is how to construct the query for a given
user-input. At first we have to decide about the criteria users can
search for. Since the index provides the pairs of contexts and search
words, we are able to restrict the search to all these context with
the help of the |Query| data structure and the |Specifier|
constructor. This allows us to search for modules, functions,
types, signatures and all other contexts we use during the creation of
the index.\\

% First describe the idea, that the use of a specific language increases
% the usability. But it also restricts the user in her usage of the
% search engine, if this language gets more complex. So this results in
% a compromise between a simply to use language and a language that can
% be parsed.  Show the example of searching IO, where the restriction to
% modules minimizes the result.\\
The search mechanism as part of the user-experience is supposed to be
as simple as possible. The use of a specific language increases the
usability, since an expression has its explicit syntax. A good example
is the search term |''io''|, since in Curry \emph{IO} is the name for
a module, a type and a constructor. Furthermore there are many
functions in the IO module, that contain the word \emph{io}. This
means that the search for \emph{io} results in a great amount of
hits. To reduce the number of hits, we can restrict the search to
a specific context. Therefor we want to provide specifiers the user
combinats with the search term, for example
|'':function IO''| searches for \emph{IO} in the context of function
names only. But this special syntax restricts the user in the use of
the search engine, if the language gets more complex. Thus to provide
a user-friendly search engine, we have to make a compromise between a
simply to use language and a language that can be parsed.

% Set the focus on signatures. Because Hayoo! does not find signatures with
% redundant parentheses, Curr(y)gle supports parenthesized signatures
% and parenthesized query parts in general. \\
Besides these specifiers we want to parse type signatures of Curry
functions and data structures. Since Hayoo! is not able to parse
redundant parenthesized signatures, we want to address this problem
with great care. Let's assume a beginner searches for a function with
the type signature |''IO -> (IO Int)''|. The type \emph{IO Int} and
type constructors in general do not need parentheses, but as beginner
you might think they do. Thus we want to support parenthesized
signatures and parenthesized query parts in general.

Last but not least we want to provide binary conjunctions like \emph{And},
\emph{Or} and \emph{Not}. On the one hand a combination of more search
words is desirable, because popular search engines like
Google\texttrademark~ use this feature. This increases the probability
that users assume binary operations are standard features and expect
search engines to provide the conjunction of several search terms. On
the other hand if the desired result is still vague, a combination of
more search words by a disjunction \emph{Or} helps to narrow down the
search results.\\
% In addition: binary operations/conjunctions. \\
% Explain that in most cases, a combination of more search words is
% desirable, because first popular search engines like Google\texttrademark~use this
% feature so it's common knowledge (the user expects this features) and
% second it's easier to search for more search words, if the desired
% result is still vague.

In the end we want to provide an intuitive but powerful syntax for the
search engine. With specifiers to restrict the search results to a
given context and with binary operations to narrow down or extend the
contexts, we want to provide a simple language for the user
queries. Additionally type signatures should be recognized, this
includes function, construction and primitive types as well as
redundant parenthesized signatures. As the number of the supported
features increases, the query gets more complex to read. Thus to reach
this goal, we need to analyse the user input and rebuild it as an
expression of our |Query| data structure. In
\hyperref[implementation:parser]{Section \ref{implementation:parser}}
we discuss our actual implementation.

% The parser becomes a complex, but very important matter.