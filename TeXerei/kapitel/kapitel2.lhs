\chapter{Preliminaries}\label{preliminaries}
This chapter gives a brief introduction to background information that
are necessary to comprehend the following chapters. %
The first section gives an introduction to the programming language
Curry. %
It outlines main concepts and features of the language and gives short
explanations for a better understanding. %
The Curry implementation we refer to in the following sections is
PAKCS \cite{pakcs}. Furthermore, we present CurryDoc \cite{currydoc2}, a
tool to generate documentation that is distributed with PAKCS, in the
second section. %
The last section introduces the Holumbus\footnote{\url{http://holumbus.fh-wedel.de/trac}} framework, a
library written in Haskell to configure and build search engines. %

\section{The programming language Curry}\label{preliminaries:curry}

% Say that curry is a functional logic programming language and what
% this section is about.\\

Curry is a functional logic programming language that is an
international development project to provide a platform for research
and teaching mostly. %
As the description suggest, it offers features of both programming
paradigms. %
The first subsection will start with some general features, followed
by two subsections that cover functional and logic concepts of
Curry. %

% Start with the structure of a curry program. \\
\subsection{General Overview}
Like in Haskell, a program consists of function definitions and data
structures. %
A Curry module \emph{Test} is a program that is saved in a file named
\emph{Test.curry}. %
% After that explain how a function definition looks like and how
% pattern matching works (mention left-hand and right-hand
% side). Currys layout applies to the so called off-side rule. Add
% that Curry is strongly-typed and also type-inferred, so a signature
% is optional. \\
The syntax of a program is quite similar to Haskell, where function
application are also written in juxtaposition. %

\begin{code}
addTwo x = x + 2 
\end{code}

The left-hand side of this function |addTwo x| is evaluated to the
right-hand side |x + 2|, i.e., the call |addTwo 3| yields |3 + 3 =
5|. %
Curry supports function definition with pattern matching, which is
often used in functional and logical programming languages. %
Pattern matching means that expressions with variables and data
constructors, like |True| and |False|, can occur in the arguments on
the left-hand side of a definition. %
The boolean operation |not|\footnote{Special characters are not
  allowed in Curry programs, we only use these symbols for the better
  understanding.} is a good example for a definition with pattern
matching. %
The definition distinguishes between more than one value, so we have
to write one rule for each possible input value. %

\begin{code}
not :: Bool -> Bool
not False = True
not True = False
\end{code}

The first line indicates that the function expects a boolean value as
argument and yields a boolean value as well\footnote{The symbol |->|
  will be used for \emph{->} throughout this thesis}. %
The following two lines are rules, that describe that |not False|
yields |True|, whereas |not True| yields |False|. %
There are no more possible values for the argument of the function
|not|, since Curry is a strongly-typed language and |True| and |False|
are the only possible values with boolean type. %
In addition Curry also allows polymorphic functions. %
For instance, the identity function returns the argument that you
apply to the function, regardless of the input value's type. %
This means you can apply the function to all types of values, because
the type does not matter. %
You use a type variable in your type signature to indicate a
polymorphic type. %
The following code presents the type signature of the identity
function. %

\begin{code}
id :: a -> a 
id value = value
\end{code}

Overall a strong type system as well as polymorphism and pattern
matching are among Curry's general features. %
The next section covers functional characteristics of Curry.
% Mention that there are more concepts provided by Curry, which can be
% grouped by their underlying paradigm (use it as connection to the
% following subsections). Also the evaluation models are introduced.\\

% Curry is a functional logic programming language. As the name suggests
% it offers features of both programming paradigms. This section will
% outline the basic characteristics that Curry uses and give a short
% introduction to these characteristics.

% A program written in Curry is a collection of function definitions and
% data structures, which can be organized as a module. A module with the
% name \emph{M} is saved as \emph{M.curry} and can be imported in other
% modules. 
% The syntax of function definitions is quite similar to
% Haskell. Most functions are used as prefix operators, so the
% application of a function to an expression is denoted by
% juxtaposition. Arithmetic operators like |+| or |-| are infix
% operators like in the mathematical notation. Furthermore Curry also
% supports a conditional expression that corresponds to
% \emph{if-then-else}-expressions of other programming languages.

% In general one can say that Curry combines the features of the
% functional programming language Haskell and the logic programming
% language Prolog.

% \begin{itemize}
% \item pattern matching
% \item types (success)
% \item strongly typed
% \item type inference algorithm
% \item (pakcs) lazy, but incomplete - the strategy evaluates
%   non-deterministic choices sequentially instead of concurrently
% \item statically scoped (depends on layout - also: layout/off-side
%   rule), local scope with where and let
% \end{itemize}
\subsection{Functional Features}\label{preliminaries:curryInfo}
% Begin with data structures that als can be defined by the user. Use
% CurryInfo as example.\\
The boolean values mentioned in the previous section are part of the
predefined data types. %

\begin{code}
data Bool = True | False
\end{code}

This code defines a data type with the name \emph{Bool} that has
to constructors |False| and |True| with the type |Bool|. %
In this case the constructors are unary. %
Let's define a data structure that is more interesting and will be
used in the following chapters. %
For example a data structure that represents a Curry program, because
we want to create an API search engine for Curry. %
You can save a Curry program as a module and it exists of functions
and types (data structures). %

\begin{code}
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
\end{code}

In this example \emph{CurryInfo} is the name of the type and also the
name of the constructor. %
As a constructor it needs three arguments, a |ModuleInfo| and a list
of |FunctionInfo| and |TypeInfo|. %
We define these data structures that hold the information about
specific proporties of the program in chapter 3. %

% Introduce the idea of higher-order functions.\\
Functions are first-class citizens in Curry. %
This means that they can appear as argument of an expression or even
in a data structure. %
The most popular use-case is the manipulation of all elements of a
list by a given function. %
PAKCS provides the function |map|, which also exists in Haskell, that
takes two arguments, a function and a list and returns a list. %
The important thing is that the type of the function matches the
elements of the list. %
For example, a function that converts an integer to a character can
be applied to a list of integers and yields to list of characters. %
The following code presents the a definition of the function |map|. %

\begin{code}
map :: (a->b) -> [a] -> [b]
map function [] = []
map function (element:list) = function element : map list
\end{code} 

The syntax for lists is the same as in Haskell. %
|[]| is the empty list, whereas |1:2:3:[]| is the same as |[1,2,3]| or
|1:[2,3]|. %
The first representation is often used in pattern matching, to use the
first element of the list in the right-hand side of the definition. %
The first line presents the type signature. %
The function |(a->b)| takes a value of an unspecific type and returns
another unspecific type. %
The second argument is a list of elements with the type, that the
function expects. %
Furthermore, the resulting list contains elements that have the same
type as the resulting type of the function. %
The definition of map says that an empty list yields an empty list,
whereas the function is applied to the elements of a list with at
least one element recursively. %

% Explain lazy evaluation. Mention infinite structures.
The last functional concept Curry supports is \emph{lazy
  evaluation}. %
In general an expression is evaluated by replacing the left-hand side
of a definition by the right-hand side. %
The evaluation proceeds one replacement after another until it yields
a value. %
A value is an expression that does not consists of any defined
functions anymore, e.g. literals or data structures. %
If the last replacement does not result in a value, the evaluation
fails. %
If an evaluation has more than one possible replacement step, so
called subexpression can be evaluated. %
Lazy evaluation means that such a subexpression is only evaluated, if
its result is necessary to continue the evaluation and every
expression is evaluated just once. %

In summary Curry's functional programming features covers data
structures, higher-order functions and lazy evaluation as evaluation
strategy. %
Since the logic characteristics of Curry induce that there is another
strategy besides lazy evaluation, we will go one to the next section
and take a closer look into these characteristics. %
\subsection{Logic Features}
% Start with the connection between functional and logical programming:
% introduce the logical feature of non-deterministic functions. Therefor
% give an example for a rule and later a non-deterministic function.\\

Besides the already mentioned functional characteristics, Curry also
offers non-deterministic functions and logical variables. %
Logic programming languages consist of rules, for example a constant
function that represents my favourite number. %

\begin{code}
favouriteNumber Sandra = 7
\end{code}

Let's now assume that I don't have one, but two favourite numbers. %
Curry as a hybrid of functional and logic programming language allows
multiple rules for function definitions. %

\begin{code}
favouriteNumber Sandra = 3
favouriteNumber Sandra = 7
\end{code}

This function is non-deterministic because it returns different values
for the same input. %
The pattern of this function overlaps in functional programming
languages, but Curry's ability to search for results allows to define
those non-deterministic functions. %

% After that explain how logical variables work.\\
% Give an example request with free variables. Mention the similarity
% to Prolog.
Curry also offers logical variables. %
A variable is called logical, if it appears on the right-hand side but
not on the left-hand side of a rule. %
These variables are unbound values, that are instantiated to evaluate
an expression. %
It is possible, that it exists more than one binding, since Curry
computes all possible solutions of an expression. %

% As connection to free variables, mention the evaluation methods
% ``narrowing'' and ``residuation'' next. Give an example with boolean and
% constraint equality. Connect this to flexible and rigid functions.\\

Curry provides two different approaches to evaluate an expression with
logical variables. %
The first approach suspends the evaluation in hopes that the logical
variable will be bound due to another evaluation of an expression. %
If there is no other expression to bind the value, the evaluation
fails. %
This approach is called residuation and Curry uses it for boolean
operators like the boolean equality |==|. %
The second approach, called narrowing, guesses a value for an unbound
value. %
Constraint operators like the boolean constraint |=.=| use narrowing
for evaluation. %
In this context Curry distinguishes two types of operators: flexible
operators that narrow and rigid operators that use residuation. %
For example arithmetic (i.e. |+|, |-|, |*| etc) and other primitive
operations are rigid. %
However these distinctions do not have any significance for
expressions without logical variables, so called \emph{ground
  expressions}. %
As mentioned in the previous section, Curry evaluates ground
expressions with lazy strategy.\\

Simply put, one can say that Curry combines the features of the
functional programming language Haskell and the logic programming
language Prolog. %

% \begin{itemize}
% \item Non-determinism
%   \begin{itemize}
%   \item choose
%   \item vs flexible functions
%   \item not in IO!
%   \end{itemize}
% \end{itemize}
\section{CurryDoc}\label{preliminaries:currydoc}
% Explain the idea of CurryDoc, mention the similarity to the known
% javadoc. Current status of the tool (HTML and LaTeX export).\\

CurryDoc is a tool to generate documentation for a program written in
Curry. %
The current version can generate either a HTML or \LaTeX~ file as
output. %
CurryDoc works similar to code generating tools like
javadoc\footnote{\url{http://www.oracle.com/technetwork/java/javase/documentation/index-jsp-135444.html}}
as it uses the comments in the source code, which are provided by the
user, to gain information about the function or data structure. %
It also provides the type signatures of functions, since Curry uses a
type inferencer algorithm. %
In addition the CurryDoc tool analyzes the program's structure and
approximates the run-time behavior to gain further
information \cite{currydoc2}. %
This analysis includes information about in-/completeness, overlapping
pattern matches and non-/deterministic functions. %

Since CurryDoc is implemented in Curry, it uses the meta-programming
module
\emph{Flat}\footnote{\url{http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/Flat.html}}
that provides an intermediate language of the Curry program to analyze
the special function proporties. %
Such a FlatCurry \cite{flat} program consists mainly of a list of
functions, a list of types and information about the module itself. %
% As you may notice, these are the informations we want to provide for
% the API search engine.

% Introduce FlatCurry program as utility. What is FlatCurry? Give an example of a
% Curry program and the corresponding FlatCurry program.\\

\section{The Holumbus Framework}\label{preliminaries:holumbus}
The Holumbus Framework is a Haskell library created by students of FH
Wedel in connection with three master's theses \cite{searchingthesis}\cite{indexingthesis}. %
Holumbus is a library to build and configure a search engine. %
The main idea of the framework is to collect data with a specific
structure, like an API of a programming language, and to take
advantage of this structure to improve the search results. %
In addition to the framework, they also build an example application
named \emph{Hayoo!} \cite{hayoo}, an API search engine for the
functional programming language Haskell. %

The framework supports three steps to create a search engine: the
crawling, the indexing and the searching part itself. %
We focus on the latter two steps, since we use them in our
implementation. %
The indexer preprocesses the data to create the
characteristically data structure that is used to process a search
query. %
Furthermore, Holumbus provides a data structure that represents the
results of a search query. %
This data structure corresponds to the structure of the data,
which simplifies further processing. %

\missingfigure{Holumbus structure}
\todo[noline]{ info about this structure in
  more detail}

