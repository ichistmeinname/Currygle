\begin{tikzpicture}[
    >=latex,thick,
    % minimum size=6mm,line join=round,line cap=round,
    % terminal/.style={rectangle,draw,fill=white,rounded corners=3mm},
    nonterminal/.style={rectangle,draw,join=by -},
    node distance=10mm,
    support/.style={coordinate,join=by -},
  ]

    \begin{scope}[start chain,
            every node/.style={on chain},
        ]
        \node [support] {};
        \node [support]  (first)  {};
        \node [support]  (start)  {};
        \node [nonterminal] (term) {|word|};
        \node [support] (after term) {};
        \node [support] (before bin) {};
        \node [nonterminal] (bin) {|binary operator|};
        \node [support] (end) {};
        \node [support] {};
    \end{scope}
    \node (spec)  [nonterminal,above=of term] {|specifier|};
    \node (sig) [nonterminal,below=of term] {|signature|};
    \node (bottom) [support,below=of sig] {};
    \node (top) [support, above=of spec] {};
    \node (below first) [support,below=of first] {};
    \node (above first) [support,above=of first] {};
    \node (below bbin) [support,below=of before bin] {};
    \node (above bbin) [support,above=of before bin] {};
    \node (below aterm) [support,below=of after term] {};
    \node (above aterm) [support,above=of after term] {};
    \node (below start) [support,below=of start] {};
    \node (above start) [support,above=of start] {};
    \node (above end) [support,above=of end] {};

    \begin{scope}[-,decoration={post length=4pt},rounded corners=2mm]
            % specifier path
      \draw (first) -|| (above start);
      \draw (above start)  ||-  (spec);
      \draw  (spec)  -|| (above aterm);
      \draw (above aterm) ||- (before bin);
            %signature path
      \draw (first) -|| (below start);
      \draw (below start)  ||-  (sig);
      \draw (below aterm) ||- (before bin);
      \draw (sig)    -|| (below aterm);
            % binary path
      \draw (bin) -|| (above end);
      \draw (above end)  ||-  (top);
      \draw (top) -|| (above first);
      \draw (above first) ||- (start);
            % repeat spec,sig and simple
      \draw (after term) -|| (below bbin);
      \draw (below bbin) ||- (bottom);
      \draw (bottom) -||  (below first);
      \draw (below first) ||- (start);
    \end{scope}
\end{tikzpicture}

\begin{tikzpicture}[
    >=latex,thick,
    % minimum size=6mm,line join=round,line cap=round,
    % terminal/.style={rectangle,draw,fill=white,rounded corners=3mm},
    nonterminal/.style={rectangle,draw,join=by -},
    node distance=10mm,
    support/.style={coordinate,join=by -},
  ]

    \begin{scope}[start chain,
            every node/.style={on chain},
        ]
        \node [support] {};
        \node [support]  (first)  {};
        \node [support]  (start)  {};
        \node [nonterminal] (and) {|AND|};
        \node [support] (after term) {};
        \node [support] (before bin) {};
        \node [support] (end) {};
    \end{scope}
    \node (or)  [nonterminal,above=of and] {|OR|};
    \node (not) [nonterminal,below=of and] {|NOT|};
    \node (bottom) [support,below=of not] {};
    \node (top) [support, above=of or] {};
    \node (below first) [support,below=of first] {};
    \node (above first) [support,above=of first] {};
    \node (below aterm) [support,below=of after term] {};
    \node (above aterm) [support,above=of after term] {};
    \node (below start) [support,below=of start] {};
    \node (above start) [support,above=of start] {};

    \begin{scope}[-,decoration={post length=4pt},rounded corners=2mm]
            % specifier path
      \draw (first) -|| (above start);
      \draw (above start)  ||-  (or);
      \draw  (or)  -|| (above aterm);
      \draw (above aterm) ||- (before bin);
            %signature path
      \draw (first) -|| (below start);
      \draw (below start)  ||-  (not);
      \draw (below aterm) ||- (before bin);
      \draw (not)    -|| (below aterm);
            % repeat spec,sig and simple
      \draw (after term) -|| (below bbin);
      \draw (below bbin) ||- (bottom);
      \draw (bottom) -||  (below first);
      \draw (below first) ||- (start);    \end{scope}
\end{tikzpicture}
