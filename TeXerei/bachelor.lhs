% Header mit Deklarationen
%include extras/header.lhs
%include polycode.fmt
%include greek.fmt

%format == = "=="
%format ts2
%format ts1
%format <*> = "\circledast"

\begin{document}

% Römische Nummerierung für Sonderseiten, wie Verzeichnisse und Anhang
\pagenumbering{Roman}

% Titelblatt
%include extras/titelseite.lhs

% Verzeichnisse
% Kopfzeile links Kapitel, rechts leer
\renewcommand{\chaptermark}[1]{\markboth{\thechapter\ #1}{}}
\ihead{\leftmark}
\ohead{}
%include extras/verzeichnisse.lhs

% Merke mir die römische Seitenzahl in 'roemisch' und setzte Nummeriernung 
% auf arabisch für die eigentlichen Kapitel
\newpage
\newcounter{roemisch}
\setcounter{roemisch}{\value{page}}
\pagenumbering{arabic}

% Die einzelnen Kapitel
% Kopfzeile: links Kapitel, rechts Sektion
\ihead{\leftmark}
\ohead{\rightmark}
%include kapitel/kapitel1.lhs
%include kapitel/kapitel2.lhs
%include kapitel/kapitel3.lhs
%include kapitel/kapitel4.lhs
%include kapitel/kapitel5.lhs

% Setze Numerierung wieder auf römisch zurück und setzte von oben fort
% Wert ist demnach der von 'roemisch'
% \newpage
% \pagenumbering{Roman}
% \setcounter{page}{\value{roemisch}}

% Literaturverzeichnis
\bibliography{literatur/bib}

% Appendix, falls vorhanden
% \appendix
% \input{extras/anhang}

% Eidesstattliche Erklärung
% \input{extras/eidesstattliche}

\end{document}
