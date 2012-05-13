%PAKCS1.10 sicstus4 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('CurryDocRead').
:-importModule('FlatCurry').
:-importModule('Prelude').
:-importModule('CurryDocParams').
:-importModule('FlexRigid').
:-importModule('Char').
:-importModule('AnaCompleteness').


:-curryModule('CurryDocCDoc').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('CurryDocCDoc.generateCDoc',generateCDoc,4,'CurryDocCDoc.generateCDoc',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'TCons'('Prelude.IO',['TCons'('Prelude.(,)',['TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.(,,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])])])])])])])))))).
functiontype('CurryDocCDoc.generateCDoc\'2E_\'23lambda1','CurryDocCDoc.generateCDoc._#lambda1',3,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('FlatCurry.Prog',[]),'TCons'('Prelude.IO',['TCons'('Prelude.(,)',['TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.(,,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])])])])])])]))))).
functiontype('CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2','CurryDocCDoc.generateCDoc._#lambda1._#lambda2',1,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])])]))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
:-block 'CurryDocCDoc.generateCDoc'(?,?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc'(A,B,C,D,E,F,G):-makeShare(H,I),hnf('Prelude.cond'('Prelude.letrec'(I,'FlatCurry.flatCurryFileName'(A)),'Prelude.>>'('Prelude.$'(partcall(1,'Prelude.putStrLn',[]),'Prelude.++'(['^R','^e','^a','^d','^i','^n','^g','^ ','^F','^l','^a','^t','^C','^u','^r','^r','^y','^ ','^p','^r','^o','^g','^r','^a','^m','^ ','^"'],'Prelude.++'(I,['^"',^.,^.,^.]))),'Prelude.>>='('FlatCurry.readFlatCurryFile'(I),partcall(1,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1',[C,B])))),E,F,G).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1'(?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1'(A,B,C,D,E,F):-hnf(C,G,E,H),'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_3'(G,A,B,D,H,F).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_3'(?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_3'('FlatCurry.Prog'(A,B,C,D,E),F,G,H,I,J):-!,hnf('Prelude.$'(partcall(1,'Prelude.return',[]),'Prelude.(,)'(B,'Prelude.(,,)'(A,'CurryDocRead.splitComment'(F),'Prelude.map'(partcall(1,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2',[]),G)))),H,I,J).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_3'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2'(?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2'(A,B,C,D):-hnf(A,E,C,F),'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_1'(E,B,F,D).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_1'(?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_1'('Prelude.(,)'(A,B),'Prelude.(,)'(A,'CurryDocRead.splitComment'(B)),C,C):-!.
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-costCenters(['']).




%%%%% Number of shared variables: 1
