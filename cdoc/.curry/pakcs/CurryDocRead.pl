%PAKCS1.10 sicstus4 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('AnaCompleteness').
:-importModule('Char').
:-importModule('FlatCurry').
:-importModule('Prelude').


:-curryModule('CurryDocRead').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('CurryDocRead.readComments',readComments,1,'CurryDocRead.readComments',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.IO',['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])])])]))).
functiontype('CurryDocRead.readComments\'2E_\'23lambda1','CurryDocRead.readComments._#lambda1',1,'CurryDocRead.readComments\'2E_\'23lambda1',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.IO',['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])])])]))).
functiontype('CurryDocRead.classifyLine',classifyLine,1,'CurryDocRead.classifyLine',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('CurryDocRead.SourceLine',[]))).
functiontype('CurryDocRead.getFirstId',getFirstId,1,'CurryDocRead.getFirstId',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.isIdChar',isIdChar,1,'CurryDocRead.isIdChar',nofix,'FuncType'('TCons'('Prelude.Char',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocRead.groupLines',groupLines,1,'CurryDocRead.groupLines',nofix,'FuncType'('TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])])]))).
functiontype('CurryDocRead.groupLines\'2EgetComment\'2E22','CurryDocRead.groupLines.getComment.22',1,'CurryDocRead.groupLines\'2EgetComment\'2E22',nofix,'FuncType'('TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts','CurryDocRead.groupLines._#selFP2#modcmts',1,'CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'TCons'([],['TCons'('CurryDocRead.SourceLine',[])])]),'TCons'([],['TCons'('CurryDocRead.SourceLine',[])]))).
functiontype('CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts','CurryDocRead.groupLines._#selFP3#progcmts',1,'CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'TCons'([],['TCons'('CurryDocRead.SourceLine',[])])]),'TCons'([],['TCons'('CurryDocRead.SourceLine',[])]))).
functiontype('CurryDocRead.groupProgLines',groupProgLines,1,'CurryDocRead.groupProgLines',nofix,'FuncType'('TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]))).
functiontype('CurryDocRead.groupComment',groupComment,2,'CurryDocRead.groupComment',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])])))).
functiontype('CurryDocRead.skipFuncDefs',skipFuncDefs,2,'CurryDocRead.skipFuncDefs',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])])))).
functiontype('CurryDocRead.skipDataDefs',skipDataDefs,2,'CurryDocRead.skipDataDefs',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])])))).
functiontype('CurryDocRead.getFuncComment',getFuncComment,2,'CurryDocRead.getFuncComment',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocRead.getConsComment',getConsComment,2,'CurryDocRead.getConsComment',nofix,'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.Maybe',['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])))).
functiontype('CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname','CurryDocRead.getConsComment._#selFP8#consname',1,'CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt','CurryDocRead.getConsComment._#selFP9#rconscmt',1,'CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall','CurryDocRead.getConsComment._#selFP6#conscall',1,'CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt','CurryDocRead.getConsComment._#selFP7#callcmt',1,'CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.getDataComment',getDataComment,2,'CurryDocRead.getDataComment',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocRead.getCommentType',getCommentType,2,'CurryDocRead.getCommentType',nofix,'FuncType'(A,'FuncType'('TCons'([],['TCons'('Prelude.(,)',[A,B])]),'TCons'([],[B])))).
functiontype('CurryDocRead.getCommentType\'2E_\'23lambda2','CurryDocRead.getCommentType._#lambda2',2,'CurryDocRead.getCommentType\'2E_\'23lambda2',nofix,'FuncType'(A,'FuncType'('TCons'('Prelude.(,)',[A,B]),'TCons'('Prelude.Bool',[])))).
functiontype('CurryDocRead.splitComment',splitComment,1,'CurryDocRead.splitComment',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]))).
functiontype('CurryDocRead.splitCommentMain',splitCommentMain,1,'CurryDocRead.splitCommentMain',nofix,'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]))).
functiontype('CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt','CurryDocRead.splitCommentMain._#selFP11#maincmt',1,'CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest','CurryDocRead.splitCommentMain._#selFP12#rest',1,'CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]))).
functiontype('CurryDocRead.splitCommentParams',splitCommentParams,3,'CurryDocRead.splitCommentParams',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]))))).
functiontype('CurryDocRead.getOverlappingInfo',getOverlappingInfo,1,'CurryDocRead.getOverlappingInfo',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.Bool',[])))).
functiontype('CurryDocRead.getCompleteInfo',getCompleteInfo,1,'CurryDocRead.getCompleteInfo',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('AnaCompleteness.CompletenessType',[])))).
functiontype('CurryDocRead.getIndetInfo',getIndetInfo,1,'CurryDocRead.getIndetInfo',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.Bool',[])))).
functiontype('CurryDocRead.getOpCompleteInfo',getOpCompleteInfo,1,'CurryDocRead.getOpCompleteInfo',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.Bool',[])))).
functiontype('CurryDocRead.getFunctionInfo',getFunctionInfo,2,'CurryDocRead.getFunctionInfo',nofix,'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),A])]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),A))).
functiontype('CurryDocRead.isFunctionType',isFunctionType,1,'CurryDocRead.isFunctionType',nofix,'FuncType'('TCons'('FlatCurry.TypeExpr',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocRead.skipWhiteSpace',skipWhiteSpace,0,'CurryDocRead.skipWhiteSpace',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.isWhiteSpace',isWhiteSpace,1,'CurryDocRead.isWhiteSpace',nofix,'FuncType'('TCons'('Prelude.Char',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocRead.showId',showId,1,'CurryDocRead.showId',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.brackets',brackets,2,'CurryDocRead.brackets',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocRead.getLastName',getLastName,0,'CurryDocRead.getLastName',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0','CurryDocRead.classifyLine._#caseor0._#caseor0',4,'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('FuncType'(A,'TCons'([],['TCons'('Prelude.Char',[])])),'FuncType'(A,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('CurryDocRead.SourceLine',[])))))).
functiontype('CurryDocRead.classifyLine\'2E_\'23caseor0','CurryDocRead.classifyLine._#caseor0',3,'CurryDocRead.classifyLine\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('CurryDocRead.SourceLine',[]))))).
functiontype('CurryDocRead.groupLines\'2E_\'23caseor0','CurryDocRead.groupLines._#caseor0',4,'CurryDocRead.groupLines\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'FuncType'('TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'FuncType'('TCons'([],['TCons'('CurryDocRead.SourceLine',[])]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])])])))))).
functiontype('CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0','CurryDocRead.getConsComment._#caseor0._#caseor0',5,'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'(A,'FuncType'(B,'FuncType'(A,'FuncType'(B,'TCons'('Prelude.(,)',[A,B]))))))).
functiontype('CurryDocRead.getConsComment\'2E_\'23caseor0','CurryDocRead.getConsComment._#caseor0',6,'CurryDocRead.getConsComment\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.Maybe',['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])))))))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.
constructortype('CurryDocRead.Comment','Comment',1,'Comment',0,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('CurryDocRead.SourceLine',[]))).
constructortype('CurryDocRead.FuncDef','FuncDef',1,'FuncDef',1,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('CurryDocRead.SourceLine',[]))).
constructortype('CurryDocRead.DataDef','DataDef',1,'DataDef',2,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('CurryDocRead.SourceLine',[]))).
constructortype('CurryDocRead.ModDef','ModDef',0,'ModDef',3,'TCons'('CurryDocRead.SourceLine',[])).
constructortype('CurryDocRead.OtherLine','OtherLine',0,'OtherLine',4,'TCons'('CurryDocRead.SourceLine',[])).
constructortype('CurryDocRead.AnaInfo','AnaInfo',4,'AnaInfo',0,'FuncType'('FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.Bool',[])),'FuncType'('FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('AnaCompleteness.CompletenessType',[])),'FuncType'('FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.Bool',[])),'FuncType'('FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.Bool',[])),'TCons'('CurryDocRead.AnaInfo',[])))))).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
:-block 'CurryDocRead.readComments'(?,?,-,?).
'CurryDocRead.readComments'(A,B,C,D):-hnf('Prelude.>>='('Prelude.readFile'(A),partcall(1,'CurryDocRead.readComments\'2E_\'23lambda1',[])),B,C,D).

:-block 'CurryDocRead.readComments\'2E_\'23lambda1'(?,?,-,?).
'CurryDocRead.readComments\'2E_\'23lambda1'(A,B,C,D):-hnf('Prelude.return'('Prelude.$'('Prelude..'(partcall(1,'CurryDocRead.groupLines',[]),'Prelude..'(partcall(1,'Prelude.filter',[partcall(1,'Prelude.flip',['CurryDocRead.OtherLine',partcall(2,'Prelude./=',[])])]),'Prelude..'(partcall(1,'Prelude.map',[partcall(1,'CurryDocRead.classifyLine',[])]),partcall(1,'Prelude.lines',[])))),A)),B,C,D).

:-block 'CurryDocRead.classifyLine'(?,?,-,?).
'CurryDocRead.classifyLine'(A,B,C,D):-makeShare(E,F),makeShare(A,G),hnf('Prelude.cond'('Prelude.letrec'(F,'Prelude..'(partcall(1,'Prelude.takeWhile',[partcall(1,'CurryDocRead.isIdChar',[])]),'Prelude..'(partcall(1,'Prelude.dropWhile',[partcall(1,'Prelude.flip',['^ ',partcall(2,'Prelude.==',[])])]),partcall(1,'Prelude.dropWhile',[partcall(1,'CurryDocRead.isIdChar',[])])))),'CurryDocRead.classifyLine\'2E_\'23caseor0'('Prelude.&&'('Prelude.=='('Prelude.take'(3,G),[^-,^-,^-]),'Prelude.apply'('Prelude.all'(partcall(1,'Char.isSpace',[])),'Prelude.drop'(3,G))),F,G)),B,C,D).

:-block 'CurryDocRead.getFirstId'(?,?,-,?).
'CurryDocRead.getFirstId'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getFirstId_1'(E,B,F,D).

:-block 'CurryDocRead.getFirstId_1'(?,?,-,?).
'CurryDocRead.getFirstId_1'([],[],A,A).
'CurryDocRead.getFirstId_1'([A|B],C,D,E):-!,makeShare(A,F),hnf('Char.isAlpha'(F),G,D,H),'CurryDocRead.getFirstId_1_._ComplexCase'(G,F,B,C,H,E).

:-block 'CurryDocRead.getFirstId_1_._ComplexCase'(-,?,?,?,?,?),'CurryDocRead.getFirstId_1_._ComplexCase'(?,?,?,?,-,?).
'CurryDocRead.getFirstId_1_._ComplexCase'('Prelude.True',A,B,C,D,E):-hnf('Prelude.takeWhile'(partcall(1,'CurryDocRead.isIdChar',[]),[A|B]),C,D,E).
'CurryDocRead.getFirstId_1_._ComplexCase'('Prelude.False',A,B,C,D,E):-!,hnf('Prelude.=='(A,'^('),F,D,G),'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase'(F,A,B,C,G,E).

:-block 'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?),'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,-,?).
'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E):-hnf('Prelude.takeWhile'(partcall(1,'Prelude.flip',['^)',partcall(2,'Prelude./=',[])]),B),C,D,E).
'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E):-!,hnf('Prelude.otherwise',F,D,G),'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(F,A,B,C,G,E).

:-block 'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?),'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,-,?).
'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,[],C,C).
'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E):-!,hnf('Prelude.failure'('CurryDocRead.getFirstId',['Prelude.False']),C,D,E).
'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.getFirstId_1_._ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.getFirstId_1_._ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.getFirstId_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.isIdChar'(?,?,-,?).
'CurryDocRead.isIdChar'(A,B,C,D):-makeShare(A,E),hnf('Prelude.||'('Char.isAlphaNum'(E),'Prelude.||'('Prelude.=='(E,'^_'),'Prelude.=='(E,'^\''))),B,C,D).

:-block 'CurryDocRead.groupLines'(?,?,-,?).
'CurryDocRead.groupLines'(A,B,C,D):-makeShare(E,F),makeShare(A,G),makeShare(H,I),makeShare(J,K),hnf('Prelude.cond'('Prelude.letrec'(F,'Prelude.apply'('Prelude.break'(partcall(1,'Prelude.flip',['CurryDocRead.ModDef',partcall(2,'Prelude.==',[])])),G)),'Prelude.cond'('Prelude.letrec'(I,'CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts'(F)),'Prelude.cond'('Prelude.letrec'(K,'CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts'(F)),'CurryDocRead.groupLines\'2E_\'23caseor0'('Prelude.=='(K,[]),G,I,K)))),B,C,D).

:-block 'CurryDocRead.groupLines\'2EgetComment\'2E22'(?,?,-,?).
'CurryDocRead.groupLines\'2EgetComment\'2E22'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.groupLines\'2EgetComment\'2E22_1'(E,B,F,D).

:-block 'CurryDocRead.groupLines\'2EgetComment\'2E22_1'(?,?,-,?).
'CurryDocRead.groupLines\'2EgetComment\'2E22_1'('CurryDocRead.Comment'(A),B,C,D):-hnf('Prelude.++'(A,['^010']),B,C,D).
'CurryDocRead.groupLines\'2EgetComment\'2E22_1'('CurryDocRead.FuncDef'(A),[],B,B).
'CurryDocRead.groupLines\'2EgetComment\'2E22_1'('CurryDocRead.DataDef'(A),[],B,B):-!.
'CurryDocRead.groupLines\'2EgetComment\'2E22_1'('CurryDocRead.ModDef',A,B,C):-!,hnf('Prelude.failure'('CurryDocRead.groupLines\'2EgetComment\'2E22',['CurryDocRead.ModDef']),A,B,C).
'CurryDocRead.groupLines\'2EgetComment\'2E22_1'('CurryDocRead.OtherLine',A,B,C):-!,hnf('Prelude.failure'('CurryDocRead.groupLines\'2EgetComment\'2E22',['CurryDocRead.OtherLine']),A,B,C).
'CurryDocRead.groupLines\'2EgetComment\'2E22_1'('FAIL'(A),'FAIL'(A),B,B).

:-block 'CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts'(?,?,-,?).
'CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts_1'(E,B,F,D).

:-block 'CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts_1'(?,?,-,?).
'CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocRead.groupLines\'2E_\'23selFP2\'23modcmts_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts'(?,?,-,?).
'CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts_1'(E,B,F,D).

:-block 'CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts_1'(?,?,-,?).
'CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocRead.groupLines\'2E_\'23selFP3\'23progcmts_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.groupProgLines'(?,?,-,?).
'CurryDocRead.groupProgLines'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.groupProgLines_1'(E,B,F,D).

:-block 'CurryDocRead.groupProgLines_1'(?,?,-,?).
'CurryDocRead.groupProgLines_1'([],[],A,A).
'CurryDocRead.groupProgLines_1'([A|B],C,D,E):-!,hnf(A,F,D,G),'CurryDocRead.groupProgLines_1_._1'(F,B,C,G,E).

:-block 'CurryDocRead.groupProgLines_1_._1'(?,?,?,-,?).
'CurryDocRead.groupProgLines_1_._1'('CurryDocRead.Comment'(A),B,C,D,E):-hnf('CurryDocRead.groupComment'(A,B),C,D,E).
'CurryDocRead.groupProgLines_1_._1'('CurryDocRead.FuncDef'(A),B,['Prelude.(,)'('CurryDocRead.FuncDef'(C),[])|'CurryDocRead.skipFuncDefs'(C,B)],D,E):-makeShare(A,C),D=E.
'CurryDocRead.groupProgLines_1_._1'('CurryDocRead.DataDef'(A),B,['Prelude.(,)'('CurryDocRead.DataDef'(C),[])|'CurryDocRead.skipDataDefs'(C,B)],D,E):-!,makeShare(A,C),D=E.
'CurryDocRead.groupProgLines_1_._1'('CurryDocRead.ModDef',A,B,C,D):-!,hnf('Prelude.failure'('CurryDocRead.groupProgLines',['CurryDocRead.ModDef']),B,C,D).
'CurryDocRead.groupProgLines_1_._1'('CurryDocRead.OtherLine',A,B,C,D):-!,hnf('Prelude.failure'('CurryDocRead.groupProgLines',['CurryDocRead.OtherLine']),B,C,D).
'CurryDocRead.groupProgLines_1_._1'('FAIL'(A),B,'FAIL'(A),C,C).
'CurryDocRead.groupProgLines_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.groupComment'(?,?,?,-,?).
'CurryDocRead.groupComment'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocRead.groupComment_2'(F,A,C,G,E).

:-block 'CurryDocRead.groupComment_2'(?,?,?,-,?).
'CurryDocRead.groupComment_2'([],A,[],B,B).
'CurryDocRead.groupComment_2'([A|B],C,D,E,F):-!,hnf(A,G,E,H),'CurryDocRead.groupComment_2_._1'(G,B,C,D,H,F).

:-block 'CurryDocRead.groupComment_2_._1'(?,?,?,?,-,?).
'CurryDocRead.groupComment_2_._1'('CurryDocRead.Comment'(A),B,C,D,E,F):-hnf('CurryDocRead.groupComment'('Prelude.++'(C,'Prelude.++'(['^010'],A)),B),D,E,F).
'CurryDocRead.groupComment_2_._1'('CurryDocRead.FuncDef'(A),B,C,['Prelude.(,)'('CurryDocRead.FuncDef'(D),C)|'CurryDocRead.skipFuncDefs'(D,B)],E,F):-makeShare(A,D),E=F.
'CurryDocRead.groupComment_2_._1'('CurryDocRead.DataDef'(A),B,C,['Prelude.(,)'('CurryDocRead.DataDef'(D),C)|'CurryDocRead.skipDataDefs'(D,B)],E,F):-!,makeShare(A,D),E=F.
'CurryDocRead.groupComment_2_._1'('CurryDocRead.ModDef',A,B,C,D,E):-!,hnf('Prelude.failure'('CurryDocRead.groupComment',['CurryDocRead.ModDef']),C,D,E).
'CurryDocRead.groupComment_2_._1'('CurryDocRead.OtherLine',A,B,C,D,E):-!,hnf('Prelude.failure'('CurryDocRead.groupComment',['CurryDocRead.OtherLine']),C,D,E).
'CurryDocRead.groupComment_2_._1'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.groupComment_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocRead.skipFuncDefs'(?,?,?,-,?).
'CurryDocRead.skipFuncDefs'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocRead.skipFuncDefs_2'(F,A,C,G,E).

:-block 'CurryDocRead.skipFuncDefs_2'(?,?,?,-,?).
'CurryDocRead.skipFuncDefs_2'([],A,[],B,B).
'CurryDocRead.skipFuncDefs_2'([A|B],C,D,E,F):-!,hnf(A,G,E,H),'CurryDocRead.skipFuncDefs_2_._1'(G,B,C,D,H,F).

:-block 'CurryDocRead.skipFuncDefs_2_._1'(?,?,?,?,-,?).
'CurryDocRead.skipFuncDefs_2_._1'('CurryDocRead.Comment'(A),B,C,D,E,F):-hnf('CurryDocRead.groupProgLines'(['CurryDocRead.Comment'(A)|B]),D,E,F).
'CurryDocRead.skipFuncDefs_2_._1'('CurryDocRead.DataDef'(A),B,C,D,E,F):-hnf('CurryDocRead.groupProgLines'(['CurryDocRead.DataDef'(A)|B]),D,E,F).
'CurryDocRead.skipFuncDefs_2_._1'('CurryDocRead.FuncDef'(A),B,C,D,E,F):-!,makeShare(C,G),makeShare(A,H),hnf('Prelude.=='(G,H),I,E,J),'CurryDocRead.skipFuncDefs_2_._1_CurryDocRead.FuncDef_ComplexCase'(I,H,B,G,D,J,F).

:-block 'CurryDocRead.skipFuncDefs_2_._1_CurryDocRead.FuncDef_ComplexCase'(-,?,?,?,?,?,?),'CurryDocRead.skipFuncDefs_2_._1_CurryDocRead.FuncDef_ComplexCase'(?,?,?,?,?,-,?).
'CurryDocRead.skipFuncDefs_2_._1_CurryDocRead.FuncDef_ComplexCase'('Prelude.True',A,B,C,D,E,F):-hnf('CurryDocRead.skipFuncDefs'(C,B),D,E,F).
'CurryDocRead.skipFuncDefs_2_._1_CurryDocRead.FuncDef_ComplexCase'('Prelude.False',A,B,C,D,E,F):-!,hnf('CurryDocRead.groupProgLines'(['CurryDocRead.FuncDef'(A)|B]),D,E,F).
'CurryDocRead.skipFuncDefs_2_._1_CurryDocRead.FuncDef_ComplexCase'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocRead.skipFuncDefs_2_._1'('CurryDocRead.ModDef',A,B,C,D,E):-!,hnf('Prelude.failure'('CurryDocRead.skipFuncDefs',['CurryDocRead.ModDef']),C,D,E).
'CurryDocRead.skipFuncDefs_2_._1'('CurryDocRead.OtherLine',A,B,C,D,E):-!,hnf('Prelude.failure'('CurryDocRead.skipFuncDefs',['CurryDocRead.OtherLine']),C,D,E).
'CurryDocRead.skipFuncDefs_2_._1'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.skipFuncDefs_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocRead.skipDataDefs'(?,?,?,-,?).
'CurryDocRead.skipDataDefs'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocRead.skipDataDefs_2'(F,A,C,G,E).

:-block 'CurryDocRead.skipDataDefs_2'(?,?,?,-,?).
'CurryDocRead.skipDataDefs_2'([],A,[],B,B).
'CurryDocRead.skipDataDefs_2'([A|B],C,D,E,F):-!,hnf(A,G,E,H),'CurryDocRead.skipDataDefs_2_._1'(G,B,C,D,H,F).

:-block 'CurryDocRead.skipDataDefs_2_._1'(?,?,?,?,-,?).
'CurryDocRead.skipDataDefs_2_._1'('CurryDocRead.Comment'(A),B,C,D,E,F):-hnf('CurryDocRead.groupProgLines'(['CurryDocRead.Comment'(A)|B]),D,E,F).
'CurryDocRead.skipDataDefs_2_._1'('CurryDocRead.FuncDef'(A),B,C,D,E,F):-hnf('CurryDocRead.groupProgLines'(['CurryDocRead.FuncDef'(A)|B]),D,E,F).
'CurryDocRead.skipDataDefs_2_._1'('CurryDocRead.DataDef'(A),B,C,D,E,F):-!,makeShare(C,G),makeShare(A,H),hnf('Prelude.=='(G,H),I,E,J),'CurryDocRead.skipDataDefs_2_._1_CurryDocRead.DataDef_ComplexCase'(I,H,B,G,D,J,F).

:-block 'CurryDocRead.skipDataDefs_2_._1_CurryDocRead.DataDef_ComplexCase'(-,?,?,?,?,?,?),'CurryDocRead.skipDataDefs_2_._1_CurryDocRead.DataDef_ComplexCase'(?,?,?,?,?,-,?).
'CurryDocRead.skipDataDefs_2_._1_CurryDocRead.DataDef_ComplexCase'('Prelude.True',A,B,C,D,E,F):-hnf('CurryDocRead.skipDataDefs'(C,B),D,E,F).
'CurryDocRead.skipDataDefs_2_._1_CurryDocRead.DataDef_ComplexCase'('Prelude.False',A,B,C,D,E,F):-!,hnf('CurryDocRead.groupProgLines'(['CurryDocRead.DataDef'(A)|B]),D,E,F).
'CurryDocRead.skipDataDefs_2_._1_CurryDocRead.DataDef_ComplexCase'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocRead.skipDataDefs_2_._1'('CurryDocRead.ModDef',A,B,C,D,E):-!,hnf('Prelude.failure'('CurryDocRead.skipDataDefs',['CurryDocRead.ModDef']),C,D,E).
'CurryDocRead.skipDataDefs_2_._1'('CurryDocRead.OtherLine',A,B,C,D,E):-!,hnf('Prelude.failure'('CurryDocRead.skipDataDefs',['CurryDocRead.OtherLine']),C,D,E).
'CurryDocRead.skipDataDefs_2_._1'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.skipDataDefs_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocRead.getFuncComment'(?,?,?,-,?).
'CurryDocRead.getFuncComment'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocRead.getFuncComment_2'(F,A,C,G,E).

:-block 'CurryDocRead.getFuncComment_2'(?,?,?,-,?).
'CurryDocRead.getFuncComment_2'([],A,[],B,B).
'CurryDocRead.getFuncComment_2'([A|B],C,D,E,F):-!,hnf(A,G,E,H),'CurryDocRead.getFuncComment_2_._1'(G,B,C,D,H,F).

:-block 'CurryDocRead.getFuncComment_2_._1'(?,?,?,?,-,?).
'CurryDocRead.getFuncComment_2_._1'('Prelude.(,)'(A,B),C,D,E,F,G):-!,hnf(A,H,F,I),'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1'(H,B,C,D,E,I,G).

:-block 'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1'(?,?,?,?,?,-,?).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1'('CurryDocRead.FuncDef'(A),B,C,D,E,F,G):-makeShare(D,H),hnf('Prelude.=='(H,A),I,F,J),'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1_CurryDocRead.FuncDef_ComplexCase'(I,A,B,C,H,E,J,G).

:-block 'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1_CurryDocRead.FuncDef_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1_CurryDocRead.FuncDef_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1_CurryDocRead.FuncDef_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf(B,E,F,G).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1_CurryDocRead.FuncDef_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,hnf('CurryDocRead.getFuncComment'(D,C),E,F,G).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1_CurryDocRead.FuncDef_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1'('CurryDocRead.DataDef'(A),B,C,D,E,F,G):-!,hnf('CurryDocRead.getFuncComment'(D,C),E,F,G).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1'('CurryDocRead.Comment'(A),B,C,D,E,F,G):-!,hnf('Prelude.failure'('CurryDocRead.getFuncComment',['CurryDocRead.Comment'(A)]),E,F,G).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1'('CurryDocRead.ModDef',A,B,C,D,E,F):-hnf('Prelude.failure'('CurryDocRead.getFuncComment',['CurryDocRead.ModDef']),D,E,F).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1'('CurryDocRead.OtherLine',A,B,C,D,E,F):-!,hnf('Prelude.failure'('CurryDocRead.getFuncComment',['CurryDocRead.OtherLine']),D,E,F).
'CurryDocRead.getFuncComment_2_._1_Prelude.(,)_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocRead.getFuncComment_2_._1'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).
'CurryDocRead.getFuncComment_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocRead.getConsComment'(?,?,?,-,?).
'CurryDocRead.getConsComment'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocRead.getConsComment_1'(F,B,C,G,E).

:-block 'CurryDocRead.getConsComment_1'(?,?,?,-,?).
'CurryDocRead.getConsComment_1'([],A,'Prelude.Nothing',B,B).
'CurryDocRead.getConsComment_1'([A|B],C,D,E,F):-!,makeShare(G,H),makeShare(A,I),makeShare(J,K),makeShare(L,M),makeShare(C,N),hnf('Prelude.cond'('Prelude.letrec'(H,'Prelude.span'(partcall(1,'CurryDocRead.isIdChar',[]),I)),'Prelude.cond'('Prelude.letrec'(K,'CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname'(H)),'Prelude.cond'('Prelude.letrec'(M,'CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt'(H)),'CurryDocRead.getConsComment\'2E_\'23caseor0'('Prelude.=='(K,N),I,K,M,B,N)))),D,E,F).
'CurryDocRead.getConsComment_1'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname'(?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname_1'(E,B,F,D).

:-block 'CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname_1'(?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocRead.getConsComment\'2E_\'23selFP8\'23consname_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt'(?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt_1'(E,B,F,D).

:-block 'CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt_1'(?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocRead.getConsComment\'2E_\'23selFP9\'23rconscmt_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall'(?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall_1'(E,B,F,D).

:-block 'CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall_1'(?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt'(?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt_1'(E,B,F,D).

:-block 'CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt_1'(?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.getDataComment'(?,?,?,-,?).
'CurryDocRead.getDataComment'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocRead.getDataComment_2'(F,A,C,G,E).

:-block 'CurryDocRead.getDataComment_2'(?,?,?,-,?).
'CurryDocRead.getDataComment_2'([],A,[],B,B).
'CurryDocRead.getDataComment_2'([A|B],C,D,E,F):-!,hnf(A,G,E,H),'CurryDocRead.getDataComment_2_._1'(G,B,C,D,H,F).

:-block 'CurryDocRead.getDataComment_2_._1'(?,?,?,?,-,?).
'CurryDocRead.getDataComment_2_._1'('Prelude.(,)'(A,B),C,D,E,F,G):-!,hnf(A,H,F,I),'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1'(H,B,C,D,E,I,G).

:-block 'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1'(?,?,?,?,?,-,?).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1'('CurryDocRead.DataDef'(A),B,C,D,E,F,G):-makeShare(D,H),hnf('Prelude.=='(H,A),I,F,J),'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1_CurryDocRead.DataDef_ComplexCase'(I,A,B,C,H,E,J,G).

:-block 'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1_CurryDocRead.DataDef_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1_CurryDocRead.DataDef_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1_CurryDocRead.DataDef_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf(B,E,F,G).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1_CurryDocRead.DataDef_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,hnf('CurryDocRead.getDataComment'(D,C),E,F,G).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1_CurryDocRead.DataDef_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1'('CurryDocRead.FuncDef'(A),B,C,D,E,F,G):-!,hnf('CurryDocRead.getDataComment'(D,C),E,F,G).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1'('CurryDocRead.Comment'(A),B,C,D,E,F,G):-!,hnf('Prelude.failure'('CurryDocRead.getDataComment',['CurryDocRead.Comment'(A)]),E,F,G).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1'('CurryDocRead.ModDef',A,B,C,D,E,F):-hnf('Prelude.failure'('CurryDocRead.getDataComment',['CurryDocRead.ModDef']),D,E,F).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1'('CurryDocRead.OtherLine',A,B,C,D,E,F):-!,hnf('Prelude.failure'('CurryDocRead.getDataComment',['CurryDocRead.OtherLine']),D,E,F).
'CurryDocRead.getDataComment_2_._1_Prelude.(,)_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocRead.getDataComment_2_._1'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).
'CurryDocRead.getDataComment_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocRead.getCommentType'(?,?,?,-,?).
'CurryDocRead.getCommentType'(A,B,C,D,E):-hnf('Prelude.map'(partcall(1,'Prelude.snd',[]),'Prelude.filter'(partcall(1,'CurryDocRead.getCommentType\'2E_\'23lambda2',[A]),B)),C,D,E).

:-block 'CurryDocRead.getCommentType\'2E_\'23lambda2'(?,?,?,-,?).
'CurryDocRead.getCommentType\'2E_\'23lambda2'(A,B,C,D,E):-hnf('Prelude.=='('Prelude.fst'(B),A),C,D,E).

:-block 'CurryDocRead.splitComment'(?,?,-,?).
'CurryDocRead.splitComment'(A,B,C,D):-hnf('CurryDocRead.splitCommentMain'('Prelude.lines'(A)),B,C,D).

:-block 'CurryDocRead.splitCommentMain'(?,?,-,?).
'CurryDocRead.splitCommentMain'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.splitCommentMain_1'(E,B,F,D).

:-block 'CurryDocRead.splitCommentMain_1'(?,?,-,?).
'CurryDocRead.splitCommentMain_1'([],'Prelude.(,)'([],[]),A,A).
'CurryDocRead.splitCommentMain_1'([A|B],C,D,E):-!,makeShare(A,F),hnf('Prelude.||'('Prelude.=='(F,[]),'Prelude./='('Prelude.head'(F),^@)),G,D,H),'CurryDocRead.splitCommentMain_1_._ComplexCase'(G,F,B,C,H,E).

:-block 'CurryDocRead.splitCommentMain_1_._ComplexCase'(-,?,?,?,?,?),'CurryDocRead.splitCommentMain_1_._ComplexCase'(?,?,?,?,-,?).
'CurryDocRead.splitCommentMain_1_._ComplexCase'('Prelude.True',A,B,C,D,E):-makeShare(F,G),makeShare(H,I),makeShare(J,K),hnf('Prelude.cond'('Prelude.letrec'(G,'CurryDocRead.splitCommentMain'(B)),'Prelude.cond'('Prelude.letrec'(I,'CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt'(G)),'Prelude.cond'('Prelude.letrec'(K,'CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest'(G)),'Prelude.(,)'('Prelude.++'(A,['^010'|I]),K)))),C,D,E).
'CurryDocRead.splitCommentMain_1_._ComplexCase'('Prelude.False',A,B,'Prelude.(,)'([],'CurryDocRead.splitCommentParams'('Prelude.takeWhile'(partcall(1,'Char.isAlpha',[]),'Prelude.tail'(C)),'Prelude.dropWhile'(partcall(1,'Char.isAlpha',[]),'Prelude.tail'(C)),B)),D,E):-!,makeShare(A,C),D=E.
'CurryDocRead.splitCommentMain_1_._ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.splitCommentMain_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt'(?,?,-,?).
'CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt_1'(E,B,F,D).

:-block 'CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt_1'(?,?,-,?).
'CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocRead.splitCommentMain\'2E_\'23selFP11\'23maincmt_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest'(?,?,-,?).
'CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest_1'(E,B,F,D).

:-block 'CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest_1'(?,?,-,?).
'CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocRead.splitCommentMain\'2E_\'23selFP12\'23rest_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.splitCommentParams'(?,?,?,?,-,?).
'CurryDocRead.splitCommentParams'(A,B,C,D,E,F):-hnf(C,G,E,H),'CurryDocRead.splitCommentParams_3'(G,A,B,D,H,F).

:-block 'CurryDocRead.splitCommentParams_3'(?,?,?,?,-,?).
'CurryDocRead.splitCommentParams_3'([],A,B,['Prelude.(,)'(A,'Prelude.apply'('CurryDocRead.skipWhiteSpace',B))],C,C).
'CurryDocRead.splitCommentParams_3'([A|B],C,D,E,F,G):-!,makeShare(A,H),hnf('Prelude.||'('Prelude.=='(H,[]),'Prelude./='('Prelude.head'(H),^@)),I,F,J),'CurryDocRead.splitCommentParams_3_._ComplexCase'(I,H,B,C,D,E,J,G).

:-block 'CurryDocRead.splitCommentParams_3_._ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocRead.splitCommentParams_3_._ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocRead.splitCommentParams_3_._ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf('CurryDocRead.splitCommentParams'(C,'Prelude.++'(D,['^010'|A]),B),E,F,G).
'CurryDocRead.splitCommentParams_3_._ComplexCase'('Prelude.False',A,B,C,D,['Prelude.(,)'(C,'Prelude.apply'('CurryDocRead.skipWhiteSpace',D))|'CurryDocRead.splitCommentParams'('Prelude.takeWhile'(partcall(1,'Char.isAlpha',[]),'Prelude.tail'(E)),'Prelude.dropWhile'(partcall(1,'Char.isAlpha',[]),'Prelude.tail'(E)),B)],F,G):-!,makeShare(A,E),F=G.
'CurryDocRead.splitCommentParams_3_._ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocRead.splitCommentParams_3'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).

:-block 'CurryDocRead.getOverlappingInfo'(?,?,-,?).
'CurryDocRead.getOverlappingInfo'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getOverlappingInfo_1'(E,B,F,D).

:-block 'CurryDocRead.getOverlappingInfo_1'(?,?,-,?).
'CurryDocRead.getOverlappingInfo_1'('CurryDocRead.AnaInfo'(A,B,C,D),E,F,G):-!,hnf(A,E,F,G).
'CurryDocRead.getOverlappingInfo_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.getCompleteInfo'(?,?,-,?).
'CurryDocRead.getCompleteInfo'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getCompleteInfo_1'(E,B,F,D).

:-block 'CurryDocRead.getCompleteInfo_1'(?,?,-,?).
'CurryDocRead.getCompleteInfo_1'('CurryDocRead.AnaInfo'(A,B,C,D),E,F,G):-!,hnf(B,E,F,G).
'CurryDocRead.getCompleteInfo_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.getIndetInfo'(?,?,-,?).
'CurryDocRead.getIndetInfo'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getIndetInfo_1'(E,B,F,D).

:-block 'CurryDocRead.getIndetInfo_1'(?,?,-,?).
'CurryDocRead.getIndetInfo_1'('CurryDocRead.AnaInfo'(A,B,C,D),E,F,G):-!,hnf(C,E,F,G).
'CurryDocRead.getIndetInfo_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.getOpCompleteInfo'(?,?,-,?).
'CurryDocRead.getOpCompleteInfo'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.getOpCompleteInfo_1'(E,B,F,D).

:-block 'CurryDocRead.getOpCompleteInfo_1'(?,?,-,?).
'CurryDocRead.getOpCompleteInfo_1'('CurryDocRead.AnaInfo'(A,B,C,D),E,F,G):-!,hnf(D,E,F,G).
'CurryDocRead.getOpCompleteInfo_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.getFunctionInfo'(?,?,?,-,?).
'CurryDocRead.getFunctionInfo'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocRead.getFunctionInfo_1'(F,B,C,G,E).

:-block 'CurryDocRead.getFunctionInfo_1'(?,?,?,-,?).
'CurryDocRead.getFunctionInfo_1'([],A,B,C,D):-hnf('Prelude.error'('Prelude.++'(['^N','^o','^ ','^a','^n','^a','^l','^y','^s','^i','^s','^ ','^r','^e','^s','^u','^l','^t','^ ','^f','^o','^r','^ ','^f','^u','^n','^c','^t','^i','^o','^n','^ '],'Prelude.show'(A))),B,C,D).
'CurryDocRead.getFunctionInfo_1'([A|B],C,D,E,F):-!,hnf(A,G,E,H),'CurryDocRead.getFunctionInfo_1_._1'(G,B,C,D,H,F).

:-block 'CurryDocRead.getFunctionInfo_1_._1'(?,?,?,?,-,?).
'CurryDocRead.getFunctionInfo_1_._1'('Prelude.(,)'(A,B),C,D,E,F,G):-!,makeShare(D,H),hnf('Prelude.=='(A,H),I,F,J),'CurryDocRead.getFunctionInfo_1_._1_Prelude.(,)_ComplexCase'(I,A,B,C,H,E,J,G).

:-block 'CurryDocRead.getFunctionInfo_1_._1_Prelude.(,)_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocRead.getFunctionInfo_1_._1_Prelude.(,)_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocRead.getFunctionInfo_1_._1_Prelude.(,)_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf(B,E,F,G).
'CurryDocRead.getFunctionInfo_1_._1_Prelude.(,)_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,hnf('CurryDocRead.getFunctionInfo'(C,D),E,F,G).
'CurryDocRead.getFunctionInfo_1_._1_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocRead.getFunctionInfo_1_._1'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).
'CurryDocRead.getFunctionInfo_1'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocRead.isFunctionType'(?,?,-,?).
'CurryDocRead.isFunctionType'(A,B,C,D):-hnf(A,E,C,F),'CurryDocRead.isFunctionType_1'(E,B,F,D).

:-block 'CurryDocRead.isFunctionType_1'(?,?,-,?).
'CurryDocRead.isFunctionType_1'('FlatCurry.TVar'(A),'Prelude.False',B,B).
'CurryDocRead.isFunctionType_1'('FlatCurry.FuncType'(A,B),'Prelude.True',C,C).
'CurryDocRead.isFunctionType_1'('FlatCurry.TCons'(A,B),'Prelude.False',C,C):-!.
'CurryDocRead.isFunctionType_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocRead.skipWhiteSpace'(?,-,?).
'CurryDocRead.skipWhiteSpace'(A,B,C):-hnf(partcall(1,'Prelude.dropWhile',[partcall(1,'CurryDocRead.isWhiteSpace',[])]),A,B,C).

:-block 'CurryDocRead.isWhiteSpace'(?,?,-,?).
'CurryDocRead.isWhiteSpace'(A,B,C,D):-makeShare(A,E),hnf('Prelude.||'('Prelude.=='(E,'^ '),'Prelude.=='(E,'^010')),B,C,D).

:-block 'CurryDocRead.showId'(?,?,-,?).
'CurryDocRead.showId'(A,B,C,D):-makeShare(A,E),hnf('Char.isAlpha'('Prelude.head'(E)),F,C,G),'CurryDocRead.showId_ComplexCase'(F,E,B,G,D).

:-block 'CurryDocRead.showId_ComplexCase'(-,?,?,?,?),'CurryDocRead.showId_ComplexCase'(?,?,?,-,?).
'CurryDocRead.showId_ComplexCase'('Prelude.True',A,B,C,D):-hnf(A,B,C,D).
'CurryDocRead.showId_ComplexCase'('Prelude.False',A,B,C,D):-!,hnf('Prelude.++'(['^('|A],['^)']),B,C,D).
'CurryDocRead.showId_ComplexCase'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocRead.brackets'(?,?,?,-,?).
'CurryDocRead.brackets'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocRead.brackets_1'(F,B,C,G,E).

:-block 'CurryDocRead.brackets_1'(?,?,?,-,?).
'CurryDocRead.brackets_1'('Prelude.False',A,B,C,D):-hnf(A,B,C,D).
'CurryDocRead.brackets_1'('Prelude.True',A,B,C,D):-!,hnf('Prelude.++'(['^('],'Prelude.++'(A,['^)'])),B,C,D).
'CurryDocRead.brackets_1'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocRead.getLastName'(?,-,?).
'CurryDocRead.getLastName'(A,B,C):-hnf('Prelude..'('Prelude.reverse','Prelude..'(partcall(1,'Prelude.takeWhile',[partcall(1,'Prelude.flip',[^/,partcall(2,'Prelude./=',[])])]),'Prelude.reverse')),A,B,C).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0'(?,?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0'(A,B,C,D,E,F,G):-hnf(A,H,F,I),'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1'(H,B,C,D,E,I,G).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1'(-,?,?,?,?,?,?),'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1'(?,?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.True',A,B,C,'CurryDocRead.OtherLine',D,D).
'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E,F):-!,makeShare(C,G),hnf('Prelude.||'('Prelude.=='(G,['^d','^a','^t','^a']),'Prelude.=='(G,['^t','^y','^p','^e'])),H,E,I),'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(H,A,B,G,D,I,F).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(-,?,?,?,?,?,?),'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(?,?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',A,B,C,'CurryDocRead.DataDef'('Prelude.apply'(A,B)),D,D).
'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',A,B,C,'CurryDocRead.FuncDef'(C),D,D):-!.
'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0'(?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0'(A,B,C,D,E,F):-hnf(A,G,E,H),'CurryDocRead.classifyLine\'2E_\'23caseor0_1'(G,B,C,D,H,F).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0_1'(-,?,?,?,?,?),'CurryDocRead.classifyLine\'2E_\'23caseor0_1'(?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1'('Prelude.True',A,B,'CurryDocRead.Comment'([]),C,C).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E):-!,makeShare(B,F),hnf('Prelude.=='('Prelude.take'(4,F),[^-,^-,^-,'^ ']),G,D,H),'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(G,A,F,C,H,E).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(-,?,?,?,?,?),'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',A,B,'CurryDocRead.Comment'('Prelude.drop'(4,B)),C,C).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E):-!,makeShare(B,F),hnf('Prelude.=='('Prelude.take'(7,F),['^m','^o','^d','^u','^l','^e','^ ']),G,D,H),'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(G,A,F,C,H,E).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?),'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,'CurryDocRead.ModDef',C,C).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E):-!,makeShare(B,F),hnf('Prelude.=='('Prelude.take'(7,F),['^i','^m','^p','^o','^r','^t','^ ']),G,D,H),'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(G,A,F,C,H,E).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?),'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,'CurryDocRead.ModDef',C,C).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E):-!,hnf('Prelude.otherwise',F,D,G),'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(F,A,B,C,G,E).

:-block 'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?),'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,-,?).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E):-makeShare(F,G),makeShare(B,H),hnf('Prelude.cond'('Prelude.letrec'(G,'CurryDocRead.getFirstId'(H)),'CurryDocRead.classifyLine\'2E_\'23caseor0\'2E_\'23caseor0'('Prelude.=='(G,[]),A,H,G)),C,D,E).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E):-!,hnf('Prelude.failure'('CurryDocRead.classifyLine\'2E_\'23caseor0',['Prelude.False']),C,D,E).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocRead.classifyLine\'2E_\'23caseor0_1'('FAIL'(A),B,C,'FAIL'(A),D,D).

:-block 'CurryDocRead.groupLines\'2E_\'23caseor0'(?,?,?,?,?,-,?).
'CurryDocRead.groupLines\'2E_\'23caseor0'(A,B,C,D,E,F,G):-hnf(A,H,F,I),'CurryDocRead.groupLines\'2E_\'23caseor0_1'(H,B,C,D,E,I,G).

:-block 'CurryDocRead.groupLines\'2E_\'23caseor0_1'(-,?,?,?,?,?,?),'CurryDocRead.groupLines\'2E_\'23caseor0_1'(?,?,?,?,?,-,?).
'CurryDocRead.groupLines\'2E_\'23caseor0_1'('Prelude.True',A,B,C,'Prelude.(,)'([],'CurryDocRead.groupProgLines'(A)),D,D).
'CurryDocRead.groupLines\'2E_\'23caseor0_1'('Prelude.False',A,B,C,'Prelude.(,)'('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocRead.groupLines\'2EgetComment\'2E22',[])),B),'CurryDocRead.groupProgLines'('Prelude.filter'(partcall(1,'Prelude.flip',['CurryDocRead.ModDef',partcall(2,'Prelude./=',[])]),'Prelude.tail'(C)))),D,D):-!.
'CurryDocRead.groupLines\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E).

:-block 'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0'(?,?,?,?,?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0'(A,B,C,D,E,F,G,H):-hnf(A,I,G,J),'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0_1'(I,B,C,D,E,F,J,H).

:-block 'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0_1'(-,?,?,?,?,?,?,?),'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0_1'(?,?,?,?,?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D,'Prelude.(,)'(A,B),E,E).
'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,'Prelude.(,)'(C,D),E,E):-!.
'CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).

:-block 'CurryDocRead.getConsComment\'2E_\'23caseor0'(?,?,?,?,?,?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23caseor0'(A,B,C,D,E,F,G,H,I):-hnf(A,J,H,K),'CurryDocRead.getConsComment\'2E_\'23caseor0_1'(J,B,C,D,E,F,G,K,I).

:-block 'CurryDocRead.getConsComment\'2E_\'23caseor0_1'(-,?,?,?,?,?,?,?,?),'CurryDocRead.getConsComment\'2E_\'23caseor0_1'(?,?,?,?,?,?,?,-,?).
'CurryDocRead.getConsComment\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D,E,F,G,H):-makeShare(I,J),makeShare(K,L),makeShare(M,N),hnf('Prelude.cond'('Prelude.letrec'(J,'Prelude.apply'('Prelude.break'(partcall(1,'Prelude.flip',[^-,partcall(2,'Prelude.==',[])])),A)),'Prelude.cond'('Prelude.letrec'(L,'CurryDocRead.getConsComment\'2E_\'23selFP6\'23conscall'(J)),'Prelude.cond'('Prelude.letrec'(N,'CurryDocRead.getConsComment\'2E_\'23selFP7\'23callcmt'(J)),'Prelude.Just'('CurryDocRead.getConsComment\'2E_\'23caseor0\'2E_\'23caseor0'('Prelude.null'(N),B,C,L,N))))),F,G,H).
'CurryDocRead.getConsComment\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E,F,G,H):-!,hnf('CurryDocRead.getConsComment'(D,E),F,G,H).
'CurryDocRead.getConsComment\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,E,F,'FAIL'(A),G,G).

:-costCenters(['']).




%%%%% Number of shared variables: 42
