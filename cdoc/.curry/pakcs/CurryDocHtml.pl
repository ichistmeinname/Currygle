%PAKCS1.10 sicstus4 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('AnaCompleteness').
:-importModule('CategorizedHtmlList').
:-importModule('Char').
:-importModule('CurryDocParams').
:-importModule('CurryDocRead').
:-importModule('Distribution').
:-importModule('FlatCurry').
:-importModule('FlexRigid').
:-importModule('HTML').
:-importModule('List').
:-importModule('Markdown').
:-importModule('Prelude').
:-importModule('Sort').
:-importModule('Time').


:-curryModule('CurryDocHtml').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('CurryDocHtml.currydocCSS',currydocCSS,0,'CurryDocHtml.currydocCSS',nofix,'TCons'([],['TCons'('Prelude.Char',[])])).
functiontype('CurryDocHtml.generateHtmlDocs',generateHtmlDocs,7,'CurryDocHtml.generateHtmlDocs',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Time.CalendarTime',[]),'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'('Prelude.IO',['TCons'('Prelude.(,)',['TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])])]))))))))).
functiontype('CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1','CurryDocHtml.generateHtmlDocs._#lambda1',8,'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Time.CalendarTime',[]),'FuncType'('TCons'('FlatCurry.Prog',[]),'TCons'('Prelude.IO',['TCons'('Prelude.(,)',['TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])])])))))))))).
functiontype('CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda2','CurryDocHtml.generateHtmlDocs._#lambda1._#lambda2',1,'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda2',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda3','CurryDocHtml.generateHtmlDocs._#lambda1._#lambda3',1,'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda3',nofix,'FuncType'('TCons'('HTML.HtmlExp',[]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.docComment2HTML',docComment2HTML,2,'CurryDocHtml.docComment2HTML',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.replaceIdLinks',replaceIdLinks,1,'CurryDocHtml.replaceIdLinks',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14','CurryDocHtml.replaceIdLinks.checkId.14',1,'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md','CurryDocHtml.replaceIdLinks.checkId.14._#selFP2#md',1,'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun','CurryDocHtml.replaceIdLinks.checkId.14._#selFP3#dotfun',1,'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14','CurryDocHtml.replaceIdLinks.tryReplaceIdLink.14',2,'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocHtml.genHtmlExportIndex',genHtmlExportIndex,3,'CurryDocHtml.genHtmlExportIndex',nofix,'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))))).
functiontype('CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda5','CurryDocHtml.genHtmlExportIndex._#lambda5',1,'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda5',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda6','CurryDocHtml.genHtmlExportIndex._#lambda6',1,'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda6',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda7','CurryDocHtml.genHtmlExportIndex._#lambda7',1,'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda7',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8','CurryDocHtml.genHtmlExportIndex._#lambda8',1,'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('HTML.HtmlExp',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.getExportedTypes',getExportedTypes,1,'CurryDocHtml.getExportedTypes',nofix,'FuncType'('TCons'([],['TCons'('FlatCurry.TypeDecl',[])]),'TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]))).
functiontype('CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39','CurryDocHtml.getExportedTypes.getExpType.39',1,'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39',nofix,'FuncType'('TCons'('FlatCurry.TypeDecl',[]),'TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]))).
functiontype('CurryDocHtml.getExportedCons',getExportedCons,1,'CurryDocHtml.getExportedCons',nofix,'FuncType'('TCons'([],['TCons'('FlatCurry.TypeDecl',[])]),'TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]))).
functiontype('CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51','CurryDocHtml.getExportedCons.concatConsDecls.51',1,'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51',nofix,'FuncType'('TCons'([],['TCons'('FlatCurry.TypeDecl',[])]),'TCons'([],['TCons'('FlatCurry.ConsDecl',[])]))).
functiontype('CurryDocHtml.getExportedCons\'2E_\'23lambda9','CurryDocHtml.getExportedCons._#lambda9',1,'CurryDocHtml.getExportedCons\'2E_\'23lambda9',nofix,'FuncType'('TCons'('FlatCurry.ConsDecl',[]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.getExportedCons\'2E_\'23lambda10','CurryDocHtml.getExportedCons._#lambda10',1,'CurryDocHtml.getExportedCons\'2E_\'23lambda10',nofix,'FuncType'('TCons'('FlatCurry.ConsDecl',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocHtml.getExportedFuns',getExportedFuns,1,'CurryDocHtml.getExportedFuns',nofix,'FuncType'('TCons'([],['TCons'('FlatCurry.FuncDecl',[])]),'TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]))).
functiontype('CurryDocHtml.getExportedFuns\'2E_\'23lambda11','CurryDocHtml.getExportedFuns._#lambda11',1,'CurryDocHtml.getExportedFuns\'2E_\'23lambda11',nofix,'FuncType'('TCons'('FlatCurry.FuncDecl',[]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.getExportedFuns\'2E_\'23lambda12','CurryDocHtml.getExportedFuns._#lambda12',1,'CurryDocHtml.getExportedFuns\'2E_\'23lambda12',nofix,'FuncType'('TCons'('FlatCurry.FuncDecl',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocHtml.genHtmlModule',genHtmlModule,2,'CurryDocHtml.genHtmlModule',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt','CurryDocHtml.genHtmlModule._#selFP5#maincmt',1,'CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts','CurryDocHtml.genHtmlModule._#selFP6#avcmts',1,'CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]))).
functiontype('CurryDocHtml.genHtmlModule\'2E_\'23lambda13','CurryDocHtml.genHtmlModule._#lambda13',1,'CurryDocHtml.genHtmlModule\'2E_\'23lambda13',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.genHtmlModule\'2E_\'23lambda14','CurryDocHtml.genHtmlModule._#lambda14',1,'CurryDocHtml.genHtmlModule\'2E_\'23lambda14',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.genHtmlType',genHtmlType,3,'CurryDocHtml.genHtmlType',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('FlatCurry.TypeDecl',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))))).
functiontype('CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95','CurryDocHtml.genHtmlType.genHtmlCons.95',5,'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Int',[])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'('FlatCurry.ConsDecl',[]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('HTML.HtmlExp',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])])]))))))).
functiontype('CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda15','CurryDocHtml.genHtmlType.genHtmlCons.95._#lambda15',2,'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda15',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('FlatCurry.TypeExpr',[]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda16','CurryDocHtml.genHtmlType.genHtmlCons.95._#lambda16',1,'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda16',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17','CurryDocHtml.genHtmlType.genHtmlCons.95._#lambda17',2,'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt','CurryDocHtml.genHtmlType._#selFP8#datacmt',1,'CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts','CurryDocHtml.genHtmlType._#selFP9#conscmts',1,'CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]))).
functiontype('CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt','CurryDocHtml.genHtmlType._#selFP11#typecmt',1,'CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.genHtmlType\'2E_\'23lambda18','CurryDocHtml.genHtmlType._#lambda18',1,'CurryDocHtml.genHtmlType\'2E_\'23lambda18',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.genHtmlFuncShort',genHtmlFuncShort,4,'CurryDocHtml.genHtmlFuncShort',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'('FlatCurry.FuncDecl',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))))).
functiontype('CurryDocHtml.genHtmlFunc',genHtmlFunc,6,'CurryDocHtml.genHtmlFunc',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'([],['TCons'('FlatCurry.OpDecl',[])]),'FuncType'('TCons'('FlatCurry.FuncDecl',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))))))).
functiontype('CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117','CurryDocHtml.genHtmlFunc.showCall.117',2,'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117','CurryDocHtml.genHtmlFunc.genParamComment.117',3,'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))))).
functiontype('CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19','CurryDocHtml.genHtmlFunc.genParamComment.117._#lambda19',2,'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.(,)',['TCons'([],[A]),'TCons'([],['TCons'('HTML.HtmlExp',[])])])))).
functiontype('CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda20','CurryDocHtml.genHtmlFunc.genParamComment.117._#lambda20',2,'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda20',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('HTML.HtmlExp',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])])))).
functiontype('CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt','CurryDocHtml.genHtmlFunc._#selFP13#funcmt',1,'CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts','CurryDocHtml.genHtmlFunc._#selFP14#paramcmts',1,'CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]))).
functiontype('CurryDocHtml.removeDash',removeDash,1,'CurryDocHtml.removeDash',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.removeTopPar',removeTopPar,1,'CurryDocHtml.removeTopPar',nofix,'FuncType'('TCons'([],['TCons'('HTML.HtmlExp',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.genFuncPropIcons',genFuncPropIcons,3,'CurryDocHtml.genFuncPropIcons',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'('FlatCurry.Rule',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))))).
functiontype('CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146','CurryDocHtml.genFuncPropIcons.flexRigidIcon.146',1,'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146',nofix,'FuncType'('TCons'('FlatCurry.Rule',[]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153','CurryDocHtml.genFuncPropIcons.flexRigidIcon.146.imageEvalAnnot.153',1,'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153',nofix,'FuncType'('TCons'('FlexRigid.FlexRigidResult',[]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.addIconParams',addIconParams,1,'CurryDocHtml.addIconParams',nofix,'FuncType'('TCons'('HTML.HtmlExp',[]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.genFuncPropComments',genFuncPropComments,4,'CurryDocHtml.genFuncPropComments',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'('FlatCurry.Rule',[]),'FuncType'('TCons'([],['TCons'('FlatCurry.OpDecl',[])]),'TCons'([],['TCons'([],['TCons'('HTML.HtmlExp',[])])])))))).
functiontype('CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165','CurryDocHtml.genFuncPropComments.externalInfo.165',1,'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165',nofix,'FuncType'('TCons'('FlatCurry.Rule',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.genFixityInfo',genFixityInfo,2,'CurryDocHtml.genFixityInfo',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'('FlatCurry.OpDecl',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179','CurryDocHtml.genFixityInfo.showFixity.179',1,'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179',nofix,'FuncType'('TCons'('FlatCurry.Fixity',[]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.genFixityInfo\'2E_\'23lambda22','CurryDocHtml.genFixityInfo._#lambda22',2,'CurryDocHtml.genFixityInfo\'2E_\'23lambda22',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'('FlatCurry.OpDecl',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.showType',showType,3,'CurryDocHtml.showType',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'('FlatCurry.TypeExpr',[]),'TCons'([],['TCons'('Prelude.Char',[])]))))).
functiontype('CurryDocHtml.showTypeCons',showTypeCons,2,'CurryDocHtml.showTypeCons',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocHtml.translateSource2ColoredHtml',translateSource2ColoredHtml,2,'CurryDocHtml.translateSource2ColoredHtml',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.IO',['TCons'('Prelude.()',[])])))).
functiontype('CurryDocHtml.translateSource2AnchoredHtml',translateSource2AnchoredHtml,2,'CurryDocHtml.translateSource2AnchoredHtml',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.IO',['TCons'('Prelude.()',[])])))).
functiontype('CurryDocHtml.translateSource2AnchoredHtml\'2E_\'23lambda23','CurryDocHtml.translateSource2AnchoredHtml._#lambda23',3,'CurryDocHtml.translateSource2AnchoredHtml\'2E_\'23lambda23',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.IO',['TCons'('Prelude.()',[])]))))).
functiontype('CurryDocHtml.addFuncAnchors',addFuncAnchors,2,'CurryDocHtml.addFuncAnchors',nofix,'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocHtml.genMainIndexPage',genMainIndexPage,4,'CurryDocHtml.genMainIndexPage',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Time.CalendarTime',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.IO',['TCons'('Prelude.()',[])])))))).
functiontype('CurryDocHtml.htmlIndex',htmlIndex,1,'CurryDocHtml.htmlIndex',nofix,'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.htmlIndex\'2E_\'23lambda24','CurryDocHtml.htmlIndex._#lambda24',1,'CurryDocHtml.htmlIndex\'2E_\'23lambda24',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.genFunctionIndexPage',genFunctionIndexPage,4,'CurryDocHtml.genFunctionIndexPage',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Time.CalendarTime',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('FlatCurry.FuncDecl',[])]),'TCons'('Prelude.IO',['TCons'('Prelude.()',[])])))))).
functiontype('CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25','CurryDocHtml.genFunctionIndexPage._#lambda25',1,'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25',nofix,'FuncType'('TCons'('FlatCurry.FuncDecl',[]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]))).
functiontype('CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26','CurryDocHtml.genFunctionIndexPage._#lambda26',1,'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26',nofix,'FuncType'('TCons'('FlatCurry.FuncDecl',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocHtml.htmlFuncIndex',htmlFuncIndex,1,'CurryDocHtml.htmlFuncIndex',nofix,'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.showModNameRef',showModNameRef,1,'CurryDocHtml.showModNameRef',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])]))).
functiontype('CurryDocHtml.sortNames',sortNames,1,'CurryDocHtml.sortNames',nofix,'FuncType'('TCons'([],['TCons'('Prelude.(,)',[A,'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('Prelude.(,)',[A,'TCons'([],['TCons'('Prelude.Char',[])])])]))).
functiontype('CurryDocHtml.sortNames\'2E_\'23lambda27','CurryDocHtml.sortNames._#lambda27',2,'CurryDocHtml.sortNames\'2E_\'23lambda27',nofix,'FuncType'('TCons'('Prelude.(,)',[A,'TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'('Prelude.(,)',[A,'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'('Prelude.Bool',[])))).
functiontype('CurryDocHtml.genConsIndexPage',genConsIndexPage,4,'CurryDocHtml.genConsIndexPage',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Time.CalendarTime',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('FlatCurry.TypeDecl',[])]),'TCons'('Prelude.IO',['TCons'('Prelude.()',[])])))))).
functiontype('CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242','CurryDocHtml.genConsIndexPage.getCons.242',1,'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242',nofix,'FuncType'('TCons'('FlatCurry.TypeDecl',[]),'TCons'([],['TCons'('FlatCurry.ConsDecl',[])]))).
functiontype('CurryDocHtml.genConsIndexPage\'2E_\'23lambda28','CurryDocHtml.genConsIndexPage._#lambda28',1,'CurryDocHtml.genConsIndexPage\'2E_\'23lambda28',nofix,'FuncType'('TCons'('FlatCurry.ConsDecl',[]),'TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]))).
functiontype('CurryDocHtml.genConsIndexPage\'2E_\'23lambda29','CurryDocHtml.genConsIndexPage._#lambda29',1,'CurryDocHtml.genConsIndexPage\'2E_\'23lambda29',nofix,'FuncType'('TCons'('FlatCurry.ConsDecl',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocHtml.htmlConsIndex',htmlConsIndex,1,'CurryDocHtml.htmlConsIndex',nofix,'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.explainCat',explainCat,1,'CurryDocHtml.explainCat',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.opnameDoc',opnameDoc,0,'CurryDocHtml.opnameDoc',nofix,'FuncType'('TCons'([],['TCons'('HTML.HtmlExp',[])]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.showDocCSS',showDocCSS,2,'CurryDocHtml.showDocCSS',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('HTML.HtmlExp',[])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocHtml.sortStrings',sortStrings,1,'CurryDocHtml.sortStrings',nofix,'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]))).
functiontype('CurryDocHtml.firstSentence',firstSentence,1,'CurryDocHtml.firstSentence',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs','CurryDocHtml.firstSentence._#selFP16#fs',1,'CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls','CurryDocHtml.firstSentence._#selFP17#ls',1,'CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls',nofix,'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.curryDocEpilog',curryDocEpilog,2,'CurryDocHtml.curryDocEpilog',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Time.CalendarTime',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0','CurryDocHtml.generateHtmlDocs._#lambda1._#caseor0',4,'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'([],['TCons'('FlatCurry.TypeDecl',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))))).
functiontype('CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0','CurryDocHtml.replaceIdLinks.checkId.14._#caseor0',4,'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])))))).
functiontype('CurryDocHtml.genHtmlModule\'2E_\'23caseor0','CurryDocHtml.genHtmlModule._#caseor0',2,'CurryDocHtml.genHtmlModule\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.genHtmlType\'2E_\'23caseor0','CurryDocHtml.genHtmlType._#caseor0',5,'CurryDocHtml.genHtmlType\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Int',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('FlatCurry.TypeExpr',[]),'TCons'('HTML.HtmlExp',[]))))))).
functiontype('CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0','CurryDocHtml.genHtmlFunc._#caseor0._#caseor0',2,'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'([],['TCons'('HTML.HtmlExp',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.genHtmlFunc\'2E_\'23caseor0','CurryDocHtml.genHtmlFunc._#caseor0',10,'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('FlatCurry.TypeExpr',[]),'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('FlatCurry.Rule',[]),'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'([],['TCons'('HTML.HtmlExp',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))))))))))).
functiontype('CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0','CurryDocHtml.genHtmlFunc.genParamComment.117._#caseor0',4,'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('CurryDocParams.DocParams',[]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))))).
functiontype('CurryDocHtml.removeDash\'2E_\'23caseor0','CurryDocHtml.removeDash._#caseor0',2,'CurryDocHtml.removeDash\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0','CurryDocHtml.genFuncPropIcons._#caseor0',1,'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'TCons'('HTML.HtmlExp',[]))).
functiontype('CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0','CurryDocHtml.genFuncPropComments._#caseor0._#caseor0',1,'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocHtml.genFuncPropComments\'2E_\'23caseor0','CurryDocHtml.genFuncPropComments._#caseor0',2,'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'('AnaCompleteness.CompletenessType',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0','CurryDocHtml.genFuncPropComments._#caseor0._#caseor0._#caseor0',1,'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0','CurryDocHtml.genFuncPropComments._#caseor0._#caseor0._#caseor0._#caseor0',1,'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'TCons'([],['TCons'('HTML.HtmlExp',[])]))).
functiontype('CurryDocHtml.addFuncAnchors\'2E_\'23caseor0','CurryDocHtml.addFuncAnchors._#caseor0',5,'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])]))))))).
functiontype('CurryDocHtml.htmlIndex\'2E_\'23caseor0','CurryDocHtml.htmlIndex._#caseor0',2,'CurryDocHtml.htmlIndex\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('HTML.HtmlExp',[])])))).
functiontype('CurryDocHtml.firstSentence\'2E_\'23caseor0','CurryDocHtml.firstSentence._#caseor0',3,'CurryDocHtml.firstSentence\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]))))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
:-block 'CurryDocHtml.currydocCSS'(?,-,?).
'CurryDocHtml.currydocCSS'(['^c','^u','^r','^r','^y','^d','^o','^c',^.,'^c','^s','^s'],A,A).

:-block 'CurryDocHtml.generateHtmlDocs'(?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.generateHtmlDocs'(A,B,C,D,E,F,G,H,I,J):-makeShare(K,L),makeShare(E,M),hnf('Prelude.cond'('Prelude.letrec'(L,'FlatCurry.flatCurryFileName'(M)),'Prelude.>>'('Prelude.$'(partcall(1,'Prelude.putStrLn',[]),'Prelude.++'(['^R','^e','^a','^d','^i','^n','^g','^ ','^F','^l','^a','^t','^C','^u','^r','^r','^y','^ ','^p','^r','^o','^g','^r','^a','^m','^ ','^"'],'Prelude.++'(L,['^"',^.,^.,^.]))),'Prelude.>>='('FlatCurry.readFlatCurryFile'(L),partcall(1,'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1',[B,M,G,F,C,A,D])))),H,I,J).

:-block 'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1'(?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1'(A,B,C,D,E,F,G,H,I,J,K):-hnf(H,L,J,M),'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1_8'(L,A,B,C,D,E,F,G,I,M,K).

:-block 'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1_8'(?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1_8'('FlatCurry.Prog'(A,B,C,D,E),F,G,H,I,J,K,L,M,N,O):-!,makeShare(B,P),makeShare(K,Q),makeShare(H,R),makeShare(C,S),makeShare(D,T),makeShare(J,U),makeShare(F,V),hnf('Prelude.$'(partcall(1,'Prelude.return',[]),'Prelude.(,)'(P,'Prelude.++'(['HTML.h1'(['HTML.href'('Prelude.++'('Prelude.apply'('CurryDocRead.getLastName',Q),['^_','^c','^u','^r','^r','^y',^.,'^h','^t','^m','^l']),['HTML.htxt'('Prelude.++'('Prelude.apply'('CurryDocRead.getLastName',Q),[^.,'^c','^u','^r','^r','^y']))])])],'Prelude.++'('CurryDocHtml.genHtmlModule'(R,I),'Prelude.++'(['HTML.HtmlStruct'(['^t','^a','^b','^l','^e'],['Prelude.(,)'(['^b','^o','^r','^d','^e','^r'],['^1']),'Prelude.(,)'(['^w','^i','^d','^t','^h'],['^1','^0','^0','^%'])],['HTML.HtmlStruct'(['^t','^r'],[],['HTML.HtmlStruct'(['^t','^d'],['Prelude.(,)'(['^v','^a','^l','^i','^g','^n'],['^t','^o','^p']),'Prelude.(,)'(['^w','^i','^d','^t','^h'],['^2','^0','^%'])],'Prelude.++'(['HTML.h2'(['HTML.htxt'(['^E','^x','^p','^o','^r','^t','^e','^d','^ ','^n','^a','^m','^e','^s',^:])])],'Prelude.++'('CurryDocHtml.genHtmlExportIndex'('CurryDocHtml.getExportedTypes'(S),'CurryDocHtml.getExportedCons'(S),'CurryDocHtml.getExportedFuns'(T)),['HTML.h2'(['HTML.htxt'(['^I','^m','^p','^o','^r','^t','^e','^d','^ ','^m','^o','^d','^u','^l','^e','^s',^:])]),'HTML.par'('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda2',[])),P))]))),'HTML.HtmlStruct'(['^t','^d'],[],'Prelude.++'(['HTML.h2'(['HTML.htxt'(['^S','^u','^m','^m','^a','^r','^y','^ ','^o','^f','^ ','^e','^x','^p','^o','^r','^t','^e','^d','^ ','^f','^u','^n','^c','^t','^i','^o','^n','^s',^:])]),'HTML.HtmlStruct'(['^t','^a','^b','^l','^e'],['Prelude.(,)'(['^b','^o','^r','^d','^e','^r'],['^1']),'Prelude.(,)'(['^w','^i','^d','^t','^h'],['^1','^0','^0','^%'])],'Prelude.map'(partcall(1,'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda3',[]),'Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genHtmlFuncShort',[V,U,R])),T)))],'Prelude.++'('CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0'('Prelude.null'(S),R,U,S),'Prelude.++'(['HTML.h2'(['HTML.htxt'(['^E','^x','^p','^o','^r','^t','^e','^d','^ ','^f','^u','^n','^c','^t','^i','^o','^n','^s',^:])]),'HTML.hrule'],'Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genHtmlFunc',[E,V,U,Q,R])),T)))))])])],'CurryDocHtml.curryDocEpilog'(G,L)))))),M,N,O).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1_8'('FAIL'(A),B,C,D,E,F,G,H,'FAIL'(A),I,I):-nonvar(A).

:-block 'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda2'(?,?,-,?).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda2'(A,['HTML.href'('Prelude.++'('Prelude.apply'('CurryDocRead.getLastName',B),[^.,'^h','^t','^m','^l']),['HTML.htxt'(B)]),'HTML.breakline'],C,D):-makeShare(A,B),C=D.

:-block 'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda3'(?,?,-,?).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23lambda3'(A,'HTML.HtmlStruct'(['^t','^r'],[],['HTML.HtmlStruct'(['^t','^d'],[],[A])]),B,B).

:-block 'CurryDocHtml.docComment2HTML'(?,?,?,-,?).
'CurryDocHtml.docComment2HTML'(A,B,C,D,E):-hnf('CurryDocParams.withMarkdown'(A),F,D,G),'CurryDocHtml.docComment2HTML_ComplexCase'(F,A,B,C,G,E).

:-block 'CurryDocHtml.docComment2HTML_ComplexCase'(-,?,?,?,?,?),'CurryDocHtml.docComment2HTML_ComplexCase'(?,?,?,?,-,?).
'CurryDocHtml.docComment2HTML_ComplexCase'('Prelude.True',A,B,C,D,E):-hnf('Prelude.apply'('Markdown.markdownText2HTML','CurryDocHtml.replaceIdLinks'(B)),C,D,E).
'CurryDocHtml.docComment2HTML_ComplexCase'('Prelude.False',A,B,['HTML.HtmlText'('CurryDocHtml.replaceIdLinks'(B))],C,C):-!.
'CurryDocHtml.docComment2HTML_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).

:-block 'CurryDocHtml.replaceIdLinks'(?,?,-,?).
'CurryDocHtml.replaceIdLinks'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.replaceIdLinks_1'(E,B,F,D).

:-block 'CurryDocHtml.replaceIdLinks_1'(-,?,?,?),'CurryDocHtml.replaceIdLinks_1'(?,?,-,?).
'CurryDocHtml.replaceIdLinks_1'([],[],A,A).
'CurryDocHtml.replaceIdLinks_1'([A|B],C,D,E):-!,makeShare(A,F),hnf('Prelude.=='(F,^\),G,D,H),'CurryDocHtml.replaceIdLinks_1_._ComplexCase'(G,F,B,C,H,E).

:-block 'CurryDocHtml.replaceIdLinks_1_._ComplexCase'(-,?,?,?,?,?),'CurryDocHtml.replaceIdLinks_1_._ComplexCase'(?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase'('Prelude.True',A,B,C,D,E):-makeShare(B,F),hnf(F,G,D,H),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2'(G,A,G,C,H,E).

:-block 'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2'(-,?,?,?,?,?),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2'(?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2'([A|B],C,D,E,F,G):-hnf('Prelude.=='(A,'^\''),H,F,I),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase'(H,A,B,C,D,E,I,G).

:-block 'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase'('Prelude.True',A,B,C,D,['^\''|'CurryDocHtml.replaceIdLinks'(B)],E,E).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,makeShare(C,H),hnf('Prelude.=='(H,'^\''),I,F,J),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase_Prelude.False_ComplexCase'(I,A,B,H,D,E,J,G).

:-block 'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf('CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14'([],D),E,F,G).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,[C|'CurryDocHtml.replaceIdLinks'(D)],E,E):-!.
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_._ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2'([],A,B,C,D,E):-!,makeShare(A,F),hnf('Prelude.=='(F,'^\''),G,D,H),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_[]_ComplexCase'(G,F,B,C,H,E).

:-block 'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_[]_ComplexCase'(-,?,?,?,?,?),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_[]_ComplexCase'(?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_[]_ComplexCase'('Prelude.True',A,B,C,D,E):-hnf('CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14'([],[]),C,D,E).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_[]_ComplexCase'('Prelude.False',A,B,[A|'CurryDocHtml.replaceIdLinks'([])],C,C):-!.
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2_[]_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.True_2'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase'('Prelude.False',A,B,C,D,E):-!,makeShare(A,F),hnf('Prelude.=='(F,'^\''),G,D,H),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.False_ComplexCase'(G,F,B,C,H,E).

:-block 'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?),'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E):-hnf('CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14'([],B),C,D,E).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,[A|'CurryDocHtml.replaceIdLinks'(B)],C,C):-!.
'CurryDocHtml.replaceIdLinks_1_._ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocHtml.replaceIdLinks_1_._ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocHtml.replaceIdLinks_1'('FAIL'(A),'FAIL'(A),B,B).

:-block 'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14'(?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14'(A,B,C,D):-makeShare(A,E),hnf('Prelude.apply'('Prelude.elem'('^ '),E),F,C,G),'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14_ComplexCase'(F,E,B,G,D).

:-block 'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14_ComplexCase'(-,?,?,?,?),'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14_ComplexCase'(?,?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14_ComplexCase'('Prelude.True',A,['^\''|'Prelude.++'(A,['^\''])],B,B).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14_ComplexCase'('Prelude.False',A,B,C,D):-!,makeShare(E,F),makeShare(A,G),makeShare(H,I),makeShare(J,K),hnf('Prelude.cond'('Prelude.letrec'(F,'Prelude.apply'('Prelude.break'(partcall(1,'Prelude.flip',[^.,partcall(2,'Prelude.==',[])])),G)),'Prelude.cond'('Prelude.letrec'(I,'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md'(F)),'Prelude.cond'('Prelude.letrec'(K,'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun'(F)),'Prelude.++'([^<,'^c','^o','^d','^e',^>,^<,'^a','^ ','^h','^r','^e','^f',^=,'^"'],'Prelude.++'('CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0'('Prelude.null'(K),G,I,K),'Prelude.++'(['^"',^>],'Prelude.++'(G,[^<,^/,'^a',^>,^<,^/,'^c','^o','^d','^e',^>]))))))),B,C,D).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14_ComplexCase'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md'(?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md_1'(E,B,F,D).

:-block 'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md_1'(?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP2\'23md_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun'(?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun_1'(E,B,F,D).

:-block 'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun_1'(?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23selFP3\'23dotfun_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14'(?,?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2'(F,A,C,G,E).

:-block 'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2'(?,?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2'([],A,['^\''|'Prelude.apply'('Prelude.reverse',A)],B,B).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2'([A|B],C,D,E,F):-!,makeShare(A,G),hnf('Char.isSpace'(G),H,E,I),'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase'(H,G,B,C,D,I,F).

:-block 'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase'(-,?,?,?,?,?,?),'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase'(?,?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase'('Prelude.True',A,B,C,['^\''|'Prelude.++'('Prelude.apply'('Prelude.reverse',C),[A|'CurryDocHtml.replaceIdLinks'(B)])],D,D).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase'('Prelude.False',A,B,C,D,E,F):-!,makeShare(A,G),hnf('Prelude.=='(G,'^\''),H,E,I),'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase'(H,G,B,C,D,I,F).

:-block 'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?,?),'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E,F):-hnf('Prelude.++'('CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14'('Prelude.apply'('Prelude.reverse',C)),'CurryDocHtml.replaceIdLinks'(B)),D,E,F).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E,F):-!,hnf('Prelude.otherwise',G,E,H),'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(G,A,B,C,D,H,F).

:-block 'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?,?),'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E,F):-hnf('CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14'([A|C],B),D,E,F).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E,F):-!,hnf('Prelude.failure'('CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14',['Prelude.False']),D,E,F).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2_._ComplexCase'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocHtml.replaceIdLinks\'2EtryReplaceIdLink\'2E14_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocHtml.genHtmlExportIndex'(?,?,?,?,-,?).
'CurryDocHtml.genHtmlExportIndex'(A,B,C,D,E,F):-makeShare(G,H),makeShare(I,J),makeShare(K,L),hnf('Prelude.cond'('Prelude.letrec'(H,'Prelude.map'(partcall(1,'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda5',[]),'List.nub'('CurryDocHtml.sortStrings'(A)))),'Prelude.cond'('Prelude.letrec'(J,'Prelude.map'(partcall(1,'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda6',[]),'List.nub'('CurryDocHtml.sortStrings'(B)))),'Prelude.cond'('Prelude.letrec'(L,'Prelude.map'(partcall(1,'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda7',[]),'List.nub'('CurryDocHtml.sortStrings'(C)))),'Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8',[])),['Prelude.(,)'(H,['^D','^a','^t','^a','^t','^y','^p','^e','^s',^:]),'Prelude.(,)'(J,['^C','^o','^n','^s','^t','^r','^u','^c','^t','^o','^r','^s',^:]),'Prelude.(,)'(L,['^F','^u','^n','^c','^t','^i','^o','^n','^s',^:])])))),D,E,F).

:-block 'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda5'(?,?,-,?).
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda5'(A,B,C,D):-makeShare(A,E),hnf('HTML.href'([^#|E],['HTML.htxt'(E)]),B,C,D).

:-block 'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda6'(?,?,-,?).
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda6'(A,B,C,D):-makeShare(A,E),hnf('HTML.href'([^#|'Prelude.++'(E,['^_','^C','^O','^N','^S'])],['HTML.htxt'(E)]),B,C,D).

:-block 'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda7'(?,?,-,?).
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda7'(A,B,C,D):-makeShare(A,E),hnf('HTML.href'([^#|'Prelude.++'(E,['^_','^S','^H','^O','^R','^T'])],['HTML.htxt'(E)]),B,C,D).

:-block 'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8'(?,?,-,?).
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1'(E,B,F,D).

:-block 'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1'(?,?,-,?).
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1'('Prelude.(,)'(A,B),C,D,E):-!,makeShare(A,F),hnf('Prelude.null'(F),G,D,H),'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1_Prelude.(,)_ComplexCase'(G,F,B,C,H,E).

:-block 'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1_Prelude.(,)_ComplexCase'(-,?,?,?,?,?),'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1_Prelude.(,)_ComplexCase'(?,?,?,?,-,?).
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1_Prelude.(,)_ComplexCase'('Prelude.True',A,B,[],C,C).
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1_Prelude.(,)_ComplexCase'('Prelude.False',A,B,['HTML.par'('Prelude.++'(['HTML.bold'(['HTML.htxt'(B)]),'HTML.breakline'],'List.intersperse'('HTML.breakline',A)))],C,C):-!.
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocHtml.genHtmlExportIndex\'2E_\'23lambda8_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.getExportedTypes'(?,?,-,?).
'CurryDocHtml.getExportedTypes'(A,B,C,D):-hnf('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39',[])),A),B,C,D).

:-block 'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39'(?,?,-,?).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1'(E,B,F,D).

:-block 'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1'(?,?,-,?).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1'('FlatCurry.Type'(A,B,C,D),E,F,G):-hnf(A,H,F,I),'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1'(H,B,C,D,E,I,G).

:-block 'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1'(?,?,?,?,?,-,?).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1'('Prelude.(,)'(A,B),C,D,E,F,G,H):-!,hnf('Prelude.=='(C,'FlatCurry.Public'),I,G,J),'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1_Prelude.(,)_ComplexCase'(I,A,B,C,D,E,F,J,H).

:-block 'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1_Prelude.(,)_ComplexCase'(-,?,?,?,?,?,?,?,?),'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1_Prelude.(,)_ComplexCase'(?,?,?,?,?,?,?,-,?).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1_Prelude.(,)_ComplexCase'('Prelude.True',A,B,C,D,E,[B],F,F).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1_Prelude.(,)_ComplexCase'('Prelude.False',A,B,C,D,E,[],F,F):-!.
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,D,E,F,'FAIL'(A),G,G).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.Type_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E):-nonvar(A).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1'('FlatCurry.TypeSyn'(A,B,C,D),E,F,G):-!,hnf(A,H,F,I),'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1'(H,B,C,D,E,I,G).

:-block 'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1'(?,?,?,?,?,-,?).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1'('Prelude.(,)'(A,B),C,D,E,F,G,H):-!,hnf('Prelude.=='(C,'FlatCurry.Public'),I,G,J),'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'(I,A,B,C,D,E,F,J,H).

:-block 'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'(-,?,?,?,?,?,?,?,?),'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'(?,?,?,?,?,?,?,-,?).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'('Prelude.True',A,B,C,D,E,[B],F,F).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'('Prelude.False',A,B,C,D,E,[],F,F):-!.
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,D,E,F,'FAIL'(A),G,G).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1_FlatCurry.TypeSyn_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E):-nonvar(A).
'CurryDocHtml.getExportedTypes\'2EgetExpType\'2E39_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.getExportedCons'(?,?,-,?).
'CurryDocHtml.getExportedCons'(A,B,C,D):-hnf('Prelude.map'(partcall(1,'CurryDocHtml.getExportedCons\'2E_\'23lambda9',[]),'Prelude.filter'(partcall(1,'CurryDocHtml.getExportedCons\'2E_\'23lambda10',[]),'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51'(A))),B,C,D).

:-block 'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51'(?,?,-,?).
'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1'(E,B,F,D).

:-block 'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1'(?,?,-,?).
'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1'([],[],A,A).
'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1'([A|B],C,D,E):-!,hnf(A,F,D,G),'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1_._1'(F,B,C,G,E).

:-block 'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1_._1'(?,?,?,-,?).
'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1_._1'('FlatCurry.TypeSyn'(A,B,C,D),E,F,G,H):-hnf('CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51'(E),F,G,H).
'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1_._1'('FlatCurry.Type'(A,B,C,D),E,F,G,H):-!,hnf('Prelude.++'(D,'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51'(E)),F,G,H).
'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1_._1'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).
'CurryDocHtml.getExportedCons\'2EconcatConsDecls\'2E51_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.getExportedCons\'2E_\'23lambda9'(?,?,-,?).
'CurryDocHtml.getExportedCons\'2E_\'23lambda9'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.getExportedCons\'2E_\'23lambda9_1'(E,B,F,D).

:-block 'CurryDocHtml.getExportedCons\'2E_\'23lambda9_1'(?,?,-,?).
'CurryDocHtml.getExportedCons\'2E_\'23lambda9_1'('FlatCurry.Cons'(A,B,C,D),E,F,G):-!,hnf(A,H,F,I),'CurryDocHtml.getExportedCons\'2E_\'23lambda9_1_FlatCurry.Cons_1'(H,B,C,D,E,I,G).

:-block 'CurryDocHtml.getExportedCons\'2E_\'23lambda9_1_FlatCurry.Cons_1'(?,?,?,?,?,-,?).
'CurryDocHtml.getExportedCons\'2E_\'23lambda9_1_FlatCurry.Cons_1'('Prelude.(,)'(A,B),C,D,E,F,G,H):-!,hnf(B,F,G,H).
'CurryDocHtml.getExportedCons\'2E_\'23lambda9_1_FlatCurry.Cons_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E):-nonvar(A).
'CurryDocHtml.getExportedCons\'2E_\'23lambda9_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.getExportedCons\'2E_\'23lambda10'(?,?,-,?).
'CurryDocHtml.getExportedCons\'2E_\'23lambda10'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.getExportedCons\'2E_\'23lambda10_1'(E,B,F,D).

:-block 'CurryDocHtml.getExportedCons\'2E_\'23lambda10_1'(?,?,-,?).
'CurryDocHtml.getExportedCons\'2E_\'23lambda10_1'('FlatCurry.Cons'(A,B,C,D),E,F,G):-!,hnf('Prelude.=='(C,'FlatCurry.Public'),E,F,G).
'CurryDocHtml.getExportedCons\'2E_\'23lambda10_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.getExportedFuns'(?,?,-,?).
'CurryDocHtml.getExportedFuns'(A,B,C,D):-hnf('Prelude.map'(partcall(1,'CurryDocHtml.getExportedFuns\'2E_\'23lambda11',[]),'Prelude.filter'(partcall(1,'CurryDocHtml.getExportedFuns\'2E_\'23lambda12',[]),A)),B,C,D).

:-block 'CurryDocHtml.getExportedFuns\'2E_\'23lambda11'(?,?,-,?).
'CurryDocHtml.getExportedFuns\'2E_\'23lambda11'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.getExportedFuns\'2E_\'23lambda11_1'(E,B,F,D).

:-block 'CurryDocHtml.getExportedFuns\'2E_\'23lambda11_1'(?,?,-,?).
'CurryDocHtml.getExportedFuns\'2E_\'23lambda11_1'('FlatCurry.Func'(A,B,C,D,E),F,G,H):-!,hnf(A,I,G,J),'CurryDocHtml.getExportedFuns\'2E_\'23lambda11_1_FlatCurry.Func_1'(I,B,C,D,E,F,J,H).

:-block 'CurryDocHtml.getExportedFuns\'2E_\'23lambda11_1_FlatCurry.Func_1'(?,?,?,?,?,?,-,?).
'CurryDocHtml.getExportedFuns\'2E_\'23lambda11_1_FlatCurry.Func_1'('Prelude.(,)'(A,B),C,D,E,F,G,H,I):-!,hnf(B,G,H,I).
'CurryDocHtml.getExportedFuns\'2E_\'23lambda11_1_FlatCurry.Func_1'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F):-nonvar(A).
'CurryDocHtml.getExportedFuns\'2E_\'23lambda11_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.getExportedFuns\'2E_\'23lambda12'(?,?,-,?).
'CurryDocHtml.getExportedFuns\'2E_\'23lambda12'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.getExportedFuns\'2E_\'23lambda12_1'(E,B,F,D).

:-block 'CurryDocHtml.getExportedFuns\'2E_\'23lambda12_1'(?,?,-,?).
'CurryDocHtml.getExportedFuns\'2E_\'23lambda12_1'('FlatCurry.Func'(A,B,C,D,E),F,G,H):-!,hnf('Prelude.=='(C,'FlatCurry.Public'),F,G,H).
'CurryDocHtml.getExportedFuns\'2E_\'23lambda12_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genHtmlModule'(?,?,?,-,?).
'CurryDocHtml.genHtmlModule'(A,B,C,D,E):-makeShare(F,G),makeShare(H,I),makeShare(J,K),hnf('Prelude.cond'('Prelude.letrec'(G,'CurryDocRead.splitComment'(B)),'Prelude.cond'('Prelude.letrec'(I,'CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt'(G)),'Prelude.cond'('Prelude.letrec'(K,'CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts'(G)),'Prelude.++'('CurryDocHtml.genHtmlModule\'2E_\'23caseor0'('CurryDocParams.withMarkdown'(A),I),'Prelude.++'('Prelude.map'(partcall(1,'CurryDocHtml.genHtmlModule\'2E_\'23lambda13',[]),'CurryDocRead.getCommentType'(['^a','^u','^t','^h','^o','^r'],K)),'Prelude.map'(partcall(1,'CurryDocHtml.genHtmlModule\'2E_\'23lambda14',[]),'CurryDocRead.getCommentType'(['^v','^e','^r','^s','^i','^o','^n'],K))))))),C,D,E).

:-block 'CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt'(?,?,-,?).
'CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt_1'(E,B,F,D).

:-block 'CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt_1'(?,?,-,?).
'CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocHtml.genHtmlModule\'2E_\'23selFP5\'23maincmt_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts'(?,?,-,?).
'CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts_1'(E,B,F,D).

:-block 'CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts_1'(?,?,-,?).
'CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocHtml.genHtmlModule\'2E_\'23selFP6\'23avcmts_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genHtmlModule\'2E_\'23lambda13'(?,?,-,?).
'CurryDocHtml.genHtmlModule\'2E_\'23lambda13'(A,B,C,D):-hnf('HTML.par'(['HTML.bold'(['HTML.htxt'(['^A','^u','^t','^h','^o','^r',^:,'^ '])]),'HTML.htxt'(A)]),B,C,D).

:-block 'CurryDocHtml.genHtmlModule\'2E_\'23lambda14'(?,?,-,?).
'CurryDocHtml.genHtmlModule\'2E_\'23lambda14'(A,B,C,D):-hnf('HTML.par'(['HTML.bold'(['HTML.htxt'(['^V','^e','^r','^s','^i','^o','^n',^:,'^ '])]),'HTML.htxt'(A)]),B,C,D).

:-block 'CurryDocHtml.genHtmlType'(?,?,?,?,-,?).
'CurryDocHtml.genHtmlType'(A,B,C,D,E,F):-hnf(C,G,E,H),'CurryDocHtml.genHtmlType_3'(G,A,B,D,H,F).

:-block 'CurryDocHtml.genHtmlType_3'(?,?,?,?,-,?).
'CurryDocHtml.genHtmlType_3'('FlatCurry.Type'(A,B,C,D),E,F,G,H,I):-hnf(A,J,H,K),'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1'(J,B,C,D,E,F,G,K,I).

:-block 'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1'(?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1'('Prelude.(,)'(A,B),C,D,E,F,G,H,I,J):-!,hnf('Prelude.=='(C,'FlatCurry.Public'),K,I,L),'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1_Prelude.(,)_ComplexCase'(K,A,B,C,D,E,F,G,H,L,J).

:-block 'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1_Prelude.(,)_ComplexCase'(-,?,?,?,?,?,?,?,?,?,?),'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1_Prelude.(,)_ComplexCase'(?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1_Prelude.(,)_ComplexCase'('Prelude.True',A,B,C,D,E,F,G,H,I,J):-makeShare(K,L),makeShare(B,M),makeShare(N,O),makeShare(P,Q),makeShare(F,R),hnf('Prelude.cond'('Prelude.letrec'(L,'CurryDocRead.splitComment'('CurryDocRead.getDataComment'(M,G))),'Prelude.cond'('Prelude.letrec'(O,'CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt'(L)),'Prelude.cond'('Prelude.letrec'(Q,'CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts'(L)),['HTML.h3'(['HTML.anchor'(M,['HTML.htxt'(M)])]),'HTML.par'('CurryDocHtml.docComment2HTML'(R,O)),'HTML.par'(['CurryDocHtml.explainCat'(['^C','^o','^n','^s','^t','^r','^u','^c','^t','^o','^r','^s',^:]),'HTML.breakline','HTML.dlist'('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95',['CurryDocRead.getCommentType'(['^c','^o','^n','^s'],Q),D,M,R])),E))]),'HTML.hrule']))),H,I,J).
'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1_Prelude.(,)_ComplexCase'('Prelude.False',A,B,C,D,E,F,G,[],H,H):-!.
'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,D,E,F,G,H,'FAIL'(A),I,I).
'CurryDocHtml.genHtmlType_3_FlatCurry.Type_1'('FAIL'(A),B,C,D,E,F,'FAIL'(A),G,G):-nonvar(A).
'CurryDocHtml.genHtmlType_3'('FlatCurry.TypeSyn'(A,B,C,D),E,F,G,H,I):-!,hnf(A,J,H,K),'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1'(J,B,C,D,E,F,G,K,I).

:-block 'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1'(?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1'('Prelude.(,)'(A,B),C,D,E,F,G,H,I,J):-!,hnf('Prelude.=='(C,'FlatCurry.Public'),K,I,L),'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'(K,A,B,C,D,E,F,G,H,L,J).

:-block 'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'(-,?,?,?,?,?,?,?,?,?,?),'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'(?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'('Prelude.True',A,B,C,D,E,F,G,H,I,J):-makeShare(K,L),makeShare(B,M),makeShare(N,O),makeShare(A,P),hnf('Prelude.cond'('Prelude.letrec'(L,'CurryDocRead.splitComment'('CurryDocRead.getDataComment'(M,G))),'Prelude.cond'('Prelude.letrec'(O,'CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt'(L)),['HTML.h3'(['HTML.anchor'(M,['HTML.htxt'(M)])]),'HTML.par'('CurryDocHtml.docComment2HTML'(F,O)),'HTML.par'(['CurryDocHtml.explainCat'(['^T','^y','^p','^e','^ ','^s','^y','^n','^o','^n','^y','^m',^:]),'HTML.nbsp','CurryDocHtml.genHtmlType\'2E_\'23caseor0'('Prelude.&&'('Prelude.=='(M,['^S','^t','^r','^i','^n','^g']),'Prelude.=='(P,['^P','^r','^e','^l','^u','^d','^e'])),M,D,P,E)]),'HTML.hrule'])),H,I,J).
'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'('Prelude.False',A,B,C,D,E,F,G,[],H,H):-!.
'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,D,E,F,G,H,'FAIL'(A),I,I).
'CurryDocHtml.genHtmlType_3_FlatCurry.TypeSyn_1'('FAIL'(A),B,C,D,E,F,'FAIL'(A),G,G):-nonvar(A).
'CurryDocHtml.genHtmlType_3'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).

:-block 'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95'(?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95'(A,B,C,D,E,F,G,H):-hnf(E,I,G,J),'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5'(I,A,B,C,D,F,J,H).

:-block 'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5'(?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5'('FlatCurry.Cons'(A,B,C,D),E,F,G,H,I,J,K):-!,hnf(A,L,J,M),'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1'(L,B,C,D,E,F,G,H,I,M,K).

:-block 'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1'(?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1'('Prelude.(,)'(A,B),C,D,E,F,G,H,I,J,K,L):-!,hnf('Prelude.=='(D,'FlatCurry.Public'),M,K,N),'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1_Prelude.(,)_ComplexCase'(M,A,B,C,D,E,F,G,H,I,J,N,L).

:-block 'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1_Prelude.(,)_ComplexCase'(-,?,?,?,?,?,?,?,?,?,?,?,?),'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1_Prelude.(,)_ComplexCase'(?,?,?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1_Prelude.(,)_ComplexCase'('Prelude.True',A,B,C,D,E,F,G,H,I,['Prelude.(,)'(['Prelude.apply'('CurryDocHtml.opnameDoc',['HTML.anchor'('Prelude.++'(J,['^_','^C','^O','^N','^S']),['HTML.htxt'(J)])]),'HTML.code'(['HTML.HtmlText'('Prelude.++'(['^ ',^:,^:,'^ '],'Prelude.++'('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda15',[A])),E),'Prelude.++'(G,'Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda16',[])),H)))))])],'Prelude.maybe'([],partcall(1,'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17',[F]),'CurryDocRead.getConsComment'(I,J)))],K,L):-makeShare(B,J),K=L.
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1_Prelude.(,)_ComplexCase'('Prelude.False',A,B,C,D,E,F,G,H,I,[],J,J):-!.
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,D,E,F,G,H,I,J,'FAIL'(A),K,K).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5_FlatCurry.Cons_1'('FAIL'(A),B,C,D,E,F,G,H,'FAIL'(A),I,I):-nonvar(A).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95_5'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F):-nonvar(A).

:-block 'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda15'(?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda15'(A,B,C,D,E):-hnf('Prelude.++'(['^ '],'Prelude.++'('CurryDocHtml.showType'(A,'Prelude.True',B),['^ ',^-,^>,'^ '])),C,D,E).

:-block 'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda16'(?,?,-,?).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda16'(A,['^ ','Prelude.chr'('Prelude.+'(97,A))],B,B).

:-block 'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17'(?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17_2'(F,A,C,G,E).

:-block 'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17_2'(?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17_2'('Prelude.(,)'(A,B),C,['HTML.par'('Prelude.++'(['HTML.code'(['HTML.htxt'(A)]),'HTML.htxt'(['^ ',^:,'^ '])],'CurryDocHtml.removeTopPar'('CurryDocHtml.docComment2HTML'(C,'CurryDocHtml.removeDash'(B)))))],D,D):-!.
'CurryDocHtml.genHtmlType\'2EgenHtmlCons\'2E95\'2E_\'23lambda17_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt'(?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt_1'(E,B,F,D).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt_1'(?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocHtml.genHtmlType\'2E_\'23selFP8\'23datacmt_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts'(?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts_1'(E,B,F,D).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts_1'(?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocHtml.genHtmlType\'2E_\'23selFP9\'23conscmts_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt'(?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt_1'(E,B,F,D).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt_1'(?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocHtml.genHtmlType\'2E_\'23selFP11\'23typecmt_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23lambda18'(?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23lambda18'(A,['^ ','Prelude.chr'('Prelude.+'(97,A))],B,B).

:-block 'CurryDocHtml.genHtmlFuncShort'(?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFuncShort'(A,B,C,D,E,F,G):-hnf(D,H,F,I),'CurryDocHtml.genHtmlFuncShort_4'(H,A,B,C,E,I,G).

:-block 'CurryDocHtml.genHtmlFuncShort_4'(?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFuncShort_4'('FlatCurry.Func'(A,B,C,D,E),F,G,H,I,J,K):-!,hnf(A,L,J,M),'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1'(L,B,C,D,E,F,G,H,I,M,K).

:-block 'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1'(?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1'('Prelude.(,)'(A,B),C,D,E,F,G,H,I,J,K,L):-!,hnf('Prelude.=='(D,'FlatCurry.Public'),M,K,N),'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1_Prelude.(,)_ComplexCase'(M,A,B,C,D,E,F,G,H,I,J,N,L).

:-block 'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1_Prelude.(,)_ComplexCase'(-,?,?,?,?,?,?,?,?,?,?,?,?),'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1_Prelude.(,)_ComplexCase'(?,?,?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1_Prelude.(,)_ComplexCase'('Prelude.True',A,B,C,D,E,F,G,H,I,['HTML.table'([['Prelude.++'(['Prelude.apply'('CurryDocHtml.opnameDoc',['HTML.anchor'('Prelude.++'(J,['^_','^S','^H','^O','^R','^T']),['HTML.href'([^#|J],['HTML.htxt'('CurryDocRead.showId'(J))])])]),'HTML.code'(['HTML.HtmlText'('Prelude.++'(['^ ',^:,^:,'^ '],'CurryDocHtml.showType'(K,'Prelude.False',E)))]),'HTML.nbsp','HTML.nbsp'],'CurryDocHtml.genFuncPropIcons'(I,'Prelude.(,)'(K,J),F))],['CurryDocHtml.removeTopPar'('CurryDocHtml.docComment2HTML'(G,'CurryDocHtml.firstSentence'('Prelude.fst'('CurryDocRead.splitComment'('CurryDocRead.getFuncComment'(J,H))))))]])],L,M):-makeShare(B,J),makeShare(A,K),L=M.
'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1_Prelude.(,)_ComplexCase'('Prelude.False',A,B,C,D,E,F,G,H,I,[],J,J):-!.
'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,D,E,F,G,H,I,J,'FAIL'(A),K,K).
'CurryDocHtml.genHtmlFuncShort_4_FlatCurry.Func_1'('FAIL'(A),B,C,D,E,F,G,H,'FAIL'(A),I,I):-nonvar(A).
'CurryDocHtml.genHtmlFuncShort_4'('FAIL'(A),B,C,D,'FAIL'(A),E,E):-nonvar(A).

:-block 'CurryDocHtml.genHtmlFunc'(?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc'(A,B,C,D,E,F,G,H,I):-hnf(F,J,H,K),'CurryDocHtml.genHtmlFunc_6'(J,A,B,C,D,E,G,K,I).

:-block 'CurryDocHtml.genHtmlFunc_6'(?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc_6'('FlatCurry.Func'(A,B,C,D,E),F,G,H,I,J,K,L,M):-!,hnf(A,N,L,O),'CurryDocHtml.genHtmlFunc_6_FlatCurry.Func_1'(N,B,C,D,E,F,G,H,I,J,K,O,M).

:-block 'CurryDocHtml.genHtmlFunc_6_FlatCurry.Func_1'(?,?,?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc_6_FlatCurry.Func_1'('Prelude.(,)'(A,B),C,D,E,F,G,H,I,J,K,L,M,N):-!,makeShare(O,P),makeShare(J,Q),makeShare(A,R),makeShare(B,S),makeShare(F,T),hnf('Prelude.cond'('Prelude.letrec'(P,'CurryDocHtml.genFuncPropComments'(Q,'Prelude.(,)'(R,S),T,K)),'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0'('Prelude.=='(D,'FlatCurry.Public'),I,H,E,Q,R,T,G,S,P)),L,M,N).
'CurryDocHtml.genHtmlFunc_6_FlatCurry.Func_1'('FAIL'(A),B,C,D,E,F,G,H,I,J,'FAIL'(A),K,K):-nonvar(A).
'CurryDocHtml.genHtmlFunc_6'('FAIL'(A),B,C,D,E,F,'FAIL'(A),G,G):-nonvar(A).

:-block 'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117'(?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117'(A,B,C,D,E):-makeShare(A,F),makeShare(B,G),hnf('Prelude.||'('Char.isAlpha'('Prelude.head'(F)),'Prelude./='('Prelude.length'(G),2)),H,D,I),'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117_ComplexCase'(H,F,G,C,I,E).

:-block 'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117_ComplexCase'(-,?,?,?,?,?),'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117_ComplexCase'(?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117_ComplexCase'('Prelude.True',A,B,C,D,E):-hnf('Prelude.++'(['^('],'Prelude.++'('CurryDocRead.showId'(A),'Prelude.++'('Prelude.apply'('Prelude.concatMap'(partcall(1,'Prelude.++',[['^ ']])),B),['^)']))),C,D,E).
'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117_ComplexCase'('Prelude.False',A,B,C,D,E):-!,makeShare(B,F),hnf('Prelude.++'(['^('],'Prelude.++'('Prelude.!!'(F,0),'Prelude.++'(['^ '],'Prelude.++'(A,'Prelude.++'(['^ '],'Prelude.++'('Prelude.!!'(F,1),['^)'])))))),C,D,E).
'CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).

:-block 'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117'(?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117'(A,B,C,D,E,F):-makeShare(G,H),makeShare(C,I),makeShare(A,J),hnf('Prelude.cond'('Prelude.letrec'(H,'Prelude.map'(partcall(1,'Prelude.span',[partcall(1,'CurryDocRead.isIdChar',[])]),'CurryDocRead.getCommentType'(['^p','^a','^r','^a','^m'],I))),'Prelude.++'('CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0'('Prelude.=='(H,[]),B,J,H),['HTML.dlist'('Prelude.map'(partcall(1,'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda20',[J]),'CurryDocRead.getCommentType'(['^r','^e','^t','^u','^r','^n'],I)))])),D,E,F).

:-block 'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19'(?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19_2'(F,A,C,G,E).

:-block 'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19_2'(?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19_2'('Prelude.(,)'(A,B),C,'Prelude.(,)'([],'Prelude.++'(['HTML.code'(['HTML.htxt'(A)]),'HTML.htxt'(['^ ',^:,'^ '])],'CurryDocHtml.removeTopPar'('CurryDocHtml.docComment2HTML'(C,'CurryDocHtml.removeDash'(B))))),D,D):-!.
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda20'(?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda20'(A,B,'Prelude.(,)'(['CurryDocHtml.explainCat'(['^R','^e','^t','^u','^r','^n','^s',^:])],'CurryDocHtml.removeTopPar'('CurryDocHtml.docComment2HTML'(A,B))),C,C).

:-block 'CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt'(?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt_1'(E,B,F,D).

:-block 'CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt_1'(?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts'(?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts_1'(E,B,F,D).

:-block 'CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts_1'(?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.removeDash'(?,?,-,?).
'CurryDocHtml.removeDash'(A,B,C,D):-makeShare(E,F),hnf('Prelude.cond'('Prelude.letrec'(F,'Prelude.dropWhile'(partcall(1,'Char.isSpace',[]),A)),'CurryDocHtml.removeDash\'2E_\'23caseor0'('Prelude.=='('Prelude.take'(2,F),[^-,'^ ']),F)),B,C,D).

:-block 'CurryDocHtml.removeTopPar'(?,?,-,?).
'CurryDocHtml.removeTopPar'(A,B,C,D):-makeShare(A,E),hnf(E,F,C,G),'CurryDocHtml.removeTopPar_1'(F,F,B,G,D).

:-block 'CurryDocHtml.removeTopPar_1'(-,?,?,?,?),'CurryDocHtml.removeTopPar_1'(?,?,?,-,?).
'CurryDocHtml.removeTopPar_1'([A|B],C,D,E,F):-hnf(A,G,E,H),'CurryDocHtml.removeTopPar_1_._1'(G,B,C,D,H,F).

:-block 'CurryDocHtml.removeTopPar_1_._1'(-,?,?,?,?,?),'CurryDocHtml.removeTopPar_1_._1'(?,?,?,?,-,?).
'CurryDocHtml.removeTopPar_1_._1'('HTML.HtmlStruct'(A,B,C),D,E,F,G,H):-hnf(A,I,G,J),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1'(I,B,C,D,E,F,J,H).

:-block 'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1'(-,?,?,?,?,?,?,?),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1'(?,?,?,?,?,?,-,?).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1'([A|B],C,D,E,F,G,H,I):-hnf('Prelude.=='(A,'^p'),J,H,K),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase'(J,A,B,C,D,E,F,G,K,I).

:-block 'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase'(-,?,?,?,?,?,?,?,?,?),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase'(?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase'('Prelude.True',A,B,C,D,E,F,G,H,I):-hnf(B,J,H,K),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2'(J,A,C,D,E,F,G,K,I).

:-block 'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2'(-,?,?,?,?,?,?,?,?),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2'(?,?,?,?,?,?,?,-,?).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2'([],A,B,C,D,E,F,G,H):-hnf(B,I,G,J),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2'(I,A,C,D,E,F,J,H).

:-block 'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2'(-,?,?,?,?,?,?,?),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2'(?,?,?,?,?,?,-,?).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2'([],A,B,C,D,E,F,G):-hnf(C,H,F,I),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2_[]_3'(H,A,B,D,E,I,G).

:-block 'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2_[]_3'(-,?,?,?,?,?,?),'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2_[]_3'(?,?,?,?,?,-,?).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2_[]_3'([],A,B,C,D,E,F):-hnf(B,D,E,F).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2_[]_3'([A|B],C,D,E,F,G,H):-!,hnf(E,F,G,H).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2_[]_3'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2'([A|B],C,D,E,F,G,H,I):-!,hnf(F,G,H,I).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2_[]_2'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2'([A|B],C,D,E,F,G,H,I,J):-!,hnf(G,H,I,J).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase_Prelude.True_2'('FAIL'(A),B,C,D,E,F,'FAIL'(A),G,G).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase'('Prelude.False',A,B,C,D,E,F,G,H,I):-!,hnf(F,G,H,I).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1_._ComplexCase'('FAIL'(A),B,C,D,E,F,G,'FAIL'(A),H,H).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1'([],A,B,C,D,E,F,G):-!,hnf(D,E,F,G).
'CurryDocHtml.removeTopPar_1_._1_HTML.HtmlStruct_1'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.removeTopPar_1_._1'('HTML.HtmlText'(A),B,C,D,E,F):-hnf(C,D,E,F).
'CurryDocHtml.removeTopPar_1_._1'('HTML.HtmlCRef'(A,B),C,D,E,F,G):-hnf(D,E,F,G).
'CurryDocHtml.removeTopPar_1_._1'('HTML.HtmlEvent'(A,B),C,D,E,F,G):-!,hnf(D,E,F,G).
'CurryDocHtml.removeTopPar_1_._1'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocHtml.removeTopPar_1'([],A,B,C,D):-!,hnf(A,B,C,D).
'CurryDocHtml.removeTopPar_1'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocHtml.genFuncPropIcons'(?,?,?,?,-,?).
'CurryDocHtml.genFuncPropIcons'(A,B,C,D,E,F):-makeShare(G,H),hnf('Prelude.cond'('Prelude.letrec'(H,'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0'('Prelude.apply'('CurryDocRead.getOverlappingInfo'(A),B))),[H,'HTML.nbsp','CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146'(C)]),D,E,F).

:-block 'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146'(?,?,-,?).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146_1'(E,B,F,D).

:-block 'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146_1'(?,?,-,?).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146_1'('FlatCurry.External'(A),B,C,D):-hnf('HTML.htxt'([]),B,C,D).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146_1'('FlatCurry.Rule'(A,B),C,D,E):-!,hnf('CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153'('FlexRigid.getFlexRigid'(B)),C,D,E).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153'(?,?,-,?).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153_1'(E,B,F,D).

:-block 'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153_1'(?,?,-,?).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153_1'('FlexRigid.ConflictFR',A,B,C):-hnf('HTML.href'(['^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l',^#,'^f','^l','^e','^x','^r','^i','^g','^i','^d','^_','^e','^x','^p','^l','^a','^i','^n'],['Prelude.$'(partcall(1,'CurryDocHtml.addIconParams',[]),'HTML.image'(['^f','^l','^e','^x','^r','^i','^g','^i','^d',^.,'^g','^i','^f'],['^f','^l','^e','^x','^i','^b','^l','^e',^+,'^r','^i','^g','^i','^d']))]),A,B,C).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153_1'('FlexRigid.UnknownFR',A,B,C):-hnf('HTML.htxt'([]),A,B,C).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153_1'('FlexRigid.KnownRigid',A,B,C):-hnf('HTML.href'(['^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l',^#,'^r','^i','^g','^i','^d','^_','^e','^x','^p','^l','^a','^i','^n'],['Prelude.$'(partcall(1,'CurryDocHtml.addIconParams',[]),'HTML.image'(['^r','^i','^g','^i','^d',^.,'^g','^i','^f'],['^r','^i','^g','^i','^d']))]),A,B,C).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153_1'('FlexRigid.KnownFlex',A,B,C):-!,hnf('HTML.href'(['^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l',^#,'^f','^l','^e','^x','^_','^e','^x','^p','^l','^a','^i','^n'],['Prelude.$'(partcall(1,'CurryDocHtml.addIconParams',[]),'HTML.image'(['^f','^l','^e','^x',^.,'^g','^i','^f'],['^f','^l','^e','^x','^i','^b','^l','^e']))]),A,B,C).
'CurryDocHtml.genFuncPropIcons\'2EflexRigidIcon\'2E146\'2EimageEvalAnnot\'2E153_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.addIconParams'(?,?,-,?).
'CurryDocHtml.addIconParams'(A,B,C,D):-hnf('HTML.addAttr'('HTML.addAttr'(A,'Prelude.(,)'(['^a','^l','^i','^g','^n'],['^m','^i','^d','^d','^l','^e'])),'Prelude.(,)'(['^b','^o','^r','^d','^e','^r'],['^0'])),B,C,D).

:-block 'CurryDocHtml.genFuncPropComments'(?,?,?,?,?,-,?).
'CurryDocHtml.genFuncPropComments'(A,B,C,D,E,F,G):-makeShare(H,I),makeShare(A,J),makeShare(B,K),makeShare(L,M),makeShare(N,O),makeShare(P,Q),hnf('Prelude.cond'('Prelude.letrec'(I,'Prelude.apply'('CurryDocRead.getCompleteInfo'(J),K)),'Prelude.cond'('Prelude.letrec'(M,'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0'('Prelude.=='(I,'AnaCompleteness.Complete'),I)),'Prelude.cond'('Prelude.letrec'(O,'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0'('Prelude.apply'('CurryDocRead.getIndetInfo'(J),K))),'Prelude.cond'('Prelude.letrec'(Q,'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0'('Prelude.apply'('CurryDocRead.getOpCompleteInfo'(J),K))),'Prelude.filter'(partcall(1,'Prelude.flip',[[],partcall(2,'Prelude./=',[])]),['CurryDocHtml.genFixityInfo'(K,D),M,O,Q,'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165'(C)]))))),E,F,G).

:-block 'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165'(?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165_1'(E,B,F,D).

:-block 'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165_1'(?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165_1'('FlatCurry.External'(A),['HTML.htxt'(['^e','^x','^t','^e','^r','^n','^a','^l','^l','^y','^ ','^d','^e','^f','^i','^n','^e','^d'])],B,B).
'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165_1'('FlatCurry.Rule'(A,B),[],C,C):-!.
'CurryDocHtml.genFuncPropComments\'2EexternalInfo\'2E165_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genFixityInfo'(?,?,?,-,?).
'CurryDocHtml.genFixityInfo'(A,B,C,D,E):-hnf('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genFixityInfo\'2E_\'23lambda22',[A])),B),C,D,E).

:-block 'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179'(?,?,-,?).
'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179_1'(E,B,F,D).

:-block 'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179_1'(?,?,-,?).
'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179_1'('FlatCurry.InfixOp',['^n','^o','^n',^-,'^a','^s','^s','^o','^c','^i','^a','^t','^i','^v','^e'],A,A).
'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179_1'('FlatCurry.InfixlOp',['^l','^e','^f','^t',^-,'^a','^s','^s','^o','^c','^i','^a','^t','^i','^v','^e'],A,A).
'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179_1'('FlatCurry.InfixrOp',['^r','^i','^g','^h','^t',^-,'^a','^s','^s','^o','^c','^i','^a','^t','^i','^v','^e'],A,A):-!.
'CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genFixityInfo\'2E_\'23lambda22'(?,?,?,-,?).
'CurryDocHtml.genFixityInfo\'2E_\'23lambda22'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2'(F,A,C,G,E).

:-block 'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2'(?,?,?,-,?).
'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2'('FlatCurry.Op'(A,B,C),D,E,F,G):-!,hnf('Prelude.=='(A,D),H,F,I),'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2_FlatCurry.Op_ComplexCase'(H,A,B,C,D,E,I,G).

:-block 'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2_FlatCurry.Op_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2_FlatCurry.Op_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2_FlatCurry.Op_ComplexCase'('Prelude.True',A,B,C,D,['HTML.htxt'('Prelude.++'(['^d','^e','^f','^i','^n','^e','^d','^ ','^a','^s','^ '],'Prelude.++'('CurryDocHtml.genFixityInfo\'2EshowFixity\'2E179'(B),'Prelude.++'(['^ ','^i','^n','^f','^i','^x','^ ','^o','^p','^e','^r','^a','^t','^o','^r','^ ','^w','^i','^t','^h','^ ','^p','^r','^e','^c','^e','^d','^e','^n','^c','^e','^ '],'Prelude.show'(C)))))],E,E).
'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2_FlatCurry.Op_ComplexCase'('Prelude.False',A,B,C,D,[],E,E):-!.
'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2_FlatCurry.Op_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.genFixityInfo\'2E_\'23lambda22_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocHtml.showType'(?,?,?,?,-,?).
'CurryDocHtml.showType'(A,B,C,D,E,F):-hnf(C,G,E,H),'CurryDocHtml.showType_3'(G,A,B,D,H,F).

:-block 'CurryDocHtml.showType_3'(?,?,?,?,-,?).
'CurryDocHtml.showType_3'('FlatCurry.TVar'(A),B,C,['Prelude.chr'('Prelude.+'(97,A))],D,D).
'CurryDocHtml.showType_3'('FlatCurry.FuncType'(A,B),C,D,E,F,G):-makeShare(C,H),makeShare(A,I),hnf('CurryDocRead.brackets'(D,'Prelude.++'('CurryDocHtml.showType'(H,'CurryDocRead.isFunctionType'(I),I),'Prelude.++'(['^ ',^-,^&,'^g','^t','^;','^ '],'CurryDocHtml.showType'(H,'Prelude.False',B)))),E,F,G).
'CurryDocHtml.showType_3'('FlatCurry.TCons'(A,B),C,D,E,F,G):-!,makeShare(B,H),hnf('Prelude.=='(H,[]),I,F,J),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase'(I,A,H,C,D,E,J,G).

:-block 'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf('CurryDocHtml.showTypeCons'(C,A),E,F,G).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,makeShare(A,H),makeShare(B,I),hnf('Prelude.&&'('Prelude.=='(H,'Prelude.(,)'(['^P','^r','^e','^l','^u','^d','^e'],['^[','^]'])),'Prelude.=='('Prelude.head'(I),'FlatCurry.TCons'('Prelude.(,)'(['^P','^r','^e','^l','^u','^d','^e'],['^C','^h','^a','^r']),[]))),J,F,K),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase'(J,H,I,C,D,E,K,G).

:-block 'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,['^S','^t','^r','^i','^n','^g'],E,E).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,makeShare(A,H),hnf('Prelude.=='(H,'Prelude.(,)'(['^P','^r','^e','^l','^u','^d','^e'],['^[','^]'])),I,F,J),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(I,H,B,C,D,E,J,G).

:-block 'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf('Prelude.++'(['^['],'Prelude.++'('CurryDocHtml.showType'(C,'Prelude.False','Prelude.head'(B)),['^]'])),E,F,G).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,makeShare(A,H),hnf('Prelude.=='('Prelude.take'(2,'Prelude.snd'(H)),['^(','^,']),I,F,J),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(I,H,B,C,D,E,J,G).

:-block 'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf('Prelude.++'(['^('],'Prelude.++'('Prelude.concat'('List.intersperse'(['^,'],'Prelude.map'(partcall(1,'CurryDocHtml.showType',['Prelude.False',C]),B))),['^)'])),E,F,G).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,hnf('Prelude.otherwise',H,F,I),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(H,A,B,C,D,E,I,G).

:-block 'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-makeShare(C,H),hnf('CurryDocRead.brackets'(D,'Prelude.++'('CurryDocHtml.showTypeCons'(H,A),'Prelude.++'(['^ '],'Prelude.concat'('List.intersperse'(['^ '],'Prelude.map'(partcall(1,'CurryDocHtml.showType',['Prelude.True',H]),B)))))),E,F,G).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,hnf('Prelude.failure'('CurryDocHtml.showType',['Prelude.False']),E,F,G).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.showType_3_FlatCurry.TCons_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.showType_3'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).

:-block 'CurryDocHtml.showTypeCons'(?,?,?,-,?).
'CurryDocHtml.showTypeCons'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocHtml.showTypeCons_2'(F,A,C,G,E).

:-block 'CurryDocHtml.showTypeCons_2'(?,?,?,-,?).
'CurryDocHtml.showTypeCons_2'('Prelude.(,)'(A,B),C,D,E,F):-!,makeShare(A,G),hnf('Prelude.=='(G,['^P','^r','^e','^l','^u','^d','^e']),H,E,I),'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase'(H,G,B,C,D,I,F).

:-block 'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase'(-,?,?,?,?,?,?),'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase'(?,?,?,?,?,-,?).
'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase'('Prelude.True',A,B,C,D,E,F):-hnf(B,D,E,F).
'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase'('Prelude.False',A,B,C,D,E,F):-!,makeShare(A,G),hnf('Prelude.=='(C,G),H,E,I),'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase_Prelude.False_ComplexCase'(H,G,B,C,D,I,F).

:-block 'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase_Prelude.False_ComplexCase'(-,?,?,?,?,?,?),'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase_Prelude.False_ComplexCase'(?,?,?,?,?,-,?).
'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E,F):-makeShare(B,G),hnf('Prelude.++'([^<,'^a','^ ','^h','^r','^e','^f',^=,'^"',^#],'Prelude.++'(G,'Prelude.++'(['^"',^>],'Prelude.++'(G,[^<,^/,'^a',^>])))),D,E,F).
'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E,F):-!,makeShare(B,G),hnf('Prelude.++'([^<,'^a','^ ','^h','^r','^e','^f',^=,'^"'],'Prelude.++'(A,'Prelude.++'([^.,'^h','^t','^m','^l',^#],'Prelude.++'(G,'Prelude.++'(['^"',^>],'Prelude.++'(G,[^<,^/,'^a',^>])))))),D,E,F).
'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocHtml.showTypeCons_2_Prelude.(,)_ComplexCase'('FAIL'(A),B,C,D,'FAIL'(A),E,E).
'CurryDocHtml.showTypeCons_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocHtml.translateSource2ColoredHtml'(?,?,?,-,?).
'CurryDocHtml.translateSource2ColoredHtml'(A,B,C,D,E):-makeShare(F,G),makeShare(B,H),hnf('Prelude.cond'('Prelude.letrec'(G,'Prelude.++'(A,'Prelude.++'([^/],'Prelude.++'('Prelude.apply'('CurryDocRead.getLastName',H),['^_','^c','^u','^r','^r','^y',^.,'^h','^t','^m','^l'])))),'Prelude.>>'('Prelude.putStrLn'('Prelude.++'(['^W','^r','^i','^t','^i','^n','^g','^ ','^s','^o','^u','^r','^c','^e','^ ','^f','^i','^l','^e','^ ','^a','^s','^ ','^H','^T','^M','^L','^ ','^t','^o','^ ','^"'],'Prelude.++'(G,['^"',^.,^.,^.]))),'Distribution.callFrontendWithParams'('Distribution.HTML','Distribution.setQuiet'('Prelude.True','Distribution.setOutfile'(G,'Distribution.defaultParams')),H))),C,D,E).

:-block 'CurryDocHtml.translateSource2AnchoredHtml'(?,?,?,-,?).
'CurryDocHtml.translateSource2AnchoredHtml'(A,B,C,D,E):-makeShare(A,F),makeShare(B,G),hnf('Prelude.>>'('Prelude.putStrLn'('Prelude.++'(['^W','^r','^i','^t','^i','^n','^g','^ ','^s','^o','^u','^r','^c','^e','^ ','^f','^i','^l','^e','^ ','^a','^s','^ ','^H','^T','^M','^L','^ ','^t','^o','^ ','^"'],'Prelude.++'(F,'Prelude.++'([^/],'Prelude.++'('Prelude.apply'('CurryDocRead.getLastName',G),['^_','^c','^u','^r','^r','^y',^.,'^h','^t','^m','^l','^"',^.,^.,^.]))))),'Prelude.>>='('Prelude.readFile'('Prelude.++'(G,[^.,'^c','^u','^r','^r','^y'])),partcall(1,'CurryDocHtml.translateSource2AnchoredHtml\'2E_\'23lambda23',[G,F]))),C,D,E).

:-block 'CurryDocHtml.translateSource2AnchoredHtml\'2E_\'23lambda23'(?,?,?,?,-,?).
'CurryDocHtml.translateSource2AnchoredHtml\'2E_\'23lambda23'(A,B,C,D,E,F):-makeShare(B,G),hnf('Prelude.writeFile'('Prelude.++'(A,'Prelude.++'([^/],'Prelude.++'('Prelude.apply'('CurryDocRead.getLastName',G),['^_','^c','^u','^r','^r','^y',^.,'^h','^t','^m','^l']))),'CurryDocHtml.showDocCSS'('Prelude.++'(G,[^.,'^c','^u','^r','^r','^y']),['HTML.HtmlStruct'(['^P','^R','^E'],[],['HTML.HtmlText'('CurryDocHtml.addFuncAnchors'([],'Prelude.lines'(C)))])])),D,E,F).

:-block 'CurryDocHtml.addFuncAnchors'(?,?,?,-,?).
'CurryDocHtml.addFuncAnchors'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocHtml.addFuncAnchors_2'(F,A,C,G,E).

:-block 'CurryDocHtml.addFuncAnchors_2'(?,?,?,-,?).
'CurryDocHtml.addFuncAnchors_2'([],A,[],B,B).
'CurryDocHtml.addFuncAnchors_2'([A|B],C,D,E,F):-!,makeShare(G,H),makeShare(A,I),hnf('Prelude.cond'('Prelude.letrec'(H,'CurryDocRead.getFirstId'(I)),'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0'('Prelude.||'('Prelude.=='(H,[]),'Prelude.apply'('Prelude.elem'(H),[['^d','^a','^t','^a'],['^t','^y','^p','^e'],['^i','^m','^p','^o','^r','^t'],['^m','^o','^d','^u','^l','^e'],['^i','^n','^f','^i','^x'],['^i','^n','^f','^i','^x','^l'],['^i','^n','^f','^i','^x','^r']])),I,H,C,B)),D,E,F).
'CurryDocHtml.addFuncAnchors_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocHtml.genMainIndexPage'(?,?,?,?,?,-,?).
'CurryDocHtml.genMainIndexPage'(A,B,C,D,E,F,G):-makeShare(C,H),hnf('Prelude.>>'('Prelude.putStrLn'('Prelude.++'(['^W','^r','^i','^t','^i','^n','^g','^ ','^i','^n','^d','^e','^x','^ ','^p','^a','^g','^e','^ ','^t','^o','^ ','^"'],'Prelude.++'(H,[^/,'^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l','^"',^.,^.,^.]))),'Prelude.writeFile'('Prelude.++'(H,[^/,'^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l']),'CurryDocHtml.showDocCSS'(['^D','^o','^c','^u','^m','^e','^n','^t','^a','^t','^i','^o','^n','^ ','^o','^f','^ ','^C','^u','^r','^r','^y','^ ','^m','^o','^d','^u','^l','^e','^s'],'Prelude.++'('CurryDocHtml.htmlIndex'(D),'CurryDocHtml.curryDocEpilog'(A,B))))),E,F,G).

:-block 'CurryDocHtml.htmlIndex'(?,?,-,?).
'CurryDocHtml.htmlIndex'(A,B,C,D):-makeShare(A,E),hnf('Prelude.++'('CurryDocHtml.htmlIndex\'2E_\'23caseor0'('Prelude.=='('Prelude.length'(E),1),E),['HTML.ulist'([['HTML.href'(['^f','^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l'],['HTML.htxt'(['^A','^l','^l','^ ','^f','^u','^n','^c','^t','^i','^o','^n','^s'])])],['HTML.href'(['^c','^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l'],['HTML.htxt'(['^A','^l','^l','^ ','^c','^o','^n','^s','^t','^r','^u','^c','^t','^o','^r','^s'])])]]),'HTML.bold'(['HTML.htxt'(['^E','^x','^p','^l','^a','^n','^a','^t','^i','^o','^n','^s','^ ','^o','^f','^ ','^t','^h','^e','^ ','^i','^c','^o','^n','^s','^ ','^u','^s','^e','^d','^ ','^i','^n','^ ','^t','^h','^e','^ ','^d','^o','^c','^u','^m','^e','^n','^t','^a','^t','^i','^o','^n',^:])]),'HTML.par'(['HTML.anchor'(['^d','^e','^t','^_','^e','^x','^p','^l','^a','^i','^n'],['HTML.image'(['^d','^e','^t',^.,'^g','^i','^f'],['^d','^e','^t','^e','^r','^m','^i','^n','^i','^s','^t','^i','^c'])]),'HTML.htxt'(['^ ','^F','^u','^n','^c','^t','^i','^o','^n','^ ','^i','^s','^ ','^d','^e','^t','^e','^r','^m','^i','^n','^i','^s','^t','^i','^c','^a','^l','^l','^y','^ ','^d','^e','^f','^i','^n','^e','^d','^,','^ ','^i',^.,'^e',^.,'^,']),'HTML.htxt'(['^ ','^p','^a','^t','^t','^e','^r','^n','^s','^ ','^a','^r','^e','^ ','^p','^a','^i','^r','^w','^i','^s','^e','^ ','^e','^x','^c','^l','^u','^s','^i','^v','^e'])]),'HTML.par'(['HTML.anchor'(['^n','^o','^n','^d','^e','^t','^_','^e','^x','^p','^l','^a','^i','^n'],['HTML.image'(['^n','^o','^n','^d','^e','^t',^.,'^g','^i','^f'],['^n','^o','^n',^-,'^d','^e','^t','^e','^r','^m','^i','^n','^i','^s','^t','^i','^c'])]),'HTML.htxt'(['^ ','^F','^u','^n','^c','^t','^i','^o','^n','^ ','^i','^s','^ ','^n','^o','^n',^-,'^d','^e','^t','^e','^r','^m','^i','^n','^i','^s','^t','^i','^c','^a','^l','^l','^y','^ ','^d','^e','^f','^i','^n','^e','^d','^,','^ ','^i',^.,'^e',^.,'^,']),'HTML.htxt'(['^ ','^c','^o','^n','^t','^a','^i','^n','^s','^ ','^o','^v','^e','^r','^l','^a','^p','^p','^i','^n','^g','^ ','^p','^a','^t','^t','^e','^r','^n','^s'])]),'HTML.par'(['HTML.anchor'(['^r','^i','^g','^i','^d','^_','^e','^x','^p','^l','^a','^i','^n'],['HTML.image'(['^r','^i','^g','^i','^d',^.,'^g','^i','^f'],['^r','^i','^g','^i','^d'])]),'HTML.htxt'(['^ ','^F','^u','^n','^c','^t','^i','^o','^n','^ ','^i','^s','^ ','^r','^i','^g','^i','^d'])]),'HTML.par'(['HTML.anchor'(['^f','^l','^e','^x','^_','^e','^x','^p','^l','^a','^i','^n'],['HTML.image'(['^f','^l','^e','^x',^.,'^g','^i','^f'],['^f','^l','^e','^x','^i','^b','^l','^e'])]),'HTML.htxt'(['^ ','^F','^u','^n','^c','^t','^i','^o','^n','^ ','^i','^s','^ ','^f','^l','^e','^x','^i','^b','^l','^e'])]),'HTML.par'(['HTML.anchor'(['^f','^l','^e','^x','^r','^i','^g','^i','^d','^_','^e','^x','^p','^l','^a','^i','^n'],['HTML.image'(['^f','^l','^e','^x','^r','^i','^g','^i','^d',^.,'^g','^i','^f'],['^f','^l','^e','^x','^i','^b','^l','^e',^+,'^r','^i','^g','^i','^d'])]),'HTML.htxt'(['^ ','^F','^u','^n','^c','^t','^i','^o','^n','^ ','^i','^s','^ ','^p','^a','^r','^t','^i','^a','^l','^l','^y','^ ','^f','^l','^e','^x','^i','^b','^l','^e','^ ','^a','^n','^d','^ ','^p','^a','^r','^t','^i','^a','^l','^l','^y','^ ','^r','^i','^g','^i','^d'])])]),B,C,D).

:-block 'CurryDocHtml.htmlIndex\'2E_\'23lambda24'(?,?,-,?).
'CurryDocHtml.htmlIndex\'2E_\'23lambda24'(A,['HTML.href'('Prelude.++'(B,[^.,'^h','^t','^m','^l']),['HTML.htxt'('Prelude.++'(B,[^.,'^c','^u','^r','^r','^y','^ ']))])],C,D):-makeShare(A,B),C=D.

:-block 'CurryDocHtml.genFunctionIndexPage'(?,?,?,?,?,-,?).
'CurryDocHtml.genFunctionIndexPage'(A,B,C,D,E,F,G):-makeShare(H,I),makeShare(C,J),hnf('Prelude.cond'('Prelude.letrec'(I,'Prelude.map'(partcall(1,'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25',[]),'Prelude.filter'(partcall(1,'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26',[]),D))),'Prelude.>>'('Prelude.putStrLn'('Prelude.++'(['^W','^r','^i','^t','^i','^n','^g','^ ','^f','^u','^n','^c','^t','^i','^o','^n','^ ','^i','^n','^d','^e','^x','^ ','^p','^a','^g','^e','^ ','^t','^o','^ ','^"'],'Prelude.++'(J,[^/,'^f','^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l','^"',^.,^.,^.]))),'Prelude.writeFile'('Prelude.++'(J,[^/,'^f','^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l']),'CurryDocHtml.showDocCSS'(['^I','^n','^d','^e','^x','^ ','^t','^o','^ ','^a','^l','^l','^ ','^f','^u','^n','^c','^t','^i','^o','^n','^s'],'Prelude.++'('CurryDocHtml.htmlFuncIndex'('CurryDocHtml.sortNames'(I)),'CurryDocHtml.curryDocEpilog'(A,B)))))),E,F,G).

:-block 'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25'(?,?,-,?).
'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25_1'(E,B,F,D).

:-block 'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25_1'(?,?,-,?).
'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25_1'('FlatCurry.Func'(A,B,C,D,E),F,G,H):-!,hnf(A,F,G,H).
'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda25_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26'(?,?,-,?).
'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26_1'(E,B,F,D).

:-block 'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26_1'(?,?,-,?).
'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26_1'('FlatCurry.Func'(A,B,C,D,E),F,G,H):-!,hnf('Prelude.=='(C,'FlatCurry.Public'),F,G,H).
'CurryDocHtml.genFunctionIndexPage\'2E_\'23lambda26_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.htmlFuncIndex'(?,?,-,?).
'CurryDocHtml.htmlFuncIndex'(A,B,C,D):-hnf('Prelude.++'(['HTML.h1'(['HTML.htxt'(['^I','^n','^d','^e','^x','^ ','^t','^o','^ ','^a','^l','^l','^ ','^f','^u','^n','^c','^t','^i','^o','^n','^s'])])],'CategorizedHtmlList.categorizeByItemKey'('Prelude.map'(partcall(1,'CurryDocHtml.showModNameRef',[]),A))),B,C,D).

:-block 'CurryDocHtml.showModNameRef'(?,?,-,?).
'CurryDocHtml.showModNameRef'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.showModNameRef_1'(E,B,F,D).

:-block 'CurryDocHtml.showModNameRef_1'(?,?,-,?).
'CurryDocHtml.showModNameRef_1'('Prelude.(,)'(A,B),'Prelude.(,)'(C,['HTML.href'('Prelude.++'(D,'Prelude.++'([^.,'^h','^t','^m','^l',^#],C)),['HTML.htxt'(C)]),'HTML.nbsp','HTML.nbsp','HTML.htxt'(['^(']),'HTML.href'('Prelude.++'('Prelude.apply'('CurryDocRead.getLastName',D),[^.,'^h','^t','^m','^l']),['HTML.htxt'(D)]),'HTML.htxt'(['^)'])]),E,F):-!,makeShare(B,C),makeShare(A,D),E=F.
'CurryDocHtml.showModNameRef_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.sortNames'(?,?,-,?).
'CurryDocHtml.sortNames'(A,B,C,D):-hnf('Sort.mergeSort'(partcall(2,'CurryDocHtml.sortNames\'2E_\'23lambda27',[]),A),B,C,D).

:-block 'CurryDocHtml.sortNames\'2E_\'23lambda27'(?,?,?,-,?).
'CurryDocHtml.sortNames\'2E_\'23lambda27'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocHtml.sortNames\'2E_\'23lambda27_1'(F,B,C,G,E).

:-block 'CurryDocHtml.sortNames\'2E_\'23lambda27_1'(?,?,?,-,?).
'CurryDocHtml.sortNames\'2E_\'23lambda27_1'('Prelude.(,)'(A,B),C,D,E,F):-!,hnf(C,G,E,H),'CurryDocHtml.sortNames\'2E_\'23lambda27_1_Prelude.(,)_3'(G,A,B,D,H,F).

:-block 'CurryDocHtml.sortNames\'2E_\'23lambda27_1_Prelude.(,)_3'(?,?,?,?,-,?).
'CurryDocHtml.sortNames\'2E_\'23lambda27_1_Prelude.(,)_3'('Prelude.(,)'(A,B),C,D,E,F,G):-!,hnf('Prelude.apply'('Prelude.apply'('Sort.leqStringIgnoreCase',D),B),E,F,G).
'CurryDocHtml.sortNames\'2E_\'23lambda27_1_Prelude.(,)_3'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).
'CurryDocHtml.sortNames\'2E_\'23lambda27_1'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocHtml.genConsIndexPage'(?,?,?,?,?,-,?).
'CurryDocHtml.genConsIndexPage'(A,B,C,D,E,F,G):-makeShare(H,I),makeShare(C,J),hnf('Prelude.cond'('Prelude.letrec'(I,'Prelude.map'(partcall(1,'CurryDocHtml.genConsIndexPage\'2E_\'23lambda28',[]),'Prelude.filter'(partcall(1,'CurryDocHtml.genConsIndexPage\'2E_\'23lambda29',[]),'Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242',[])),D)))),'Prelude.>>'('Prelude.putStrLn'('Prelude.++'(['^W','^r','^i','^t','^i','^n','^g','^ ','^c','^o','^n','^s','^t','^r','^u','^c','^t','^o','^r','^ ','^i','^n','^d','^e','^x','^ ','^p','^a','^g','^e','^ ','^t','^o','^ ','^"'],'Prelude.++'(J,[^/,'^c','^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l','^"',^.,^.,^.]))),'Prelude.writeFile'('Prelude.++'(J,[^/,'^c','^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l']),'CurryDocHtml.showDocCSS'(['^I','^n','^d','^e','^x','^ ','^t','^o','^ ','^a','^l','^l','^ ','^c','^o','^n','^s','^t','^r','^u','^c','^t','^o','^r','^s'],'Prelude.++'('CurryDocHtml.htmlConsIndex'('CurryDocHtml.sortNames'(I)),'CurryDocHtml.curryDocEpilog'(A,B)))))),E,F,G).

:-block 'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242'(?,?,-,?).
'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242_1'(E,B,F,D).

:-block 'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242_1'(?,?,-,?).
'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242_1'('FlatCurry.Type'(A,B,C,D),E,F,G):-hnf(D,E,F,G).
'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242_1'('FlatCurry.TypeSyn'(A,B,C,D),[],E,E):-!.
'CurryDocHtml.genConsIndexPage\'2EgetCons\'2E242_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genConsIndexPage\'2E_\'23lambda28'(?,?,-,?).
'CurryDocHtml.genConsIndexPage\'2E_\'23lambda28'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genConsIndexPage\'2E_\'23lambda28_1'(E,B,F,D).

:-block 'CurryDocHtml.genConsIndexPage\'2E_\'23lambda28_1'(?,?,-,?).
'CurryDocHtml.genConsIndexPage\'2E_\'23lambda28_1'('FlatCurry.Cons'(A,B,C,D),E,F,G):-!,hnf(A,E,F,G).
'CurryDocHtml.genConsIndexPage\'2E_\'23lambda28_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.genConsIndexPage\'2E_\'23lambda29'(?,?,-,?).
'CurryDocHtml.genConsIndexPage\'2E_\'23lambda29'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genConsIndexPage\'2E_\'23lambda29_1'(E,B,F,D).

:-block 'CurryDocHtml.genConsIndexPage\'2E_\'23lambda29_1'(?,?,-,?).
'CurryDocHtml.genConsIndexPage\'2E_\'23lambda29_1'('FlatCurry.Cons'(A,B,C,D),E,F,G):-!,hnf('Prelude.=='(C,'FlatCurry.Public'),E,F,G).
'CurryDocHtml.genConsIndexPage\'2E_\'23lambda29_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.htmlConsIndex'(?,?,-,?).
'CurryDocHtml.htmlConsIndex'(A,B,C,D):-hnf('Prelude.++'(['HTML.h1'(['HTML.htxt'(['^I','^n','^d','^e','^x','^ ','^t','^o','^ ','^a','^l','^l','^ ','^c','^o','^n','^s','^t','^r','^u','^c','^t','^o','^r','^s'])])],'CategorizedHtmlList.categorizeByItemKey'('Prelude.map'(partcall(1,'CurryDocHtml.showModNameRef',[]),A))),B,C,D).

:-block 'CurryDocHtml.explainCat'(?,?,-,?).
'CurryDocHtml.explainCat'(A,B,C,D):-hnf('HTML.textstyle'(['^e','^x','^p','^l','^a','^i','^n','^c','^a','^t'],A),B,C,D).

:-block 'CurryDocHtml.opnameDoc'(?,-,?).
'CurryDocHtml.opnameDoc'(A,B,C):-hnf(partcall(1,'HTML.style',[['^o','^p','^n','^a','^m','^e']]),A,B,C).

:-block 'CurryDocHtml.showDocCSS'(?,?,?,-,?).
'CurryDocHtml.showDocCSS'(A,B,C,D,E):-hnf('HTML.showHtmlPage'('HTML.addPageParam'('HTML.page'(A,B),'HTML.pageCSS'('CurryDocHtml.currydocCSS'))),C,D,E).

:-block 'CurryDocHtml.sortStrings'(?,?,-,?).
'CurryDocHtml.sortStrings'(A,B,C,D):-hnf('Sort.mergeSort'('Sort.leqStringIgnoreCase',A),B,C,D).

:-block 'CurryDocHtml.firstSentence'(?,?,-,?).
'CurryDocHtml.firstSentence'(A,B,C,D):-makeShare(E,F),makeShare(G,H),makeShare(I,J),hnf('Prelude.cond'('Prelude.letrec'(F,'Prelude.apply'('Prelude.break'(partcall(1,'Prelude.flip',[^.,partcall(2,'Prelude.==',[])])),A)),'Prelude.cond'('Prelude.letrec'(H,'CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs'(F)),'Prelude.cond'('Prelude.letrec'(J,'CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls'(F)),'CurryDocHtml.firstSentence\'2E_\'23caseor0'('Prelude.=='(J,[]),H,J)))),B,C,D).

:-block 'CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs'(?,?,-,?).
'CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs_1'(E,B,F,D).

:-block 'CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs_1'(?,?,-,?).
'CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(A,C,D,E).
'CurryDocHtml.firstSentence\'2E_\'23selFP16\'23fs_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls'(?,?,-,?).
'CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls_1'(E,B,F,D).

:-block 'CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls_1'(?,?,-,?).
'CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls_1'('Prelude.(,)'(A,B),C,D,E):-!,hnf(B,C,D,E).
'CurryDocHtml.firstSentence\'2E_\'23selFP17\'23ls_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocHtml.curryDocEpilog'(?,?,?,-,?).
'CurryDocHtml.curryDocEpilog'(A,B,['HTML.hrule','HTML.italic'(['HTML.htxt'(['^G','^e','^n','^e','^r','^a','^t','^e','^d','^ ','^b','^y','^ ']),'HTML.bold'(['HTML.htxt'(['^C','^u','^r','^r','^y','^D','^o','^c'])]),'HTML.htxt'('Prelude.++'(['^ ','^('],'Prelude.++'(A,['^)','^ ','^a','^t','^ ']))),'HTML.htxt'('Time.calendarTimeToString'(B))])],C,C).

:-block 'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0'(?,?,?,?,?,-,?).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0'(A,B,C,D,E,F,G):-hnf(A,H,F,I),'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0_1'(H,B,C,D,E,I,G).

:-block 'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0_1'(-,?,?,?,?,?,?),'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0_1'(?,?,?,?,?,-,?).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0_1'('Prelude.True',A,B,C,[],D,D).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E,F):-!,hnf('Prelude.++'(['HTML.h2'(['HTML.htxt'(['^E','^x','^p','^o','^r','^t','^e','^d','^ ','^d','^a','^t','^a','^t','^y','^p','^e','^s',^:])]),'HTML.hrule'],'Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genHtmlType',[B,A])),C)),D,E,F).
'CurryDocHtml.generateHtmlDocs\'2E_\'23lambda1\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E).

:-block 'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0'(?,?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0'(A,B,C,D,E,F,G):-hnf(A,H,F,I),'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0_1'(H,B,C,D,E,I,G).

:-block 'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0_1'(-,?,?,?,?,?,?),'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0_1'(?,?,?,?,?,-,?).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0_1'('Prelude.True',A,B,C,[^#|A],D,D).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E,F):-!,hnf('Prelude.++'(B,'Prelude.++'([^.,'^h','^t','^m','^l',^#],'Prelude.tail'(C))),D,E,F).
'CurryDocHtml.replaceIdLinks\'2EcheckId\'2E14\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E).

:-block 'CurryDocHtml.genHtmlModule\'2E_\'23caseor0'(?,?,?,-,?).
'CurryDocHtml.genHtmlModule\'2E_\'23caseor0'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocHtml.genHtmlModule\'2E_\'23caseor0_1'(F,B,C,G,E).

:-block 'CurryDocHtml.genHtmlModule\'2E_\'23caseor0_1'(-,?,?,?,?),'CurryDocHtml.genHtmlModule\'2E_\'23caseor0_1'(?,?,?,-,?).
'CurryDocHtml.genHtmlModule\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D):-hnf('Prelude.apply'('Markdown.markdownText2HTML',A),B,C,D).
'CurryDocHtml.genHtmlModule\'2E_\'23caseor0_1'('Prelude.False',A,['HTML.par'(['HTML.HtmlText'(A)])],B,B):-!.
'CurryDocHtml.genHtmlModule\'2E_\'23caseor0_1'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23caseor0'(?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23caseor0'(A,B,C,D,E,F,G,H):-hnf(A,I,G,J),'CurryDocHtml.genHtmlType\'2E_\'23caseor0_1'(I,B,C,D,E,F,J,H).

:-block 'CurryDocHtml.genHtmlType\'2E_\'23caseor0_1'(-,?,?,?,?,?,?,?),'CurryDocHtml.genHtmlType\'2E_\'23caseor0_1'(?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlType\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D,E,F,G):-hnf('HTML.code'(['HTML.htxt'(['^S','^t','^r','^i','^n','^g','^ ',^=,'^ ','^[','^C','^h','^a','^r','^]'])]),E,F,G).
'CurryDocHtml.genHtmlType\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E,F,G):-!,hnf('HTML.code'(['HTML.HtmlText'('Prelude.++'(A,'Prelude.++'('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocHtml.genHtmlType\'2E_\'23lambda18',[])),B),'Prelude.++'(['^ ',^=,'^ '],'CurryDocHtml.showType'(C,'Prelude.False',D)))))]),E,F,G).
'CurryDocHtml.genHtmlType\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).

:-block 'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0'(?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0_1'(F,B,C,G,E).

:-block 'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0_1'(-,?,?,?,?),'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0_1'(?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.True',A,[],B,B).
'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.False',A,['HTML.dlist'(['Prelude.(,)'(['CurryDocHtml.explainCat'(['^F','^u','^r','^t','^h','^e','^r','^ ','^i','^n','^f','^o','^s',^:])],['HTML.ulist'(A)])])],B,B):-!.
'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0_1'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0'(?,?,?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0'(A,B,C,D,E,F,G,H,I,J,K,L,M):-hnf(A,N,L,O),'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0_1'(N,B,C,D,E,F,G,H,I,J,K,O,M).

:-block 'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0_1'(-,?,?,?,?,?,?,?,?,?,?,?,?),'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0_1'(?,?,?,?,?,?,?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D,E,F,G,H,I,J,K,L):-makeShare(M,N),makeShare(H,O),makeShare(P,Q),makeShare(R,S),makeShare(E,T),makeShare(G,U),makeShare(I,V),hnf('Prelude.cond'('Prelude.letrec'(N,'CurryDocRead.splitComment'('CurryDocRead.getFuncComment'(O,A))),'Prelude.cond'('Prelude.letrec'(Q,'CurryDocHtml.genHtmlFunc\'2E_\'23selFP13\'23funcmt'(N)),'Prelude.cond'('Prelude.letrec'(S,'CurryDocHtml.genHtmlFunc\'2E_\'23selFP14\'23paramcmts'(N)),'Prelude.++'(['Prelude.$'(partcall(1,'HTML.par',[]),'Prelude.++'(['Prelude.apply'('CurryDocHtml.opnameDoc',['HTML.anchor'(O,['HTML.href'('Prelude.++'('Prelude.apply'('CurryDocRead.getLastName',B),'Prelude.++'(['^_','^c','^u','^r','^r','^y',^.,'^h','^t','^m','^l',^#],O)),['HTML.htxt'('CurryDocRead.showId'(O))])])]),'HTML.code'(['HTML.HtmlText'('Prelude.++'(['^ ',^:,^:,'^ '],'CurryDocHtml.showType'(T,'Prelude.False',C)))]),'HTML.nbsp','HTML.nbsp'],'CurryDocHtml.genFuncPropIcons'(D,'Prelude.(,)'(T,O),F)))],'Prelude.++'('CurryDocHtml.docComment2HTML'(U,Q),'Prelude.++'('CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117'(U,O,S),'Prelude.++'('CurryDocHtml.genHtmlFunc\'2E_\'23caseor0\'2E_\'23caseor0'('Prelude.=='(V,[]),V),['HTML.hrule']))))))),J,K,L).
'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E,F,G,H,I,[],J,J):-!.
'CurryDocHtml.genHtmlFunc\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,E,F,G,H,I,J,'FAIL'(A),K,K).

:-block 'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0'(?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0'(A,B,C,D,E,F,G):-hnf(A,H,F,I),'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0_1'(H,B,C,D,E,I,G).

:-block 'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0_1'(-,?,?,?,?,?,?),'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0_1'(?,?,?,?,?,-,?).
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0_1'('Prelude.True',A,B,C,[],D,D).
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0_1'('Prelude.False',A,B,C,['HTML.par'(['CurryDocHtml.explainCat'(['^E','^x','^a','^m','^p','^l','^e','^ ','^c','^a','^l','^l',^:]),'HTML.nbsp','HTML.code'(['HTML.htxt'('CurryDocHtml.genHtmlFunc\'2EshowCall\'2E117'(A,'Prelude.map'(partcall(1,'Prelude.fst',[]),D)))])]),'HTML.dlist'('Prelude.++'(['Prelude.(,)'(['CurryDocHtml.explainCat'(['^P','^a','^r','^a','^m','^e','^t','^e','^r','^s',^:])],[])],'Prelude.map'(partcall(1,'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23lambda19',[B]),D)))],E,F):-!,makeShare(C,D),E=F.
'CurryDocHtml.genHtmlFunc\'2EgenParamComment\'2E117\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,'FAIL'(A),E,E).

:-block 'CurryDocHtml.removeDash\'2E_\'23caseor0'(?,?,?,-,?).
'CurryDocHtml.removeDash\'2E_\'23caseor0'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocHtml.removeDash\'2E_\'23caseor0_1'(F,B,C,G,E).

:-block 'CurryDocHtml.removeDash\'2E_\'23caseor0_1'(-,?,?,?,?),'CurryDocHtml.removeDash\'2E_\'23caseor0_1'(?,?,?,-,?).
'CurryDocHtml.removeDash\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D):-hnf('Prelude.dropWhile'(partcall(1,'Char.isSpace',[]),'Prelude.drop'(2,A)),B,C,D).
'CurryDocHtml.removeDash\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D):-!,hnf(A,B,C,D).
'CurryDocHtml.removeDash\'2E_\'23caseor0_1'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0'(?,?,-,?).
'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0_1'(E,B,F,D).

:-block 'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0_1'(-,?,?,?),'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0_1'(?,?,-,?).
'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0_1'('Prelude.True',A,B,C):-hnf('HTML.href'(['^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l',^#,'^n','^o','^n','^d','^e','^t','^_','^e','^x','^p','^l','^a','^i','^n'],['Prelude.$'(partcall(1,'CurryDocHtml.addIconParams',[]),'HTML.image'(['^n','^o','^n','^d','^e','^t',^.,'^g','^i','^f'],['^n','^o','^n',^-,'^d','^e','^t','^e','^r','^m','^i','^n','^i','^s','^t','^i','^c']))]),A,B,C).
'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0_1'('Prelude.False',A,B,C):-!,hnf('HTML.href'(['^i','^n','^d','^e','^x',^.,'^h','^t','^m','^l',^#,'^d','^e','^t','^_','^e','^x','^p','^l','^a','^i','^n'],['Prelude.$'(partcall(1,'CurryDocHtml.addIconParams',[]),'HTML.image'(['^d','^e','^t',^.,'^g','^i','^f'],['^d','^e','^t','^e','^r','^m','^i','^n','^i','^s','^t','^i','^c']))]),A,B,C).
'CurryDocHtml.genFuncPropIcons\'2E_\'23caseor0_1'('FAIL'(A),'FAIL'(A),B,B).

:-block 'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0'(?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0_1'(E,B,F,D).

:-block 'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0_1'(-,?,?,?),'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0_1'(?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.True',['^i','^n','^c','^o','^m','^p','^l','^e','^t','^e','^l','^y','^ ','^d','^e','^f','^i','^n','^e','^d'],A,A).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.False',['^i','^n','^c','^o','^m','^p','^l','^e','^t','^e','^l','^y','^ ','^d','^e','^f','^i','^n','^e','^d','^ ','^i','^n','^ ','^e','^a','^c','^h','^ ','^d','^i','^s','^j','^u','^n','^c','^t','^i','^o','^n','^ ','^(','^b','^u','^t','^ ','^m','^i','^g','^h','^t','^ ','^b','^e','^ ','^c','^o','^m','^p','^l','^e','^t','^e','^)'],A,A):-!.
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0_1'('FAIL'(A),'FAIL'(A),B,B).

:-block 'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0'(?,?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0_1'(F,B,C,G,E).

:-block 'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0_1'(-,?,?,?,?),'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0_1'(?,?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0_1'('Prelude.True',A,[],B,B).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0_1'('Prelude.False',A,['HTML.htxt'('CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0'('Prelude.=='(A,'AnaCompleteness.InComplete')))],B,B):-!.
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0_1'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0'(?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'(E,B,F,D).

:-block 'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'(-,?,?,?),'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'(?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.True',['HTML.htxt'(['^m','^i','^g','^h','^t','^ ','^b','^e','^h','^a','^v','^e','^ ','^i','^n','^d','^e','^t','^e','^r','^m','^i','^n','^i','^s','^t','^i','^c','^a','^l','^l','^y'])],A,A).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.False',[],A,A):-!.
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'('FAIL'(A),'FAIL'(A),B,B).

:-block 'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0'(?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0'(A,B,C,D):-hnf(A,E,C,F),'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'(E,B,F,D).

:-block 'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'(-,?,?,?),'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'(?,?,-,?).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.True',['HTML.htxt'(['^s','^o','^l','^u','^t','^i','^o','^n','^ ','^c','^o','^m','^p','^l','^e','^t','^e','^,','^ ','^i',^.,'^e',^.,'^,','^ ','^a','^b','^l','^e','^ ','^t','^o','^ ','^c','^o','^m','^p','^u','^t','^e','^ ','^a','^l','^l','^ ','^s','^o','^l','^u','^t','^i','^o','^n','^s'])],A,A).
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.False',[],A,A):-!.
'CurryDocHtml.genFuncPropComments\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0\'2E_\'23caseor0_1'('FAIL'(A),'FAIL'(A),B,B).

:-block 'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0'(?,?,?,?,?,?,-,?).
'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0'(A,B,C,D,E,F,G,H):-hnf(A,I,G,J),'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1'(I,B,C,D,E,F,J,H).

:-block 'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1'(-,?,?,?,?,?,?,?),'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1'(?,?,?,?,?,?,-,?).
'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D,E,F,G):-hnf('Prelude.++'('HTML.htmlQuote'('Prelude.++'(A,['^010'])),'CurryDocHtml.addFuncAnchors'(C,D)),E,F,G).
'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E,F,G):-!,makeShare(B,H),makeShare(C,I),hnf('Prelude.apply'('Prelude.elem'(H),I),J,F,K),'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(J,A,H,I,D,E,K,G).

:-block 'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(-,?,?,?,?,?,?,?),'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(?,?,?,?,?,?,-,?).
'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E,F,G):-hnf('Prelude.++'('Prelude.++'(A,['^010']),'CurryDocHtml.addFuncAnchors'(C,D)),E,F,G).
'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E,F,G):-!,makeShare(B,H),hnf('Prelude.++'([^<,'^A','^ ','^N','^A','^M','^E',^=,'^"'],'Prelude.++'(H,'Prelude.++'(['^"',^>,^<,^/,'^A',^>],'Prelude.++'('HTML.htmlQuote'('Prelude.++'(A,['^010'])),'CurryDocHtml.addFuncAnchors'([H|C],D))))),E,F,G).
'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).
'CurryDocHtml.addFuncAnchors\'2E_\'23caseor0_1'('FAIL'(A),B,C,D,E,'FAIL'(A),F,F).

:-block 'CurryDocHtml.htmlIndex\'2E_\'23caseor0'(?,?,?,-,?).
'CurryDocHtml.htmlIndex\'2E_\'23caseor0'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocHtml.htmlIndex\'2E_\'23caseor0_1'(F,B,C,G,E).

:-block 'CurryDocHtml.htmlIndex\'2E_\'23caseor0_1'(-,?,?,?,?),'CurryDocHtml.htmlIndex\'2E_\'23caseor0_1'(?,?,?,-,?).
'CurryDocHtml.htmlIndex\'2E_\'23caseor0_1'('Prelude.True',A,['HTML.h1'(['HTML.htxt'(['^D','^o','^c','^u','^m','^e','^n','^t','^a','^t','^i','^o','^n','^ ','^o','^f','^ ','^t','^h','^e','^ ','^C','^u','^r','^r','^y','^ ','^p','^r','^o','^g','^r','^a','^m','^ ']),'HTML.href'('Prelude.++'('Prelude.head'(B),[^.,'^h','^t','^m','^l']),['HTML.htxt'('Prelude.++'('Prelude.head'(B),[^.,'^c','^u','^r','^r','^y']))])])],C,D):-makeShare(A,B),C=D.
'CurryDocHtml.htmlIndex\'2E_\'23caseor0_1'('Prelude.False',A,['HTML.h1'(['HTML.htxt'(['^D','^o','^c','^u','^m','^e','^n','^t','^a','^t','^i','^o','^n','^ ','^o','^f','^ ','^t','^h','^e','^ ','^C','^u','^r','^r','^y','^ ','^p','^r','^o','^g','^r','^a','^m','^s',^:])]),'HTML.ulist'('Prelude.map'(partcall(1,'CurryDocHtml.htmlIndex\'2E_\'23lambda24',[]),'Sort.mergeSort'('Sort.leqStringIgnoreCase',A)))],B,B):-!.
'CurryDocHtml.htmlIndex\'2E_\'23caseor0_1'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocHtml.firstSentence\'2E_\'23caseor0'(?,?,?,?,-,?).
'CurryDocHtml.firstSentence\'2E_\'23caseor0'(A,B,C,D,E,F):-hnf(A,G,E,H),'CurryDocHtml.firstSentence\'2E_\'23caseor0_1'(G,B,C,D,H,F).

:-block 'CurryDocHtml.firstSentence\'2E_\'23caseor0_1'(-,?,?,?,?,?),'CurryDocHtml.firstSentence\'2E_\'23caseor0_1'(?,?,?,?,-,?).
'CurryDocHtml.firstSentence\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D,E):-hnf(A,C,D,E).
'CurryDocHtml.firstSentence\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E):-!,makeShare(B,F),hnf('Prelude.&&'('Prelude./='('Prelude.tail'(F),[]),'CurryDocRead.isWhiteSpace'('Prelude.head'('Prelude.tail'(F)))),G,D,H),'CurryDocHtml.firstSentence\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(G,A,F,C,H,E).

:-block 'CurryDocHtml.firstSentence\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(-,?,?,?,?,?),'CurryDocHtml.firstSentence\'2E_\'23caseor0_1_Prelude.False_ComplexCase'(?,?,?,?,-,?).
'CurryDocHtml.firstSentence\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',A,B,C,D,E):-hnf('Prelude.++'(A,[^.]),C,D,E).
'CurryDocHtml.firstSentence\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',A,B,C,D,E):-!,hnf('Prelude.++'(A,'Prelude.++'([^.],'CurryDocHtml.firstSentence'('Prelude.tail'(B)))),C,D,E).
'CurryDocHtml.firstSentence\'2E_\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(A),B,C,'FAIL'(A),D,D).
'CurryDocHtml.firstSentence\'2E_\'23caseor0_1'('FAIL'(A),B,C,'FAIL'(A),D,D).

:-costCenters(['']).




%%%%% Number of shared variables: 106
