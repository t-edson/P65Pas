{Unidad con funciones de exploración de código}
unit CodeTools;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLType, LCLProc, SynEdit, SynEditHighlighter, LazUTF8,
  MisUtils, SynFacilCompletion, SynFacilHighlighter, SynFacilBasic, XpresBas,
  XpresElementsPIC, FrameEditView, Parser, Globales;
type
  { TCodeTool }
  TCodeTool = class
  private
    //Referencias importantes
    fraEdit   : TfraEditView;
    cxp       : TCompilerBase;
    opEve0: TFaOpenEvent;   //Para pasar parámetro a cxpTreeElemsFindElement´()
  public
    procedure ReadCurIdentif(out tok: string; out tokType: integer; out
      lex: TSynFacilComplet; out curX: integer);
    procedure GoToDeclaration;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private  //Completado de código
    procedure cxpTreeElemsFindElement(elem: TxpElement);
    procedure AddListUnits(OpEve: TFaOpenEvent);
    procedure CopyListItems(OpEve, OpEveSrc: TFaOpenEvent);
    procedure FieldsComplet(ident: string; opEve: TFaOpenEvent; tokPos: TSrcPos);
    procedure Fill_IFtemplate(opEve: TFaOpenEvent);
    procedure Fill_THENtemplate(opEve: TFaOpenEvent);
    procedure Fill_templates(opEve: TFaOpenEvent);
    procedure Fill_SpecialIdentif(opEve: TFaOpenEvent; hl: TSynFacilComplet;
      tokName: string; idIcon: integer);
    procedure GeneralIdentifierCompletion(opEve: TFaOpenEvent; curEnv: TFaCursorEnviron;
      out Cancel: boolean);
    procedure OpenAfterDot1(opEve: TFaOpenEvent; curEnv: TFaCursorEnviron; out
      Cancel: boolean);
    procedure OpenAfterDot2(opEve: TFaOpenEvent; curEnv: TFaCursorEnviron; out
      Cancel: boolean);
  public
    procedure SetCompletion(ed: TSynEditor);
  public  //Inicialización
    procedure SetCompiler(cxp0: TCompilerBase);
    constructor Create(fraEdit0: TfraEditView);
  end;

implementation

procedure TCodeTool.ReadCurIdentif(out tok: string; out tokType: integer;
                                   out lex: TSynFacilComplet; out curX: integer);
{Da infomación sobre el token actual. Si no encuentra información, devuelve cadena
nula en "tok".}
var
  sed: TSynEdit;
  toks: TATokInfo;
  hl: TSynCustomHighlighter;
  tokIdx: integer;
begin
  if fraEdit.Count=0 then begin
    tok := '';
    exit;
  end;
  sed := fraEdit.ActiveEditor.SynEdit;
  if sed.Lines.Count = 0 then begin
    tok := '';
    exit;
  end;
  hl := sed.Highlighter;  //toma su resalatdor
  if hl is TSynFacilComplet then begin
    //Es TSynFacilComplet, usamos su propio resaltador como lexer
    //Además el mismo resaltador tiene acceso al contenido del SynEdit
    lex := TSynFacilComplet(hl);  //accede a TSynFacilComplet
    lex.ExploreLine(sed.CaretXY, toks, tokIdx);  //Explora línea actual
    tok := toks[tokIdx].txt;
    curX := toks[tokIdx].posIni+1;
    tokType := toks[tokIdx].TokTyp;
//    MsgBox('%d', [high(toks)]);
  end else begin
    //Es otro resaltador
//    lin := sed.Lines[sed.CaretY - 1];
    tok := '';
  end;
end;
procedure TCodeTool.GoToDeclaration;
{Salta a la zona de declaración, del elemento que está bajo el cursor, en al ventana de
edición actual. Solo salta, si logra identificar al identificador.}
var
  tok: string;
  tokType, curX: integer;
  lex: TSynFacilComplet;
  callPos: TSrcPos;
  ed: TSynEditor;
  ele: TxpElement;
//  curBody: TxpEleBody;
begin
  ed := fraEdit.ActiveEditor;
  //Primero ubica el token
  ReadCurIdentif(tok, tokType, lex, curX);
  if tok='' then exit;  //No encontró token
  if tokType <> lex.tnIdentif then exit;  //No es identificador
  //Asegurarse que "synTree" está actualizado.
  cxp.Compile(fraEdit.ActiveEditor.FileName, false);  //Solo primera pasada
  if cxp.HayError then begin
    //Basta que haya compilado hasta donde se encuentra el identifiacdor, para que funciones.
//    MsgErr('Compilation error.');  //tal vez debería dar más información sobre el error
//    exit;
  end;
  callPos.col := curX;
  callPos.row := ed.SynEdit.CaretY;
  callPos.fil := ed.FileName;
  ele := cxp.TreeElems.GetElementCalledAt(callPos);
  if ele = nil then begin
    //No lo ubica, puede ser que esté en la sección de declaración
    ele := cxp.TreeElems.GetELementDeclaredAt(callPos);
    if ele <> nil then begin
      //Es el punto donde se declara
      if ele.idClass = eltUnit then begin
        fraEdit.SelectOrLoad(TxpEleUnit(ele).srcFile);
//        MsgBox(ele.name);
      end else begin
        //Es otra declaración
      end;
    end else begin
      MsgExc('Unknown identifier: %s', [tok]);
    end;
//    curBody := cxp.TreeElems.GetElementBodyAt(ed.SynEdit.CaretXY);
//    if curBody=nil then begin
//
//    end;
  end else begin
//      MsgBox('%s', [ele.name]);
    //Ubica la declaración del elemento
    if not fraEdit.SelectOrLoad(ele.srcDec, false) then begin
      MsgExc('Cannot load file: %s', [ele.srcDec.fil]);
    end;
  end;
end;
procedure TCodeTool.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{Procesa el evento de teclado, para cuando se tiene el editor seleccionado.}
var
  ed: TSynEditor;
begin
  if not fraEdit.HasFocus then exit;
  if fraEdit.Count=0 then exit;
  ed := fraEdit.ActiveEditor;
  if (Shift = [ssAlt]) and (Key = VK_UP) then begin
    //Se pide ubicar la declaración del elemento
    GoToDeclaration;
  end;
  if not ed.SynEdit.SelAvail then begin
    //No hay selección. Pero se pulsa ...
    if (Shift = [ssCtrl]) and (Key = VK_C) then begin  //Ctrl+C
      ed.SynEdit.SelectWord;
      ed.Copy;
    end;
    if (Shift = [ssCtrl]) and (Key = VK_INSERT) then begin  //Ctrl+Insert
      ed.SynEdit.SelectWord;
      ed.Copy;
    end;
  end;
end;
//Completado de código
procedure TCodeTool.cxpTreeElemsFindElement(elem: TxpElement);
var
  xfun: TxpEleFun;
begin
  if elem.idClass = eltFunc then begin
    //Es función
    xfun := TxpEleFun(elem);
    if high(xfun.pars) = -1 then begin
      //Sin parámetros
      opEve0.AddItem(elem.name+'', 5);
    end else begin
      opEve0.AddItem(elem.name+'(\_)', 5);
    end;
  end else begin
    opEve0.AddItem(elem.name, 5);
  end;
end;
procedure TCodeTool.AddListUnits(OpEve: TFaOpenEvent);
{Agrega la lista de unidades disponibles, a la lista Items[] de un Evento de apertura.}
var
  directorio, nomArc: String;
  SearchRec: TSearchRec;
begin
  if OpEve=nil then exit;
  //Directorio /units
  directorio := patUnits;
  if FindFirst(directorio + '\*.pas', faDirectory, SearchRec) = 0 then begin
    repeat
      nomArc := SysToUTF8(SearchRec.Name);
      if SearchRec.Attr and faDirectory = faDirectory then begin
        //directorio
      end else begin //archivo
        //Argega nombre de archivo
        nomArc := copy(nomArc, 1, length(nomArc)-4);
        opEve.AddItem(nomArc, -1);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  //Directorio /devices
  directorio := cxp.devicesPath;
  if FindFirst(directorio + '\*.pas', faDirectory, SearchRec) = 0 then begin
    repeat
      nomArc := SysToUTF8(SearchRec.Name);
      if SearchRec.Attr and faDirectory = faDirectory then begin
        //directorio
      end else begin //archivo
        //Argega nombre de archivo
        nomArc := copy(nomArc, 1, length(nomArc)-4);
        opEve.AddItem(nomArc, -1);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

end;
procedure TCodeTool.CopyListItems(OpEve, OpEveSrc: TFaOpenEvent);
var
  it : TFaCompletItem;
begin
  for it in OpEveSrc.Items do begin
    opEve.AddItem(it.Caption, -1);
  end;
end;
procedure TCodeTool.FieldsComplet(ident: string; opEve: TFaOpenEvent;
  tokPos: TSrcPos);
{LLena un objeto opEve, con los campos del identificador "ident", de acuerdo a su tipo.}
{Se solicita lista de campos de un identificador, para el completado de código en el
editor.}
var
  ele: TxpElement;
  xVar: TxpEleVar;
begin
  opEve.ClearItems;  //limpia primero
  //Asegurarse que "synTree" está actualizado.
  cxp.Compile(fraEdit.ActiveEditor.FileName, false);  //Solo primera pasada
  if cxp.HayError then begin
    //Basta que haya compilado hasta donde se encuentra el identificador, para que funciones.
//    MsgErr('Compilation error.');  //tal vez debería dar más información sobre el error
//    exit;
  end;
  ele := cxp.TreeElems.GetElementCalledAt(tokPos);
  if ele = nil then begin
    //No identifica a este elemento
    exit;
  end;
  if ele.idClass = eltVar then begin
    //Es una variable, vemos el tipo
    xVar := TxpEleVar(ele);
    if xVar.typ = cxp.typByte then begin
      opEve.AddItem('bit0', 11);
      opEve.AddItem('bit1', 11);
      opEve.AddItem('bit2', 11);
      opEve.AddItem('bit3', 11);
      opEve.AddItem('bit4', 11);
      opEve.AddItem('bit5', 11);
      opEve.AddItem('bit6', 11);
      opEve.AddItem('bit7', 11);
    end;
    if xVar.typ = cxp.typWord then begin
      opEve.AddItem('high', 11);
      opEve.AddItem('low' , 11);
    end;
    if xVar.typ = cxp.typDWord then begin
      opEve.AddItem('Low', 11);
      opEve.AddItem('High', 11);
      opEve.AddItem('Extra', 11);
      opEve.AddItem('Ultra', 11);
      opEve.AddItem('LowWord', 11);
      opEve.AddItem('HighWord', 11);
    end;
  end else begin
    //No implementado en otro elemento
    exit;
  end;
end;
procedure TCodeTool.Fill_IFtemplate(opEve: TFaOpenEvent);
begin
  //Platillas IF
  opEve.AddItem('if ... then ...;|if \_ then\n\n\uend;', 5);
  opEve.AddItem('if ... then ... else ...;|if \_ then\n\n\uelse\n\n\uend;', 5);
  opEve.AddItem('if ... then ... elsif ...;|if \_ then\n\n\uelsif  then\n\n\uelse\n\n\uend;', 5);
end;
procedure TCodeTool.Fill_THENtemplate(opEve: TFaOpenEvent);
begin
  //Platillas THEN
  opEve.AddItem('then ...;|then\n\u  \_\n\uend;', 5);
  opEve.AddItem('then ... else ...;|then\n\u  \_\n\uelse\n\n\uend;', 5);
  opEve.AddItem('then ... elsif ...;|then\n\u  \_\n\uelsif  then\n\n\uelse\n\n\uend;', 5);
end;
procedure TCodeTool.Fill_templates(opEve: TFaOpenEvent);
begin
  //Platillas generales
  opEve.AddItem('absolute \_$00;', 5);
  opEve.AddItem('begin ... end; |begin\n  \_\n\uend;', 5);
  opEve.AddItem('while ... |while \_ do\n\n\uend;', 5);
  opEve.AddItem('repeat ... |repeat \n\t\_\n\uuntil;', 5);
  opEve.AddItem('for i:=0 to ... |for i:=0 to \_ do\n\n\uend;', 5);
  opEve.AddItem('asm', 5);
  opEve.AddItem('asm ... end |asm \n\t\_\n\uend', 5);
  opEve.AddItem('delay_ms', 5);
  opEve.AddItem('delay_ms(100);', 5);
  opEve.AddItem('inc(\_);', 5);
  opEve.AddItem('dec(\_);', 5);
  opEve.AddItem('chr(\_);', 5);
  opEve.AddItem('chr(65);', 5);
  opEve.AddItem('ord(\_);', 5);
  opEve.AddItem('ord(''A'');', 5);
  opEve.AddItem('SetAsInput(\_);', 5);
  opEve.AddItem('SetAsOutput(\_);', 5);
  opEve.AddItem('SetBank(\_);', 5);
end;
procedure TCodeTool.Fill_SpecialIdentif(opEve: TFaOpenEvent; hl: TSynFacilComplet;
                                            tokName: string; idIcon: integer);
var
  SpecIdent: TArrayTokSpec;
  tipTok, i: Integer;
begin
  SpecIdent := TSynFacilComplet2(hl).SpecIdentif;  //Accede a campo protegido
  tipTok := hl.GetAttribIDByName(tokName);   //tipo de atributo
  for i:= 0 to high(SpecIdent) do begin
    if SpecIdent[i].tTok = tipTok then begin
      opEve.AddItem(SpecIdent[i].orig, idIcon);
    end;
  end;
end;
procedure TCodeTool.GeneralIdentifierCompletion(opEve: TFaOpenEvent;
  curEnv: TFaCursorEnviron; out Cancel: boolean);
{La idea de este método es implementar el completado de un identifcador, en cualquier
parte en que se encuentre el cursor.
Pero actualmente solo se aplica para cualquier bloque que no sea el bloque principal
(Cuerpo del programa principal o cuerpo de procedimientos). EL completado del blooue
MAIN, se está haciendo, todavía, con el archivo XML.}
var
  curPos: TPoint;
  ed: TSynEditor;
  ele: TxpElement;
begin
  ed := fraEdit.ActiveEditor;
  if ed = nil then exit;
  //LLena
  opEve.ClearAvails;
  opEve.ClearItems;  //limpia primero
  //Asegurarse que "synTree" está actualizado.
  cxp.Compile(ed.FileName, false);  //Solo primera pasada
  if cxp.HayError then begin
    //Basta que haya compilado hasta donde se encuentra el identificador, para que funciones.
//    MsgErr('Compilation error.');  //tal vez debería dar más información sobre el error
//    exit;
  end;
  //Identifica la zona en qee se encuentra el identificador
  {Calcula las coordenadas actuales del cursor. En X, retrocede 1, porque si hay un error
  con el identificador actual (lo que es normal porque se está empezando a escribir), el
  bloque actual terminará antes}
  curPos.x := ed.SynEdit.CaretX - 1;
  curPos.y := ed.SynEdit.CaretY;
//  eleBod := cxp.TreeElems.GetElementBodyAt(curPos);
//  if eleBod = nil then begin
//    //No identifica a un Body
////    exit;
//  end;
//  MsgBox(eleBod.Path);
  //Elementos comunes
  Fill_IFtemplate(opEve);
  Fill_THENtemplate(opEve);
  Fill_templates(opEve);
  //Carga palabras reservadas de la sintaxis
  Fill_SpecialIdentif(opEve, ed.hl, 'Keyword', 2);
  Fill_SpecialIdentif(opEve, ed.hl, 'Boolean', 4);
  //Carga identificadores accesibles desde la posición actual
  ele := cxp.TreeElems.GetElementAt(curPos);
  if ele = nil then begin
    //No identifica la posición actual
    exit;
  end;
  cxp.TreeElems.curNode := ele;  //Se posiciona en ese nodo
  //Realiza la búsqueda con FindFirst, usando evento OnFindElement
  opEve0 := opEve;      //Para que el evento identifique al opEve
  cxp.TreeElems.OnFindElement := @cxpTreeElemsFindElement;
  cxp.TreeElems.FindFirst('#');  //Nunca lo va a encontrar pero va a explorar todo el árbol
  cxp.TreeElems.OnFindElement := nil;

  //Deja el filtro
  Cancel := false;
end;
procedure TCodeTool.OpenAfterDot1(opEve: TFaOpenEvent;
  curEnv: TFaCursorEnviron; out Cancel: boolean);
var
  ident: String;
  tokPos: TSrcPos;
begin
  if fraEdit.ActiveEditor=nil then exit;
  ident := curEnv.tok_2^.txt;
  //Calcula la posición del elemento
  tokPos.row := fraEdit.ActiveEditor.SynEdit.CaretY;
  tokPos.col := curEnv.tok_2^.posIni+1;
  tokPos.fil := fraEdit.ActiveEditor.FileName;
  //Dispara evento
  FieldsComplet(ident, opEve, tokPos);
  Cancel := false;
end;
procedure TCodeTool.OpenAfterDot2(opEve: TFaOpenEvent;
  curEnv: TFaCursorEnviron; out Cancel: boolean);
var
  ident: String;
  tokPos: TSrcPos;
begin
  if fraEdit.ActiveEditor=nil then exit;
  ident := curEnv.tok_3^.txt;
  //Calcula la posición del elemento
  tokPos.row := fraEdit.ActiveEditor.SynEdit.CaretY;
  tokPos.col := curEnv.tok_3^.posIni+1;
  tokPos.fil := fraEdit.ActiveEditor.FileName;
  //Dispara evento
  FieldsComplet(ident, opEve, tokPos);
  Cancel := false;
end;
procedure TCodeTool.SetCompletion(ed: TSynEditor);
var
  opEve1, opEve3, opEve2, opEve: TFaOpenEvent;
begin
  //Llena eventos de apertura para la sección de unidades
  //Configura eventos de apertura para nombres de unidades.
  opEve1 := ed.hl.FindOpenEvent('unit1');
  if OpEve1=nil then exit;
  opEve1.ClearItems;
  AddListUnits(opEve1);  //Configura unidades disponibles

  opEve2 := ed.hl.FindOpenEvent('unit2');
  if OpEve2=nil then exit;
  opEve2.ClearItems;
  CopyListItems(opEve2, opEve1);  //Copia lista de ítems

  opEve3 := ed.hl.FindOpenEvent('unit3');
  if OpEve3=nil then exit;
  opEve3.ClearItems;
  CopyListItems(opEve3, opEve1);  //Copia lista de ítems

  //Configura eventos, para "después del punto."
  opEve := ed.hl.FindOpenEvent('AfterDot1');
  if OpEve=nil then exit;
  opEve.OnLoadItems := @OpenAfterDot1;
  opEve := ed.hl.FindOpenEvent('AfterDot2');
  opEve.OnLoadItems := @OpenAfterDot2;

  //Configura completado dinámico, en cualquier punto del programa
  opEve := ed.hl.FindOpenEvent('BE4');
  if OpEve=nil then exit;
  opEve.OnLoadItems := @GeneralIdentifierCompletion;

end;

procedure TCodeTool.SetCompiler(cxp0: TCompilerBase);
begin
  cxp     := cxp0;
  //Habría que cambiar algunas configuraciones de acuerdo al compilador usado
end;

constructor TCodeTool.Create(fraEdit0: TfraEditView);
begin
  fraEdit := fraEdit0;
end;

end.

