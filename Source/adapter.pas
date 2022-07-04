{Unit to define the base calse TAdapter}
unit adapter;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, FrameEditView ;

type
  {Adaptador para controlar a diversos compiladores}
  TAdapter = class
    //*** Se debería eliminar el uso de eventos para hacer la clase más independiente.
    OnAfterCompile: procedure of object;   //Al finalizar la compilación.
    OnWarning: procedure(warTxt, fname: string; row, col: integer) of object;
    OnError  : procedure(errTxt, fname: string; row, col: integer) of object;
    OnInfo   : procedure(infTxt, fname: string; row, col: integer) of object;
  public      //Información
    function CompilerName: string; virtual; abstract;
    function hexFilePath: string; virtual; abstract;
    function mainFilePath: string; virtual; abstract;
    function CPUname: string; virtual; abstract;
    function RAMusedStr: string; virtual; abstract;
    function SampleCode: string; virtual; abstract;
  public      //Manejo de Codetool
    procedure SetCompletion(ed: TSynEditor); virtual; abstract;
    procedure CTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure GoToDeclaration; virtual; abstract;
  public      //Ejecución
    procedure Exec(srcFile, outFile: string; pars: string); virtual; abstract;
    procedure CheckSyntax(srcFile: string; pars: string); virtual; abstract;
    procedure UpdateCompletionForEditors; virtual; abstract;
    procedure DumpCode(lins: TSTrings); virtual; abstract;
  end;

implementation

end.

