program TickyTicky;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  TickyComponent in 'TickyComponent.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
