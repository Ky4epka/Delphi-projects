program DissolveClock;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  DissolveClickUnit in 'DissolveClickUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := false;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
