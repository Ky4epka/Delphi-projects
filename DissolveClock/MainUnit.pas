unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, DissolveClickUnit,
  Vcl.StdCtrls, Vcl.Menus;

type
  TForm1 = class(TForm)
    CloseBtnTimer: TTimer;
    clickClose: TPanel;
    clickArea: TShape;
    PosTimer: TTimer;
    TrayIcon: TTrayIcon;
    PopupMenu: TPopupMenu;
    Exit1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure clickAreaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure clickAreaMouseEnter(Sender: TObject);
    procedure clickAreaMouseLeave(Sender: TObject);
    procedure clickAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseBtnTimerTimer(Sender: TObject);
    procedure PosTimerTimer(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    FClocks: TDissolveClock;
    FDate: TDissolveClock;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FClocks.Repaint;
end;

procedure TForm1.clickAreaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickClose.Color := clOlive;
end;

procedure TForm1.clickAreaMouseEnter(Sender: TObject);
begin
  clickClose.Color := $00BFFFF9;
end;

procedure TForm1.clickAreaMouseLeave(Sender: TObject);
begin
  clickClose.Color := clWhite;
end;

procedure TForm1.clickAreaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Rect(0, 0, clickArea.Width, clickArea.Height).Contains(Point(X, Y))
  then
  begin
    clickClose.Color := $00BFFFF9;
    Close;
  end
  else clickClose.Color := clWhite;
end;

procedure TForm1.CloseBtnTimerTimer(Sender: TObject);
var
  Pt: TPoint;
  R: TRect;
begin
  clickClose.Left := ClientWidth - clickClose.Width;
  clickClose.Top := ClientHeight - clickClose.Height;

  GetCursorPos(Pt);
  Winapi.Windows.GetWindowRect(Handle, R);
  clickClose.Visible := R.Contains(Pt);

  if (clickClose.Visible)
  then clickClose.BringToFront;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  W: Integer;
begin
  FClocks := TDissolveClock.Create(Self);
  FClocks.Parent := Self;
  FClocks.Left := 0;
  FClocks.Top := 25;
  FClocks.Width := 450;
  FClocks.Height := 100;
  FClocks.AutoSize := true;
  FClocks.ClockType := dctCustom;
  FClocks.CustomDigitCollection := [ddtHour2, ddtHour1, ddtMinute2, ddtMinute1];
  FClocks.TimeDelimiters := [dtdHour];
  FClocks.Show;
  FClocks.ParentBackground := false;
  FClocks.ParentColor := false;
  FClocks.Color := clBlack;

  FDate:= TDissolveClock.Create(Self);
  FDate.Parent := Self;
  FDate.Top := 0;
  FDate.Width := 250;
  FDate.Height := 25;
  FDate.ClockType := dctDate;
  FDate.DateDelimiters := [dddYear, dddMonth];
  FDate.Show;
  FDate.ParentBackground := false;
  FDate.ParentColor := false;
  FDate.Color := clBlack;
  FDate.AutoSize := true;
  W := FDate.Width;
  FDate.AutoSize := false;
  FDate.Width := W;
  AutoSize := true;
  FDate.Left := Width - W;
end;

procedure TForm1.PosTimerTimer(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
  if (PosTimer.Interval = 1)
  then PosTimer.Interval := 1000;

  Left := Screen.WorkAreaWidth - Width;
  Top := Screen.WorkAreaHeight - Height;
end;

end.
