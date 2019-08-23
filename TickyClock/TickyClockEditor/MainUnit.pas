unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TickyComponent, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    TickyTimer: TTimer;
    TickyPanel: TPanel;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    TemplateCharEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LoadTemplatesBtn: TButton;
    SaveTemplatesBtn: TButton;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    BigEdit: TSpinEdit;
    Label7: TLabel;
    SmallEdit: TSpinEdit;
    SaveToTemplateBtn: TButton;
    NewTemplateBtn: TButton;
    OpenTemplateDialog: TOpenDialog;
    SaveTemplateDialog: TSaveDialog;
    TemplateWidthEdit: TSpinEdit;
    TemplateHeightEdit: TSpinEdit;
    ElementEdit: TSpinEdit;
    Label5: TLabel;
    TemplateEdit: TSpinEdit;
    Label1: TLabel;
    ApplyTemplateBtn: TButton;
    MainLayer: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure TickyTimerTimer(Sender: TObject);
    procedure LoadTemplatesBtnClick(Sender: TObject);
    procedure SaveTemplatesBtnClick(Sender: TObject);
    procedure SaveToTemplateBtnClick(Sender: TObject);
    procedure NewTemplateBtnClick(Sender: TObject);
    procedure TemplateEditChange(Sender: TObject);
    procedure ApplyTemplateBtnClick(Sender: TObject);
    procedure ElementEditChange(Sender: TObject);
  private
    fTicky: TTickyTicky;
    fCurTmp: Integer;
    fCurElem: Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ApplyTemplateBtnClick(Sender: TObject);
 var
  B: TBitmap;
begin

 if ((fCurTmp>=0) and (fCurTmp<fTicky.TemplateCount)) then begin
  fTicky.ChangeTemplate(fCurTmp, TemplateCharEdit.Text[1], TemplateWidthEdit.Value, TemplateHeightEdit.Value);
 end;

end;

procedure TForm1.ElementEditChange(Sender: TObject);
 var
  I, C: Integer;
  Elem: TTickyElement;
begin
  fCurElem := ElementEdit.Value;

  if ((fCurTmp>=0) and (fCurTmp<fTicky.TemplateCount)) and ((fCurElem>=0) and (fCurElem<=fTicky.Templates[fCurTmp].Size)) then begin

    C := 0;
    for I := 0 to fTicky.ComponentCount - 1 do begin

      if (fTicky.Components[I].ClassName=TTickyElement.ClassName) then begin
        Elem := TTickyElement(fTicky.Components[I]);

        if (fCurElem=C) then begin
          Elem.Border := true;
          BigEdit.Value := Elem.BigTarget;
          SmallEdit.Value := Elem.SmallTarget;
        end
        else begin
          Elem.Border := false;
        end;

        Inc(C);
      end;

    end;

  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
 var
  Elem: TTickyElement;
  I: Integer;
begin
 fTicky := TTickyTicky.Create(Self);
 fTicky.Parent := TickyPanel;
 fTicky.Align := alClient;

 for I := 0 to 12 - 1 do begin

   Elem := TTickyElement.Create(fTicky);
   With Elem do begin
     Left := (I mod 3) * 50;
     Top := (I div 3) * 50;
     Width := 50;
     Height := 50;
     BigRadiusMod := 0.7;
     BigLineWidth := 2;
     BigLineColor := clBlack;
     BigLineStyle := psSolid;
     BigProgress := 90;
     BigDelta := 3;
     BigTarget := 0;
     SmallRadiusMod := 1;
     SmallLineWidth := 1;
     SmallLineColor := clBlack;
     SmallLineStyle := psSolid;
     SmallProgress := 270;
     SmallDelta := 3;
     SmallTarget := 0;
     Border := false;
     BorderStyle := psSolid;
     BorderColor := clBlack;
     BorderWidth := 1;
   end;

 end;

 TickyTimer.Enabled := true;
end;

procedure TForm1.LoadTemplatesBtnClick(Sender: TObject);
begin

  if (OpenTemplateDialog.Execute(Handle)) then begin
    fTicky.LoadTemplateFromFile(OpenTemplateDialog.FileName);
    fCurTmp := 0;
    fCurElem := 0;
    TemplateEdit.Value := 0;
    ElementEdit.Value := 0;
    TemplateEdit.OnChange(Self);
    ElementEdit.OnChange(Self);
  end;

end;

procedure TForm1.NewTemplateBtnClick(Sender: TObject);
begin
  fTicky.NewTemplate(TemplateWidthEdit.Value, TemplateHeightEdit.Value);
end;

procedure TForm1.SaveTemplatesBtnClick(Sender: TObject);
begin

  if (SaveTemplateDialog.Execute(Handle)) then begin
    fTicky.SaveTempateToFile(SaveTemplateDialog.FileName+'.tmps');
  end;

end;

procedure TForm1.SaveToTemplateBtnClick(Sender: TObject);
begin

 if ((fCurTmp>=0) and (fCurTmp<fTicky.TemplateCount)) and ((fCurElem>=0) and (fCurElem<=fTicky.Templates[fCurTmp].Size)) then begin
  fTicky.Templates[fCurTmp].Template[fCurElem] := Point(BigEdit.Value, SmallEdit.Value);
  fTicky.UseTemplate(fCurTmp);
 end;

end;

procedure TForm1.TemplateEditChange(Sender: TObject);
 var
  I: Integer;
  Elem: TTickyElement;
begin

  for I := 0 to fTicky.ComponentCount - 1 do begin

    if (fTicky.Components[I].ClassName=TTickyElement.ClassName) then begin
      Elem := TTickyElement(fTicky.Components[I]);
      //Elem.Border := false;
    end;

  end;

  fCurTmp := TemplateEdit.Value;
  fTicky.UseTemplate(fCurTmp);

  if (fCurTmp>=0) and (fCurTmp<fTicky.TemplateCount) then begin
    TemplateCharEdit.Text := fTicky.Templates[fCurTmp].Ch;
    TemplateWidthEdit.Value := fTicky.Templates[fCurTmp].Width;
    TemplateHeightEdit.Value := fTicky.Templates[fCurTmp].Height;
  end
  else begin
    TemplateCharEdit.Text := '';
    TemplateWidthEdit.Value := 0;
    TemplateHeightEdit.Value := 0;
  end;

end;

procedure TForm1.TickyTimerTimer(Sender: TObject);
begin
 fTicky.Update;
end;

end.
