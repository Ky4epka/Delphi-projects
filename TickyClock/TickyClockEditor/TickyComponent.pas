unit TickyComponent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Math;

type
  TTickyBorderType = (btRectangle, btCircle);

  TTickyElement = class (TControl)

  private
    fBigRadiusMod: Real;
    fBigLineWidth: Integer;
    fBigLineColor: TColor;
    fBigLineStyle: TPenStyle;
    fBigProgress: Integer;
    fBigDelta: Integer;
    fBigTarget: Integer;
    fSmallRadiusMod: Real;
    fSmallLineWidth: Integer;
    fSmallLineColor: TColor;
    fSmallLineStyle: TPenStyle;
    fSmallProgress: Integer;
    fSmallDelta: Integer;
    fSmallTarget: Integer;

    fBorder: Boolean;
    fBorderType: TTickyBorderType;
    fBorderStyle: TPenStyle;
    fBorderColor: TColor;
    fBorderWidth: Integer;

    procedure SetBigRadiusMod(Value: Real);
    procedure SetBigProgress(Value: Integer);
    procedure SetBigDelta(Value: Integer);
    procedure SetBigTarget(Value: Integer);

    procedure SetSmallRadiusMod(Value: Real);
    procedure SetSmallProgress(Value: Integer);
    procedure SetSmallDelta(Value: Integer);
    procedure SetSmallTarget(Value: Integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw; virtual;

    procedure Reset;

    property BigRadiusMod: Real read fBigRadiusMod write SetBigRadiusMod;
    property BigLineWidth: Integer read fBigLineWidth write fBigLineWidth;
    property BigLineColor: TColor read fBigLineColor write fBigLineColor;
    property BigLineStyle: TPenStyle read fBigLineStyle write fBigLineStyle;
    property BigProgress: Integer read fBigProgress write SetBigProgress;
    property BigDelta: Integer read fBigDelta write SetBigDelta;
    property BigTarget: Integer read fBigTarget write SetBigTarget;

    property SmallRadiusMod: Real read fSmallRadiusMod write SetSmallRadiusMod;
    property SmallLineWidth: Integer read fSmallLineWidth write fSmallLineWidth;
    property SmallLineColor: TColor read fSmallLineColor write fSmallLineColor;
    property SmallLineStyle: TPenStyle read fSmallLineStyle write fSmallLineStyle;
    property SmallProgress: Integer read fSmallProgress write SetSmallProgress;
    property SmallDelta: Integer read fSmallDelta write SetSmallDelta;
    property SmallTarget: Integer read fSmallTarget write SetSmallTarget;

    property Border: Boolean read fBorder write fBorder;
    property BorderType: TTickyBorderType read fBorderType write fBorderType;
    property BorderStyle: TPenStyle read fBorderStyle write fBorderStyle;
    property BorderColor: TColor read fBorderColor write fBorderColor;
    property BorderWidth: Integer read fBorderWidth write fBorderWidth;

  end;

  TTickyTemplate = record
   Template: Array of TPoint;
   Size: Integer;
   Width: Integer;
   Height: Integer;
   Ch: Char;
  end;

  TTickyTicky = class (TImage)

  private
    fLayer: TBitmap;
    fTemplateCount: Integer;
    fTemplates: Array of TTickyTemplate;

    function GetTemplate(I: Integer): TTickyTemplate;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update; override;

    procedure LoadTemplateFromFile(const FileName: String);
    procedure SaveTempateToFile(const FileName: String);

    procedure NewTemplate(AWidth, AHeight: Integer);
    procedure UseTemplate(Index: Integer);
    procedure ChangeTemplate(Index: Integer; Ch: Char; Width, Height: Integer);

    property Layer: TBitmap read fLayer;
    property Templates[Index: Integer]: TTickyTemplate read GetTemplate;
    property TemplateCount: Integer read fTemplateCount;
end;

implementation

function NormalizeAngle(Value: Integer): Integer;
begin
  Value := Value mod 360;

  if (Value<0) then begin
    Value := Value+360;
  end;

  Result := Value;
end;

function CompareAngles(Angle1, Angle2: Integer): Integer;
 var
  RA1, RA2: Integer;
begin
  Result := 0;

  if (Angle1=Angle2) then begin
    Exit;
  end;

  RA1 := 0;
  RA2 := NormalizeAngle(Angle2-Angle1);

  if (RA2>180) then begin
   Result := -1;
  end
  else begin
   Result := 1;
  end;

end;

procedure TTickyElement.SetBigRadiusMod(Value: Real);
begin

  if (Value<0) then begin
    Value := 0
  end
  else if (Value > 1) then begin
    Value := 1;
  end;

  fBigRadiusMod := Value;
end;

procedure TTickyElement.SetBigProgress(Value: Integer);
begin
 Value := Value mod 360;

 if (Value<0) then begin
  Value := 360+Value;
 end;

 fBigProgress := Value;
end;

procedure TTickyElement.SetBigDelta(Value: Integer);
begin
 Value := Value mod 360;

 if (Value<0) then begin
  Value := 360+Value;
 end;

 fBigDelta := Value;
end;

procedure TTickyElement.SetBigTarget(Value: Integer);
begin
 Value := Value mod 360;

 if (Value<0) then begin
  Value := 360+Value;
 end;

 fBigTarget := Value;
end;

procedure TTickyElement.SetSmallRadiusMod(Value: Real);
begin

  if (Value<0) then begin
    Value := 0
  end
  else if (Value > 1) then begin
    Value := 1;
  end;

  fSmallRadiusMod := Value;
end;

procedure TTickyElement.SetSmallProgress(Value: Integer);
begin
 Value := Value mod 360;

 if (Value<0) then begin
  Value := 360+Value;
 end;

 fSmallProgress := Value;
end;

procedure TTickyElement.SetSmallDelta(Value: Integer);
begin
 Value := Value mod 360;

 if (Value<0) then begin
  Value := 360+Value;
 end;

 fSmallDelta := Value;
end;

procedure TTickyElement.SetSmallTarget(Value: Integer);
begin
 Value := Value mod 360;

 if (Value<0) then begin
  Value := 360+Value;
 end;

 fSmallTarget := Value;
end;

constructor TTickyElement.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  Reset;
end;

destructor TTickyElement.Destroy;
begin
  Reset;
  Inherited Destroy;
end;

procedure TTickyElement.Draw;
 var
  fBigRadius, fSmallRadius: Integer;
  fW: Integer;
  CX, CY: Integer;
  Ticky: TTickyTicky;
begin

 if (not (Assigned(Owner)) or (Owner.ClassName<>TTickyTicky.ClassName)) then begin
   Exit;
 end;

 Ticky := TTickyTicky(Owner);
 CX := Left + Width div 2;
 CY := Top + Height div 2;
 fW := Min(Width, Height);
 fBigRadius := Round((fW / 2)*fBigRadiusMod);

 if (Abs(NormalizeAngle(fBigProgress-fBigTarget))>fBigDelta) then begin
   fBigProgress := fBigProgress + CompareAngles(fBigProgress, fBigTarget)*fBigDelta;
 end
 else begin
   fBigProgress := fBigTarget;
 end;

 fSmallRadius := Round((fW / 2)*fSmallRadiusMod);

 if (Abs(NormalizeAngle(fSmallProgress-fSmallTarget))>fSmallDelta) then begin
   fSmallProgress := fSmallProgress + CompareAngles(fSmallProgress, fSmallTarget)*fSmallDelta;
 end
 else begin
   fSmallProgress := fSmallTarget;
 end;

 With Ticky.Layer.Canvas do begin

  if (fBorder) then begin
   Pen.Style := fBorderStyle;
   Pen.Color := fBorderColor;
   Pen.Width := fBorderWidth;
   Brush.Style := bsSolid;
   Brush.Color := clWhite;

   if (fBorderType=btRectangle) then begin
     Rectangle(Left, Top, Left+Width, Top+Height);
   end
   else if (fBorderType=btCircle) then begin
     Ellipse(Left, Top, Left+Width, Top+Height);
   end;

  end;

  Pen.Color := fSmallLineColor;
  Pen.Width := fSmallLineWidth;
  Pen.Style := fSmallLineStyle;
  MoveTo(CX, CY);
  LineTo(CX+Round(fSmallRadius*Cos((fSmallProgress-90)*PI/180.0)), CY+Round(fSmallRadius*Sin((fSmallProgress-90)*PI/180.0)));
  Pen.Color := fBigLineColor;
  Pen.Width := fBigLineWidth;
  Pen.Style := fBigLineStyle;
  MoveTo(CX, CY);
  LineTo(CX+Round(fBigRadius*Cos((fBigProgress-90)*PI/180.0)), CY+Round(fBigRadius*Sin((fBigProgress-90)*PI/180.0)));
 end;

end;

procedure TTickyElement.Reset;
begin
 fBigProgress := 0;
 fBigDelta := 0;
 fBigTarget := 0;
 fSmallProgress := 0;
 fSmallDelta := 0;
 fSmallTarget := 0;
end;


constructor TTickyTicky.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  fLayer := TBitmap.Create;
  fLayer.PixelFormat := pf24bit;
end;

destructor TTickyTicky.Destroy;
begin
  fLayer.Free;
  Inherited Destroy;
end;

procedure TTickyTicky.Update;
 var
  I: Integer;
  Element: TTickyElement;
begin

  if ((fLayer.Width<>Width) or (fLayer.Height<>Height)) then begin
    fLayer.SetSize(Width,Height);
  end;

  With fLayer.Canvas do begin
   Pen.Color := clWhite;
   Pen.Style := psSolid;
   Brush.Color := clWhite;
   Brush.Style := bsSolid;
   Rectangle(0,0,fLayer.Width,fLayer.Height);
   Pen.Color := clWhite;
   Pen.Style := psSolid;
   Brush.Color := clWhite;
   Brush.Style := bsSolid;
  end;

  for I := 0 to ComponentCount - 1 do begin

    if (Components[I].ClassName=TTickyElement.ClassName) then begin
     Element := TTickyElement(Components[I]);
     Element.Draw;
    end;

  end;

  Canvas.Draw(0,0,fLayer);
  Inherited Update;
end;

function ExecuteStringByChar(Ch: String; S: String; var SStart: Integer; var SEnd: Integer): String;
 var
  I: Integer;
  R: Boolean;
  C: Char;
begin
 Result := '';
 R := false;
 SStart := 0;
 SEnd := 0;

 for I := 1 to Length(S) do begin
  C := S[I];

  if (Pos(C, Ch)<>0) then begin

   if (R) then begin
    SEnd := I;
    exit;
   end;

   SStart := I;
   R := true;
  end
  else begin

    if (R) then begin
     Result := Result + C;
    end;

  end;

 end;

end;

procedure FillPoint(var Pt: TPoint; S: String);
 var
  I: Integer;
  Buf: String;
  C: Char;
  Cntr: Integer;
begin
 Buf := '';
 Pt := Point(0,0);
 Cntr := 0;

 for I := 1 to Length(S) do begin
   C := S[I];

   if ((C>='0') and (C<='9')) then begin
     Buf := Buf+C;

     if (I=Length(S)) then begin

      if (Cntr=0) then begin
        Pt.X := StrToInt(Buf);
      end
      else if (Cntr=1) then begin
        Pt.Y := StrToInt(Buf);
        Exit;
      end;

      Buf := '';
      Inc(Cntr);
     end;

   end
   else begin

    if (Length(Buf)>0) then begin

      if (Cntr=0) then begin
        Pt.X := StrToInt(Buf);
      end
      else if (Cntr=1) then begin
        Pt.Y := StrToInt(Buf);
        Exit;
      end;

      Buf := '';
      Inc(Cntr);
    end;

   end;

 end;

end;

procedure FillTemplateByString(var T: TTickyTemplate; S: String);
 var
  St, En: Integer;
  Buf: String;
  Pt: TPoint;
begin
 T.Size := 0;

 while (Length(S)>0) do begin
  Buf := ExecuteStringByChar('[]', S, St, En);

  if (St=En) then begin
    Break;
  end
  else begin
    Delete(S, St, En-St+1);
    T.Size := T.Size+1;
    SetLength(T.Template,T.Size);
    FillPoint(T.Template[T.Size-1], Buf);
  end;

 end;

end;

procedure TTickyTicky.LoadTemplateFromFile(const FileName: string);
 var
  F: TextFile;
  Line,Buf: String;
  ReadTemp: Boolean;
  St,En: Integer;
begin
  AssignFile(F, FileName);
  Reset(F);
  SetLength(Line, 4096);
  ReadTemp := false;
  fTemplateCount := 0;
  SetLength(fTemplates, 0);

  while not (EoF(F)) do begin
    ReadLn(F, Line);

    if (ReadTemp) then begin

      if (Pos('char', Line)<>0) then begin
       Buf := ExecuteStringByChar('"',Line,St,En);

       if (Length(Buf)>0) then begin
        fTemplates[fTemplateCount-1].Ch:=Buf[1];
       end;

      end else if (Pos('width', Line)<>0) then begin
       Buf := ExecuteStringByChar('"',Line,St,En);

       if (Length(Buf)>0) then begin
        fTemplates[fTemplateCount-1].Width:=StrToInt(Buf);
       end;

      end else if (Pos('height', Line)<>0) then begin
       Buf := ExecuteStringByChar('"',Line,St,En);

       if (Length(Buf)>0) then begin
        fTemplates[fTemplateCount-1].Height:=StrToInt(Buf);
       end;

      end else if (Pos('points', Line)<>0) then begin
       Buf := ExecuteStringByChar('"',Line,St,En);

       if (Length(Buf)>0) then begin
        FillTemplateByString(fTemplates[fTemplateCount-1], Buf);
       end;

        ReadTemp := false;
      end;

    end;

    if (Pos('Template', Line)<>0) then begin
      ReadTemp := true;
      fTemplateCount := fTemplateCount + 1;
      SetLength(fTemplates, fTemplateCount);

      With fTemplates[fTemplateCount-1] do begin
       Size := 0;
       Width := 0;
       Height := 0;
       Ch := #0;
      end;

    end;

  end;

  CloseFile(F);
end;

procedure TTickyTicky.SaveTempateToFile(const FileName: string);
 var
  F: TextFile;
  I, J: Integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);

  for I := 0 to fTemplateCount - 1 do begin
    WriteLn(F, 'Template');
    WriteLn(F, '  char = "', fTemplates[I].Ch, '"');
    WriteLn(F, '  width = "', fTemplates[I].Width, '"');
    WriteLn(F, '  height = "', fTemplates[I].Height, '"');
    Write(F, '  points = "');

    for J := 0 to fTemplates[I].Size - 1 do begin
     Write(F, Format('[%d %d]', [fTemplates[I].Template[J].X, fTemplates[I].Template[J].Y]));

     if (J < fTemplates[I].Size) then begin
       Write(F, ', ');
     end;

    end;

    Write(F, '"');
  end;

  CloseFile(F);
end;

procedure TTickyTicky.NewTemplate(AWidth: Integer; AHeight: Integer);
begin
  Inc(fTemplateCount);
  SetLength(fTemplates, fTemplateCount);

  With fTemplates[fTemplateCount-1] do begin
    ChangeTemplate(fTemplateCount-1, #0, AWidth, AHeight);
  end;

end;

procedure TTickyTicky.UseTemplate(Index: Integer);
 var
  I: Integer;
  Elem: TTickyElement;
begin

  if (Index<0) or (Index>=fTemplateCount) then begin
   Exit;
  end;

  for I := 0 to fTemplates[Index].Size - 1 do begin

    if (I<ComponentCount) and (Components[I].ClassName=TTickyElement.ClassName) then begin
      Elem := TTickyElement(Components[I]);
      Elem.BigTarget := fTemplates[Index].Template[I].X;
      Elem.SmallTarget := fTemplates[Index].Template[I].Y;
    end;

  end;

end;

procedure TTickyTicky.ChangeTemplate(Index: Integer; Ch: Char; Width: Integer; Height: Integer);
begin

  if (Index<0) or (Index>=fTemplateCount) then begin
   Exit;
  end;

  fTemplates[Index].Ch := Ch;

  if (fTemplates[Index].Size<>Width*Height) then begin
    fTemplates[Index].Size := Width*Height;
    fTemplates[Index].Width := Width;
    fTemplates[Index].Height := Height;
    SetLength(fTemplates[Index].Template, fTemplates[Index].Size);
  end;

end;

function TTickyTicky.GetTemplate(I: Integer): TTickyTemplate;
begin

  if (I<0) or (I>=fTemplateCount) then begin
    Exit;
  end;

  Result := fTemplates[I];
end;

end.
