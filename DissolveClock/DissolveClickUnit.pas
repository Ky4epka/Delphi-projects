unit DissolveClickUnit;

interface

uses
  WinApi.Windows,
  System.Generics.Collections,
  VCL.Forms,
  VCL.Graphics,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  System.Classes,
  System.Math,
  System.SysUtils;

type
  TDisolveDigitType = (ddtNone,
                       ddtYear4, ddtYear3, ddtYear2, ddtYear1,
                       ddtMonth2, ddtMonth1,
                       ddtDay2, ddtDay1,
                       ddtHour2, ddtHour1,
                       ddtMinute2, ddtMinute1,
                       ddtSecond2, ddtSecond1);
  TDisolveDigitTypeCollection = set of TDisolveDigitType;

  TDisolveClockType = (dctDate, dctTime, dctCustom, dctDateAndTime);

  TDissolveDateDelimiter = (dddYear, dddMonth);
  TDissolveDateDelimiters = set of TDissolveDateDelimiter;

  TDissolveTimeDelimiter = (dtdHour, dtdMinute);
  TDissolveTimeDelimiters = set of TDissolveTimeDelimiter;

  TDissolveElement = (deNone,
                      deYear4, deYear3, deYear2, deYear1,
                      deYearSeparator,
                      deMonth2, deMonth1,
                      deMonthSeparator,
                      deDay2, deDay1,
                      deHour2, deHour1,
                      deHourSeparator,
                      deMinute2, deMinute1,
                      deMinuteSeparator,
                      deSecond2, deSecond1
                      );

  TDissolveType = (dtEvenly, dtRandom);
  TDissolveProgressType = (dptDownUp, dptIntersect);
  TDissolveProgress = (dpNone, dpBegin, dpProcess, dpEnd);
  TDissolveProgressDirection = (dpdNone, dpdUp, dpdDown);

const
  ClockTypeMap: Array [TDisolveClockType] of TDisolveDigitTypeCollection
                = ([ddtYear4, ddtYear3, ddtYear2, ddtYear1,
                    ddtMonth2, ddtMonth1,
                    ddtDay2, ddtDay1],
                   [ddtHour2, ddtHour1,
                    ddtMinute2, ddtMinute1,
                    ddtSecond2, ddtSecond1],
                   [],
                   [ddtYear4, ddtYear3, ddtYear2, ddtYear1,
                    ddtMonth2, ddtMonth1,
                    ddtDay2, ddtDay1,
                    ddtHour2, ddtHour1,
                    ddtMinute2, ddtMinute1,
                    ddtSecond2, ddtSecond1]
                   );

  CLASS_DISSOLVE_DIGIT_PROGRESS_TIMER_INTERVAL = 30;

type
  TDissolvePixel = packed record
    dp_X,
    dp_Y: Word;

    dp_Color: TColor;
    dp_Red,
    dp_Green,
    dp_Blue: Byte;
  end;

  TDissolveDigit = class (TImage)
  private
    class var FProgressTimer: TTimer;
    class var FObjects: TList<TDissolveDigit>;
    class var FActiveObjects: TList<TDissolveDigit>;
    class var FActivatedObjects: TList<TDissolveDigit>;
    class var FDeactivatedObjects: TList<TDissolveDigit>;

    class procedure Handled_OnTimer(Sender: TObject);

    class procedure DoInitialize;
    class procedure DoFinalize;

    class procedure Object_Created(AObject: TDissolveDigit);
    class procedure Object_Destroyed(AObject: TDissolveDigit);

    class procedure Object_ProcessActivatedQuery();
    class procedure Object_ProcessDeactivatedQuery();

    class procedure Object_StartProgress(AObject: TDissolveDigit);
    class procedure Object_EndProgress(AObject: TDissolveDigit);

  protected
    FDigitType: TDisolveDigitType;
    FLayer,
    FTemplate,
    FSwapTemplate: TBitmap;
    FProgress: Boolean;
    FTransparentColor: TColor;
    FDissolveTime: DWORD;
    FDissolveType: TDissolveType;
    FProgressType: TDissolveProgressType;
    FProgressState: TDissolveProgress;
    FProgressDirection: TDissolveProgressDirection;
    FFillMap: Array of Array of Boolean;
    FPixels: TList<TDissolvePixel>;
    FTemplatePixels: TList<TDissolvePixel>;
    FSwapTemplatePixels: TList<TDissolvePixel>;
    FActivePixels: TList<TDissolvePixel>;
    FActivePixels2: TList<TDissolvePixel>;
    FStepCount: Integer;
    FStepCollecter: Real;
    FStepCollecter2: Real;
    FPixelCount: Integer;
    FPixelCount2: Integer;
    FOnProgressBegin: TNotifyEvent;
    FOnProgressEnd: TNotifyEvent;
    FValue: Char;

    procedure Paint; override;
    procedure Resize; override;

    procedure CollectPixels(ALayer: TBitmap; var List: TList<TDissolvePixel>);
    procedure FinishPixels;

    procedure DoProgress; virtual;

    procedure SetTransparentColor(Value: TColor);
    procedure SetProgress(Value: Boolean);
    procedure SetTemplate(Template: TBitmap);
    procedure SetSwapTemplate(Template: TBitmap);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Repaint; override;

    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property Progress: Boolean read FProgress write SetProgress;
    property ProgressType: TDissolveProgressType read FProgressType write FProgressType;
    property DigitType: TDisolveDigitType read FDigitType write FDigitType;
    property Template: TBitmap read FTemplate write SetTemplate;
    property SwapTemplate: TBitmap read FSwapTemplate write SetSwapTemplate;
    property DissolveType: TDissolveType read FDissolveType write FDissolveType;
    property DissolveTime: DWORD read FDissolveTime write FDissolveTime;
    property Value: Char read FValue write FValue;
    property OnProgressBegin: TNotifyEvent read FOnProgressBegin write FOnProgressBegin;
    property OnProgressEnd: TNotifyEvent read FOnProgressEnd write FOnProgressEnd;
  end;

  TDissolveClock = class (TPanel)

  protected
    FDigits: TList<TDissolveDigit>;
    FActiveDigits: TList<TDissolveDigit>;
    FDateDelimiterList: TList<TImage>;
    FTimeDelimiterList: TList<TImage>;

    FDateDelimiters: TDissolveDateDelimiters;
    FTimeDelimiters: TDissolveTimeDelimiters;
    FCustomDigitCollection: TDisolveDigitTypeCollection;
    FClockType: TDisolveClockType;
    FTemplates: TList<TBitmap>;
    FTimer: TTimer;
    FPrevTime: TSystemTime;
    FFillTime: Boolean;

    procedure LoadTemplates;
    procedure RecalcDigitsPosition;

    function GetDigit(DigitType: TDisolveDigitType): TDissolveDigit;

    procedure SetDateDelimiters(Value: TDissolveDateDelimiters);
    procedure SetTimeDelimiters(Value: TDissolveTimeDelimiters);
    procedure SetCustomDigitCollection(Collection: TDisolveDigitTypeCollection);
    procedure SetClockType(Value: TDisolveClockType);

    procedure UpdateTime;

    procedure Handler_OnTimer(Sender: TObject);

    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Repaint; override;

    property DateDelimiters: TDissolveDateDelimiters read FDateDelimiters write SetDateDelimiters;
    property TimeDelimiters: TDissolveTimeDelimiters read FTimeDelimiters write SetTimeDelimiters;
    property CustomDigitCollection: TDisolveDigitTypeCollection read FCustomDigitCollection write SetCustomDigitCollection;
    property ClockType: TDisolveClockType read FClockType write SetClockType;
    property Digits[DigitType: TDisolveDigitType]: TDissolveDigit read GetDigit;
  end;

implementation

var
  FDissolveDigit_ClassInitialized: Boolean = false;

class procedure TDissolveDigit.Handled_OnTimer(Sender: TObject);
var
  Item: TDissolveDigit;
begin
  Object_ProcessActivatedQuery;

  for Item in FActiveObjects do
  begin
    if (Item.Progress)
    then Item.DoProgress;
  end;

  Object_ProcessDeactivatedQuery;
end;

class procedure TDissolveDigit.DoInitialize;
var
  O: TObject;
begin
  FObjects := TList<TDissolveDigit>.Create;
  FActiveObjects := TList<TDissolveDigit>.Create;
  FActivatedObjects := TList<TDissolveDigit>.Create;
  FDeactivatedObjects := TList<TDissolveDigit>.Create;
  FProgressTimer := TTimer.Create(nil);
  FProgressTimer.OnTimer := Handled_OnTimer;
  FProgressTimer.Interval := CLASS_DISSOLVE_DIGIT_PROGRESS_TIMER_INTERVAL;
  FProgressTimer.Enabled := false;
end;

class procedure TDissolveDigit.DoFinalize;
begin
  FDeactivatedObjects.Free;
  FActivatedObjects.Free;
  FActiveObjects.Free;
  FProgressTimer.Free;
  FObjects.Free;
end;

class procedure TDissolveDigit.Object_Created;
begin
  if not (FDissolveDigit_ClassInitialized)
  then DoInitialize;

  FObjects.Add(AObject);
end;

class procedure TDissolveDigit.Object_Destroyed;
begin
  Object_EndProgress(AObject);
  if (FDissolveDigit_ClassInitialized) and (FObjects.Count = 0)
  then DoFinalize;
end;

class procedure TDissolveDigit.Object_ProcessActivatedQuery;
var
  I: Integer;
begin
  for I := 0 to FActivatedObjects.Count - 1
  do if not (FActiveObjects.Contains(FActivatedObjects[I]))
     then FActiveObjects.Add(FActivatedObjects[I]);

  FActivatedObjects.Clear;
end;

class procedure TDissolveDigit.Object_ProcessDeactivatedQuery;
var
  I: Integer;
begin
  for I := FDeactivatedObjects.Count - 1 downto 0
  do if (FActiveObjects.Contains(FDeactivatedObjects[I]))
     then FActiveObjects.Remove(FDeactivatedObjects[I]);
  FDeactivatedObjects.Clear;
end;

class procedure TDissolveDigit.Object_StartProgress(AObject: TDissolveDigit);
begin
  if not (FActivatedObjects.Contains(AObject))
  then FActivatedObjects.Add(AObject);

  if (FActiveObjects.Count = 0) then
  begin
    Object_ProcessActivatedQuery;
    FProgressTimer.Enabled := true;
  end;
end;

class procedure TDissolveDigit.Object_EndProgress(AObject: TDissolveDigit);
begin
  if not (FDeactivatedObjects.Contains(AObject))
  then FDeactivatedObjects.Add(AObject);

  if (FActiveObjects.Count = 0) then
  begin
    Object_ProcessDeactivatedQuery;
    FProgressTimer.Enabled := false;
  end;
end;

constructor TDissolveDigit.Create(AOwner: TComponent);
begin
  Object_Created(Self);
  FLayer := TBitmap.Create;
  FLayer.PixelFormat := pf24bit;
  FTemplate := TBitmap.Create;
  FTemplate.PixelFormat := pf24bit;
  FSwapTemplate := TBitmap.Create;
  FSwapTemplate.PixelFormat := pf24bit;
  Inherited Create(AOwner);
  Picture.Graphic := FLayer;
  FProgress := false;
  FTransparentColor := clWhite;
  FDissolveTime := 1000;
  FDissolveType := dtEvenly;
  FProgressType := dptDownUp;
  FProgressState := dpNone;
  FProgressDirection := dpdNone;
  FTemplatePixels := TList<TDissolvePixel>.Create;
  FSwapTemplatePixels := TList<TDissolvePixel>.Create;
  FActivePixels := TList<TDissolvePixel>.Create;
  FActivePixels2 := TList<TDissolvePixel>.Create;
  DigitType := ddtNone;
  Width := 100;
  Height := 100;
  Stretch := false;
end;

destructor TDissolveDigit.Destroy;
begin
  FActivePixels2.Free;
  FActivePixels.Free;
  FSwapTemplatePixels.Free;
  FTemplatePixels.Free;
  FSwapTemplate.Free;
  FTemplate.Free;
  FLayer.Free;
  Inherited Destroy;
  Object_Destroyed(Self);
end;

procedure TDissolveDigit.Repaint;
begin
  Inherited Repaint;

  if (Assigned(Picture.Graphic))
  then Canvas.StretchDraw(Rect(0, 0, ClientWidth, ClientHeight), FLayer);
end;

procedure TDissolveDigit.Paint;
begin
  Inherited Paint;
  //Canvas.Draw(0, 0, FLayer);
end;

procedure TDissolveDigit.Resize;
begin
  Inherited Resize;
  Repaint;
end;

procedure TDissolveDigit.CollectPixels;
var
  Line: Pointer;
  I, J: Integer;
  Pxl1: PRGBTriple;
  DPxl: TDissolvePixel;
begin
  List.Clear;

  for I := 0 to ALayer.Height - 1 do
  begin
    Line := ALayer.ScanLine[I];
    for J := 0 to ALayer.Width - 1 do
    begin
      Pxl1 := Pointer(Integer(Line) + 3 * J);

      if (TColor(RGB(Pxl1^.rgbtRed, Pxl1^.rgbtGreen, Pxl1^.rgbtBlue)) <> FTransparentColor) then
      begin
        DPxl.dp_X := Word(J);
        DPxl.dp_Y := Word(I);
        DPxl.dp_Color := RGB(Pxl1.rgbtRed, Pxl1.rgbtGreen, Pxl1.rgbtBlue);
        DPxl.dp_Red := Pxl1^.rgbtRed;
        DPxl.dp_Green := Pxl1^.rgbtGreen;
        DPxl.dp_Blue := Pxl1^.rgbtBlue;
        List.Add(DPxl);
      end;
    end;
  end;
end;

procedure TDissolveDigit.FinishPixels;
var
  I: Integer;
begin
  if (FProgressType = dptIntersect)
  then FPixels := FSwapTemplatePixels;

  for I := 0 to FPixels.Count - 1 do
  begin
    FLayer.Canvas.Pixels[FPixels[I].dp_X, FPixels[I].dp_Y] := FPixels[I].dp_Color;
  end;
end;

procedure TDissolveDigit.DoProgress;

  procedure DeleteIntersections(APixels, DeleteFrom: TList<TDissolvePixel>);
  var
    I, J: Integer;
  begin
    for I := 0 to APixels.Count - 1 do
    begin
      J := 0;
      while (J < DeleteFrom.Count)do
      begin
        if (APixels[I].dp_X = DeleteFrom[J].dp_X)
          and (APixels[I].dp_Y = DeleteFrom[J].dp_Y)
        then
        begin
          DeleteFrom.Delete(J);
          Dec(J);
        end;

        Inc(J);
      end;
    end;
  end;

var
  NperSec: Real;
  StepsTotal: Real;
  StepCost: Real;
  StepCost2: Real;
  BufVal: Real;
  ProcessSteps, I, Index: Integer;
  PxlData: TDissolvePixel;

begin

  case FProgressState of
    dpNone: ;

    dpBegin:
    begin
      FPixels := FTemplatePixels;
      FActivePixels.Clear;
      FActivePixels.AddRange(FPixels);
      FLayer.Canvas.Draw(0, 0, FTemplate);
      //Repaint;
      FStepCount := 0;
      FStepCollecter := 0;

      case FProgressType of
        dptDownUp: FProgressDirection := dpdDown;
        dptIntersect:
        begin
          FStepCollecter2 := 0;
          FActivePixels2.Clear;
          FActivePixels2.AddRange(FSwapTemplatePixels);
          DeleteIntersections(FSwapTemplatePixels, FActivePixels);
          DeleteIntersections(FTemplatePixels, FActivePixels2);
          FPixelCount := FActivePixels.Count;
          FPixelCount2 := FActivePixels2.Count;
        end;
      end;

      FProgressState := dpProcess;

      if (Assigned(FOnProgressBegin))
      then FOnProgressBegin(Self);
    end;

    dpProcess:
    begin
      NPerSec := 1000 / CLASS_DISSOLVE_DIGIT_PROGRESS_TIMER_INTERVAL;
      StepsTotal := NPerSec * (FDissolveTime / 1000);

      case FProgressType of
        dptDownUp:
        begin
          StepCost := FPixels.Count / (StepsTotal / 2);
        end;

        dptIntersect:
        begin
          StepCost := FPixelCount / StepsTotal;
          StepCost2 := FPixelCount2 / StepsTotal;
        end;
      end;

      FStepCollecter := FStepCollecter + StepCost;
      ProcessSteps := Trunc(FStepCollecter);
      if (ProcessSteps > 0) then
      begin
        FStepCollecter := FStepCollecter - ProcessSteps;
        ProcessSteps := Min(ProcessSteps, FActivePixels.Count);

        for I := 0 to ProcessSteps - 1 do
        begin
          if (FActivePixels.Count = 0)
          then Break;

          case FDissolveType of
            dtEvenly:
            begin
              if (ProcessSteps > 1)
              then
              begin
                BufVal := I * (FActivePixels.Count / 2.0 / ProcessSteps);
                if (I mod 2 = 1)
                then BufVal := (FActivePixels.Count / 2.0) - BufVal
                else BufVal := (FActivePixels.Count / 2.0) + BufVal;

                Index := EnsureRange(Round(BufVal),
                                     0,
                                     FActivePixels.Count - 1);
                PxlData := FActivePixels[Index];
                FActivePixels.Delete(Index);
              end
              else
              begin
                Index := Random(FActivePixels.Count - 1);
                PxlData := FActivePixels[Index];
                FActivePixels.Delete(Index);
              end;
            end;
            dtRandom:
            begin
              Index := Random(FActivePixels.Count - 1);
              PxlData := FActivePixels[Index];
              FActivePixels.Delete(Index);
            end;
          end;

          case FProgressType of
            dptDownUp:
            begin
              case FProgressDirection of
                dpdNone: ;
                dpdUp: FLayer.Canvas.Pixels[PxlData.dp_X, PxlData.dp_Y] := PxlData.dp_Color;
                dpdDown: FLayer.Canvas.Pixels[PxlData.dp_X, PxlData.dp_Y] := FTransparentColor;
              end;
            end;
            dptIntersect: FLayer.Canvas.Pixels[PxlData.dp_X, PxlData.dp_Y] := FTransparentColor;
          end;
        end;

        //Repaint;
      end;

      if (FProgressType = dptIntersect) then
      begin
        FStepCollecter2 := FStepCollecter2 + StepCost2;
        ProcessSteps := Trunc(FStepCollecter2);
        if (ProcessSteps > 0) then
        begin
          FStepCollecter2 := FStepCollecter2 - ProcessSteps;
          ProcessSteps := Min(ProcessSteps, FActivePixels2.Count);

          for I := 0 to ProcessSteps - 1 do
          begin
            if (FActivePixels2.Count = 0)
            then Break;

            case FDissolveType of
              dtEvenly:
              begin
                if (ProcessSteps > 1)
                then
                begin
                  BufVal := I * (FActivePixels2.Count / 2.0 / ProcessSteps);
                  if (I mod 2 = 1)
                  then BufVal := (FActivePixels2.Count / 2.0) - BufVal
                  else BufVal := (FActivePixels2.Count / 2.0) + BufVal;

                  Index := EnsureRange(Round(BufVal),
                                       0,
                                       FActivePixels2.Count - 1);
                  PxlData := FActivePixels2[Index];
                  FActivePixels2.Delete(Index);
                end
                else
                begin
                  Index := Random(FActivePixels2.Count - 1);
                  PxlData := FActivePixels2[Index];
                  FActivePixels2.Delete(Index);
                end;
              end;
              dtRandom:
              begin
                Index := Random(FActivePixels2.Count - 1);
                PxlData := FActivePixels2[Index];
                FActivePixels2.Delete(Index);
              end;
            end;

            FLayer.Canvas.Pixels[PxlData.dp_X, PxlData.dp_Y] := PxlData.dp_Color;
          end;
        //  Repaint;
        end;

        if (FProgressType = dptDownUp)
          and (FStepCount = Trunc(StepsTotal) div 2)
        then
        begin
          FProgressDirection := dpdUp;
          FPixels := FSwapTemplatePixels;
          FActivePixels.Clear;
          FActivePixels.AddRange(FPixels);
        end;
      end;

      if (FStepCount >= Trunc(StepsTotal))
        {or (FActivePixels.Count = 0)}
      then FProgressState := dpEnd;

      Inc(FStepCount);
    end;

    dpEnd:
    begin
      SetLength(FFillMap, 0, 0);
      FinishPixels;
      Progress := false;
      //Repaint;

      if (Assigned(FOnProgressEnd))
      then FOnProgressEnd(Self);
    end;
  end;

  Repaint;
end;

procedure TDissolveDigit.SetTransparentColor(Value: TColor);
var
  PrevCol: TColor;
  Line: Pointer;
  I, J: Integer;
  Pxl1: PRGBTriple;
begin
  PrevCol := FTransparentColor;
  FTransparentColor := Value;

  for I := 0 to FLayer.Height - 1 do
  begin
    Line := FLayer.ScanLine[I];
    for J := 0 to FLayer.Width - 1 do
    begin
      Pxl1 := Pointer(Integer(Line) + 3 * J);

      if (RGB(Pxl1.rgbtRed, Pxl1.rgbtGreen, Pxl1.rgbtBlue) = PrevCol) then
      begin
        Pxl1^.rgbtRed := GetRValue(FTransparentColor);
        Pxl1^.rgbtGreen := GetGValue(FTransparentColor);
        Pxl1^.rgbtBlue := GetBValue(FTransparentColor);
      end;
    end;
  end;
end;

procedure TDissolveDigit.SetProgress(Value: Boolean);
begin
  FProgress := Value;

  if (Value)
  then
  begin
    FProgressState := dpBegin;
    Object_StartProgress(Self)
  end
  else
  begin
    Object_EndProgress(Self);
    FProgressState := dpEnd;
  end;
end;

procedure TDissolveDigit.SetTemplate(Template: TBitmap);
begin
  FTemplate.Assign(Template);
  FTemplate.PixelFormat := pf24bit;
  FLayer.Width := FTemplate.Width;
  FLayer.Height := FTemplate.Height;
  FLayer.Canvas.Draw(0,0, Template);

  if (Assigned(Picture.Graphic)) then
  begin
    Picture.Graphic.Width := FLayer.Width;
    Picture.Graphic.Height := FLayer.Height;
  end;

  CollectPixels(FTemplate, FTemplatePixels);
  Repaint;
end;

procedure TDissolveDigit.SetSwapTemplate(Template: TBitmap);
begin
  FSwapTemplate.Assign(Template);
  FSwapTemplate.PixelFormat := pf24bit;
  CollectPixels(FSwapTemplate, FSwapTemplatePixels);
end;


constructor TDissolveClock.Create(AOwner: TComponent);
var
  I: Integer;
  D: TDissolveDigit;
  Img: TImage;
begin
  Inherited Create(AOwner);
  BorderStyle := bsNone;
  BevelOuter := bvNone;
  FDigits := TList<TDissolveDigit>.Create;
  FActiveDigits := TList<TDissolveDigit>.Create;
  FTemplates := TList<TBitmap>.Create;
  LoadTemplates;
  FDateDelimiterList := TList<TImage>.Create;
  FTimeDelimiterList := TList<TImage>.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 100;
  FTimer.OnTimer := Handler_OnTimer;
  FFillTime := true;
  FDateDelimiters := [];
  FTimeDelimiters := [];
  ZeroMemory(@FPrevTime, SizeOf(FPrevTime));

  for I := Ord(TDisolveDigitType(0)) to Ord(High(TDisolveDigitType)) do
  begin
    D := TDissolveDigit.Create(Self);
    D.Parent := Self;
    D.Width := Height;
    D.Height := Height;
    D.DigitType := TDisolveDigitType(I);
    D.DissolveType := dtRandom;
    D.DissolveTime := 800;
    D.ProgressType := dptIntersect;
    D.SwapTemplate := FTemplates[0];
    D.Value := '0';
    FDigits.Add(D);
  end;

  for I := Ord(TDissolveDateDelimiter(0)) to Ord(High(TDissolveDateDelimiter)) do
  begin
    Img := TImage.Create(Self);            
    Img.Parent := Self;  
    Img.Picture.LoadFromFile('Templates\DateDelimiter.bmp');
    Img.AutoSize := true;
    Img.Hide;
    FDateDelimiterList.Add(Img);
  end;

  for I := Ord(TDissolveTimeDelimiter(0)) to Ord(High(TDissolveTimeDelimiter)) do
  begin
    Img := TImage.Create(Self);            
    Img.Parent := Self;            
    Img.Picture.LoadFromFile('Templates\TimeDelimiter.bmp');
    Img.AutoSize := true;
    Img.Hide;
    FTimeDelimiterList.Add(Img);
  end;

  //AutoSize := true;
  ClockType := dctCustom;
  FTimer.Enabled := true;
end;

destructor TDissolveClock.Destroy;
begin
  FTimeDelimiterList.Free;
  FDateDelimiterList.Free;
  FTimer.Free;
  FTemplates.Free;
  FDigits.Free;
  FActiveDigits.Free;
  Inherited Destroy;
end;

procedure TDissolveCLock.Repaint;
var
  Item: TDissolveDigit;
begin
  Inherited Repaint;

  for Item in FDigits
  do Item.Repaint;
end;

procedure TDissolveClock.LoadTemplates;
var
  I: Integer;
  Bmp: TBitmap;
begin
  for I := 0 to 9 do
  begin
    Bmp := TBitmap.Create;
    Bmp.LoadFromFile('Templates\Template' + IntToStr(I) + '.bmp');
    FTemplates.Add(Bmp);
  end;
end;

procedure TDissolveClock.RecalcDigitsPosition;
var
  I, Offset: Integer;
  DigTypes: TDisolveDigitTypeCollection;
  DateDelims: TDissolveDateDelimiters;
  TimeDelims: TDissolveTimeDelimiters;
begin
  if (FActiveDigits.Count = 0)
  then Exit;

  DigTypes := CustomDigitCollection;
  DateDelims := [];
  TimeDelims := [];
  Offset := 0;

  for I := 0 to FActiveDigits.Count - 1 do
  begin
    FActiveDigits[I].Left := Offset;                  
    Inc(Offset, FActiveDigits[I].Width);

    case FActiveDigits[I].DigitType of
      ddtYear4, ddtYear3, ddtYear2, ddtYear1:
      begin
        if (dddYear in FDateDelimiters)
          and not (dddYear in DateDelims)
          and (((ddtYear1 in DigTypes)
               and not (ddtYear2 in DigTypes)
               and not (ddtYear3 in DigTypes)
               and not (ddtYear4 in DigTypes))
               or ((ddtYear2 in DigTypes)
                    and not (ddtYear1 in DigTypes)
                    and not (ddtYear3 in DigTypes)
                    and not (ddtYear4 in DigTypes))
               or ((ddtYear3 in DigTypes)
                    and not (ddtYear1 in DigTypes)
                    and not (ddtYear2 in DigTypes)
                    and not (ddtYear4 in DigTypes))
               or ((ddtYear4 in DigTypes)
                    and not (ddtYear1 in DigTypes)
                    and not (ddtYear2 in DigTypes)
                    and not (ddtYear3 in DigTypes)))
          and not (dddYear in DateDelims)
        then
        begin
          DateDelims := DateDelims + [dddYear];
          FDateDelimiterList[Ord(dddYear)].Left := Offset;
          Inc(Offset, FDateDelimiterList[Ord(dddYear)].Width);
        end;
      end;

      ddtMonth2, ddtMonth1:
      begin
        if (dddMonth in FDateDelimiters)
          and not (dddMonth in DateDelims)
          and (((ddtMonth1 in DigTypes)
               and not (ddtMonth2 in DigTypes)
               or ((ddtMonth2 in DigTypes)
                    and not (ddtMonth1 in DigTypes))))
          and not (dddMonth in DateDelims)
        then
        begin
          DateDelims := DateDelims + [dddMonth];
          FDateDelimiterList[Ord(dddMonth)].Left := Offset;
          Inc(Offset, FDateDelimiterList[Ord(dddMonth)].Width);
        end;
      end;

      ddtHour2, ddtHour1:
      begin
        if (dtdHour in FTimeDelimiters)
          and not (dtdHour in TimeDelims)
          and (((ddtHour1 in DigTypes)
               and not (ddtHour2 in DigTypes)
               or ((ddtHour2 in DigTypes)
                    and not (ddtHour1 in DigTypes))))
        then
        begin
          TimeDelims := TimeDelims + [dtdHour];
          FTimeDelimiterList[Ord(dtdHour)].Left := Offset;
          Inc(Offset, FTimeDelimiterList[Ord(dtdHour)].Width);
        end;
      end;

      ddtMinute2, ddtMinute1:
      begin
        if (dtdMinute in FTimeDelimiters)
          and not (dtdMinute in TimeDelims)
          and (((ddtMinute1 in DigTypes)
               and not (ddtMinute2 in DigTypes)
               or ((ddtMinute2 in DigTypes)
                    and not (ddtMinute1 in DigTypes))))
        then
        begin
          TimeDelims := TimeDelims + [dtdMinute];
          FTimeDelimiterList[Ord(dtdMinute)].Left := Offset;
          Inc(Offset, FTimeDelimiterList[Ord(dtdMinute)].Width);
        end;
      end;
    end;

    Exclude(DigTypes, FActiveDigits[I].DigitType);
  end;

end;

function TDissolveClock.GetDigit(DigitType: TDisolveDigitType): TDissolveDigit;
begin
  Result := FDigits[Ord(DigitType)];
end;

procedure TDissolveClock.SetDateDelimiters(Value: TDissolveDateDelimiters);
var
  I: Integer;
begin
  for I := Ord(TDissolveDateDelimiter(0)) to Ord(High(TDissolveDateDelimiter)) do
  begin
    FDateDelimiterList[I].Visible := TDissolveDateDelimiter(I) in Value;
  end;
  
  FDateDelimiters := Value;
  RecalcDigitsPosition;
end;

procedure TDissolveClock.SetTimeDelimiters(Value: TDissolveTimeDelimiters);
var
  I: Integer;
  Ident: TDissolveTimeDelimiter;
begin
  for I := Ord(TDissolveTimeDelimiter(0)) to Ord(High(TDissolveTimeDelimiter)) do
  begin
    FTimeDelimiterList[I].Visible := TDissolveTimeDelimiter(I) in Value;    
  end;
  
  FTimeDelimiters := Value;
  RecalcDigitsPosition;
end;

procedure TDissolveClock.SetCustomDigitCollection(Collection: TDisolveDigitTypeCollection);
var
  I: Integer;
begin
  FCustomDigitCollection := Collection;
  FActiveDigits.Clear;

  for I := 0 to FDigits.Count - 1
  do if (FDigits[I].DigitType in Collection) then
     begin
       FActiveDigits.Add(FDigits[I]);
       FDigits[I].Show;
     end
     else FDigits[I].Hide;

  RecalcDigitsPosition;
end;

procedure TDissolveClock.SetClockType(Value: TDisolveClockType);
begin
  FClockType := Value;
  SetCustomDigitCollection(ClockTypeMap[Value]);
end;

procedure TDissolveClock.UpdateTime;
var
  CurTime: TSystemTime;
  d1, d2, d3, d4, sd1, sd2, sd3, sd4: Byte;
  TD: TDissolveDigit;
begin
  GetLocalTime(CurTime);

  if (FClockType = dctDate)
    or (FClockType = dctDateAndTime)
    or (FClockType = dctCustom)
  then
  begin
    if (FPrevTime.wYear <> CurTime.wYear) then
    begin
      sd1 := FPrevTime.wYear mod 10;
      sd2 := (FPrevTime.wYear div 10) mod 10;
      sd3 := (FPrevTime.wYear div 100) mod 10;
      sd4 := (FPrevTime.wYear div 1000) mod 10;

      d1 := CurTime.wYear mod 10;
      d2 := (CurTime.wYear div 10) mod 10;
      d3 := (CurTime.wYear div 100) mod 10;
      d4 := (CurTime.wYear div 1000) mod 10;

      if (ddtYear4 in FCustomDigitCollection)
        and ((sd4 <> d4) or (FFillTime))
      then
      begin
        Digits[ddtYear4].Value := Char(Byte(d4) + Ord('0'));
        Digits[ddtYear4].Template := Digits[ddtYear4].SwapTemplate;
        Digits[ddtYear4].SwapTemplate := FTemplates[d4];
        Digits[ddtYear4].Progress := true;
      end;

      if (ddtYear3 in FCustomDigitCollection)
        and ((sd3 <> d3) or (FFillTime))
      then
      begin
        Digits[ddtYear3].Value := Char(Byte(d3) + Ord('0'));
        Digits[ddtYear3].Template := Digits[ddtYear3].SwapTemplate;
        Digits[ddtYear3].SwapTemplate := FTemplates[d3];
        Digits[ddtYear3].Progress := true;
      end;

      if (ddtYear2 in FCustomDigitCollection)
        and ((sd2 <> d2) or (FFillTime))
      then
      begin
        Digits[ddtYear2].Value := Char(Byte(d2) + Ord('0'));
        Digits[ddtYear2].Template := Digits[ddtYear2].SwapTemplate;
        Digits[ddtYear2].SwapTemplate := FTemplates[d2];
        Digits[ddtYear2].Progress := true;
      end;

      if (ddtYear1 in FCustomDigitCollection)
        and ((sd1 <> d1) or (FFillTime))
      then
      begin
        Digits[ddtYear1].Value := Char(Byte(d1) + Ord('0'));
        Digits[ddtYear1].Template := Digits[ddtYear1].SwapTemplate;
        Digits[ddtYear1].SwapTemplate := FTemplates[d1];
        Digits[ddtYear1].Progress := true;
      end;
    end;

    if (FPrevTime.wMonth <> CurTime.wMonth) then
    begin
      sd1 := FPrevTime.wMonth mod 10;
      sd2 := (FPrevTime.wMonth div 10) mod 10;

      d1 := CurTime.wMonth mod 10;
      d2 := (CurTime.wMonth div 10) mod 10;

      if (ddtMonth2 in FCustomDigitCollection)
        and ((sd2 <> d2) or (FFillTime))
      then
      begin
        Digits[ddtMonth2].Value := Char(Byte(d2) + Ord('0'));
        Digits[ddtMonth2].Template := Digits[ddtMonth2].SwapTemplate;
        Digits[ddtMonth2].SwapTemplate := FTemplates[d2];
        Digits[ddtMonth2].Progress := true;
      end;

      if (ddtMonth1 in FCustomDigitCollection)
        and ((sd1 <> d1) or (FFillTime))
      then
      begin
        Digits[ddtMonth1].Value := Char(Byte(d1) + Ord('0'));
        Digits[ddtMonth1].Template := Digits[ddtMonth1].SwapTemplate;
        Digits[ddtMonth1].SwapTemplate := FTemplates[d1];
        Digits[ddtMonth1].Progress := true;
      end;
    end;

    if (FPrevTime.wDay <> CurTime.wDay) then
    begin
      sd1 := FPrevTime.wDay mod 10;
      sd2 := (FPrevTime.wDay div 10) mod 10;

      d1 := CurTime.wDay mod 10;
      d2 := (CurTime.wDay div 10) mod 10;

      if (ddtDay2 in FCustomDigitCollection)
        and ((sd2 <> d2) or (FFillTime))
      then
      begin
        Digits[ddtDay2].Value := Char(Byte(d2) + Ord('0'));
        Digits[ddtDay2].Template := Digits[ddtDay2].SwapTemplate;
        Digits[ddtDay2].SwapTemplate := FTemplates[d2];
        Digits[ddtDay2].Progress := true;
      end;

      if (ddtDay1 in FCustomDigitCollection)
        and ((sd1 <> d1) or (FFillTime))
      then
      begin
        Digits[ddtDay1].Value := Char(Byte(d1) + Ord('0'));
        Digits[ddtDay1].Template := Digits[ddtDay1].SwapTemplate;
        Digits[ddtDay1].SwapTemplate := FTemplates[d1];
        Digits[ddtDay1].Progress := true;
      end;
    end;

  end;

  if (FClockType = dctTime)
    or (FClockType = dctDateAndTime)
    or (FClockType = dctCustom)
  then
  begin
    if (FPrevTime.wSecond <> CurTime.wSecond) then
    begin
      sd1 := FPrevTime.wHour mod 10;
      sd2 := (FPrevTime.wHour div 10) mod 10;

      d1 := CurTime.wHour mod 10;
      d2 := (CurTime.wHour div 10) mod 10;

      if (ddtHour2 in FCustomDigitCollection)
        and ((sd2 <> d2) or (FFillTime))
      then
      begin
        TD := DIgits[ddtHour2];
        TD.Value := Char(Byte(d2) + Ord('0'));
        TD.Template := TD.SwapTemplate;
        TD.Template.SaveToFile('test.bmp');
        TD.SwapTemplate := FTemplates[d2];
        TD.Progress := true;
      end;

      if (ddtHour1 in FCustomDigitCollection)
        and ((sd1 <> d1) or (FFillTime))
      then
      begin
        Digits[ddtHour1].Value := Char(Byte(d1) + Ord('0'));
        Digits[ddtHour1].Template := Digits[ddtHour1].SwapTemplate;
        Digits[ddtHour1].SwapTemplate := FTemplates[d1];
        Digits[ddtHour1].Progress := true;
      end;
    end;

    if (FPrevTime.wMinute <> CurTime.wMinute) then
    begin
      sd1 := FPrevTime.wMinute mod 10;
      sd2 := (FPrevTime.wMinute div 10) mod 10;

      d1 := CurTime.wMinute mod 10;
      d2 := (CurTime.wMinute div 10) mod 10;

      if (ddtMinute2 in FCustomDigitCollection)
        and ((sd2 <> d2) or (FFillTime))
      then
      begin
        Digits[ddtMinute2].Value := Char(Byte(d2) + Ord('0'));
        Digits[ddtMinute2].Template := Digits[ddtMinute2].SwapTemplate;
        Digits[ddtMinute2].SwapTemplate := FTemplates[d2];
        Digits[ddtMinute2].Progress := true;
      end;

      if (ddtMinute1 in FCustomDigitCollection)
        and ((sd1 <> d1) or (FFillTime))
      then
      begin
        Digits[ddtMinute1].Value := Char(Byte(d1) + Ord('0'));
        Digits[ddtMinute1].Template := Digits[ddtMinute1].SwapTemplate;
        Digits[ddtMinute1].SwapTemplate := FTemplates[d1];
        Digits[ddtMinute1].Progress := true;
      end;
    end;

    if (FPrevTime.wSecond <> CurTime.wSecond) then
    begin
      sd1 := FPrevTime.wSecond mod 10;
      sd2 := (FPrevTime.wSecond div 10) mod 10;

      d1 := CurTime.wSecond mod 10;
      d2 := (CurTime.wSecond div 10) mod 10;

      if (ddtSecond2 in FCustomDigitCollection)
        and ((sd2 <> d2) or (FFillTime))
      then
      begin
        Digits[ddtSecond2].Value := Char(Byte(d2) + Ord('0'));
        Digits[ddtSecond2].Template := Digits[ddtSecond2].SwapTemplate;
        Digits[ddtSecond2].SwapTemplate := FTemplates[d2];
        Digits[ddtSecond2].Progress := true;
      end;

      if (ddtSecond1 in FCustomDigitCollection)
        and ((sd1 <> d1) or (FFillTime))
      then
      begin
        Digits[ddtSecond1].Value := Char(Byte(d1) + Ord('0'));
        Digits[ddtSecond1].Template := Digits[ddtSecond1].SwapTemplate;
        Digits[ddtSecond1].SwapTemplate := FTemplates[d1];
        Digits[ddtSecond1].Progress := true;
      end;
    end;

    if (FFillTime) then
    begin
      FFillTime := false;
      Repaint;
    end;
  end;

  FPrevTime := CurTime;
end;

procedure TDissolveClock.Handler_OnTimer(Sender: TObject);
begin
  UpdateTime;
end;

procedure TDissolveClock.Resize;
var
  I: Integer;
begin
  for I := 0 to FDigits.Count - 1 do
  begin
    FDigits[I].Width := Height;
    FDigits[I].Height := Height;
  end;                 

  for I := 0 to FDateDelimiterList.Count - 1 do
  begin
   FDateDelimiterList[I].Height := Height;
  end;             
                         
  for I := 0 to FTimeDelimiterList.Count - 1 do
  begin
   FTimeDelimiterList[I].Height := Height;
  end;             
  RecalcDigitsPosition;
end;

end.
