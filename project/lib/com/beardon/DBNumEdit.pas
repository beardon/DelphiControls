unit DBNumEdit;

interface

uses
  Windows, SysUtils, Classes, Dialogs, DBCtrls, DB, Messages, Controls, Mask;

type
  TMoney = LongInt;

  THackFieldDataLink = class(TFieldDataLink)
  protected
    FModified: Boolean;
  end;

  TDBMoneyEdit = class(TCustomMaskEdit)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: WideString;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetTextMargins: TPoint;
    procedure ResetMaxLength;
    procedure SetDataField(const Value: WideString);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: WideString read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


procedure Register;

implementation

uses
  Forms,
  Graphics,
  StdCtrls;

procedure Register;
begin
  RegisterComponents('Beardon', [TDBMoneyEdit]);
end;


{***************** TDBMoneyEdit Methods *******************}
procedure TDBMoneyEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

constructor TDBMoneyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TDBMoneyEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TDBMoneyEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TDBMoneyEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBMoneyEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TDBMoneyEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDBMoneyEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key >= #32) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..High(Char):
      if not FDataLink.Edit then
        Key := #0;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TDBMoneyEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TDBMoneyEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TDBMoneyEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TDBMoneyEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TDBMoneyEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBMoneyEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBMoneyEdit.GetDataField: WideString;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBMoneyEdit.SetDataField(const Value: WideString);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

function TDBMoneyEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBMoneyEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDBMoneyEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBMoneyEdit.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TDBMoneyEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FAlignment <> FDataLink.Field.Alignment then
    begin
      EditText := '';  {forces update}
      FAlignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      if FDataLink.Editing and THackFieldDataLink(FDataLink).FModified then
        Modified := True;
    end;
  end else
  begin
    FAlignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name else
      EditText := '';
  end;
end;

procedure TDBMoneyEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TDBMoneyEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TDBMoneyEdit.WMUndo(var Message: TMessage);
begin
  if FDataLink.Edit then
    inherited;
end;

procedure TDBMoneyEdit.WMPaste(var Message: TMessage);
begin
  if FDataLink.Edit then
    inherited
end;

procedure TDBMoneyEdit.WMCut(var Message: TMessage);
begin
  if FDataLink.Edit then
    inherited;
end;

procedure TDBMoneyEdit.WMClear(var Message: TMessage);
begin
  if FDataLink.Edit then
    inherited;
end;

procedure TDBMoneyEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TDBMoneyEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

procedure TDBMoneyEdit.WMPaint(var Message: TWMPaint);
const
  AlignStyle : array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;
  ExStyle: DWORD;
begin
  AAlignment := FAlignment;
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
  if ((AAlignment = taLeftJustify) or FFocused) and
    not (csPaintCopy in ControlState) then
  begin
    if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if UseRightToLeftReading then ExStyle := ExStyle or WS_EX_RTLREADING;
      if UseRightToLeftScrollbar then ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
    end;
    inherited;
    Exit;
  end;
{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      if not Enabled then
        Font.Color := clGrayText;
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      if PasswordChar <> #0 then S := StringOfChar(PasswordChar, Length(S));
      Margins := GetTextMargins;
      case AAlignment of
        taLeftJustify: Left := Margins.X;
        taRightJustify: Left := ClientWidth - TextWidth(S) - Margins.X - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      if SysLocale.MiddleEast then UpdateTextFlags;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TDBMoneyEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TDBMoneyEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

function TDBMoneyEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBMoneyEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

end.
