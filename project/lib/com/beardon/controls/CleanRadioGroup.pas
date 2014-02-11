unit CleanRadioGroup;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, ExtCtrls, Messages;

type
  TCleanCustomRadioGroup = class(TCustomGroupBox)
  private
    FButtons: TList;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FWordWrap: Boolean;
    function GetButtons(Index: Integer): TRadioButton;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure SetWordWrap(Value: Boolean);
    procedure UpdateButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
    function CanModify: Boolean; virtual;
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ResizeWidth;
    property Buttons[Index: Integer]: TRadioButton read GetButtons;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  end;

  TCleanRadioGroup = class(TCleanCustomRadioGroup)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Columns;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ItemIndex;
    property Items;
    property Constraints;
    property ParentBiDiMode;
    property ParentBackground default True;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  Dialogs,
  Forms,
  Graphics,
  Themes,
  Windows;

procedure Register;
begin
  RegisterComponents('Beardon', [TCleanRadioGroup]);
end;

{ TGroupButton }

type
  TCleanGroupButton = class(TRadioButton)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor InternalCreate(RadioGroup: TCleanCustomRadioGroup);
    destructor Destroy; override;
  end;

constructor TCleanGroupButton.InternalCreate(RadioGroup: TCleanCustomRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  Parent := RadioGroup;
end;

destructor TCleanGroupButton.Destroy;
begin
  TCleanCustomRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TCleanGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TCleanCustomRadioGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TCleanGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TCleanCustomRadioGroup(Parent).KeyPress(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TCleanCustomRadioGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TCleanGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TCleanCustomRadioGroup(Parent).KeyDown(Key, Shift);
end;

procedure TCleanGroupButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

{ TCustomRadioGroup }

constructor TCleanCustomRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks, csParentBackground];
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
end;

destructor TCleanCustomRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TCleanCustomRadioGroup.FlipChildren(AllLevels: Boolean);
begin
  { The radio buttons are flipped using BiDiMode }
end;

procedure TCleanCustomRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    ButtonHeight := Metrics.tmHeight;
    TopMargin := 0;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TCleanGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          ALeft := (I div ButtonsPerCol) * ButtonWidth;
          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TCleanCustomRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;

procedure TCleanCustomRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then
      FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TCleanCustomRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TCleanCustomRadioGroup.Paint;
begin
  with Canvas do
  begin
    Font := Self.Font;
  end;
end;

procedure TCleanCustomRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  if HandleAllocated then
    UpdateButtons;
end;

procedure TCleanCustomRadioGroup.CreateWnd;
begin
  inherited CreateWnd;
  UpdateButtons;
end;

procedure TCleanCustomRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    TCleanGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do
    TCleanGroupButton(FButtons.Last).Free;
end;

procedure TCleanCustomRadioGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TCleanCustomRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then FItemIndex := Value else
  begin
    HandleNeeded;
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        TCleanGroupButton(FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        TCleanGroupButton(FButtons[FItemIndex]).Checked := True;
    end;
  end;
end;

procedure TCleanCustomRadioGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TCleanCustomRadioGroup.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    UpdateButtons;
  end;
end;

procedure TCleanCustomRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
  begin
    TCleanGroupButton(FButtons[I]).Caption := FItems[I];
    TCleanGroupButton(FButtons[I]).WordWrap := FWordWrap;
  end;
  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    TCleanGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TCleanCustomRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    TCleanGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TCleanCustomRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TCleanCustomRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TCleanCustomRadioGroup.CanModify: Boolean;
begin
  Result := True;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TCleanCustomRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TCleanCustomRadioGroup.ResizeWidth;
const
  RADIO_BUTTON_SIZE = 16;
  RADIO_BUTTON_BUFFER = 10;
var
  i, w, maxW: Integer;
  c: Graphics.TBitmap;
begin
  maxW := 0;
  c := Graphics.TBitmap.Create;
  c.Canvas.Font.Assign(Font);
  for i := 0 to Items.Count - 1 do
  begin
    w := c.Canvas.TextWidth(Items[i]) + RADIO_BUTTON_SIZE + RADIO_BUTTON_BUFFER;
    if (w > maxW) then
    begin
      maxW := w;
    end;
  end;
  c.Free;
  Width := maxW * Columns;
end;

function TCleanCustomRadioGroup.GetButtons(Index: Integer): TRadioButton;
begin
  Result := TRadioButton(FButtons[Index]);
end;

end.
