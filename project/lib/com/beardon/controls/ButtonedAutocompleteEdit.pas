unit ButtonedAutocompleteEdit;

interface

uses
  AutocompleteManager,
  Classes,
  ExtCtrls,
  Messages,
  StdCtrls;

const
  MSG_HIDEWORDLIST = WM_USER + 222;

type
  TButtonedAutocompleteEdit = class(TButtonedEdit)
  private
    FAutocompleteManager: TAutocompleteManager;
    FCaretPos: Integer;
    FWordList: TListBox;
    FWordListHeight: Integer;
    FWordListWidth: Integer;
    procedure HandleWordListLostFocus(ASender: TObject);
    procedure HandleWordListSelectItem(ASender: TObject);
    procedure HandleWordListKeyPress(Sender: TObject; var Key: Char);
    procedure HandleWordListKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleHideWordList(var AMsg); overload; message MSG_HIDEWORDLIST;
    procedure HandleHideWordList; overload;
    procedure SetWordListHeight(const Value: Integer);
    procedure SetWordListWidth(const Value: Integer);
    procedure SetWords(Words: TStrings);
    procedure RegainFocus;
  protected
    procedure ShowWordList(AWords: TStrings);
    procedure HideWordList;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Words: TStrings write SetWords;
  published
    property WordListHeight: Integer read FWordListHeight write SetWordListHeight;
    property WordListWidth: Integer read FWordListWidth write SetWordListWidth;
  end;

  procedure Register;

implementation

uses
  Controls,
  Windows;

procedure Register;
begin
  RegisterComponents('Beardon', [TButtonedAutocompleteEdit]);
end;

constructor TButtonedAutocompleteEdit.Create(AOwner: TComponent);
begin
  inherited;
  Parent := TWinControl(AOwner);
  FAutocompleteManager := TAutocompleteManager.Create;
  FWordListHeight := 60;
end;

destructor TButtonedAutocompleteEdit.Destroy;
begin
  FAutocompleteManager.Free;
  inherited Destroy;
end;

procedure TButtonedAutocompleteEdit.Change;
var
  S: TStrings;
begin
  inherited;
  if (FAutocompleteManager.IsRecognized(Self.Text)) then
  begin
    S := TStringList.Create;
    try
      if (FAutocompleteManager.IsRecognized(Self.Text, S)) then
        ShowWordList(S);
    finally
      S.Free;
    end;
  end
  else
    HideWordList;
end;

procedure TButtonedAutocompleteEdit.HandleHideWordList(var AMsg);
begin
  HandleHideWordList;
end;

procedure TButtonedAutocompleteEdit.DoExit;
begin
  if Assigned(FWordList) and FWordList.Visible and not FWordList.Focused then
    HideWordList;
  inherited;
end;

procedure TButtonedAutocompleteEdit.HandleHideWordList;
begin
  FWordList.Free;
  FWordList := nil;
end;

procedure TButtonedAutocompleteEdit.HandleWordListKeyDown(ASender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_UP) and (FWordList.ItemIndex=0) then
    RegainFocus;
end;

procedure TButtonedAutocompleteEdit.HandleWordListKeyPress(Sender: TObject;
  var Key: Char);
begin
  case Key of
    #13: begin
           Key := #0;
           Self.Text := FWordList.Items[FWordList.ItemIndex];
           Self.SetFocus;
           Self.SelStart := Length(Self.Text);
           Self.SelLength := 0;
           HideWordList;
         end;
    #27: begin
           RegainFocus;
           HideWordList;
         end;
    else begin
      RegainFocus;
    end;
  end;
end;

procedure TButtonedAutocompleteEdit.HandleWordListLostFocus(ASender: TObject);
begin
  if not Self.Focused then
    HideWordList;
end;

procedure TButtonedAutocompleteEdit.HandleWordListSelectItem(ASender: TObject);
begin
  Self.Text := FWordList.Items[FWordList.ItemIndex];
  HideWordList;
end;

procedure TButtonedAutocompleteEdit.HideWordList;
begin
  PostMessage(Self.Handle, MSG_HIDEWORDLIST, 0, 0);
end;

procedure TButtonedAutocompleteEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    HideWordList
  else if (Key=VK_DOWN) and Assigned(FWordList) and FWordList.Visible then
  begin
    FCaretPos := Self.SelStart;
    FWordList.SetFocus;
    if FWordList.ItemIndex < 0 then
      FWordList.ItemIndex := 0;
  end
  else
    inherited;
end;

procedure TButtonedAutocompleteEdit.RegainFocus;
begin
  Self.SetFocus;
  Self.SelStart := FCaretPos;
  Self.SelLength := 0;
end;

procedure TButtonedAutocompleteEdit.SetWordListHeight(const Value: Integer);
begin
  if FWordListHeight <> Value then
  begin
    FWordListHeight := Value;
    if Assigned(FWordList) then
      FWordList.Height := FWordListHeight;
  end;
end;

procedure TButtonedAutocompleteEdit.SetWordListWidth(const Value: Integer);
begin
  if FWordListWidth <> Value then
  begin
    FWordListWidth := Value;
    if Assigned(FWordList) then
    begin
      if FWordListWidth < 1 then
        FWordList.Width := Self.Width
      else
        FWordList.Width := FWordListWidth;
    end;
  end;
end;

procedure TButtonedAutocompleteEdit.SetWords(Words: TStrings);
begin
  FAutocompleteManager.LoadFromStrings(Words);
end;

procedure TButtonedAutocompleteEdit.ShowWordList(AWords: TStrings);
begin
  if FWordList=nil then
  begin
    FWordList := TListBox.Create(Self);
//    FWordList.ParentCtl3D := False;
//    FWordList.Ctl3D := False;
    FWordList.Parent := Self.Parent;
    FWordList.TabStop := False;
    FWordList.OnExit := HandleWordListLostFocus;
    FWordList.OnKeyPress := HandleWordListKeyPress;
    FWordList.OnKeyDown := HandleWordListKeyDown;
  end;

  FWordList.Items.Assign(AWords);
  if FWordListWidth < 1 then
    FWordList.SetBounds(Self.Left, Self.Top + Self.Height, Self.Width, FWordListHeight)
  else
    FWordList.SetBounds(Self.Left, Self.Top + Self.Height, FWordListWidth, FWordListHeight);

  FWordList.Show;
end;

end.
