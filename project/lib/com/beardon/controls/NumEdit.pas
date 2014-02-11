unit NumEdit;

interface

uses
  Classes,
  Dialogs,
  StdCtrls,
  SysUtils,
  Windows;

type
  TMoney = Longint;

  TIntEdit = class(TEdit)
  private
    { Private declarations }
    function GetValue: Int64;
    procedure SetValue(Value: Int64);
  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value: Int64 read GetValue write SetValue;
  end;

 TFloatEdit = class(TEdit)
  private
    { Private declarations }
    function GetValue: Extended;
    procedure SetValue(Value: Extended);
  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value: Extended read GetValue write SetValue;
  end;

 TMoneyEdit = class(TEdit)
  private
    { Private declarations }
    FFormatStr: string;
    function GetValue: TMoney;
    procedure SetValue(Value: TMoney);
  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value: TMoney read GetValue write SetValue;
    property FormatStr: string read FFormatStr write FFormatStr;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Beardon', [TIntEdit, TFloatEdit, TMoneyEdit]);
end;

{****************** TIntEdit Methods *********************}
constructor TIntEdit.Create(AOwner: TComponent);
begin
  inherited;
  Text := '0';
  Width := 33;
end;

function TIntEdit.GetValue: Int64;
begin
  Result := StrToIntDef(Text, 0);
end;

procedure TIntEdit.SetValue(Value: Int64);
begin
  Text := IntToStr(Value);
end;

procedure TIntEdit.KeyPress(var Key: Char);
begin
  if (not CharInSet(Key, ['+', '-', '0'..'9', #8,#13])) then
  begin
    Key := #0;
    MessageBeep(MB_ICONEXCLAMATION);
  end
  else
    inherited KeyPress(Key);
end;

{***************** TFloatEdit Methods *******************}
constructor TFloatEdit.Create(AOwner: TComponent);
begin
  inherited;
  Text := '0.0';
  Width := 33;
end;

function TFloatEdit.GetValue: Extended;
begin
  try
    if (Text = '') then
      Text := '0';
    Result := StrToFloat(Text);
  except
    on E: EConvertError do
    begin
      ShowMessage(E.ClassName + #13 + E.Message);
      Result := 0;
    end;
  end;
end;

procedure TFloatEdit.SetValue(Value: Extended);
begin
  Text := FloatTostr(Value);
end;

procedure TFloatEdit.KeyPress(var Key: Char);
begin
  if (not CharInSet(Key, ['+', '-', FormatSettings.DecimalSeparator, '0'..'9', #0..#31]))
    or ((Key = FormatSettings.DecimalSeparator) and (Pos(FormatSettings.DecimalSeparator, Text) > 0))
  then
  begin
    Key := #0;
    MessageBeep(MB_ICONEXCLAMATION);
  end
  else
    inherited KeyPress(Key);
end;

{***************** TMoneyEdit Methods *******************}
constructor TMoneyEdit.Create(AOwner: TComponent);
begin
  inherited;
  Text := '0.00';
  Width := 33;
  FormatStr := '###0.00';
end;

function TMoneyEdit.GetValue: TMoney;
var
  wnum: Currency;
begin
  try
    if (Text = '') then
      Text:='0';
    wnum := StrToCurr(Text);
    wnum := (wnum * 100); // change to pennies
    GetValue := Round(wnum);
  except
    on E: EConvertError do
    begin
      ShowMessage(E.ClassName + #13 + E.Message);
      Result := 0;
    end;
  end;
end;

procedure TMoneyEdit.SetValue(Value: TMoney);
begin
  Text := FormatFloat(FormatStr, (Value / 100));
end;

procedure TMoneyEdit.KeyPress(var Key: Char);
const
  EDIT_KEYS = [#0..#31];
  GOOD_KEYS = ['-','0'..'9'];
var
  ok: Boolean;
  valids: TSysCharSet;
  place: Integer;
begin
  if (not CharInSet(key, EDIT_KEYS)) then
  begin
    { Only allow 1 DecimalSeparator }
    place := Pos(FormatSettings.DecimalSeparator,text);
    if (place > 0) then
      valids := GOOD_KEYS
    else
      valids := GOOD_KEYS + [FormatSettings.DecimalSeparator];
    ok := CharInSet(Key, valids);
    if (ok) then
    begin
      if (key = FormatSettings.DecimalSeparator) then
        ok := (SelStart >= (Length(Text) - 2))
      else
      begin
        if (place > 0) then // max is 2 digits to the left of the decimalSeparator
        begin
          if (Self.SelStart >= place) then  // appears to be 0 referenced
            ok := ((place + 2) > Length(Text));
        end;
      end;
    end;
    if (not OK) then
    begin
      Key := #0;
      MessageBeep(MB_ICONEXCLAMATION);
    end;
  end;
  inherited KeyPress(Key);
end;

end.
