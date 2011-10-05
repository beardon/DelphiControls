unit NumEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMoney   = Longint;
  TIntEdit = class(TEdit)
  private
    { Private declarations }
    function getvalue:int64;
    procedure setvalue(NewVal:Int64);
  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value: Int64 read GetValue write SetValue;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

 TFloatEdit = class(TEdit)
  private
    { Private declarations }
    function getvalue:Extended;
    procedure setvalue(NewVal:Extended);
  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value: Extended read GetValue write SetValue;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

 TMoneyEdit = class(TEdit)
  private
    { Private declarations }
    fFormatStr  : string;
    function getvalue: Tmoney;
    procedure setvalue(NewVal:Tmoney);
  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value: TMoney read GetValue write SetValue;
    property FormatStr : string read fFormatStr write fFormatStr;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Beardon', [TIntEdit, TFloatEdit, TMoneyEdit]);
end;


{****************** TIntEdit Mehtods *********************}
constructor TIntEdit.create(Aowner:TComponent);
begin
  inherited;
  text:='0';
  width:=33;
end;

function TIntEdit.getvalue:int64;
begin
  result:=StrToIntDef(text,0);
end;

procedure TIntEdit.Setvalue(Newval:int64);
begin
  text:=inttostr(newval);
end;

procedure TIntEdit.KeyPress(var Key: Char);
begin
  if not ( Key in ['+', '-', '0'..'9', #8,#13] ) then
  begin
    Key := #0;
    MessageBeep(MB_ICONEXCLAMATION);
  end
  else inherited KeyPress(Key);
end;

{***************** TFloatEdit Methods *******************}
constructor TFloatEdit.create(Aowner:TComponent);
begin
  inherited;
  text:='0.0';
  width:=33;
end;

function TFloatEdit.getvalue:Extended;
begin
  try
    if text='' then text:='0';
    result:=StrToFloat(text);
  except
    on E: EConvertError do
    begin
      ShowMessage(E.ClassName + #13 + E.Message);
      result:=0;
    end;
  end;
end;

procedure TFloatEdit.Setvalue(Newval:extended);
begin
  text:=FloatTostr(newval);
end;

procedure TFloatEdit.KeyPress(var Key: Char);
begin
  if not (Key in ['+', '-', DecimalSeparator, '0'..'9', #0..#31] )
    or ((key = decimalseparator) and (pos(decimalseparator,text)>0))
  then
  begin
    Key := #0;
    MessageBeep(MB_ICONEXCLAMATION);
  end
  else inherited KeyPress(Key);
end;


{***************** TMoneyEdit Methods *******************}
constructor TMoneyEdit.create(Aowner:TComponent);
begin
  inherited;
  text:='0.0';
  width:=33;
  FormatStr := '###0.00';
end;

function TMoneyEdit.getvalue:TMoney;
var
   wnum   :  currency;
begin
  try
    if text='' then text:='0';
    wnum := StrToCurr(text);
    wnum := (wnum * 100);  // change to pennies;
    GetValue := round(wnum);
  except
    on E: EConvertError do
    begin
      ShowMessage(E.ClassName + #13 + E.Message);
      result:=0;
    end;
  end;
end;

procedure TMoneyEdit.Setvalue(Newval:TMoney);
begin
  text:=FormatFloat(FormatStr,(newval / 100));
end;

procedure TMoneyEdit.KeyPress(var Key: Char);
const
   editKeys  = [#0..#31];
   GoodKeys  = ['-','0'..'9'];
var
   OK     : boolean;
   valids : set of char;
   place  : integer;
begin
   if not (key in EditKeys) then
   begin
      { Only allow 1 DecimalSeparator }
      place := pos(DecimalSeparator,text);
      if (place > 0) then
         valids := GoodKeys
      else
         valids := GoodKeys + [DecimalSeparator];

      OK := (Key in valids);
      if (OK) then
      begin
         if (key = DecimalSeparator) then
         begin
            ok := (SelStart >= (length(text) - 2));
         end
         else
         begin
            if (place > 0) then
            begin
               {Max is 2 digits to the left of the decimalSeparator}
               if (Self.SelStart >= place) then  //appears to be 0 referenced
                  ok := ((place + 2) > length(text));
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
