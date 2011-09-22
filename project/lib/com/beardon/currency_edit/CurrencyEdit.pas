unit CurrencyEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TCurrencyEdit = class(TButtonedEdit)
  private
    fFormatStr: string;
    procedure addImage;
    function getValue: Extended;
    procedure setValue(value: Extended);
  protected

    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Value: Extended read getValue write setValue;
    property FormatStr: string read fFormatStr write fFormatStr;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Images;
    property ImeMode;
    property ImeName;
    property LeftButton;
    property MaxLength;
    property OEMConvert;
    property NumbersOnly;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property RightButton;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnGesture;
    property OnLeftButtonClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnRightButtonClick;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  pngimage;

{$R BeardonControls.dres}

procedure Register;
begin
  RegisterComponents('Beardon', [TCurrencyEdit]);
end;

constructor TCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  Text := '0.00';
  Width := 50;
  FormatStr := '"$"###0.00';
  addImage;
end;

destructor TCurrencyEdit.Destroy;
begin
  Images.Free;
  inherited Destroy;
end;

procedure TCurrencyEdit.addImage;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.LoadFromResourceName(hInstance, 'CurrencyBMP');
    Images := TImageList.Create(Self);
    Images.Add(bmp, nil);
    LeftButton.DisabledImageIndex := 0;
    LeftButton.HotImageIndex := 0;
    LeftButton.ImageIndex := 0;
    LeftButton.PressedImageIndex := 0;
    LeftButton.Visible := True;
  finally
    bmp.Free;
  end;
end;

function TCurrencyEdit.getValue: Extended;
var
  wnum: Currency;
begin
  try
    if (Text = '') then
    begin
      Text := '0.00';
    end;
    wnum := StrToCurr(Text);
    Result := Round(wnum);
  except
    on E: EConvertError do
    begin
      ShowMessage(E.ClassName + #13 + E.Message);
      Result := 0;
    end;
  end;
end;

procedure TCurrencyEdit.setValue(value: Extended);
begin
  Text := FormatFloat(FormatStr, value);
end;

procedure TCurrencyEdit.KeyPress(var Key: Char);
const
  EDIT_KEYS  = [#0..#31];
  GOOD_KEYS  = ['0'..'9'];
var
  valid: Boolean;
  place: Integer;
begin
  if not CharInSet(Key, EDIT_KEYS) then
  begin
    { Only allow 1 DecimalSeparator }
    place := Pos(FormatSettings.DecimalSeparator, Text);
    if (place > 0) then
      valid := CharInSet(Key, GOOD_KEYS)
    else
      valid := CharInSet(Key, GOOD_KEYS + [FormatSettings.DecimalSeparator]);
    if (valid) then
    begin
      if (key = FormatSettings.DecimalSeparator) then
      begin
        valid := (SelStart >= (Length(Text) - 2));
      end
      else
      begin
        if (place > 0) then
        begin
          {Max is 2 digits to the left of the decimalSeparator}
          if (Self.SelStart >= place) then  //appears to be 0 referenced
            valid := ((place + 2) > Length(Text));
        end;
      end;
    end;
    if (not valid) then
    begin
      Key := #0;
      MessageBeep(MB_ICONEXCLAMATION);
    end;
  end;

  inherited KeyPress(Key);
end;

end.
