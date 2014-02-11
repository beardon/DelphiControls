unit AutocompleteManager;

interface

uses
  Classes;

type
  TAutocompleteManager = class
  private
    FWords: TStrings;
    procedure AddWord(const AWord: string);
  public
    constructor Create;
    destructor Destroy; override;
    function IsRecognized(AWord: string): Boolean; overload;
    function IsRecognized(AWord: string; AWordList: TStrings): Boolean; overload;
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromStrings(const AStrings: TStrings);
    property Words: TStrings read FWords write FWords;
  end;

implementation

uses
  SysUtils;

constructor TAutocompleteManager.Create;
begin
  FWords := TStringList.Create;
  TStringList(FWords).Duplicates := dupIgnore;
end;

destructor TAutocompleteManager.Destroy;
begin
  FWords.Free;
  inherited;
end;

procedure TAutocompleteManager.AddWord(const AWord: string);
begin
  FWords.Add(UpperCase(AWord) + '=' + AWord);
end;

function TAutocompleteManager.IsRecognized(AWord: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  AWord := UpperCase(AWord);
  for i := 0 to FWords.Count-1 do
  begin
    Result := System.Pos(AWord, FWords.Names[i]) > 0;
    if Result then
      Break;
  end;
end;

function TAutocompleteManager.IsRecognized(AWord: string;
  AWordList: TStrings): Boolean;
var
  i: Integer;
begin
  Result := False;
  AWord := UpperCase(AWord);
  AWordList.Clear;
  for i := 0 to FWords.Count-1 do
  begin
    if System.Pos(AWord, FWords.Names[i]) > 0 then
    begin
      Result := True;
      AWordList.Add(FWords.ValueFromIndex[i]);
    end;
  end;
end;

procedure TAutocompleteManager.LoadFromFile(const AFilename: string);
var
  i: Integer;
  F: TStrings;
begin
  F := TStringList.Create;
  try
    F.LoadFromFile(AFilename);
    for i := 0 to F.Count-1 do
      AddWord(F[i]);
  finally
    F.Free;
  end;
end;

procedure TAutocompleteManager.LoadFromStrings(const AStrings: TStrings);
var
  i: Integer;
begin
  for i := 0 to AStrings.Count - 1 do
  begin
    AddWord(AStrings[i]);
  end;
end;

end.
