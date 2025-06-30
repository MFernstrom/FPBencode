{
  Copyright 2025 - Marcus Fernstrom
  MIT License - See license file for details
  Version 1.0
}
unit bencode;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections;

type
  // Bencode data types
  TBencodeType = (btString, btInteger, btList, btDictionary);

  // Forward declarations
  TBencodeNode = class;
  TBencodeList = class;
  TBencodeDictionary = class;

  // Exception classes
  EBencodeError = class(Exception);
  EBencodeParseError = class(EBencodeError);
  EBencodeTypeError = class(EBencodeError);
  EBencodeKeyError = class(EBencodeError);

  // List and dictionary type definitions
  TBencodeNodeList = {$ifdef FPC}specialize{$endif} TObjectList<TBencodeNode>;

  // Dictionary pair record
  TBencodeDictPair = record
    Key: string;
    Value: TBencodeNode;
  end;

  // Use array for dictionary pairs (more compatible than generic list for records)
  TBencodeDictPairArray = array of TBencodeDictPair;

  // Main bencode node class
  TBencodeNode = class
  private
    FNodeType: TBencodeType;
    FStringValue: string;
    FIntegerValue: Int64;
    FListValue: TBencodeNodeList;
    FDictValue: TBencodeDictPairArray;

    procedure SetAsString(const Value: string);
    procedure SetAsInteger(const Value: Int64);
    function GetAsString: string;
    function GetAsInteger: Int64;
    function GetAsList: TBencodeList;
    function GetAsDictionary: TBencodeDictionary;
    function GetCount: Integer;
    function GetListItem(Index: Integer): TBencodeNode;
    function GetDictItem(const Key: string): TBencodeNode;
    procedure SetListItem(Index: Integer; const Value: TBencodeNode);
    procedure SetDictItem(const Key: string; const Value: TBencodeNode);

  public
    constructor Create(NodeType: TBencodeType = btString);
    destructor Destroy; override;

    // Type checking
    property NodeType: TBencodeType read FNodeType;
    function IsString: Boolean;
    function IsInteger: Boolean;
    function IsList: Boolean;
    function IsDictionary: Boolean;

    // Value access
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsList: TBencodeList read GetAsList;
    property AsDictionary: TBencodeDictionary read GetAsDictionary;

    // Collection access
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TBencodeNode read GetListItem write SetListItem; default;
    property Values[const Key: string]: TBencodeNode read GetDictItem write SetDictItem;

    // Utility methods
    function Clone: TBencodeNode;
    function ToString: string; override;
    procedure Dump(Indent: Integer = 0);
  end;

  // Helper classes for easier list manipulation
  TBencodeList = class
  private
    FNode: TBencodeNode;
    function GetItem(Index: Integer): TBencodeNode;
    procedure SetItem(Index: Integer; const Value: TBencodeNode);
  public
    constructor Create(Node: TBencodeNode);

    function Add(Node: TBencodeNode): Integer; overload;
    function Add(const Value: string): Integer; overload;
    function Add(const Value: Int64): Integer; overload;
    function AddList: TBencodeNode;
    function AddDictionary: TBencodeNode;

    procedure Delete(Index: Integer);
    procedure Clear;
    function Count: Integer;

    property Items[Index: Integer]: TBencodeNode read GetItem write SetItem; default;
  end;

  // Helper class for easier dictionary manipulation
  TBencodeDictionary = class
  private
    FNode: TBencodeNode;
    function GetValue(const Key: string): TBencodeNode;
    procedure SetValue(const Key: string; const Value: TBencodeNode);
  public
    constructor Create(Node: TBencodeNode);

    procedure Add(const Key: string; Node: TBencodeNode); overload;
    procedure Add(const Key: string; const Value: string); overload;
    procedure Add(const Key: string; const Value: Int64); overload;
    function AddList(const Key: string): TBencodeNode;
    function AddDictionary(const Key: string): TBencodeNode;

    function ContainsKey(const Key: string): Boolean;
    procedure Remove(const Key: string);
    procedure Clear;
    function Count: Integer;
    function Keys: TStringArray;

    property Values[const Key: string]: TBencodeNode read GetValue write SetValue; default;
  end;

  // Main encoder/decoder class
  TBencodeCodec = class
  private
    class function ParseInteger(const Data: string; var Position: Integer): Int64;
    class function ParseString(const Data: string; var Position: Integer): string;
    class function ParseNode(const Data: string; var Position: Integer; Depth: Integer = 0): TBencodeNode;
    class procedure EncodeNode(Node: TBencodeNode; Stream: TStream);

  public
    // Decoding
    class function Decode(const Data: string): TBencodeNode; overload;
    class function Decode(Stream: TStream): TBencodeNode; overload;
    class function DecodeFromFile(const FileName: string): TBencodeNode;

    // Encoding
    class function Encode(Node: TBencodeNode): string; overload;
    class procedure Encode(Node: TBencodeNode; Stream: TStream); overload;
    class procedure EncodeToFile(Node: TBencodeNode; const FileName: string);

    // Utility
    class function IsValidBencode(const Data: string): Boolean;
  end;

// Convenience functions
function BencodeString(const Value: string): TBencodeNode;
function BencodeInteger(const Value: Int64): TBencodeNode;
function BencodeList: TBencodeNode;
function BencodeDictionary: TBencodeNode;

// Helper function for stream reading
function BytesToString(const Buffer; Size: Integer): string;

implementation

const
  MAX_PARSE_DEPTH = 100; // Prevent stack overflow

// Helper function for stream reading
function BytesToString(const Buffer; Size: Integer): string;
var
  P: PByte;
  I: Integer;
begin
  SetLength(Result, Size);
  P := @Buffer;
  for I := 1 to Size do
  begin
    Result[I] := Chr(P^);
    Inc(P);
  end;
end;

{ TBencodeNode }

constructor TBencodeNode.Create(NodeType: TBencodeType);
begin
  inherited Create;
  FNodeType := NodeType;
  FStringValue := '';
  FIntegerValue := 0;
  FListValue := nil;
  SetLength(FDictValue, 0);

  case FNodeType of
    btList: FListValue := TBencodeNodeList.Create(True);
    btDictionary: ; // FDictValue already initialized as empty array
  end;
end;

destructor TBencodeNode.Destroy;
var
  I: Integer;
begin
  if Assigned(FListValue) then
    FreeAndNil(FListValue);

  // Free all dictionary values
  for I := 0 to Length(FDictValue) - 1 do
    FreeAndNil(FDictValue[I].Value);
  SetLength(FDictValue, 0);

  inherited Destroy;
end;

function TBencodeNode.IsString: Boolean;
begin
  Result := FNodeType = btString;
end;

function TBencodeNode.IsInteger: Boolean;
begin
  Result := FNodeType = btInteger;
end;

function TBencodeNode.IsList: Boolean;
begin
  Result := FNodeType = btList;
end;

function TBencodeNode.IsDictionary: Boolean;
begin
  Result := FNodeType = btDictionary;
end;

procedure TBencodeNode.SetAsString(const Value: string);
begin
  if FNodeType <> btString then
    raise EBencodeTypeError.Create('Node is not a string type');
  FStringValue := Value;
end;

procedure TBencodeNode.SetAsInteger(const Value: Int64);
begin
  if FNodeType <> btInteger then
    raise EBencodeTypeError.Create('Node is not an integer type');
  FIntegerValue := Value;
end;

function TBencodeNode.GetAsString: string;
begin
  if FNodeType <> btString then
    raise EBencodeTypeError.Create('Node is not a string type');
  Result := FStringValue;
end;

function TBencodeNode.GetAsInteger: Int64;
begin
  if FNodeType <> btInteger then
    raise EBencodeTypeError.Create('Node is not an integer type');
  Result := FIntegerValue;
end;

function TBencodeNode.GetAsList: TBencodeList;
begin
  if FNodeType <> btList then
    raise EBencodeTypeError.Create('Node is not a list type');
  Result := TBencodeList.Create(Self);
end;

function TBencodeNode.GetAsDictionary: TBencodeDictionary;
begin
  if FNodeType <> btDictionary then
    raise EBencodeTypeError.Create('Node is not a dictionary type');
  Result := TBencodeDictionary.Create(Self);
end;

function TBencodeNode.GetCount: Integer;
begin
  case FNodeType of
    btList: Result := FListValue.Count;
    btDictionary: Result := Length(FDictValue);
  else
    Result := 0;
  end;
end;

function TBencodeNode.GetListItem(Index: Integer): TBencodeNode;
begin
  if FNodeType <> btList then
    raise EBencodeTypeError.Create('Node is not a list type');
  if (Index < 0) or (Index >= FListValue.Count) then
    raise EBencodeError.CreateFmt('List index %d out of bounds', [Index]);
  Result := FListValue[Index];
end;

function TBencodeNode.GetDictItem(const Key: string): TBencodeNode;
var
  I: Integer;
begin
  if FNodeType <> btDictionary then
    raise EBencodeTypeError.Create('Node is not a dictionary type');

  for I := 0 to Length(FDictValue) - 1 do
    if FDictValue[I].Key = Key then
      Exit(FDictValue[I].Value);

  raise EBencodeKeyError.CreateFmt('Key "%s" not found in dictionary', [Key]);
end;

procedure TBencodeNode.SetListItem(Index: Integer; const Value: TBencodeNode);
begin
  if FNodeType <> btList then
    raise EBencodeTypeError.Create('Node is not a list type');
  if (Index < 0) or (Index >= FListValue.Count) then
    raise EBencodeError.CreateFmt('List index %d out of bounds', [Index]);
  FListValue[Index] := Value;
end;

procedure TBencodeNode.SetDictItem(const Key: string; const Value: TBencodeNode);
var
  I, InsertPos: Integer;
  Found: Boolean;
begin
  if FNodeType <> btDictionary then
    raise EBencodeTypeError.Create('Node is not a dictionary type');

  Found := False;
  InsertPos := Length(FDictValue);

  // Check if key already exists
  for I := 0 to Length(FDictValue) - 1 do
    if FDictValue[I].Key = Key then
    begin
      FreeAndNil(FDictValue[I].Value);
      FDictValue[I].Value := Value;
      Found := True;
      Break;
    end;

  if not Found then
  begin
    // Find correct insertion position to maintain sorted order
    InsertPos := 0;
    for I := 0 to Length(FDictValue) - 1 do
      if CompareStr(Key, FDictValue[I].Key) > 0 then
        InsertPos := I + 1
      else
        Break;

    // Expand array
    SetLength(FDictValue, Length(FDictValue) + 1);

    // Shift elements to make room
    for I := Length(FDictValue) - 1 downto InsertPos + 1 do
      FDictValue[I] := FDictValue[I - 1];

    // Insert new pair
    FDictValue[InsertPos].Key := Key;
    FDictValue[InsertPos].Value := Value;
  end;
end;

function TBencodeNode.Clone: TBencodeNode;
var
  I: Integer;
begin
  Result := TBencodeNode.Create(FNodeType);

  case FNodeType of
    btString: Result.FStringValue := FStringValue;
    btInteger: Result.FIntegerValue := FIntegerValue;
    btList:
      for I := 0 to FListValue.Count - 1 do
        Result.FListValue.Add(FListValue[I].Clone);
    btDictionary:
      begin
        SetLength(Result.FDictValue, Length(FDictValue));
        for I := 0 to Length(FDictValue) - 1 do
        begin
          Result.FDictValue[I].Key := FDictValue[I].Key;
          Result.FDictValue[I].Value := FDictValue[I].Value.Clone;
        end;
      end;
  end;
end;

function TBencodeNode.ToString: string;
begin
  case FNodeType of
    btString: Result := Format('String("%s")', [FStringValue]);
    btInteger: Result := Format('Integer(%d)', [FIntegerValue]);
    btList: Result := Format('List[%d items]', [FListValue.Count]);
    btDictionary: Result := Format('Dictionary[%d items]', [Length(FDictValue)]);
  end;
end;

procedure TBencodeNode.Dump(Indent: Integer);
var
  I: Integer;
  Spaces: string;
begin
  Spaces := StringOfChar(' ', Indent);

  case FNodeType of
    btString: WriteLn(Spaces + 'String: "' + FStringValue + '"');
    btInteger: WriteLn(Spaces + 'Integer: ' + IntToStr(FIntegerValue));
    btList:
      begin
        WriteLn(Spaces + 'List[' + IntToStr(FListValue.Count) + ']:');
        for I := 0 to FListValue.Count - 1 do
        begin
          Write(Spaces + '  [' + IntToStr(I) + '] ');
          FListValue[I].Dump(Indent + 4);
        end;
      end;
    btDictionary:
      begin
        WriteLn(Spaces + 'Dictionary[' + IntToStr(Length(FDictValue)) + ']:');
        for I := 0 to Length(FDictValue) - 1 do
        begin
          WriteLn(Spaces + '  "' + FDictValue[I].Key + '": ');
          FDictValue[I].Value.Dump(Indent + 4);
        end;
      end;
  end;
end;

{ TBencodeList }

constructor TBencodeList.Create(Node: TBencodeNode);
begin
  inherited Create;
  if not Assigned(Node) or (Node.NodeType <> btList) then
    raise EBencodeTypeError.Create('Node must be a list type');
  FNode := Node;
end;

function TBencodeList.GetItem(Index: Integer): TBencodeNode;
begin
  Result := FNode.GetListItem(Index);
end;

procedure TBencodeList.SetItem(Index: Integer; const Value: TBencodeNode);
begin
  FNode.SetListItem(Index, Value);
end;

function TBencodeList.Add(Node: TBencodeNode): Integer;
begin
  Result := FNode.FListValue.Add(Node);
end;

function TBencodeList.Add(const Value: string): Integer;
var
  Node: TBencodeNode;
begin
  Node := BencodeString(Value);
  Result := Add(Node);
end;

function TBencodeList.Add(const Value: Int64): Integer;
var
  Node: TBencodeNode;
begin
  Node := BencodeInteger(Value);
  Result := Add(Node);
end;

function TBencodeList.AddList: TBencodeNode;
begin
  Result := BencodeList;
  Add(Result);
end;

function TBencodeList.AddDictionary: TBencodeNode;
begin
  Result := BencodeDictionary;
  Add(Result);
end;

procedure TBencodeList.Delete(Index: Integer);
begin
  FNode.FListValue.Delete(Index);
end;

procedure TBencodeList.Clear;
begin
  FNode.FListValue.Clear;
end;

function TBencodeList.Count: Integer;
begin
  Result := FNode.FListValue.Count;
end;

{ TBencodeDictionary }

constructor TBencodeDictionary.Create(Node: TBencodeNode);
begin
  inherited Create;
  if not Assigned(Node) or (Node.NodeType <> btDictionary) then
    raise EBencodeTypeError.Create('Node must be a dictionary type');
  FNode := Node;
end;

function TBencodeDictionary.GetValue(const Key: string): TBencodeNode;
begin
  Result := FNode.GetDictItem(Key);
end;

procedure TBencodeDictionary.SetValue(const Key: string; const Value: TBencodeNode);
begin
  FNode.SetDictItem(Key, Value);
end;

procedure TBencodeDictionary.Add(const Key: string; Node: TBencodeNode);
begin
  SetValue(Key, Node);
end;

procedure TBencodeDictionary.Add(const Key: string; const Value: string);
begin
  Add(Key, BencodeString(Value));
end;

procedure TBencodeDictionary.Add(const Key: string; const Value: Int64);
begin
  Add(Key, BencodeInteger(Value));
end;

function TBencodeDictionary.AddList(const Key: string): TBencodeNode;
begin
  Result := BencodeList;
  Add(Key, Result);
end;

function TBencodeDictionary.AddDictionary(const Key: string): TBencodeNode;
begin
  Result := BencodeDictionary;
  Add(Key, Result);
end;

function TBencodeDictionary.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FNode.FDictValue) - 1 do
    if FNode.FDictValue[I].Key = Key then
      Exit(True);
  Result := False;
end;

procedure TBencodeDictionary.Remove(const Key: string);
var
  I, J: Integer;
begin
  for I := 0 to Length(FNode.FDictValue) - 1 do
    if FNode.FDictValue[I].Key = Key then
    begin
      FreeAndNil(FNode.FDictValue[I].Value);
      // Shift elements down
      for J := I to Length(FNode.FDictValue) - 2 do
        FNode.FDictValue[J] := FNode.FDictValue[J + 1];
      // Shrink array
      SetLength(FNode.FDictValue, Length(FNode.FDictValue) - 1);
      Exit;
    end;
  raise EBencodeKeyError.CreateFmt('Key "%s" not found in dictionary', [Key]);
end;

procedure TBencodeDictionary.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(FNode.FDictValue) - 1 do
    FreeAndNil(FNode.FDictValue[I].Value);
  SetLength(FNode.FDictValue, 0);
end;

function TBencodeDictionary.Count: Integer;
begin
  Result := Length(FNode.FDictValue);
end;

function TBencodeDictionary.Keys: TStringArray;
var
  I: Integer;
begin
  SetLength(Result, Length(FNode.FDictValue));
  for I := 0 to Length(FNode.FDictValue) - 1 do
    Result[I] := FNode.FDictValue[I].Key;
end;

{ TBencodeCodec }

class function TBencodeCodec.ParseInteger(const Data: string; var Position: Integer): Int64;
var
  StartPos, EndPos: Integer;
  IntStr: string;
  HasMinus: Boolean;
begin
  if (Position > Length(Data)) or (Data[Position] <> 'i') then
    raise EBencodeParseError.Create('Expected "i" at start of integer');

  Inc(Position); // Skip 'i'
  StartPos := Position;
  HasMinus := False;

  // Check for negative sign
  if (Position <= Length(Data)) and (Data[Position] = '-') then
  begin
    HasMinus := True;
    Inc(Position);
  end;

  // Find end of integer
  while (Position <= Length(Data)) and (Data[Position] <> 'e') do
  begin
    if not (Data[Position] in ['0'..'9']) then
      raise EBencodeParseError.CreateFmt('Invalid character "%s" in integer at position %d', [Data[Position], Position]);
    Inc(Position);
  end;

  if Position > Length(Data) then
    raise EBencodeParseError.Create('Unterminated integer (missing "e")');

  EndPos := Position;
  Inc(Position); // Skip 'e'

  IntStr := Copy(Data, StartPos, EndPos - StartPos);

  // Validate integer format
  if IntStr = '' then
    raise EBencodeParseError.Create('Empty integer');
  if IntStr = '-' then
    raise EBencodeParseError.Create('Invalid integer "-"');
  if IntStr = '-0' then
    raise EBencodeParseError.Create('Invalid integer "-0"');
  if (Length(IntStr) > 1) and (IntStr[1] = '0') and not HasMinus then
    raise EBencodeParseError.Create('Invalid integer with leading zero');
  if (Length(IntStr) > 2) and (IntStr[1] = '-') and (IntStr[2] = '0') then
    raise EBencodeParseError.Create('Invalid negative integer with leading zero');

  if not TryStrToInt64(IntStr, Result) then
    raise EBencodeParseError.CreateFmt('Invalid integer "%s"', [IntStr]);
end;

class function TBencodeCodec.ParseString(const Data: string; var Position: Integer): string;
var
  LenStr: string;
  Len: Integer;
  StartPos: Integer;
begin
  StartPos := Position;

  // Parse length
  while (Position <= Length(Data)) and (Data[Position] in ['0'..'9']) do
  begin
    LenStr := LenStr + Data[Position];
    Inc(Position);
  end;

  if LenStr = '' then
    raise EBencodeParseError.Create('Expected string length');
  if (Position > Length(Data)) or (Data[Position] <> ':') then
    raise EBencodeParseError.Create('Expected ":" after string length');

  // Validate no leading zeros (except for "0")
  if (Length(LenStr) > 1) and (LenStr[1] = '0') then
    raise EBencodeParseError.Create('Invalid string length with leading zero');

  if not TryStrToInt(LenStr, Len) then
    raise EBencodeParseError.CreateFmt('Invalid string length "%s"', [LenStr]);
  if Len < 0 then
    raise EBencodeParseError.Create('Negative string length');

  Inc(Position); // Skip ':'

  if Position + Len - 1 > Length(Data) then
    raise EBencodeParseError.Create('String length exceeds available data');

  Result := Copy(Data, Position, Len);
  Inc(Position, Len);
end;

class function TBencodeCodec.ParseNode(const Data: string; var Position: Integer; Depth: Integer): TBencodeNode;
var
  Key: string;
  Value: TBencodeNode;
  LastKey: string;
begin
  if Depth > MAX_PARSE_DEPTH then
    raise EBencodeParseError.Create('Maximum parse depth exceeded');

  if Position > Length(Data) then
    raise EBencodeParseError.Create('Unexpected end of data');

  case Data[Position] of
    'i': // Integer
      begin
        Result := TBencodeNode.Create(btInteger);
        try
          Result.FIntegerValue := ParseInteger(Data, Position);
        except
          FreeAndNil(Result);
          raise;
        end;
      end;

    'l': // List
      begin
        Result := TBencodeNode.Create(btList);
        try
          Inc(Position); // Skip 'l'

          while (Position <= Length(Data)) and (Data[Position] <> 'e') do
          begin
            Value := ParseNode(Data, Position, Depth + 1);
            Result.FListValue.Add(Value);
          end;

          if Position > Length(Data) then
            raise EBencodeParseError.Create('Unterminated list (missing "e")');
          Inc(Position); // Skip 'e'
        except
          FreeAndNil(Result);
          raise;
        end;
      end;

    'd': // Dictionary
      begin
        Result := TBencodeNode.Create(btDictionary);
        try
          Inc(Position); // Skip 'd'
          LastKey := '';

          while (Position <= Length(Data)) and (Data[Position] <> 'e') do
          begin
            // Parse key (must be string)
            if not (Data[Position] in ['0'..'9']) then
              raise EBencodeParseError.Create('Dictionary key must be a string');
            Key := ParseString(Data, Position);

            // Check key ordering
            if (LastKey <> '') and (CompareStr(Key, LastKey) <= 0) then
              raise EBencodeParseError.Create('Dictionary keys must be sorted');
            LastKey := Key;

            // Parse value
            Value := ParseNode(Data, Position, Depth + 1);

            // Add to dictionary using SetDictItem which maintains order
            Result.Values[Key] := Value;
          end;

          if Position > Length(Data) then
            raise EBencodeParseError.Create('Unterminated dictionary (missing "e")');
          Inc(Position); // Skip 'e'
        except
          FreeAndNil(Result);
          raise;
        end;
      end;

    '0'..'9': // String
      begin
        Result := TBencodeNode.Create(btString);
        try
          Result.FStringValue := ParseString(Data, Position);
        except
          FreeAndNil(Result);
          raise;
        end;
      end;

  else
    raise EBencodeParseError.CreateFmt('Unexpected character "%s" at position %d', [Data[Position], Position]);
  end;
end;

class function TBencodeCodec.Decode(const Data: string): TBencodeNode;
var
  Position: Integer;
begin
  if Data = '' then
    raise EBencodeParseError.Create('Empty bencode data');

  Position := 1;
  Result := ParseNode(Data, Position);

  if Position <= Length(Data) then
    raise EBencodeParseError.Create('Extra data after bencode structure');
end;

class function TBencodeCodec.Decode(Stream: TStream): TBencodeNode;
var
  Data: string;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
begin
  Data := '';
  repeat
    BytesRead := Stream.Read(Buffer, SizeOf(Buffer));
    if BytesRead > 0 then
      Data := Data + BytesToString(Buffer, BytesRead);
  until BytesRead = 0;

  Result := Decode(Data);
end;

class function TBencodeCodec.DecodeFromFile(const FileName: string): TBencodeNode;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := Decode(FileStream);
  finally
    FreeAndNil(FileStream);
  end;
end;

class procedure TBencodeCodec.EncodeNode(Node: TBencodeNode; Stream: TStream);
var
  I: Integer;
  Temp: string;
begin
  case Node.NodeType of
    btString:
      begin
        Temp := IntToStr(Length(Node.FStringValue)) + ':' + Node.FStringValue;
        Stream.WriteBuffer(Temp[1], Length(Temp));
      end;

    btInteger:
      begin
        Temp := 'i' + IntToStr(Node.FIntegerValue) + 'e';
        Stream.WriteBuffer(Temp[1], Length(Temp));
      end;

    btList:
      begin
        Temp := 'l';
        Stream.WriteBuffer(Temp[1], Length(Temp));
        for I := 0 to Node.FListValue.Count - 1 do
          EncodeNode(Node.FListValue[I], Stream);
        Temp := 'e';
        Stream.WriteBuffer(Temp[1], Length(Temp));
      end;

    btDictionary:
      begin
        Temp := 'd';
        Stream.WriteBuffer(Temp[1], Length(Temp));
        for I := 0 to Length(Node.FDictValue) - 1 do
        begin
          // Encode key
          Temp := IntToStr(Length(Node.FDictValue[I].Key)) + ':' + Node.FDictValue[I].Key;
          Stream.WriteBuffer(Temp[1], Length(Temp));
          // Encode value
          EncodeNode(Node.FDictValue[I].Value, Stream);
        end;
        Temp := 'e';
        Stream.WriteBuffer(Temp[1], Length(Temp));
      end;
  end;
end;

class function TBencodeCodec.Encode(Node: TBencodeNode): string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    Encode(Node, Stream);
    Result := Stream.DataString;
  finally
    FreeAndNil(Stream);
  end;
end;

class procedure TBencodeCodec.Encode(Node: TBencodeNode; Stream: TStream);
begin
  EncodeNode(Node, Stream);
end;

class procedure TBencodeCodec.EncodeToFile(Node: TBencodeNode; const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    Encode(Node, FileStream);
  finally
    FreeAndNil(FileStream);
  end;
end;

class function TBencodeCodec.IsValidBencode(const Data: string): Boolean;
var
  Node: TBencodeNode;
begin
  try
    Node := Decode(Data);
    try
      Result := True;
    finally
      FreeAndNil(Node);
    end;
  except
    Result := False;
  end;
end;

// Convenience functions

function BencodeString(const Value: string): TBencodeNode;
begin
  Result := TBencodeNode.Create(btString);
  Result.AsString := Value;
end;

function BencodeInteger(const Value: Int64): TBencodeNode;
begin
  Result := TBencodeNode.Create(btInteger);
  Result.AsInteger := Value;
end;

function BencodeList: TBencodeNode;
begin
  Result := TBencodeNode.Create(btList);
end;

function BencodeDictionary: TBencodeNode;
begin
  Result := TBencodeNode.Create(btDictionary);
end;

end.
