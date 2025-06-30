program bencode_tests;

{$mode objfpc}{$H+}

uses
  SysUtils, bencode;

var
  TestCount, PassCount: Integer;

procedure Assert(Condition: Boolean; const Message: string);
begin
  Inc(TestCount);
  if Condition then
  begin
    Inc(PassCount);
    WriteLn('[PASS] ', Message);
  end
  else
  begin
    WriteLn('[FAIL] ', Message);
  end;
end;

procedure AssertException(const TestData: string; const Message: string);
var
  ExceptionCaught: Boolean;
  Node: TBencodeNode;
begin
  Inc(TestCount);
  ExceptionCaught := False;
  try
    Node := TBencodeCodec.Decode(TestData);
    try
      // If we get here, no exception was thrown
    finally
      FreeAndNil(Node);
    end;
  except
    ExceptionCaught := True;
  end;

  if ExceptionCaught then
  begin
    Inc(PassCount);
    WriteLn('[PASS] ', Message);
  end
  else
  begin
    WriteLn('[FAIL] ', Message);
  end;
end;

procedure AssertTypeException(Node: TBencodeNode; TestType: string; const Message: string);
var
  ExceptionCaught: Boolean;
begin
  Inc(TestCount);
  ExceptionCaught := False;
  try
    case TestType of
      'string': WriteLn(Node.AsString);
      'integer': WriteLn(Node.AsInteger);
      'list': Node.AsList;
      'dict': Node.AsDictionary;
    end;
  except
    ExceptionCaught := True;
  end;

  if ExceptionCaught then
  begin
    Inc(PassCount);
    WriteLn('[PASS] ', Message);
  end
  else
  begin
    WriteLn('[FAIL] ', Message);
  end;
end;

procedure TestStringEncoding;
var
  Node: TBencodeNode;
  Encoded: string;
begin
  WriteLn('=== String Encoding Tests ===');

  // Test examples from specification
  Node := BencodeString('spam');
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = '4:spam', 'String "spam" should encode to "4:spam"');
  finally
    FreeAndNil(Node);
  end;

  Node := BencodeString('');
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = '0:', 'Empty string should encode to "0:"');
  finally
    FreeAndNil(Node);
  end;

  // Test longer string
  Node := BencodeString('hello world');
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = '11:hello world', 'String "hello world" should encode correctly');
  finally
    FreeAndNil(Node);
  end;
end;

procedure TestStringDecoding;
var
  Node: TBencodeNode;
begin
  WriteLn;
  WriteLn('=== String Decoding Tests ===');

  // Test examples from specification
  Node := TBencodeCodec.Decode('4:spam');
  try
    Assert(Node.IsString, 'Decoded node should be string type');
    Assert(Node.AsString = 'spam', 'Decoded string should be "spam"');
  finally
    FreeAndNil(Node);
  end;

  Node := TBencodeCodec.Decode('0:');
  try
    Assert(Node.IsString, 'Decoded node should be string type');
    Assert(Node.AsString = '', 'Decoded string should be empty');
  finally
    FreeAndNil(Node);
  end;

  // Test invalid string encodings
  AssertException('05:hello', 'String with leading zero length should fail');
  AssertException('5:hell', 'String shorter than declared length should fail');
  AssertException('-1:', 'Negative string length should fail');
end;

procedure TestIntegerEncoding;
var
  Node: TBencodeNode;
  Encoded: string;
begin
  WriteLn;
  WriteLn('=== Integer Encoding Tests ===');

  // Test examples from specification
  Node := BencodeInteger(3);
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'i3e', 'Integer 3 should encode to "i3e"');
  finally
    FreeAndNil(Node);
  end;

  Node := BencodeInteger(-3);
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'i-3e', 'Integer -3 should encode to "i-3e"');
  finally
    FreeAndNil(Node);
  end;

  Node := BencodeInteger(0);
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'i0e', 'Integer 0 should encode to "i0e"');
  finally
    FreeAndNil(Node);
  end;

  // Test large integers
  Node := BencodeInteger(1234567890);
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'i1234567890e', 'Large integer should encode correctly');
  finally
    FreeAndNil(Node);
  end;
end;

procedure TestIntegerDecoding;
var
  Node: TBencodeNode;
begin
  WriteLn;
  WriteLn('=== Integer Decoding Tests ===');

  // Test examples from specification
  Node := TBencodeCodec.Decode('i3e');
  try
    Assert(Node.IsInteger, 'Decoded node should be integer type');
    Assert(Node.AsInteger = 3, 'Decoded integer should be 3');
  finally
    FreeAndNil(Node);
  end;

  Node := TBencodeCodec.Decode('i-3e');
  try
    Assert(Node.IsInteger, 'Decoded node should be integer type');
    Assert(Node.AsInteger = -3, 'Decoded integer should be -3');
  finally
    FreeAndNil(Node);
  end;

  Node := TBencodeCodec.Decode('i0e');
  try
    Assert(Node.IsInteger, 'Decoded node should be integer type');
    Assert(Node.AsInteger = 0, 'Decoded integer should be 0');
  finally
    FreeAndNil(Node);
  end;

  // Test invalid integer encodings
  AssertException('i-0e', 'Negative zero should fail');
  AssertException('i03e', 'Integer with leading zero should fail');
  AssertException('ie', 'Empty integer should fail');
  AssertException('i3', 'Unterminated integer should fail');
  AssertException('i3x4e', 'Integer with invalid character should fail');
end;

procedure TestListEncoding;
var
  Node: TBencodeNode;
  List: TBencodeList;
  Encoded: string;
begin
  WriteLn;
  WriteLn('=== List Encoding Tests ===');

  // Test empty list
  Node := BencodeList;
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'le', 'Empty list should encode to "le"');
  finally
    FreeAndNil(Node);
  end;

  // Test list from specification: l4:spam4:eggse
  Node := BencodeList;
  try
    List := Node.AsList;
    List.Add('spam');
    List.Add('eggs');

    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'l4:spam4:eggse', 'List ["spam", "eggs"] should encode correctly');
  finally
    FreeAndNil(Node);
  end;

  // Test mixed type list
  Node := BencodeList;
  try
    List := Node.AsList;
    List.Add('hello');
    List.Add(42);
    List.Add('world');

    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'l5:helloi42e5:worlde', 'Mixed type list should encode correctly');
  finally
    FreeAndNil(Node);
  end;
end;

procedure TestListDecoding;
var
  Node: TBencodeNode;
  List: TBencodeList;
begin
  WriteLn;
  WriteLn('=== List Decoding Tests ===');

  // Test empty list
  Node := TBencodeCodec.Decode('le');
  try
    Assert(Node.IsList, 'Decoded node should be list type');
    Assert(Node.Count = 0, 'Empty list should have 0 items');
  finally
    FreeAndNil(Node);
  end;

  // Test list from specification
  Node := TBencodeCodec.Decode('l4:spam4:eggse');
  try
    Assert(Node.IsList, 'Decoded node should be list type');
    Assert(Node.Count = 2, 'List should have 2 items');

    List := Node.AsList;
    Assert(List.Items[0].AsString = 'spam', 'First item should be "spam"');
    Assert(List.Items[1].AsString = 'eggs', 'Second item should be "eggs"');
  finally
    FreeAndNil(Node);
  end;

  // Test invalid list encodings
  AssertException('l4:spam', 'Unterminated list should fail');
end;

procedure TestDictionaryEncoding;
var
  Node: TBencodeNode;
  Dict: TBencodeDictionary;
  Encoded: string;
begin
  WriteLn;
  WriteLn('=== Dictionary Encoding Tests ===');

  // Test empty dictionary
  Node := BencodeDictionary;
  try
    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'de', 'Empty dictionary should encode to "de"');
  finally
    FreeAndNil(Node);
  end;

  // Test dictionary from specification: d3:cow3:moo4:spam4:eggse
  Node := BencodeDictionary;
  try
    Dict := Node.AsDictionary;
    Dict.Add('cow', 'moo');
    Dict.Add('spam', 'eggs');

    Encoded := TBencodeCodec.Encode(Node);
    Assert(Encoded = 'd3:cow3:moo4:spam4:eggse', 'Dictionary should encode with sorted keys');
  finally
    FreeAndNil(Node);
  end;

  // Test dictionary with different value types
  Node := BencodeDictionary;
  try
    Dict := Node.AsDictionary;
    Dict.Add('number', 42);
    Dict.Add('string', 'hello');

    Encoded := TBencodeCodec.Encode(Node);
    // Keys should be sorted: "number" comes before "string"
    Assert(Encoded = 'd6:numberi42e6:string5:helloe', 'Dictionary with mixed types should encode correctly');
  finally
    FreeAndNil(Node);
  end;
end;

procedure TestDictionaryDecoding;
var
  Node: TBencodeNode;
  Dict: TBencodeDictionary;
begin
  WriteLn;
  WriteLn('=== Dictionary Decoding Tests ===');

  // Test empty dictionary
  Node := TBencodeCodec.Decode('de');
  try
    Assert(Node.IsDictionary, 'Decoded node should be dictionary type');
    Assert(Node.Count = 0, 'Empty dictionary should have 0 items');
  finally
    FreeAndNil(Node);
  end;

  // Test dictionary from specification
  Node := TBencodeCodec.Decode('d3:cow3:moo4:spam4:eggse');
  try
    Assert(Node.IsDictionary, 'Decoded node should be dictionary type');
    Assert(Node.Count = 2, 'Dictionary should have 2 items');

    Dict := Node.AsDictionary;
    Assert(Dict.ContainsKey('cow'), 'Dictionary should contain key "cow"');
    Assert(Dict.ContainsKey('spam'), 'Dictionary should contain key "spam"');
    Assert(Dict.Values['cow'].AsString = 'moo', 'Key "cow" should have value "moo"');
    Assert(Dict.Values['spam'].AsString = 'eggs', 'Key "spam" should have value "eggs"');
  finally
    FreeAndNil(Node);
  end;

  // Test invalid dictionary encodings
  AssertException('d4:spam', 'Unterminated dictionary should fail');
  AssertException('di42e4:spam4:eggse', 'Dictionary with non-string key should fail');

  // Test unsorted keys (this should fail according to spec)
  AssertException('d4:spam4:eggs3:cow3:mooe', 'Dictionary with unsorted keys should fail');
end;

procedure TestComplexStructures;
var
  Node: TBencodeNode;
  Dict: TBencodeDictionary;
  List: TBencodeList;
  Encoded, Expected: string;
begin
  WriteLn;
  WriteLn('=== Complex Structure Tests ===');

  // Test nested structures
  Node := BencodeDictionary;
  try
    Dict := Node.AsDictionary;

    // Add a list of dictionaries
    List := Dict.AddList('files').AsList;
    List.AddDictionary.AsDictionary.Add('name', 'file1.txt');
    List.Items[0].AsDictionary.Add('size', 1024);

    List.AddDictionary.AsDictionary.Add('name', 'file2.txt');
    List.Items[1].AsDictionary.Add('size', 2048);

    // Add nested dictionary
    Dict.AddDictionary('info').AsDictionary.Add('version', 1);

    Encoded := TBencodeCodec.Encode(Node);

    // Verify it can be decoded back
    FreeAndNil(Node);
    Node := TBencodeCodec.Decode(Encoded);

    Assert(Node.IsDictionary, 'Complex structure should decode as dictionary');
    Assert(Node.Values['files'].IsList, 'Files should be a list');
    Assert(Node.Values['files'].Count = 2, 'Files list should have 2 items');
    Assert(Node.Values['info'].IsDictionary, 'Info should be a dictionary');

  finally
    FreeAndNil(Node);
  end;
end;

procedure TestRoundTripEncoding;
var
  Original, Decoded: TBencodeNode;
  Encoded: string;
  TestCases: array[0..5] of string = (
    '4:spam',
    'i42e',
    'le',
    'de',
    'l4:spam4:eggse',
    'd3:cow3:moo4:spam4:eggse'
  );
  I: Integer;
begin
  WriteLn;
  WriteLn('=== Round-trip Encoding Tests ===');

  for I := 0 to High(TestCases) do
  begin
    Original := TBencodeCodec.Decode(TestCases[I]);
    try
      Encoded := TBencodeCodec.Encode(Original);
      Assert(Encoded = TestCases[I], Format('Round-trip test %d should preserve encoding', [I + 1]));

      // Double-check by decoding again
      Decoded := TBencodeCodec.Decode(Encoded);
      try
        // Just check that it decodes without error
        Assert(True, Format('Round-trip test %d should decode correctly', [I + 1]));
      finally
        FreeAndNil(Decoded);
      end;

    finally
      FreeAndNil(Original);
    end;
  end;

  // Test a more complex but valid structure
  WriteLn('Testing complex nested structure...');
  Original := BencodeDictionary;
  try
    with Original.AsDictionary do
    begin
      Add('announce', 'http://example.com');
      AddDictionary('info').AsDictionary.Add('name', 'test.txt');
      Values['info'].AsDictionary.Add('length', 1024);
    end;

    Encoded := TBencodeCodec.Encode(Original);
    Decoded := TBencodeCodec.Decode(Encoded);
    try
      Assert(Decoded.IsDictionary, 'Complex structure should be dictionary');
      Assert(Decoded.Values['announce'].AsString = 'http://example.com', 'Announce should match');
      Assert(Decoded.Values['info'].Values['name'].AsString = 'test.txt', 'Name should match');
      Assert(Decoded.Values['info'].Values['length'].AsInteger = 1024, 'Length should match');
    finally
      FreeAndNil(Decoded);
    end;

  finally
    FreeAndNil(Original);
  end;
end;

procedure TestErrorHandling;
begin
  WriteLn;
  WriteLn('=== Error Handling Tests ===');

  // Test various malformed inputs
  AssertException('', 'Empty input should fail');
  AssertException('x', 'Invalid character should fail');
  AssertException('4:spam extra', 'Extra data after structure should fail');
  AssertException('i', 'Incomplete integer should fail');
  AssertException('l', 'Incomplete list should fail');
  AssertException('d', 'Incomplete dictionary should fail');
end;

procedure TestTypeAccess;
var
  Node: TBencodeNode;
begin
  WriteLn;
  WriteLn('=== Type Access Tests ===');

  // Test string access on wrong type
  Node := BencodeInteger(42);
  try
    AssertTypeException(Node, 'string', 'Accessing string value on integer should fail');
  finally
    FreeAndNil(Node);
  end;

  // Test integer access on wrong type
  Node := BencodeString('hello');
  try
    AssertTypeException(Node, 'integer', 'Accessing integer value on string should fail');
  finally
    FreeAndNil(Node);
  end;

  // Test list access on wrong type
  Node := BencodeString('hello');
  try
    AssertTypeException(Node, 'list', 'Accessing list on string should fail');
  finally
    FreeAndNil(Node);
  end;

  // Test dictionary access on wrong type
  Node := BencodeList;
  try
    AssertTypeException(Node, 'dict', 'Accessing dictionary on list should fail');
  finally
    FreeAndNil(Node);
  end;
end;

procedure RunAllTests;
begin
  TestCount := 0;
  PassCount := 0;

  TestStringEncoding;
  TestStringDecoding;
  TestIntegerEncoding;
  TestIntegerDecoding;
  TestListEncoding;
  TestListDecoding;
  TestDictionaryEncoding;
  TestDictionaryDecoding;
  TestComplexStructures;
  TestRoundTripEncoding;
  TestErrorHandling;
  TestTypeAccess;

  WriteLn;
  WriteLn('=== Test Summary ===');
  WriteLn(Format('Passed: %d/%d tests', [PassCount, TestCount]));

  if PassCount = TestCount then
  begin
    WriteLn('SUCCESS: All tests passed!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('FAILURE: Some tests failed!');
    ExitCode := 1;
  end;
end;

begin
  try
    WriteLn('FreePascal Bencode Library Test Suite');
    WriteLn('====================================');

    RunAllTests;

  except
    on E: Exception do
    begin
      WriteLn('Unexpected error: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
