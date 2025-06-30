# FreePascal Bencode Library

[![FreePascal](https://img.shields.io/badge/FreePascal-3.0%2B-blue.svg)](https://www.freepascal.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey.svg)]()

A comprehensive and robust **Bencode** encoding/decoding library for FreePascal and Lazarus. Bencode is the encoding format used by the BitTorrent protocol for storing and transmitting loosely structured data.

## What is Bencode?

Bencode (pronounced "*bee-encode*") is a simple data serialization format used by BitTorrent for .torrent files and peer communication. It supports four data types:

- **Strings**: `4:spam` (length:data)
- **Integers**: `i42e` (i + number + e)  
- **Lists**: `l4:spam4:eggse` (l + items + e)
- **Dictionaries**: `d3:cow3:moo4:spam4:eggse` (d + key-value pairs + e)

## Features

✅ **Full Bencode Specification Support** - Complete implementation of the BitTorrent bencode standard  
✅ **Type-Safe API** - Comprehensive error handling with custom exception classes  
✅ **Memory Safe** - Automatic resource management with proper destructors  
✅ **Stream & File I/O** - Support for encoding/decoding from strings, streams, and files  
✅ **Dictionary Key Validation** - Automatic key sorting and validation per specification  
✅ **Large File Support** - 64-bit integer support for files >4GB  
✅ **Cross-Platform** - Works on Windows, Linux, and macOS  
✅ **Well Tested** - Comprehensive test suite with 60+ test cases  

## Installation

### Requirements
- FreePascal 3.0 or later
- Lazarus (optional, for IDE support)

### Setup
1. Download `bencode.pas` from this repository
2. Copy it to your project directory or add to your library path
3. Add `bencode` to your `uses` clause

```pascal
uses
  SysUtils, Classes, bencode;
```

## Quick Start

### Basic Usage

```pascal
program SimpleExample;
uses bencode;

var
  Node: TBencodeNode;
  Encoded: string;
begin
  // Create and encode a string
  Node := BencodeString('Hello, World!');
  try
    Encoded := TBencodeCodec.Encode(Node);
    WriteLn('Encoded: ', Encoded); // Output: 13:Hello, World!
  finally
    FreeAndNil(Node);
  end;
  
  // Decode it back
  Node := TBencodeCodec.Decode('13:Hello, World!');
  try
    WriteLn('Decoded: ', Node.AsString); // Output: Hello, World!
  finally
    FreeAndNil(Node);
  end;
end.
```

### Working with Dictionaries

```pascal
var
  Root: TBencodeNode;
  Dict: TBencodeDictionary;
begin
  // Create a dictionary structure
  Root := BencodeDictionary;
  try
    Dict := Root.AsDictionary;
    
    // Add various data types
    Dict.Add('name', 'My Torrent');
    Dict.Add('size', 1048576);
    Dict.Add('private', 1);
    
    // Add nested structures
    Dict.AddList('files').AsList.Add('file1.txt');
    Dict.Values['files'].AsList.Add('file2.txt');
    
    // Encode the entire structure
    WriteLn(TBencodeCodec.Encode(Root));
    
  finally
    FreeAndNil(Root);
  end;
end;
```

### Reading a Torrent File

```pascal
var
  Torrent: TBencodeNode;
  Info: TBencodeDictionary;
begin
  // Load and parse a .torrent file
  Torrent := TBencodeCodec.DecodeFromFile('example.torrent');
  try
    WriteLn('Announce URL: ', Torrent.Values['announce'].AsString);
    
    if Torrent.Values['info'].IsDictionary then
    begin
      Info := Torrent.Values['info'].AsDictionary;
      WriteLn('Name: ', Info.Values['name'].AsString);
      
      if Info.ContainsKey('length') then
        WriteLn('Size: ', Info.Values['length'].AsInteger, ' bytes');
    end;
    
  finally
    FreeAndNil(Torrent);
  end;
end;
```

### Creating a Simple Torrent

```pascal
var
  Torrent: TBencodeNode;
  Dict, Info: TBencodeDictionary;
begin
  Torrent := BencodeDictionary;
  try
    Dict := Torrent.AsDictionary;
    
    // Basic torrent metadata
    Dict.Add('announce', 'http://tracker.example.com/announce');
    Dict.Add('comment', 'Created with FreePascal Bencode Library');
    Dict.Add('creation date', DateTimeToUnix(Now));
    
    // File information
    Info := Dict.AddDictionary('info').AsDictionary;
    Info.Add('name', 'example.txt');
    Info.Add('length', 12345);
    Info.Add('piece length', 32768);
    Info.Add('pieces', 'dummy_hash_data_goes_here');
    
    // Save the torrent file
    TBencodeCodec.EncodeToFile(Torrent, 'created.torrent');
    WriteLn('Torrent file created successfully!');
    
  finally
    FreeAndNil(Torrent);
  end;
end;
```

## API Reference

### Core Classes

#### `TBencodeNode` - Main Data Container
```pascal
// Type checking
function IsString/IsInteger/IsList/IsDictionary: Boolean;

// Value access
property AsString: string;
property AsInteger: Int64;
property AsList: TBencodeList;
property AsDictionary: TBencodeDictionary;

// Collection access
property Count: Integer;
property Items[Index: Integer]: TBencodeNode;      // For lists
property Values[Key: string]: TBencodeNode;        // For dictionaries

// Utility
function Clone: TBencodeNode;
procedure Dump(Indent: Integer = 0);
```

#### `TBencodeCodec` - Encoding/Decoding Engine
```pascal
// Decoding
class function Decode(const Data: string): TBencodeNode;
class function Decode(Stream: TStream): TBencodeNode;
class function DecodeFromFile(const FileName: string): TBencodeNode;

// Encoding  
class function Encode(Node: TBencodeNode): string;
class procedure Encode(Node: TBencodeNode; Stream: TStream);
class procedure EncodeToFile(Node: TBencodeNode; const FileName: string);

// Validation
class function IsValidBencode(const Data: string): Boolean;
```

#### Helper Classes
- **`TBencodeList`** - Easy list manipulation with `Add()`, `Delete()`, `Clear()` methods
- **`TBencodeDictionary`** - Dictionary operations with `Add()`, `Remove()`, `ContainsKey()` methods

### Convenience Functions
```pascal
function BencodeString(const Value: string): TBencodeNode;
function BencodeInteger(const Value: Int64): TBencodeNode;  
function BencodeList: TBencodeNode;
function BencodeDictionary: TBencodeNode;
```

### Exception Handling
```pascal
try
  Node := TBencodeCodec.Decode(SomeData);
  // Use node...
except
  on E: EBencodeParseError do
    WriteLn('Parse error: ', E.Message);
  on E: EBencodeTypeError do  
    WriteLn('Type mismatch: ', E.Message);
  on E: EBencodeKeyError do
    WriteLn('Key not found: ', E.Message);
end;
```

## Testing

Run the comprehensive test suite to verify the implementation:

```bash
fpc bencode_tests.pas
./bencode_tests

# Expected output:
# FreePascal Bencode Library Test Suite
# ====================================
# ...
# Passed: 79/79 tests
# SUCCESS: All tests passed!
```
## Specification Compliance

This library implements the complete Bencode specification as used by BitTorrent:

- ✅ String encoding with length prefixes
- ✅ Integer encoding with proper validation (no leading zeros, no negative zero)
- ✅ List encoding with arbitrary nesting
- ✅ Dictionary encoding with mandatory key sorting
- ✅ Comprehensive error detection for malformed data
- ✅ Support for binary data in strings

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

**Made with ❤️ for the FreePascal community**

If you find this library useful, please give it a ⭐ star on GitHub!