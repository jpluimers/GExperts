(*
This software is licensed according to the "Modified BSD License",
where the following substitutions are made in the license template:
  <OWNER> = Karl Waclawek
  <ORGANIZATION> = Karl Waclawek
  <YEAR> = 1998 - 2004
It can be obtained from http://opensource.org/licenses/bsd-license.html.
*)

{                            Classes

  TBufferStream:        A memory stream whose memory buffer can be set externally
  TWideStringStream:    A TStream interface into WideStrings
  TFastStringStream:    An improved TStringStream
  TFilter:              Base class for implementing data transformations
  TFilterWriter/Reader: Implements buffer management for writing to or
                        reading from a TFilter instance
  TBufferedCopyFilter:  TFilter that just copies the data - can be used
                        for buffered reading/writing
  TByteSwapFilter:      TFilter for byte swapping of words
  TByteToWordFilter:    TFilter for expanding bytes to words
  TBase64Encoder,
  TBase64Decoder:       TFilters for Base64 en/decoding
  TStreamReader:        IReader interface for TStream
  TStreamWriter:        IWriter interface for TStream
  TBufferReader:        IReader interface for a fixed size buffer
  TBufferWriter:        IWriter interface for a fixed size buffer
  TSearchList:          Adds a binary search function to TList
  TStack:               An array based stack implementation
  TStringTable,
  TWideStringTable:     Hash tables that can be used for string "interning"

                            Routines

  ApplyFilter:          Helper routine to simplify stream filtering
}

unit KDSClasses;

{$I KDS.inc}

interface

uses
  Classes, Sysutils, KDSGenTypes;

resourcestring
  SCannotReadOddPos = 'Cannot read WideString from odd byte position';
  SCannotWriteOddPos = 'Cannot write WideString to odd byte position';
  SOddSizeInvalid = 'Odd size not valid for WideString';
  SNegativeSizeInvalid = 'Negative stream size invalid';
  SNegativeCapacityInvalid = 'Negative stream capacity invalid';
  SOddPosInvalid = 'Odd byte position not valid for WideString';
  SStringPositionInvalid = 'String position invalid';
  SCapacityLessSize = 'Capacity cannot be less than size';
  STargetNil = 'Target must not be nil';
  // STargetClosed = 'Target closed';
  SBufferSizeNotPositive = 'Buffer size must be positive';
  SReadBufferHasData = 'Read buffer has unprocessed data';
  SInvalidProcessInBuffer = 'Invalid call to ProcessInBuffer()';
  SInvalidProcessOutBuffer = 'Invalid call to ProcessOutBuffer()';
  SRecursiveRead = 'Recursive call to Read()';
  SRecursiveWrite = 'Recursive call to Write()';
  SStackCapacityError = 'Invalid Capacity for Stack: %d';
  SSwapDistanceError = 'Invalid Swap Distance: %d';
  SRotateDistanceError = 'Invalid Rotate Distance: %d';
  SInvalidBase64Encoding = 'Invalid Base64 encoding: %s';
  SBase64EndDetected = 'End of Base64 conversion detected';

  SWriterNotOpen = 'Writer not open';
  SReaderNotOpen = 'Reader not open';
  SReaderClosed = 'Reader closed';
  SFilterOpen = 'Filter open';
  SFilterNotOpen = 'Filter not open';
  SFilterClosed = 'Filter closed';
  SFilterClosing = 'Filter closing';
  SFilterNotClosed = 'Filter not closed';
  SFilterNil = 'Filter must not be nil';
  SFilterProcessorNil = 'Filter processor must not be nil';
  SFilterDriverNil = 'Filter driver must not be nil';
  SInvalidFilterInBuffer = 'Invalid call to ProcessInBuffer()';
  SInvalidFilterOutBuffer = 'Invalid call to ProcessOutBuffer()';
  SExtraDataFound = 'Extra data found';
  SUnexpectedFilterState = 'Unexpected filter state';
  SProcessorArgs = 'Invalid processor arguments';
  SStreamNil = 'Stream must not be nil';
  SWriterError = 'Writer error';
  SInvalidBuffer = 'Invalid buffer';
  SIOObjectNotReset = 'IO object not reset';
  SUnprocessedInput = 'Unprocessed input: %d bytes';

  SStringTableTooSmall = 'String table capacity too small';

const
  MaxWideStringPosition = MaxInt shr 1;
  MaxStringPosition = MaxInt;
  MaxStackSize = MaxInt div (4 * SizeOf(Pointer));
  DefaultFilterBufferSize = 16384;

type
  { used to access a memory buffer, does NOT reallocate or free the buffer,
    is therefore NOT a RESIZABLE stream                                     }
  TBufferStream = class(TCustomMemoryStream)
    public
      procedure SetBuffer(Buffer: Pointer; Size: Longint); virtual;
      function Write(const Buffer; Count: Longint): Longint; override;
    end;

  TWideStringStream = class(TStream)
    private
      FDataP: PChar;
      FSize: Longint;
      FCapacity: Longint;
      FPosition: Longint;
      procedure InternalSetCapacity(NewCapacity: Longint);
      procedure InternalGrowCapacity(Needed: Longint);
      procedure InternalSetSize(NewSize: Longint);
      function GetData: PWideChar;
    protected
      // exposes internal data buffer - use with care
      property Data: PWideChar read GetData;
      procedure SetCapacity(NewCapacity: Longint); virtual;
      procedure SetSize(NewSize: Longint); override;
      procedure SetStringPosition(Value: Longint);
      function GetStringPosition: Longint;
      procedure SetStringLength(Value: Longint);
      function GetStringLength: Longint;
      function GetDataString: WideString;
    public
      constructor Create(const AString: WideString); overload;
      destructor Destroy; override;
      function Read(var Buffer; Count: Longint): Longint; override;
      function ReadString(Count: Longint): WideString;
      function Seek(Offset: Longint; Origin: Word): Longint; override;
      function Write(const Buffer; Count: Longint): Longint; override;
      function WriteNullTerminated(Buffer: PWideChar): Longint;
      procedure WriteChars(Chars: PWideChar; Count: Longint);
      procedure WriteString(const AString: WideString);
      property Capacity: Longint read FCapacity write SetCapacity;
      property StringLength: Longint read GetStringLength write SetStringLength;
      property StringPosition: Longint read GetStringPosition write SetStringPosition;
      property DataString: WideString read GetDataString;
    end;

  TFastStringStream = class(TStream)
    private
      FData: string;
      FSize: Longint;
      FPosition: Longint;
      procedure InternalGrowCapacity(Needed: Longint);
      procedure InternalSetSize(NewSize: Longint);
      function GetData: PChar;
    protected
      // exposes internal data buffer - use with care
      property Data: PChar read GetData;
      function GetCapacity: Longint; virtual;
      procedure SetCapacity(NewCapacity: Longint); virtual;
      procedure SetSize(NewSize: Longint); override;
      procedure SetStringPosition(Value: Longint);
      function GetStringPosition: Longint;
      procedure SetStringLength(Value: Longint);
      function GetStringLength: Longint;
      function GetDataString: string;
    public
      constructor Create(const AString: string); overload;
      function Read(var Buffer; Count: Longint): Longint; override;
      function ReadString(Count: Longint): string;
      function Seek(Offset: Longint; Origin: Word): Longint; override;
      function Write(const Buffer; Count: Longint): Longint; override;
      function WriteNullTerminated(Buffer: PChar): Longint;
      procedure WriteChars(Chars: PChar; Count: Longint);
      procedure WriteString(const AString: string);
      property Capacity: Longint read GetCapacity write SetCapacity;
      property StringLength: Longint read GetStringLength write SetStringLength;
      property StringPosition: Longint read GetStringPosition write SetStringPosition;
      property DataString: string read GetDataString;
    end;

  TIOState = (iosInitialized, iosOpen, iosClosing, iosClosed);
  PIOState = ^TIOState;

  EIO = class(Exception);

  IIO = interface
    ['{564BF641-2F6C-4BD4-A584-446745E753F8}']
    // cleans up and re-initializes IO process
    procedure Reset;
    // closes underlying "process" - State will be iosClosing or iosClosed
    // legal to call in State iosOpen only
    procedure Close;
    // returns current state
    function State: TIOState;
    end;

  EWriter = class(EIO);

  IWriter = interface(IIO)
    ['{A313E050-EC6A-489C-B520-570B98488315}']
    // legal to call in states iosInitialized, iosOpen;
    // when called in state iosInitialized will move state to iosOpen
    function Write(const Buffer; Count: Integer): Integer;
    // tries to flush out all remaining data to target;
    // legal to call in states iosOpen, iosClosing
    procedure Flush;
    end;

  EReader = class(EIO);

  IReader = interface(IIO)
    ['{3048A200-C853-4653-99F6-EAA759124B5C}']
    // legal to call in states iosInitialized, iosOpen, iosClosing;
    // when called in state iosInitialized will move state to iosOpen
    function Read(var Buffer; Count: Integer): Integer;
    // fills read buffers from target;
    // legal to call in states iosInitialized, iosOpen
    function Fill: Integer;
    end;

  IBufferedWriter = interface(IWriter)
    ['{930DB200-180D-416D-882D-0A69B478BAB2}']
      // data in write-buffer, ready to be flushed
    procedure GetFlushData(out StartP, EndP: PChar);
    function GetBufferSize: Integer;
    procedure SetBufferSize(Value: Integer);
    property BufferSize: Integer
      read GetBufferSize write SetBufferSize;
    end;

  IBufferedReader = interface(IReader)
    ['{CA2B7CA3-4C03-40D6-BF6D-EA6227A97659}']
    procedure GetFillData(out StartP, EndP: PChar);
    function GetBufferSize: Integer;
    procedure SetBufferSize(Value: Integer);
    property BufferSize: Integer
      read GetBufferSize write SetBufferSize;
    end;

  // exposes Stream through IWriter interface
  TStreamWriter = class(TInterfacedObject, IIO, IWriter)
    private
      FStream: TStream;
      FState: TIOState;
    protected
      property Stream: TStream read FStream;
      // called on first Write
      procedure Open; virtual;
      { IIO }
      procedure Reset; virtual;
      // closes stream writer - State will be iosClosing or iosClosed
      procedure Close; virtual;
      // returns current state
      function State: TIOState;
      { IWriter }
      function Write(const Buffer; Count: Integer): Integer;
      procedure Flush;
    public
      constructor Create(Stream: TStream);
    end;

  // exposes Stream through IReader interface
  TStreamReader = class(TInterfacedObject, IIO, IReader)
    private
      FStream: TStream;
      FState: TIOState;
      FCloseOnPartialRead: Boolean;
    protected
      property Stream: TStream read FStream;
      // called on first Read
      procedure Open; virtual;
      { IIO }
      procedure Reset; virtual;
      // closes stream reader - State will be iosClosing or iosClosed
      procedure Close; virtual;
      // returns current state
      function State: TIOState;
      { IReader }
      function Read(var Buffer; Count: Integer): Integer;
      function Fill: Integer;
    public
      constructor Create(Stream: TStream);
      // when True, reader calls Close when Read() returns less than Count
      property CloseOnPartialRead: Boolean
        read FCloseOnPartialRead write FCloseOnPartialRead;
    end;

  // base class for TBufferWriter and TBufferReader
  TBufferIO = class(TInterfacedObject, IIO)
    private
      FBuffer: Pointer;
      FSize: Integer;
      FPosition: Integer;
      FState: TIOState;
    protected
      procedure CheckBuffer(BufPtr: Pointer; BufSize: Integer);
      property Buffer: Pointer read FBuffer;
      // called on first Read or Write
      procedure Open;
      { IIO }
      procedure Reset;
      // closes instance - State will be iosClosed
      procedure Close;
      // returns current state
      function State: TIOState;
    public
      constructor Create; overload;
      constructor Create(const Buffer; Size: Integer); overload;
      procedure SetBuffer(const Buffer; Size: Integer);
      property Position: Integer read FPosition;
    end;

  // writes to a fixed size buffer through the IWriter interface
  TBufferWriter = class(TBufferIO, IWriter)
    protected
      { IWriter }
      // calls Close() when end of buffer is encountered (Result < Count)
      function Write(const Buffer; Count: Integer): Integer;
      procedure Flush;
    end;

  // reads from a fixed size buffer through the IReader interface
  TBufferReader = class(TBufferIO, IReader)
    protected
      { IReader }
      // calls Close() when end of buffer is encountered (Result < Count)
      function Read(var Buffer; Count: Integer): Integer;
      function Fill: Integer;
    end;

  TInProcessor = function(var StartP: PChar; Needed: Integer): Integer of object;
  TOutProcessor = function(var StartP: PChar; Needed: Integer): Integer of object;

  TFilterDriver = class;

  EFilter = class(Exception);
  TFilterProc = procedure(var InStartP: PChar; InSize: Integer;
    var OutStartP: PChar; OutSize: Integer) of object;
  TFilterState = (flsClosed, flsOpen, flsClosing);

  TFilter = class
    private
      FProcessor: TFilterProc;
      FDriver: TFilterDriver;
      FState: TFilterState;
      FHaveOutBuffer: Boolean;
    protected
      procedure SetProcessor(Value: TFilterProc);
      procedure Open; virtual;    // called on first Filter.Process execution
      procedure InternalClose;    // called when Filter is to be closed
      procedure Closed; virtual;  // called right after Filter was closed
      procedure ClosedProcessor(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      procedure OpenProcessor(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      { Purpose: get next input buffer and report buffer progress
        Parameters:
        - InstartP: current position of input pointer - any input data
          following it (if applicable) have not been processed yet
        - Needed: Size of requested input buffer - counted from InStartP
        Returns: Size of allocated buffer, starting at InStartP
        Note:
        - the function failed if returned size < requested size
        - if Needed is less than the unprocessed amount from the previous
          in-buffer then it will be increased to "unprocessed amount" + 1  }
      function NextInBuffer(var InStartP: PChar; Needed: Integer = 0): Integer;
      { Purpose: get next output buffer and report buffer progress
        Parameters:
        - OutStartP: current position of output pointer - any output
          space following it (if applicable) has not been used yet
        - Needed: Size of requested output buffer - counted from OutStartP
        Returns: Size of allocated buffer, starting at InStartP
        Note:
        - the function failed if returned size < requested size
        - if Needed is less than the unused amount from the previous
          out-buffer then it will be increased to "unused amount" + 1    }
      function NextOutBuffer(var OutStartP: PChar; Needed: Integer = 0): Integer;
      // Process is called by Driver
      procedure Process(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      property Processor: TFilterProc read FProcessor write SetProcessor;
      property Driver: TFilterDriver read FDriver;
    public
      constructor Create(Driver: TFilterDriver); virtual;
      procedure Close; virtual;
      property State: TFilterState read FState;
    end;
  TFilterClass = class of TFilter;

  EFilterDriver = class(Exception);
  TFilterDriverState = (fdsInitialized, fdsActive, fdsBusy);

  TFilterDriver = class
    private
      FFilter: TFilter;
      FState: TFilterDriverState;
    protected
      function InErrorProcessor(var StartP: PChar; Needed: Integer): Integer;
      function OutErrorProcessor(var StartP: PChar; Needed: Integer): Integer;
      function GetProcessInBuffer: TInProcessor; virtual; abstract;
      function GetProcessOutBuffer: TOutProcessor; virtual; abstract;
      property ProcessInBuffer: TInProcessor read GetProcessInBuffer;
      property ProcessOutBuffer: TOutProcessor read GetProcessOutBuffer;
    public
      constructor Create(FilterClass: TFilterClass);
      destructor Destroy; override;
      // will also close the Filter if not already closed
      procedure Reset; virtual;
      // buffer of (pending) data not complete enough for processing
      procedure GetPendingData(out StartP, EndP: PChar); virtual; abstract;
      // amount of data still needed to complete conversion
      function NeededData: Integer; virtual; abstract;
      procedure ClearNeeded; virtual; abstract;
      // buffer of processed data waiting for delivery
      procedure GetWaitingData(out StartP, EndP: PChar); virtual; abstract;
      property Filter: TFilter read FFilter;
      property State: TFilterDriverState read FState;
    end;

  IFilterDriver = interface
    ['{92C052E3-E2D2-4F23-AE68-C545A0462287}']
    // buffer of (pending) data not complete enough for processing
    procedure GetPendingData(out StartP, EndP: PChar);
    // amount of data still needed to complete conversion
    function NeededData: Integer;
    procedure ClearNeeded;
    // buffer of processed data waiting for delivery
    procedure GetWaitingData(out StartP, EndP: PChar);
    function Filter: TFilter;
    function State: TFilterDriverState;
    end;

  EFilterWriter = class(EFilterDriver);

  TFilterWriterImpl = class(TFilterDriver)
    private
      FBufferSize: Integer;
      FWriteTarget: IWriter;
      FBufferP, FBufStartP, FFlushP: PChar;    // write buffer
      FInBufSize: Integer;
      FInBufP, FPendingP, FNeededP: PChar;     // pending input buffer
      FOutBufSize: Integer;
      FOutBufP, FOutStartP, FWaitingP: PChar;  // waiting output buffer
      // pointers to start/end of Buffer argument (Write)
      FExternalStartP, FExternalEndP: PChar;
      FProcessInBuffer: TInProcessor;          // current input buffer handler
      FProcessOutBuffer: TOutProcessor;        // current output buffer handler
      // internal buffer management routines
      function InProcessor(var StartP: PChar; Needed: Integer): Integer;
      function PendingProcessor(var StartP: PChar; Needed: Integer): Integer;
      function OutProcessor(var StartP: PChar; Needed: Integer): Integer;
      function WaitingProcessor(var StartP: PChar; Needed: Integer): Integer;
    protected
      function GetProcessInBuffer: TInProcessor; override;
      function GetProcessOutBuffer: TOutProcessor; override;
      function FlushBuffer: Boolean;
      procedure SetBufferSize(Value: Integer); virtual;
      procedure SetWriteTarget(const Value: IWriter); virtual;
    public
      constructor Create(FilterClass: TFilterClass; const Target: IWriter;
        BufferSize: Integer);
      destructor Destroy; override;
      procedure Reset; override;
      function Write(const Buffer; Count: Integer): Integer;
      // process and output pending (unprocessed) and waiting data
      procedure Flush;
      // data in write-buffer, ready to be flushed
      procedure GetFlushData(out StartP, EndP: PChar);
      procedure GetPendingData(out StartP, EndP: PChar); override;
      function NeededData: Integer; override;
      procedure ClearNeeded; override;
      procedure GetWaitingData(out StartP, EndP: PChar); override;
      property BufferSize: Integer read FBufferSize write SetBufferSize;
      property WriteTarget: IWriter read FWriteTarget write SetWriteTarget;
    end;

  TFilterWriter = class(TInterfacedObject, IWriter, IBufferedWriter, IFilterDriver)
    private
      FImpl: TFilterWriterImpl;
    protected
      property Impl: TFilterWriterImpl read FImpl
        implements IWriter, IBufferedWriter, IFilterDriver;
      function IOState: TIOState;
      function IWriter.State = IOState;
      function IBufferedWriter.State = IOState;
      procedure IOClose;
      procedure IWriter.Close = IOClose;
      procedure IBufferedWriter.Close = IOClose;
      function GetBufferSize: Integer;
      function GetFilter: TFilter;
      function IFilterDriver.Filter = GetFilter;
      function DriverState: TFilterDriverState;
      function IFilterDriver.State = DriverState;
    public
      constructor Create(FilterClass: TFilterClass; const Target: IWriter;
        BufferSize: Integer = DefaultFilterBufferSize);
      destructor Destroy; override;
    end;

  EFilterReader = class(EFilterDriver);

  TFilterReaderImpl = class(TFilterDriver)
    private
      FBufferSize: Integer;
      FReadTarget: IReader;
      FBufferP, FBufStartP, FFillP: PChar;     // read buffer
      FInBufSize: Integer;
      FInBufP, FPendingP, FNeededP: PChar;     // pending input buffer
      FOutBufSize: Integer;
      FOutBufP, FOutStartP, FWaitingP: PChar;  // waiting output buffer
      // pointers to start/end of Buffer argument (Read)
      FExternalStartP, FExternalEndP: PChar;
      FProcessInBuffer: TInProcessor;          // current input buffer handler
      FProcessOutBuffer: TOutProcessor;        // current output buffer handler
      // internal buffer management routines
      function InProcessor(var StartP: PChar; Needed: Integer): Integer;
      function PendingProcessor(var StartP: PChar; Needed: Integer): Integer;
      function OutProcessor(var StartP: PChar; Needed: Integer): Integer;
      function WaitingProcessor(var StartP: PChar; Needed: Integer): Integer;
    protected
      function GetProcessInBuffer: TInProcessor; override;
      function GetProcessOutBuffer: TOutProcessor; override;
      function FillBuffer: Integer;
      procedure SetBufferSize(Value: Integer); virtual;
      procedure SetReadTarget(const Value: IReader); virtual;
    public
      constructor Create(FilterClass: TFilterClass; const Target: IReader;
        BufferSize: Integer);
      destructor Destroy; override;
      procedure Reset; override;
      function Read(var Buffer; Count: Integer): Integer;
      // fill in-buffer //kw what about: only if empty?
      function Fill: Integer;
      // unprocessed data in read-buffer
      procedure GetFillData(out StartP, EndP: PChar); 
      procedure GetPendingData(out StartP, EndP: PChar); override;
      function NeededData: Integer; override;
      procedure ClearNeeded; override;
      procedure GetWaitingData(out StartP, EndP: PChar); override;
      property BufferSize: Integer read FBufferSize write SetBufferSize;
      property ReadTarget: IReader read FReadTarget write SetReadTarget;
    end;

  TFilterReader = class(TInterfacedObject, IReader, IBufferedReader, IFilterDriver)
    private
      FImpl: TFilterReaderImpl;
    protected
      property Impl: TFilterReaderImpl read FImpl
        implements IReader, IBufferedReader, IFilterDriver;
      function IOState: TIOState;
      function IReader.State = IOState;
      function IBufferedReader.State = IOState;
      procedure IOClose;
      procedure IReader.Close = IOClose;
      procedure IBufferedReader.Close = IOClose;
      function GetBufferSize: Integer;
      function GetFilter: TFilter;
      function IFilterDriver.Filter = GetFilter;
      function DriverState: TFilterDriverState;
      function IFilterDriver.State = DriverState;
    public
      constructor Create(FilterClass: TFilterClass; const Target: IReader;
        BufferSize: Integer = DefaultFilterBufferSize);
      destructor Destroy; override;
    end;

  TByteSwapFilter = class(TFilter)
    protected
      procedure SwapBytes(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      procedure Open; override;
    public
      function CheckInput: Boolean;
    end;

  TByteToWordFilter = class(TFilter)
    protected
      procedure ByteToWord(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      procedure Open; override;
    end;

  TBufferedCopyFilter = class(TFilter)
    protected
      procedure CopyBytes(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      procedure Open; override;
    end;

  TBase64Encoder = class(TFilter)
    private
      FLineLength: Word;  // if 0, no linebreaks will be inserted on encoding
      FCurrentLength: Word;  // working line length
      FUnixLineBreak: Boolean;
    protected
      procedure Encode(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      procedure EncodeEpilog(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      procedure Open; override;
      property CurrentLength: Word read FCurrentLength write FCurrentLength;
    public
      constructor Create(Driver: TFilterDriver); override;
      procedure Close; override;
      property LineLength: Word read FLineLength write FLineLength;
      property UnixLineBreak: Boolean read FUnixLineBreak write FUnixLineBreak;
    end;

  TBase64Strictness = (b64IgnoreAll, b64IgnoreWhitespace, b64Strict);

  TBase64Decoder = class(TFilter)
    private
      FStrictness: TBase64Strictness;
    protected
      procedure Decode(var InStartP: PChar; InSize: Integer;
        var OutStartP: PChar; OutSize: Integer);
      procedure Open; override;
      procedure Closed; override;
    public
      constructor Create(Driver: TFilterDriver); override;
      function CheckInput: Boolean;
      property Strictness: TBase64Strictness read FStrictness write FStrictness;
    end;

  { adds a Find method to TList - should ONLY be used when list is SORTED! }
  TSearchList = class(TList)
    private
      FCompare: TListSortCompare;
    public
      function IndexOf(Item: Pointer): Integer;
      function Locate(Item: Pointer; var ItemIndx: Integer): Integer;
      procedure Sort; overload;
      property Compare: TListSortCompare read FCompare write FCompare;
    end;

  EStackError = class(Exception);
  PStackArray = ^TStackArray;
	TStackArray = array[0..MaxStackSize - 1] of Pointer;

	TStack = class
    private
      FStackArrP: PStackArray;
      FPos: Integer;
      FMaxPos: Integer;
    protected
      procedure Grow;
      function GetCapacity: Integer;
      procedure SetCapacity(Value: Integer);
      procedure CheckSwapDistance(Distance, Count: Integer);
      procedure CheckRotateDistance(Distance, Count: Integer);
      property StackArrP: PStackArray read FStackArrP;
      property Pos: Integer read FPos write FPos;
      property MaxPos: Integer read FMaxPos;
    public
      constructor Create(Size: Integer);
      destructor Destroy; override;
      function GetCount: Integer;
      procedure Push(ItemP: Pointer);
      function Pop: Pointer;
      function Top: Pointer;
      procedure Clear; virtual;
      procedure Swap(Distance: Integer);
      procedure RotateUp(Distance: Integer);
      procedure RotateDown(Distance: Integer);
      property Capacity: Integer read GetCapacity write SetCapacity;
		end;

  EStringTable = class(Exception);

  TPointers = array[0..(MaxInt div SizeOf(Pointer)) - 1] of Pointer;
  PPointers = ^TPointers;
  TAnsiChars = array[0..(MaxInt div SizeOf(AnsiChar)) - 1] of AnsiChar;
  PAnsiChars = ^TAnsiChars;

  { String hash table class for "interning" strings (like in Java).
    Strings are returned as pointers - use a string(Pointer) cast.
    Empty strings are ignored:
      - Intern('') returns nil
      - Find('') returns False
      - Remove('') does nothing }
  TStringTable = class
    private
      FStrings: PPointers;
      FSizeLog: Byte;
      FCount: Longword;
    protected
      property Strings: PPointers read FStrings;
      // table capacity must be power of 2
      procedure SetSizeLog(Value: Byte);
      procedure IncCount;
      function FindSlot(const Key: array of AnsiChar; out Indx: Integer): Boolean;
    public
      constructor Create(SizeLog: Byte = 6);
      destructor Destroy; override;
      // returns internal matching string, adds new string when not found
      function Intern(const Value: string): Pointer; overload;
      function Intern(const Value: array of AnsiChar): Pointer; overload;
      // Len = -1 means the strings is null-terminated
      function Intern(Value: PAnsiChar; Len: Integer = -1): Pointer; overload;
      // removes string - if found
      function Remove(const Value: string): Boolean; overload;
      function Remove(const Value: array of AnsiChar): Boolean; overload;
      // Len = -1 means the strings is null-terminated
      function Remove(Value: PAnsiChar; Len: Integer = -1): Boolean; overload;
      // finds registered string - returns nil if unsuccessful
      function Find(const Value: string): Pointer; overload;
      function Find(const Value: array of AnsiChar): Pointer; overload;
      // Len = -1 means the strings is null-terminated
      function Find(Value: PAnsiChar; Len: Integer = -1): Pointer; overload;
      // clears table of all strings
      procedure Clear;
      // remove all unreferenced strings (with a reference count of 1)
      procedure Collect;
      // returns iterator cookie for starting a new iteration
      function StartIter: Cardinal;
      // returns string for Iter argument (iterator cookie) and advances
      // the iterator; returns nil if a next entry does not exist
      function Next(var Iter: Cardinal): Pointer;
      // table capacity (as power of 2); must be >= (2 * Count)
      property SizeLog: Byte read FSizeLog write SetSizeLog;
      property Count: Longword read FCount;
    end;

  TWideChars = array[0..(MaxInt div SizeOf(WideChar)) - 1] of WideChar;
  PWideChars = ^TWideChars;

  // like TStringTable, but use a WideString(Pointer) cast
  TWideStringTable = class
    private
      FStrings: PPointers;
      FSizeLog: Byte;
      FCount: Longword;
    protected
      property Strings: PPointers read FStrings;
      // table capacity must be power of 2
      procedure SetSizeLog(Value: Byte);
      procedure IncCount;
      function FindSlot(const Key: array of WideChar; out Indx: Integer): Boolean;
    public
      constructor Create(SizeLog: Byte = 6);
      destructor Destroy; override;
      // returns registered string, adds new string when not found
      function Intern(const Value: WideString): Pointer; overload;
      function Intern(const Value: array of WideChar): Pointer; overload;
      // Len = -1 means the strings is null-terminated
      function Intern(Value: PWideChar; Len: Integer = -1): Pointer; overload;
      // removes string - if found
      function Remove(const Value: WideString): Boolean; overload;
      function Remove(const Value: array of WideChar): Boolean; overload;
      // Len = -1 means the strings is null-terminated
      function Remove(Value: PWideChar; Len: Integer = -1): Boolean; overload;
      // finds registered string - returns nil if unsuccessful
      function Find(const Value: WideString): Pointer; overload;
      function Find(const Value: array of WideChar): Pointer; overload;
      // Len = -1 means the strings is null-terminated
      function Find(Value: PWideChar; Len: Integer = -1): Pointer; overload;
      // clears table of all strings
      procedure Clear;
{$IFDEF LINUX}  // wide strings are only reference counted in Kylix
      // remove all unreferenced strings (with a reference count of 1)
      procedure Collect;
{$ENDIF}
      // returns iterator cookie for starting a new iteration
      function StartIter: Cardinal;
      // returns string for Iter argument (iterator cookie) and advances it;
      // returns nil if next entry does not exist
      function Next(var Iter: Cardinal): Pointer;
      // table capacity (as power of 2); must be >= (2 * Count)
      property SizeLog: Byte read FSizeLog write SetSizeLog;
      property Count: Longword read FCount;
    end;

// Processes (up to) Count bytes from source into target stream.
// It is not an error if the source stream contains less than Count bytes.
// To process the whole source stream just set Count to a very large value.
procedure ApplyFilter(FilterClass: TFilterClass; Source, Target: TStream;
  Count: Cardinal); overload;
// The Source must contain at least SourceCount bytes, and the target's
// size (TargetSize) must be large enough to hold all of the output
function ApplyFilter(FilterClass: TFilterClass; const Source; SourceCount: Integer;
  var Target; TargetSize: Integer): Integer; overload;

implementation

uses KDSUtils;

procedure ApplyFilter(FilterClass: TFilterClass; Source, Target: TStream;
  Count: Cardinal);
  const
    BufferSize = 16384;
  var
    SW, FW: IWriter;
    Buffer: packed array[0..BufferSize-1] of Byte;
    ReadCount, WriteIndx: Cardinal;
    Written: Integer;
    WriteState: TIOState;
    EOF: Boolean;
  begin
  // wrap IWriter interface around target stream
  SW := TStreamWriter.Create(Target);
  FW := TFilterWriter.Create(FilterClass, SW, BufferSize);
  repeat
    if BufferSize < Count then
      begin
      ReadCount := Source.Read(Buffer, BufferSize);
      EOF := ReadCount < BufferSize;
      end
    else
      begin
      ReadCount := Source.Read(Buffer, Count);
      EOF := ReadCount < Count;
      end;
    Dec(Count, ReadCount);
    WriteIndx := 0;
    WriteState := FW.State;
    // for a filter we cannot expect that all of the buffer can be written at once
    while (WriteIndx < ReadCount) and
      (WriteState in [iosInitialized, iosOpen]) do
      begin
      Written := FW.Write(Buffer[WriteIndx], ReadCount);
      WriteState := FW.State;
      Inc(WriteIndx, Written);
      end;
    until EOF or (WriteState in [iosClosing, iosClosed]);
  // if we are at the end of input, and the Filter itself
  // has not detected that, then we need to close it now
  if FW.State in [iosInitialized, iosOpen] then FW.Close;
  // flush remaining output
  while FW.State = iosClosing do FW.Flush;
  end;

function ApplyFilter(FilterClass: TFilterClass; const Source; SourceCount: Integer;
  var Target; TargetSize: Integer): Integer;
  var
    BW: TBufferWriter;
    FW: IWriter;
    Written: Integer;
  begin
  // wrap IWriter interface around target buffer
  BW := TBufferWriter.Create(Target, TargetSize);
  // FW keeps IWriter reference to BW
  FW := TFilterWriter.Create(FilterClass, BW);
  FW.Reset;
  Written := FW.Write(Source, SourceCount);
  if Written < SourceCount then
    raise EFilter.CreateFmt(SUnprocessedInput, [SourceCount - Written]);
  // if we are at the end of input, and the Filter itself
  // has not detected that, then we need to close it now
  if FW.State in [iosInitialized, iosOpen] then FW.Close;
  // flush remaining output
  while FW.State = iosClosing do FW.Flush;
  Result := BW.Position;
  end;

{ TBufferStream }

procedure TBufferStream.SetBuffer(Buffer: Pointer; Size: Longint);
  begin
  SetPointer(Buffer, Size);
  Position := 0;
  end;

function TBufferStream.Write(const Buffer; Count: Longint): Longint;
  var
    Pos: Longint;
  begin
  if (Position >= 0) and (Count >= 0) then
    begin
    Pos := Position + Count;
    if Pos > Size then
      begin
      Dec(Count, Pos - Size);
      Pos := Size;
      end;
    System.Move(Buffer, Pointer(Longint(Memory) + Position)^, Count);
    Position := Pos;
    Result := Count;
    Exit;
    end;
  Result := 0;
  end;

{ TWideStringStream }

{ This is a stream interface for Widestrings.
  Purpose: avoid excessive memory re-allocations that occur with
    WideStrings because they are not reference counted.
  Note: when the DataString property is accessed, the complete
    WideString will be copied out                                }

constructor TWideStringStream.Create(const AString: WideString);
  begin
  inherited Create;
  //on creation, don't grow capacity beyond string
  FSize := Length(AString) shl 1;
  ReallocMem(FDataP, FSize);
  FCapacity := FSize;
  Move(Pointer(AString)^, FDataP^, FSize);
  end;

destructor TWideStringStream.Destroy;
  begin
  FreeMem(FDataP);
  inherited Destroy;
  end;

function TWideStringStream.GetData: PWideChar;
  begin
  Result := PWideChar(FDataP);
  end;

function TWideStringStream.GetDataString: WideString;
  begin
  SetLength(Result, StringLength);
  Move(FDataP^, Pointer(Result)^, FSize);
  end;

{ gets string length in terms of WideChars }
function TWideStringStream.GetStringLength: Longint;
  begin
  Result := FSize shr 1;
  end;

{ gets string position in terms of WideChars }
function TWideStringStream.GetStringPosition: Longint;
  begin
  if Odd(Position) then raise EStreamError.Create(SOddPosInvalid);
  Result := (Position shr 1) + 1;
  end;

procedure TWideStringStream.InternalGrowCapacity(Needed: Longint);
  var
    Delta: Longint;
  begin
  Delta := Needed shr 2;
  if Delta < 8 then Delta := 8;
  InternalSetCapacity(Needed + Delta);
  end;

{ sets stream capacity in bytes, rounds to Longword boundary }
procedure TWideStringStream.InternalSetCapacity(NewCapacity: Longint);
  begin
  NewCapacity := ((NewCapacity + 3) shr 2) shl 2; //Longword boundary
  ReallocMem(FDataP, NewCapacity);
  FCapacity := NewCapacity;
  end;

{ caller must make sure that NewSize is an even, positive number }
procedure TWideStringSTream.InternalSetSize(NewSize: Longint);
  begin
  if NewSize > Capacity then InternalGrowCapacity(NewSize);
  FSize := NewSize;
  end;

function TWideStringStream.Read(var Buffer; Count: Longint): Longint;
  begin
  Result := FSize - FPosition;
  if Result > Count then Result := Count;
  Move((FDataP + FPosition)^, Buffer, Result);
  FPosition := FPosition + Result;
  end;

{ reads Count WideChars from stream and returns them as WideString }
function TWideStringStream.ReadString(Count: Longint): WideString;
  var
    Len: Longint;
  begin
  if Odd(FPosition) then raise EReadError.Create(SCannotReadOddPos);
  Len := (FSize - FPosition) shr 1;
  if Len > Count then Len := Count;
  SetLength(Result, Len);
  Read(Pointer(Result)^, Len shl 1);
  end;

function TWideStringStream.Seek(Offset: Longint; Origin: Word): Longint;
  begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := FSize - Offset;
    end;
  if FPosition > FSize then FPosition := FSize
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
  end;

{ for user code - checks against Size }
procedure TWideStringStream.SetCapacity(NewCapacity: Longint);
  begin
  Assert(NewCapacity >= 0, SNegativeCapacityInvalid);
  if NewCapacity < FSize then raise EStreamError.Create(SCapacityLessSize);
  InternalSetCapacity(NewCapacity);
  end;

{ sets stream size in bytes }
procedure TWideStringStream.SetSize(NewSize: Longint);
  begin
  Assert(NewSize >= 0, SNegativeSizeInvalid);
  if Odd(NewSize) then raise EStreamError.Create(SOddSizeInvalid);
  InternalSetSize(NewSize);
  if FPosition > NewSize then FPosition := NewSize;
  end;

{ sets string length in terms of WideChars }
procedure TWideStringStream.SetStringLength(Value: Longint);
  begin
  SetSize(Value shl 1);
  end;

{ sets string position in terms of WideChars }
procedure TWideStringStream.SetStringPosition(Value: Longint);
  begin
  if Value < 1 then raise EStreamError.Create(SStringPositionInvalid);
  Position := (Value - 1) shl 1;
  end;

{ standard Write operation - operates in Bytes, not WideChars
  Note: does not grow capacity beyond what is immediately needed }
function TWideStringStream.Write(const Buffer; Count: Longint): Longint;
  var
    NewPos: Longint;
  begin
  Result := Count;
  NewPos := FPosition + Result;
  if NewPos > FSize then
    InternalSetSize(((NewPos + 1) shr 1) shl 1); //next larger even value
  Move(Buffer, (FDataP + FPosition)^, Result);
  FPosition := NewPos;           //next position to be written to
  end;

procedure TWideStringStream.WriteChars(Chars: PWideChar; Count: Longint);
  begin
  Write(Chars^, Count * SizeOf(WideChar));
  end;

function TWideStringStream.WriteNullTerminated(Buffer: PWideChar): Longint;
  const
    NullChar = WideChar(#0);
  var
    BufChar: WideChar;
    NewPosition: Longint;
  begin
  if Odd(FPosition) then raise EWriteError.Create(SCannotWriteOddPos);
  //Assert((not Odd(FSize)) and (FPosition <= (FSize - SizeOf(WideChar))));
  Result := 0;
  NewPosition := FPosition;
  BufChar := Buffer^;
  while BufChar <> NullChar do
    begin
    if NewPosition >= Capacity then
      InternalGrowCapacity(NewPosition + SizeOf(WideChar));
    PWideChar(FDataP + NewPosition)^ := BufChar;
    NewPosition := NewPosition + SizeOf(WideChar);
    Inc(Result);
    BufChar := Buffer[Result];
    end;
  FPosition := NewPosition;
  if NewPosition > FSize then FSize := NewPosition;
  end;

{ writes AString into stream, starting at StringPosition, overwriting
  existing characters and extending the stream if necessary           }
procedure TWideStringStream.WriteString(const AString: WideString);
  begin
  if Odd(FPosition) then raise EWriteError.Create(SCannotWriteOddPos);
  Write(Pointer(AString)^, Length(AString) shl 1);
  end;

{ TFastStringStream }

constructor TFastStringStream.Create(const AString: string);
  begin
  inherited Create;
  FData := AString;
  //on creation, don't grow capacity beyond string
  InternalSetSize(Length(AString));
  end;

function TFastStringStream.GetCapacity: Longint;
  begin
  Result := Length(FData);
  end;

function TFastStringStream.GetData: PChar;
  begin
  Result := PChar(FData);
  end;

function TFastStringStream.GetDataString: string;
  begin
  Result := FData;
  SetLength(Result, Size);
  end;

function TFastStringStream.GetStringLength: Longint;
  begin
  Result := Size;
  end;

function TFastStringStream.GetStringPosition: Longint;
  begin
  Result := FPosition + 1;
  end;

procedure TFastStringStream.InternalGrowCapacity(Needed: Longint);
  var
    Delta: Longint;
  begin
  Delta := Needed shr 2;
  if Delta < 8 then Delta := 8;
  SetLength(FData, ((Needed + Delta + 3) shr 2) shl 2); //Longword boundary
  end;

{ caller must make sure that NewSize is an even, positive number }
procedure TFastStringStream.InternalSetSize(NewSize: Longint);
  begin
  if NewSize > Capacity then InternalGrowCapacity(NewSize);
  FSize := NewSize;
  end;

function TFastStringStream.Read(var Buffer; Count: Longint): Longint;
  begin
  Result := FSize - FPosition;
  if Result > Count then Result := Count;
  Move((PChar(FData) + FPosition)^, Buffer, Result);
  FPosition := FPosition + Result;
  end;

function TFastStringStream.ReadString(Count: Longint): string;
  var
    Len: Integer;
  begin
  Len := FSize - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PChar(FData) + FPosition, Len);
  FPosition := FPosition + Len;
  end;

function TFastStringStream.Seek(Offset: Longint; Origin: Word): Longint;
  begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := FSize - Offset;
  end;
  if FPosition > FSize then FPosition := FSize
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
  end;

procedure TFastStringStream.SetCapacity(NewCapacity: Longint);
  begin
  Assert(NewCapacity >= 0, SNegativeCapacityInvalid);
  if NewCapacity < FSize then raise EStreamError.Create(SCapacityLessSize);
  SetLength(FData, ((NewCapacity + 3) shr 2) shl 2); //Longword boundary
  end;

procedure TFastStringStream.SetSize(NewSize: Longint);
  begin
  Assert(NewSize >= 0, SNegativeSizeInvalid);
  InternalSetSize(NewSize);
  if FPosition > FSize then FPosition := FSize;
  end;

procedure TFastStringStream.SetStringLength(Value: Longint);
  begin
  SetSize(Value);
  end;

procedure TFastStringStream.SetStringPosition(Value: Longint);
  begin
  if Value < 1 then raise EStreamError.Create(SStringPositionInvalid);
  Position := Value - 1;
  end;

function TFastStringStream.Write(const Buffer; Count: Longint): Longint;
  var
    NewPos: Longint;
  begin
  Result := Count;
  NewPos := FPosition + Result;
  if NewPos > Size then InternalSetSize(NewPos);
  Move(Buffer, (PChar(FData) + FPosition)^, Result);
  FPosition := NewPos;
  end;

procedure TFastStringStream.WriteChars(Chars: PChar; Count: Longint);
  begin
  Write(Chars^, Count);
  end;

function TFastStringStream.WriteNullTerminated(Buffer: PChar): Longint;
  const
    NullChar = #0;
  var
    BufChar: Char;
    NewPosition: Longint;
  begin
  Result := 0;
  NewPosition := FPosition;
  BufChar := Buffer^;
  while BufChar <> NullChar do
    begin
    Inc(NewPosition);
    if NewPosition > Capacity then InternalGrowCapacity(NewPosition);
    FData[NewPosition] := BufChar;
    Inc(Result);
    BufChar := Buffer[Result];
    end;
  FPosition := NewPosition;
  if NewPosition > FSize then FSize := NewPosition;
  end;

procedure TFastStringStream.WriteString(const AString: string);
  begin
  Write(PChar(AString)^, Length(AString));
  end;

{ TSearchList }

{ Note: in general, the Locate method does the same and is more usable }
function TSearchList.IndexOf(Item: Pointer): Integer;
  begin
  if Locate(Item, Result) <> 0 then Result := -1;
  end;

{ If the item is found, returns 0 and sets ItemIndx to the items index;
  otherwise sets ItemIndx to the index of the closest match and returns
  <0 if the item is less or >0 if the item is greater than the key;
  if the list is empty, ItemIndx will return -1                        }
function TSearchList.Locate(Item: Pointer; var ItemIndx: Integer): Integer;
  var
    Low, High: Integer;
  begin
  Result := -1;
  ItemIndx := -1;
  Low := 0;
  High := Count - 1;
  while Low <= High do
    begin
    ItemIndx := (Low + High) shr 1;
    Result := Compare(Item, List^[ItemIndx]);
    if Result < 0 then
      High := ItemIndx - 1
    else if Result > 0 then
      Low := ItemIndx + 1
    else
      Break;
    end;
  end;

procedure TSearchList.Sort;
  begin
  inherited Sort(FCompare);
  end;

{ TStack }

constructor TStack.Create(Size: Integer);
  begin
	inherited Create;
	FMaxPos := Size - 1;
  FPos := -1;
	GetMem(FStackArrP, Size * SizeOf(Pointer));
	end;

destructor TStack.Destroy;
  begin
	FreeMem(FStackArrP);
	inherited Destroy;
	end;

procedure TStack.Push(ItemP: Pointer);
  begin
	if FPos >= FMaxPos then Grow;
  Inc(FPos);
  FStackArrP^[FPos] := ItemP;
	end;

function TStack.Pop: Pointer;
  begin
	if FPos >= 0 then
    begin
		Result := FStackArrP^[FPos];
		Dec(FPos);
		end
  else
    Result := nil;
	end;

function TStack.Top: Pointer;
	begin
  if FPos >= 0 then
		Result := FStackArrP^[FPos]
  else
    Result := nil;
  end;

function TStack.GetCapacity: Integer;
  begin
  Result := FMaxPos;
  Inc(Result);
  end;

procedure TStack.SetCapacity(Value: Integer);
  begin
  if (Value <= FPos) or (Value > MaxStackSize) then
    raise EStackError.CreateFmt(SStackCapacityError, [Value]);
  if Value <> Capacity then
    begin
    ReallocMem(FStackArrP, Value * SizeOf(Pointer));
    Dec(Value);
    FMaxPos := Value;
    end;
  end;

function TStack.GetCount: Integer;
  begin
  Result := FPos;
  Inc(Result);
  end;

{ Does NOT free the stack memory, combine with SetCapacity(0), if desired }
procedure TStack.Clear;
  begin
  FPos := -1;
  end;

procedure TStack.CheckSwapDistance(Distance, Count: Integer);
  begin
  if (Distance < 0) or (Distance >= Count) then
    raise EStackError.CreateFmt(SSwapDistanceError, [Distance]);
  end;

procedure TStack.CheckRotateDistance(Distance, Count: Integer);
  begin
  if (Distance < 0) or (Distance >= Count) then
    raise EStackError.CreateFmt(SRotateDistanceError, [Distance]);
  end;

procedure TStack.Swap(Distance: Integer);
  var
    TmpItemP: Pointer;
    SwapPos: Integer;
  begin
  CheckSwapDistance(Distance, GetCount);
  SwapPos := FPos - Distance;
  TmpItemP := FStackArrP^[FPos];
  FStackArrP^[FPos] := FStackArrP^[SwapPos];
  FStackArrP^[SwapPos] := TmpItemP;
  end;

procedure TStack.RotateUp(Distance: Integer);
  var
    TmpItemP: Pointer;
    RotPos: Integer;
  begin
  CheckRotateDistance(Distance, GetCount);
  RotPos := FPos - Distance;
  TmpItemP := FStackArrP^[FPos];
  System.Move(FStackArrP^[RotPos], FStackArrP^[RotPos + 1],
    Distance * SizeOf(Pointer));
  FStackArrP^[RotPos] := TmpItemP;
  end;

procedure TStack.RotateDown(Distance: Integer);
  var
    TmpItemP: Pointer;
    RotPos: Integer;
  begin
  CheckRotateDistance(Distance, GetCount);
  RotPos := FPos - Distance;
  TmpItemP := FStackArrP^[RotPos];
  System.Move(FStackArrP^[RotPos + 1], FStackArrP^[RotPos],
    Distance * SizeOf(Pointer));
  FStackArrP^[FPos] := TmpItemP;
  end;

procedure TStack.Grow;
  var
    NewCapacity, Delta: Integer;
  begin
  NewCapacity := Capacity;
  Delta := NewCapacity shr 2;
  if Delta < 4 then Delta := 4;
  SetCapacity(NewCapacity + Delta);
  end;

{ TFilter }

constructor TFilter.Create(Driver: TFilterDriver);
  begin
  Assert(Driver <> nil);
  inherited Create;
  FDriver := Driver;
  FState := flsClosed;
  FProcessor := ClosedProcessor;
  end;

procedure TFilter.Close;
  begin
  if State = flsClosed then raise EFilter.Create(SFilterClosed);
  FState := flsClosing;
  end;

procedure TFilter.InternalClose;
  begin
  Processor := ClosedProcessor;
  FState := flsClosed;
  Closed;
  end;

procedure TFilter.Open;
  begin
  FState := flsOpen;
  Processor := OpenProcessor;
  end;

procedure TFilter.ClosedProcessor(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  begin
  Open;
  Processor(InStartP, InSize, OutStartP, OutSize);
  end;

procedure TFilter.OpenProcessor(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  begin
  raise EFilter.Create(SFilterOpen);
  end;

procedure TFilter.SetProcessor(Value: TFilterProc);
  begin
  if not Assigned(Value) then raise EFilter.Create(SFilterProcessorNil);
  if State = flsClosed then raise EFilter.Create(SFilterClosed)
  else FProcessor := Value;
  end;

function TFilter.NextInBuffer(var InStartP: PChar; Needed: Integer): Integer;
  var
    ProcessInBuf: TInProcessor;
  begin
  case State of
    flsOpen:
      begin
      ProcessInBuf := Driver.ProcessInBuffer;
      Result := ProcessInBuf(InStartP, Needed);
      end;
    flsClosing:
      Result := 0;
    else
      raise EFilter.Create(SFilterClosed);
    end;
  end;

function TFilter.NextOutBuffer(var OutStartP: PChar; Needed: Integer): Integer;
  var
    ProcessOutBuf: TOutProcessor;
  begin
  if State = flsClosed then raise EFilter.Create(SFilterClosed);
  ProcessOutBuf := Driver.ProcessOutBuffer;
  Result := ProcessOutBuf(OutStartP, Needed);
  FHaveOutBuffer := Result >= Needed;
  end;

procedure TFilter.Process(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  begin
  FHaveOutBuffer := True;
  // FBusy := True;
  // try
  Processor(InStartP, InSize, OutStartP, OutSize);
  // finally
  //   FBusy := False;
  //   end;
  { if the filter is closing (i.e. no more input accepted) and the
    processor returns without being forced by an unsuccessful request
    for out-buffer space, then this means that the filter has terminated }
  if (State = flsClosing) and FHaveOutBuffer then InternalClose;
  end;

procedure TFilter.Closed;
  begin
  // do nothing by default
  end;

{ TFilterDriver }

constructor TFilterDriver.Create(FilterClass: TFilterClass);
  begin
  inherited Create;
  FFilter := FilterClass.Create(Self);
  Reset;
  end;

destructor TFilterDriver.Destroy;
  begin
  FFilter.Free;
  inherited;
  end;

function TFilterDriver.InErrorProcessor(var StartP: PChar; Needed: Integer): Integer;
  begin
  raise EFilterDriver.Create(SInvalidFilterInBuffer);
  end;

function TFilterDriver.OutErrorProcessor(var StartP: PChar; Needed: Integer): Integer;
  begin
  raise EFilterDriver.Create(SInvalidFilterOutBuffer);
  end;

procedure TFilterDriver.Reset;
  begin
  FState := fdsInitialized;
  if Filter.State <> flsClosed then Filter.InternalClose;
  end;

{ TFilterWriter }

constructor TFilterWriter.Create(FilterClass: TFilterClass;
  const Target: IWriter; BufferSize: Integer = DefaultFilterBufferSize);
  begin
  FImpl := TFilterWriterImpl.Create(FilterClass, Target, BufferSize);
  end;

destructor TFilterWriter.Destroy;
  begin
  FImpl.Free;
  inherited;
  end;

function TFilterWriter.DriverState: TFilterDriverState;
  begin
  Result := Impl.State;
  end;

function TFilterWriter.GetBufferSize: Integer;
  begin
  Result := Impl.BufferSize;
  end;

function TFilterWriter.GetFilter: TFilter;
  begin
  Result := Impl.Filter;
  end;

procedure TFilterWriter.IOClose;
  begin
  with Impl do
    begin
    if (State = fdsInitialized) or (Filter.State <> flsOpen) then
      raise EWriter.Create(SWriterNotOpen);
    Filter.Close;  // Filter.State = flsOpen
    end;
  end;

function TFilterWriter.IOState: TIOState;
  begin
  if Impl.State = fdsInitialized then
    Result := iosInitialized
  else with Impl do
    begin
    case Filter.State of
      flsClosed:
        // if no data are waiting to be processed or flushed
        if (FOutStartP = FWaitingP) and (FBufStartP = FFlushP)
          and (WriteTarget.State = iosClosed) then
          Result := iosClosed
        else
          Result := iosClosing;
      flsOpen:
        Result := iosOpen;
      else  // flsClosing:
        Result := iosClosing;
      end;
    end;
  end;

{ TFilterWriterImpl }

constructor TFilterWriterImpl.Create(FilterClass: TFilterClass;
  const Target: IWriter; BufferSize: Integer);
  begin
  SetWriteTarget(Target);
  inherited Create(FilterClass);
  SetBufferSize(BufferSize);
  end;

destructor TFilterWriterImpl.Destroy;
  begin
  FreeMem(FBufferP);
  FreeMem(FInBufP);
  FreeMem(FOutBufP);
  inherited Destroy;
  end;

{$WARNINGS OFF}   // don't warn about undefined return value
function TFilterWriterImpl.Write(const Buffer; Count: Integer): Integer;
  var
    InStartP, OutStartP: PChar;
    InSize, OutSize: Integer;
  begin
  case State of
    fdsInitialized:
      if Filter.State = flsClosing then
        raise EFilterDriver.Create(SFilterClosing);
      // if Filter.State = flsClosed then Filter will open on Filter.Process
    fdsBusy:
      raise EFilterWriter.Create(SRecursiveWrite);
    fdsActive:
      if Filter.State <> flsOpen then
        raise EFilterDriver.Create(SFilterNotOpen);
    end;
  FState := fdsBusy;

  try
    // do we have output data waiting from a previous write?
    if @FProcessOutBuffer = @TFilterWriterImpl.WaitingProcessor then
      begin
      OutStartP := FWaitingP;
      OutSize := FOutBufSize - (OutStartP - FOutBufP);
      end
    else
      begin
      OutStartP := FBufStartP;
      OutSize := FBufferSize - (OutStartP - FBufferP);
      end;

    InStartP := @Buffer;
    FExternalStartP := InStartP;
    FExternalEndP := InStartP + Count;
    // do we have unprocessed data from a previous write?
    if @FProcessInBuffer = @TFilterWriterImpl.PendingProcessor then
      begin
      InStartP := FInBufP;
      InSize := FPendingP - InStartP;
      end
    else
      InSize := Count;

    // the in-buffer can be empty, since it is possible that the filter
    // will output data based on state - like, for instance, an epilog
    Filter.Process(InStartP, InSize, OutStartP, OutSize);

    if @FProcessOutBuffer = @TFilterWriterImpl.WaitingProcessor then
      FWaitingP := OutStartP
    else
      FBufStartP := OutStartP;
      
    if @FProcessInBuffer = @TFilterWriterImpl.PendingProcessor then
      begin
      Assert(FPendingP >= InstartP);
      if InStartP = FNeededP then   // done with pending buffer
        begin
        FPendingP := FInBufP;
        FNeededP := FInBufP;
        FProcessInBuffer := InProcessor;
        end
      else if InStartP > FInBufP then  // move to beginning of pending buffer
        begin
        InSize := FPendingP - InStartP;
        // re-use OutSize (mis-named here) as "Needed"
        OutSize := FNeededP - FPendingP;
        // move to beginning of buffer
        Move(InStartP^, FInBufP^, InSize);
        FPendingP := FInBufP + InSize;
        FNeededP := FPendingP + OutSize;
        end;
      Result := FExternalStartP - PChar(@Buffer);
      end
    else
      Result := InStartP - PChar(@Buffer);
  finally
    FState := fdsActive;
    end;
  end;
{$WARNINGS ON}

// closes WriteTarget if Filter.State = flsClosed and nothing left to flush
procedure TFilterWriterImpl.Flush;
  var
    InStartP, OutStartP: PChar;
    InSize, OutSize: Integer;
    Progress: Boolean;
  begin
  case State of
    fdsInitialized:
      raise EFilterDriver.Create(SWriterNotOpen);
    fdsBusy:
      raise EFilterWriter.Create(SRecursiveWrite);
    end;
  FState := fdsBusy;

  try
    repeat
      if Filter.State <> flsClosed then

        begin
        // do we have output data waiting from a previous write?
        if @FProcessOutBuffer = @TFilterWriterImpl.WaitingProcessor then
          begin
          OutStartP := FWaitingP;
          OutSize := FOutBufSize - (OutStartP - FOutBufP);
          end
        else
          begin
          OutStartP := FBufStartP;
          OutSize := FBufferSize - (OutStartP - FBufferP);
          end;
        // flushing means: no new input is arriving
        FExternalStartP := nil;
        FExternalEndP := nil;
        // do we have unprocessed data from a previous write?
        if @FProcessInBuffer = @TFilterWriterImpl.PendingProcessor then
          begin
          InStartP := FInBufP;
          InSize := FPendingP - InStartP;
          end
        else
          begin
          InStartP := nil;
          InSize := 0;
          end;

        Filter.Process(InStartP, InSize, OutStartP, OutSize);

        if @FProcessOutBuffer = @TFilterWriterImpl.WaitingProcessor then
          begin
          Progress := FWaitingP <> OutStartP;
          if Progress then FWaitingP := OutStartP;
          end
        else
          begin
          Progress := FBufStartP <> OutStartP;
          if Progress then FBufStartP := OutStartP;
          end;
        if Filter.State <> flsClosed then
          begin
          if @FProcessInBuffer = @TFilterWriterImpl.PendingProcessor then
            begin
            Assert(FPendingP >= InstartP);
            if InStartP > FInBufP then  // move to beginning of pending buffer
              begin
              InSize := FPendingP - InStartP;
              // re-use OutSize (mis-named here) as "Needed"
              OutSize := FNeededP - FPendingP;
              // move to beginning of buffer
              Move(InStartP^, FInBufP^, InSize);
              FPendingP := FInBufP + InSize;
              FNeededP := FPendingP + OutSize;
              Progress := True;
              end;
            end;
          // break if no progress was made - avoid endless loop
          if not Progress then Break;
          end;
        end

      else  // Filter.State = flsClosed

        begin
        // re-use InSize (mis-named here) as "WaitingSize"
        InSize := FWaitingP - FOutStartP;
        OutSize := FBufferSize - (FBufStartP - FBufferP);
        // keep copying and flushing as long as necessary
        while InSize > OutSize do
          begin
          Move(FOutStartP^, FBufStartP^, OutSize);
          Inc(FOutStartP, OutSize);
          Inc(FBufStartP, OutSize);
          if FlushBuffer then
            begin
            Dec(InSize, OutSize);
            OutSize := FBufferSize;
            end
          else           // stop processing
            Exit;
          end;

        // InSize (=WaitingSize) <= OutSize
        if InSize > 0 then
          begin
          Move(FOutStartP^, FBufStartP^, InSize);
          Inc(FBufStartP, InSize);
          end;
        // waiting buffer done - reset pointers
        FOutStartP := FOutBufP;
        FWaitingP := FOutBufP;

        // the target may be another TFilterWriter
        if WriteTarget.State = iosClosing then
          WriteTarget.Flush  // target should eventually change to iosClosed
        // if nothing more to write, close target
        else if FlushBuffer and (WriteTarget.State = iosOpen) then
          WriteTarget.Close;
        Break;
        end;
      until False;
  finally
    FState := fdsActive;
    end;
  end;

{ Precondition: StartP points into pending buffer (FInBufP)
  Parameters:
  - StartP indicates start of unprocessed portion of buffer
  - Needed indicates the required size of the input buffer
  Returns:
  - Size of input buffer - function failed if less than Needed }
function TFilterWriterImpl.PendingProcessor(var StartP: PChar; Needed: Integer): Integer;
  var
    NotProcessed, Rest: Integer;
  begin
  NotProcessed := FPendingP - StartP;
  Assert(NotProcessed >= 0);
  // fix Needed if necessary
  if NotProcessed >= Needed then Needed := NotProcessed + 1;

  Rest := FExternalEndP - FExternalStartP;

  if NotProcessed = 0 then  // nothing left in pending buffer, switch back
    begin
    FPendingP := FInBufP;
    FNeededP := FInBufP;
    StartP := FExternalStartP;
    FProcessInBuffer := InProcessor;
    Result := Rest;  // writer must return to renew in-buffer
    Exit;
    end;

  // NotProcessed > 0: move to beginning of pending buffer
  Move(StartP^, FInBufP^, NotProcessed);

  if Needed > FInBufSize then
    begin
    ReallocMem(FInBufP, Needed);
    FInBufSize := Needed;
    end;
  FPendingP := FInBufP + NotProcessed;
  FNeededP := FInBufP + Needed;

  StartP := FInBufP;

  // use Needed as "extra amount needed"
  Dec(Needed, NotProcessed);
  if Needed > Rest then Needed := Rest;   // cannot use more than Rest
  Move(FExternalStartP^, FPendingP^, Needed);
  Inc(FExternalStartP, Needed);
  Inc(FPendingP, Needed);
  // FNeededP already set
  // Result < requested size when "extra amount needed" > Rest
  Result := Needed + NotProcessed;
  end;

{ Precondition: StartP points into Buffer passed to Write
  Parameters:
  - StartP indicates start of unprocessed portion of buffer
  - Needed indicates the required size of the input buffer
  Returns:
  - Size of input buffer - function failed if less than Needed }
function TFilterWriterImpl.InProcessor(var StartP: PChar; Needed: Integer): Integer;
  var
    NotProcessed: Integer;
  begin
  NotProcessed := FExternalEndP - StartP;
  Assert(NotProcessed >= 0);
  // fix Needed if necessary
  if NotProcessed >= Needed then Needed := NotProcessed + 1;

  Result := NotProcessed;  // we can't renew in-buffer for writes

  // we need to store any unprocessed data in pending buffer
  if NotProcessed > 0 then
    begin
    FProcessInBuffer := PendingProcessor;
    if Needed > FInBufSize then
      begin
      ReallocMem(FInBufP, Needed);
      FInBufSize := Needed;
      end;
    Move(StartP^, FInBufP^, NotProcessed);
    // what we didn't process, we copied
    FExternalStartP := FExternalEndP;
    StartP := FInBufP;
    FPendingP := StartP + NotProcessed;
    FNeededP := StartP + Needed;
    end
  else
    FExternalStartP := StartP;
  end;

{ Precondition: StartP pointing into waiting buffer (FOutBufP)
  Parameters:
  - StartP indicates start of free portion of waiting buffer
  - Needed indicates the required size of the output buffer
  Returns:
  - Size of new output buffer - function failed if less than Needed
  Notes:
  - will copy any data already in waiting buffer to out-buffer,
    flushing it, if necessary, and then check if the out-buffer has
    enough space left for the Needed amount of bytes
  - if yes, will reset waiting buffer and return StartP pointing
    to the regular write buffer
  - if not, will extend the waiting buffer to a size of at least
    Needed bytes and return StartP pointing to the beginning of it }
function TFilterWriterImpl.WaitingProcessor(var StartP: PChar; Needed: Integer): Integer;
  var
    WaitingSize, OutSize: Integer;
  begin
  OutSize := FOutBufSize - (StartP - FOutBufP);     // waiting buffer
  Assert(OutSize >= 0);
  // fix Needed if necessary
  if OutSize >= Needed then Needed := OutSize + 1;

  WaitingSize := StartP - FOutStartP;
  OutSize := FBufferSize - (FBufStartP - FBufferP);  // out-buffer
  // keep copying and flushing as long as necessary
  while WaitingSize > OutSize do
    begin
    Move(FOutStartP^, FBufStartP^, OutSize);
    Inc(FOutStartP, OutSize);
    Inc(FBufStartP, OutSize);
    if FlushBuffer then
      begin
      Dec(WaitingSize, OutSize);
      OutSize := FBufferSize;
      end
    else    // return when FlushBuffer fails
      begin
      FWaitingP := StartP;
      Result := FOutBufSize - (StartP - FOutBufP);
      Exit;
      end;
    end;

  // WaitingSize <= OutSize
  Move(FOutStartP^, FBufStartP^, WaitingSize);
  Inc(FBufStartP, WaitingSize);
  Dec(OutSize, WaitingSize);  // space left in write buffer

  // at this point, the waiting buffer has all been copied
  // to the out buffer and/or been flushed from there

  // if enough space in out buffer, switch to regular write buffer
  if Needed <= OutSize then
    begin
    // waiting buffer done - reset pointers
    FOutStartP := FOutBufP;
    FWaitingP := FOutBufP;
    FProcessOutBuffer := OutProcessor;
    StartP := FBufStartP;
    Result := OutSize;
    end
  else    // need more, extend waiting buffer
    begin
    if Needed > FOutBufSize then
      begin
      ReallocMem(FOutBufP, Needed);
      FOutBufSize := Needed;
      end;
    StartP := FOutBufP;
    FOutStartP := StartP;
    FWaitingP := StartP;
    Result := Needed;
    end;
  end;

{ Preconditions:
  - StartP pointing into write buffer (FBufferP)
  - no data in waiting buffer
  Parameters:
  - StartP indicates start of free portion of write buffer (out-buffer)
  - Needed indicates the required size of the output buffer
  Returns:
  - Size of new output buffer - function failed if less than Needed
  Notes:
  - will first check the out-buffer and flush it, if full
  - then will check if the out-buffer has enough free space (>= Needed)
    and if yes, will return StartP pointing to the free space
  - if no, will open up a waiting buffer with at least the needed size }
function TFilterWriterImpl.OutProcessor(var StartP: PChar; Needed: Integer): Integer;
  var
    OutSize: Integer;
  begin
  OutSize := FBufferSize - (StartP - FBufferP);
  Assert(OutSize >= 0);
  // fix Needed if necessary
  if OutSize >= Needed then Needed := OutSize + 1;

  FBufStartP := StartP;
  if OutSize = 0 then     // flush out-buffer when full
    begin
    if FlushBuffer then   // may update FBufStartP
      OutSize := FBufferSize
    else            // could not flush everything, therefore return
      begin
      StartP := FBufStartP;
      Result := FBufferSize - (StartP - FBufferP);
      Exit;
      end;
    end;

  // if sufficient space in buffer, simply return remaining buffer
  if Needed <= OutSize then
    begin
    StartP := FBufStartP;  // could have been changed by FlushBuffer
    Result := OutSize;
    end
  else              // not enough space, open up waiting buffer
    begin
    if Needed > FOutBufSize then
      begin
      ReallocMem(FOutBufP, Needed);
      FOutBufSize := Needed;
      end;
    FProcessOutBuffer := WaitingProcessor;
    StartP := FOutBufP;
    FOutStartP := StartP;
    FWaitingP := StartP;
    Result := Needed;
    end;
  end;

procedure TFilterWriterImpl.SetBufferSize(Value: Integer);
  var
    OldBufP: PChar;
    Delta: Integer;
  begin
  if Value <= 0 then raise EFilterWriter.Create(SBufferSizeNotPositive);
  OldBufP := FBufferP;
  ReallocMem(FBufferP, Value);
  FBufferSize := Value;
  Delta := FBufferP - OldBufP;
  //adjust relative pointers
  Inc(FBufStartP, Delta);
  Inc(FFlushP, Delta);
  end;

// Returns true if all data could be flushed;
// Note: may move data around in buffer and update FBufStartP, FFlushP
function TFilterWriterImpl.FlushBuffer: Boolean;
  var
    FlushSize, Written: Integer;
  begin
  FlushSize := FBufStartP - FFlushP;
  Result := FlushSize = 0;
  if not Result then
    begin
    Written := WriteTarget.Write(FFlushP^, FlushSize);
    Dec(FlushSize, Written);
    Result := FlushSize = 0;
    end;
  if Result then
    begin
    FFlushP := FBufferP;
    FBufStartP := FBufferP;
    end
  else
    begin
    Inc(FFlushP, Written);
    // if less than half of buffer left, move it to beginning
    if FlushSize < (FBufferSize shr 1) then
      begin
      Move(FFlushP^, FBufferP^, FlushSize);
      FFlushP := FBufferP;
      FBufStartP := FBufferP + FlushSize;
      end;
    end;
  end;

function TFilterWriterImpl.NeededData: Integer;
  begin
  Result := FNeededP - FPendingP;
  end;

procedure TFilterWriterImpl.ClearNeeded;
  begin
  inherited;
  FNeededP := FPendingP;
  end;

procedure TFilterWriterImpl.GetFlushData(out StartP, EndP: PChar);
  begin
  StartP := FBufStartP;
  EndP := FFlushP;
  end;

procedure TFilterWriterImpl.GetPendingData(out StartP, EndP: PChar);
  begin
  StartP := FInBufP;
  EndP := FPendingP;
  end;

procedure TFilterWriterImpl.GetWaitingData(out StartP, EndP: PChar);
  begin
  StartP := FOutStartP;
  EndP := FWaitingP;
  end;

procedure TFilterWriterImpl.Reset;
  begin
  inherited Reset;
  FProcessInBuffer := InProcessor;
  FProcessOutBuffer := OutProcessor;
  FPendingP := FInBufP;
  FNeededP := FInBufP;
  FOutStartP := FOutBufP;
  FWaitingP := FOutBufP;
  FBufStartP := FBufferP;
  FFlushP := FBufferP;
  WriteTarget.Reset;
  end;

function TFilterWriterImpl.GetProcessInBuffer: TInProcessor;
  begin
  Result := FProcessInBuffer;
  end;

function TFilterWriterImpl.GetProcessOutBuffer: TOutProcessor;
  begin
  Result := FProcessOutBuffer;
  end;

procedure TFilterWriterImpl.SetWriteTarget(const Value: IWriter);
  begin
  if not Assigned(Value) then raise EFilterWriter.Create(STargetNil);
  FWriteTarget := Value;
  end;

{ TByteSwapFilter }

function TByteSwapFilter.CheckInput: Boolean;
  begin
  Result := Driver.NeededData <> 1;
  end;

procedure TByteSwapFilter.Open;
  begin
  inherited Open;
  Processor := SwapBytes;
  end;

procedure TByteSwapFilter.SwapBytes(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  var
    InP: PChar;
    OutP: PChar;
  begin
  InP := InStartP;
  OutP := OutStartP;
  while True do
    begin
    if OutSize < 2 then
      begin
      OutSize := NextOutBuffer(OutP, 2);
      if OutSize < 2 then Break;
      end;
    if InSize < 2 then
      begin
      InSize := NextInBuffer(InP, 2);
      if InSize < 2 then Break;
      end;
    PWord(OutP)^ := Swap(PWord(InP)^);
    Inc(PWord(OutP));
    Inc(PWord(InP));
    Dec(OutSize, SizeOf(Word));
    Dec(InSize, SizeOf(Word));
    end;
  InStartP := InP;
  OutStartP := OutP;
  end;

{ TByteToWordFilter }

procedure TByteToWordFilter.ByteToWord(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  var
    InP: PChar;
    OutP: PChar;
  begin
  InP := InStartP;
  OutP := OutStartP;
  while True do
    begin
    if OutSize < SizeOf(Word) then
      begin
      OutSize := NextOutBuffer(OutP, SizeOf(Word));
      if OutSize < SizeOf(Word) then Break;
      end;
    if InSize = 0 then
      begin
      InSize := NextInBuffer(InP, 1);
      if InSize = 0 then Break;
      end;
    PWord(OutP)^ := PByte(InP)^;
    Inc(PWord(OutP));
    Inc(PByte(InP));
    Dec(OutSize,SizeOf(Word));
    Dec(InSize);
    end;
  InStartP := InP;
  OutStartP := OutP;
  end;

procedure TByteToWordFilter.Open;
  begin
  inherited Open;
  Processor := ByteToWord;
  end;

{ TBufferedCopyFilter }

procedure TBufferedCopyFilter.CopyBytes(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  var
    BufSize: Longint;
  begin
  while True do
    begin
    // if in-buffer exhausted, request more
    if InSize = 0 then
      begin
      InSize := NextInBuffer(InStartP, 1);
      if InSize = 0 then Exit;
      end;
    // if out-buffer exhausted, request at least one more byte
    if OutSize = 0 then
      begin
      OutSize := NextOutBuffer(OutStartP, 1);
      if OutSize = 0 then Exit;
      end;
    BufSize := InSize;
    if OutSize < BufSize then
      begin
      BufSize := OutSize;
      OutSize := 0;
      Dec(InSize, BufSize);
      end
    else
      begin
      Dec(OutSize, BufSize);
      InSize := 0;
      end;
    Move(InStartP^, OutStartP^, BufSize);
    Inc(InStartP, BufSize);
    Inc(OutStartP, BufSize);
    end;
  end;

procedure TBufferedCopyFilter.Open;
  begin
  inherited Open;
  Processor := CopyBytes;
  end;

{ TBase64Encoder }

const
  B64EncTable: array [0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  B64PadChar: Char = '=';

procedure TBase64Encoder.Close;
  begin
  // we consider the input complete now, even if not a full Base64 quantum
  Driver.ClearNeeded;
  Processor := EncodeEpilog;
  inherited Close;
  // InternalClose will be called at end of EncodeEpilog
  end;

constructor TBase64Encoder.Create(Driver: TFilterDriver);
  begin
  inherited Create(Driver);
  FLineLength := 76;  // as prescribed by RFC 1521
{$IFDEF MSWINDOWS}
  FUnixLineBreak := False;
{$ENDIF}
{$IFDEF LINUX}
  FUnixLineBreak := True;
{$ENDIF}
  end;

const
  LineBreakSize: array[Boolean] of Word = (2, 1);

function InsertLineBreak(Unix: Boolean; var BufP: PChar): PChar;
  begin
  if not Unix then
    begin
    BufP^ := #13;
    Inc(BufP);
    end;
  BufP^ := #10;
  Inc(BufP);
  Result := BufP;
  end;

procedure TBase64Encoder.Encode(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  var
    InP, OutP: PChar;
    LineBreakP: PChar;
    AddLineBreak: Boolean;
    NeededSize: Shortint;
  begin
  // make local copies to avoid frequent dereferencing
  InP := InStartP;
  OutP := OutStartP;
  LineBreakP := OutP + LineLength - CurrentLength;
  while True do
    begin
    if InSize < 3 then
      begin
      InSize := NextInBuffer(InP, 3);
      if InSize < 3 then
        begin
        CurrentLength := LineLength - (LineBreakP - OutP);
        if State = flsClosing then  // end of input - epilog
          EncodeEpilog(InP, InSize, OutP, OutSize);
        Break;
        end;
      end;

    // check if we will exceed allowable line length
    NeededSize := 4;
    AddLineBreak := (OutP + NeededSize) > LineBreakP;
    if AddLineBreak then
      NeededSize := NeededSize + LineBreakSize[UnixLineBreak];
    if OutSize < NeededSize then
      begin
      // update CurrentLength before OutP can be changed
      CurrentLength := LineLength - (LineBreakP - OutP);
      OutSize := NextOutBuffer(OutP, NeededSize);
      if OutSize < NeededSize then
        Break
      else
        // OutP may have changed - update LineBreakP
        LineBreakP := OutP + LineLength - CurrentLength;
      end;

    // first output character
    if LineBreakP = OutP then
      OutP := InsertLineBreak(UnixLineBreak, LineBreakP);
    OutP^ := B64EncTable[(Byte(InP^) and $FC) shr 2];
    Inc(OutP);
    // second output character
    if LineBreakP = OutP then
      OutP := InsertLineBreak(UnixLineBreak, LineBreakP);
    OutP^ := B64EncTable[((Byte(InP^) and $03) shl 4) or
      ((Byte(InP[1]) and $F0) shr 4)];
    Inc(OutP);
    Inc(InP);
    // third output character
    if LineBreakP = OutP then
      OutP := InsertLineBreak(UnixLineBreak, LineBreakP);
    OutP^ := B64EncTable[((Byte(InP^) and $0F) shl 2) or
      ((Byte(InP[1]) and $C0) shr 6)];
    Inc(OutP);
    Inc(InP);
    // fourth output character
    if LineBreakP = OutP then
      OutP := InsertLineBreak(UnixLineBreak, LineBreakP);
    OutP^ := B64EncTable[Byte(InP^) and $3F];
    Inc(OutP);
    Inc(InP);

    Dec(OutSize, NeededSize);
    Dec(InSize, 3);

    if AddLineBreak then Inc(LineBreakP, LineLength);
    end;

  InStartP := InP;
  OutStartP := OutP;
  end;

// if there any unconverted data left (partial input quantum) then they
// are waiting in the pending buffer for processing
procedure TBase64Encoder.EncodeEpilog(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  var
    LineBreakP: PChar;
    AddLineBreak: Boolean;
    NeededSize: Shortint;
  begin
  // there should not be more data in the in-buffer than was ever
  // requested as the largest "needed" size (= FNeededP - FInBufP)
  if InSize > 3 then raise EFilter.Create(SExtraDataFound)
  else if InSize > 0 then  // we have 1, 2 or 3 bytes left to encode
    begin
    LineBreakP := OutStartP + LineLength - CurrentLength;
    // check if we will exceed allowable line length
    NeededSize := 4;
    AddLineBreak := (OutStartP + NeededSize) > LineBreakP;
    if AddLineBreak then
      NeededSize := NeededSize + LineBreakSize[UnixLineBreak];
    if OutSize < NeededSize then
      begin
      OutSize := NextOutBuffer(OutStartP, NeededSize);
      if OutSize < NeededSize then Exit;
      end;

    // first output character
    if LineBreakP = OutStartP then
      OutStartP := InsertLineBreak(UnixLineBreak, LineBreakP);
    OutStartP^ := B64EncTable[(Byte(InStartP^) and $FC) shr 2];
    Inc(OutStartP);
    // second output character
    if LineBreakP = OutStartP then
      OutStartP := InsertLineBreak(UnixLineBreak, LineBreakP);
    if InSize = 1 then
      begin
      OutStartP^ := B64EncTable[((Byte(InStartP^) and $03) shl 4)];
      Inc(OutStartP);
      Inc(InStartP);
      // third output character
      if LineBreakP = OutStartP then
        OutStartP := InsertLineBreak(UnixLineBreak, LineBreakP);
      OutStartP^ := B64PadChar;
      Inc(OutStartP);
      // fourth output character
      if LineBreakP = OutStartP then
        OutStartP := InsertLineBreak(UnixLineBreak, LineBreakP);
      OutStartP^ := B64PadChar;
      end
    else  // InSize = 2 or 3
      begin
      OutStartP^ := B64EncTable[((Byte(InStartP^) and $03) shl 4) or
        ((Byte(InStartP[1]) and $F0) shr 4)];
      Inc(OutStartP);
      Inc(InStartP);
      // third output character
      if LineBreakP = OutStartP then
        OutStartP := InsertLineBreak(UnixLineBreak, LineBreakP);
      if InSize = 2 then
        begin
        OutStartP^ := B64EncTable[((Byte(InStartP^) and $0F) shl 2)];
        Inc(OutStartP);
        Inc(InStartP);
        // fourth output character
        if LineBreakP = OutStartP then
          OutStartP := InsertLineBreak(UnixLineBreak, LineBreakP);
        OutStartP^ := B64PadChar;
        end
      else  // InSize = 3
        begin
        OutStartP^ := B64EncTable[((Byte(InStartP^) and $0F) shl 2) or
          ((Byte(InStartP[1]) and $C0) shr 6)];
        Inc(OutStartP);
        Inc(InStartP);
        // fourth output character
        if LineBreakP = OutStartP then
          OutStartP := InsertLineBreak(UnixLineBreak, LineBreakP);
        OutStartP^ := B64EncTable[Byte(InStartP^) and $3F];
        Inc(InStartP);
        end;
      end;
    Inc(OutStartP);
    end;
  end;

procedure TBase64Encoder.Open;
  begin
  inherited Open;
  Processor := Encode;
  CurrentLength := 0;
  end;

{ TBase64Decoder }

const
  { decode table is reverse mapping of encode table:
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'; }
  B64DecTable: array [$2B..$7A] of Byte =   // in order of Ord(Char)
  //  +   ,   -   .  /  0  1  2  3  4  5  6  7  8  9   :
    (62,$FF,$FF,$FF,63,52,53,54,55,56,57,58,59,60,61,$FF,
  //  ;   <   =   >   ?   @  A  B  C  D  E  F  G  H  I  J
    $FF,$FF,$FE,$FF,$FF,$FF, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  //  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
     10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
  //  [   \   ]   ^   _   `  a  b  c  d  e  f  g  h  i  j
    $FF,$FF,$FF,$FF,$FF,$FF,26,27,28,29,30,31,32,33,34,35,
  //  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
     36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51);

  B64Invalid = $FF;
  B64Eof = $FE;

function TBase64Decoder.CheckInput: Boolean;
  begin
  Result := Driver.NeededData = 0;
  end;

procedure TBase64Decoder.Closed;
  begin
  // if we need a full chunk then we are not missing data from a previous one
  if Driver.NeededData = 4 then Driver.ClearNeeded;
  end;

constructor TBase64Decoder.Create(Driver: TFilterDriver);
  begin
  inherited Create(Driver);
  FStrictNess := b64IgnoreAll;  // as per rfc 1521
  end;

type
  TInputChunk = array[0..3] of Byte;
  
procedure InvalidEncoding(Value: Char);
  begin
  raise EFilter.CreateFmt(SInvalidBase64Encoding, [Value]);
  end;

procedure TBase64Decoder.Decode(var InStartP: PChar; InSize: Integer;
  var OutStartP: PChar; OutSize: Integer);
  label
    ExitWithArgs;
  var
    Inp, OutP: PChar;
    Input: TInputChunk;
    InputP: PChar;
    InputSize: Integer;
    InIndx, InByte: Byte;
    Strict: TBase64Strictness;
    Eod: Boolean;
  begin
  // make local copies to avoid frequent dereferencing
  InP := InStartP;
  OutP := OutStartP;
  Eod := False;
  Strict := Strictness;
  while not Eod do
    begin
    // gather 4 input characters, ignoring non-Base64 data as configured,
    // and requesting more input as needed
    repeat
      InIndx := 0;
      // we must not call NextInBuffer with an updated InP until
      // the input data have been processed and written to OutP
      InputP := InP;
      while InSize > 0 do
        begin
        InByte := Byte(InputP^);
        case InByte of
          $2B..$7A:  //  within range of decoding table indices
            begin
            InByte := B64DecTable[InByte];
            case InByte of
              0..63:  // valid Base64 encoding character
                begin
                Input[InIndx] := InByte;
                Inc(InIndx);
                end;
              B64Eof:    // '=' only valid for InIndx = 2 or 3
                case InIndx of
                  0, 1:
                    if Strict <> b64IgnoreAll then InvalidEncoding(InputP^);
                  2:  // to *not* be ignored, the next input must be '=' also
                    begin
                    Inc(InputP);
                    Dec(InSize);
                    if InSize <= 0 then  // not enough input
                      begin
                      Inc(InIndx);  // don't know yet if we can ignore it
                      Break;
                      end;
                    InByte := B64DecTable[Byte(InputP^)];
                    if InByte = B64Eof then  // next input is '=' - end of data
                      begin
                      Input[InIndx] := B64Invalid;  // end of input marker
                      Inc(InIndx, 2);
                      Eod := True;
                      end
                    else
                      if Strict <> b64IgnoreAll then InvalidEncoding(InputP^);
                    end;
                  3:  // found end of encoding
                    begin
                    Input[InIndx] := B64Invalid;  // end of input marker
                    Inc(InIndx);
                    Eod := True;
                    end;
                  end;
              B64Invalid:  // we may want to ignore invalid input characters
                if Strict <> b64IgnoreAll then InvalidEncoding(InputP^);
              else
                raise EFilter.Create(SUnexpectedFilterState);
              end;
            end;
          $0A, $0D, $20, $09:  // we may want to ignore whitespace
            if Strict = b64Strict then InvalidEncoding(InputP^);
          else                 // we may ignore other characters
            if Strict <> b64IgnoreAll then InvalidEncoding(InputP^);
          end;
        Inc(InputP);
        Dec(InSize);
        // do we have enough input for one chunk of output?
        if InIndx > 3 then Break;
        end;

      if Eod then
        Break
      else if InIndx < 4 then
        begin
        InputSize := InputP - InP + 4 - InIndx;
        InSize := NextInBuffer(InP, InputSize);
        if InSize < InputSize then goto ExitWithArgs;
        end;
      until InIndx > 3;

    if OutSize < 3 then
      begin
      OutSize := NextOutBuffer(OutP, 3);
      if OutSize < 3 then goto ExitWithArgs;
      end;
    // now we have enough input and enough space for output

    // if we hit "end of data" in the data stream then there
    // were less than 3 bytes to encode ==> max two bytes of output
    if Eod then
      begin
      PByte(OutP)^ := (Input[0] shl 2) or ((Input[1] and $30) shr 4);
      Inc(OutP);
      if Input[2] <> B64Invalid then  // we have more data to generate
        begin
        PByte(OutP)^ :=
          ((Input[1] and $0F) shl 4) or ((Input[2] and $3C) shr 2);
        Inc(OutP);
        end;
      // call Close only after the last input has been processed and
      // written out, since we won't have another chance to process it
      Close;
      end
    else   // full quantum of input ==> 3 bytes of output
      begin
      PByte(OutP)^ := (Input[0] shl 2) or ((Input[1] and $30) shr 4);
      Inc(OutP);
      PByte(OutP)^ :=
        ((Input[1] and $0F) shl 4) or ((Input[2] and $3C) shr 2);
      Inc(OutP);
      PByte(OutP)^ := ((Input[2] and $03) shl 6) or (Input[3] and $3F);
      Inc(OutP);
      Dec(OutSize, 3);
      end;
    // update InP for correct progress reporting
    InP := InputP;
    end;

ExitWithArgs:
  InStartP := InP;
  OutStartP := OutP;
  end;

procedure TBase64Decoder.Open;
  begin
  inherited Open;
  Processor := Self.Decode;
  end;

{ TFilterReader }

constructor TFilterReader.Create(FilterClass: TFilterClass;
  const Target: IReader; BufferSize: Integer = DefaultFilterBufferSize);
  begin
  FImpl := TFilterReaderImpl.Create(FilterClass, Target, BufferSize);
  end;

destructor TFilterReader.Destroy;
  begin
  FImpl.Free;
  inherited;
  end;

function TFilterReader.DriverState: TFilterDriverState;
  begin
  Result := Impl.State;
  end;

function TFilterReader.GetBufferSize: Integer;
  begin
  Result := Impl.BufferSize;
  end;

function TFilterReader.GetFilter: TFilter;
  begin
  Result := Impl.Filter;
  end;

procedure TFilterReader.IOClose;
  begin
  with Impl do
    begin
    if (State = fdsInitialized) or (Filter.State <> flsOpen) then
      raise EReader.Create(SReaderNotOpen);
    Filter.Close;  // Filter.State = flsOpen
    end;
  end;

function TFilterReader.IOState: TIOState;
  begin
  if Impl.State = fdsInitialized then
    Result := iosInitialized
  else with Impl do
    begin
    case Filter.State of
      flsClosed:
        // if there are no data waiting in output buffer
        if FOutStartP = FWaitingP then
          Result := iosClosed
        else
          Result := iosClosing;
      flsOpen:
        Result := iosOpen;
      else  // flsClosing:
        Result := iosClosing;
      end;
    end;
  end;

{ TFilterReaderImpl }

procedure TFilterReaderImpl.ClearNeeded;
  begin
  inherited;
  FNeededP := FPendingP;
  end;

constructor TFilterReaderImpl.Create(FilterClass: TFilterClass;
  const Target: IReader; BufferSize: Integer);
  begin
  SetReadTarget(Target);
  inherited Create(FilterClass);
  SetBufferSize(BufferSize);
  end;

destructor TFilterReaderImpl.Destroy;
  begin
  FreeMem(FBufferP);
  FreeMem(FInBufP);
  FreeMem(FOutBufP);
  inherited Destroy;
  end;

function TFilterReaderImpl.Fill: Integer;
  begin
  if FBufStartP < FFillP then
    raise EFilterReader.Create(SReadBufferHasData);
  if (State <> fdsInitialized) or (Filter.State <> flsOpen) then
    raise EFilterReader.Create(SReaderNotOpen);
  Result := FillBuffer;
  end;

{ Prerequisite: may only be call when buffer empty
  Notes:
  - does not check for FBufStartP = FFillP
  - returns 0 if Filter is closing or Target is closed }
function TFilterReaderImpl.FillBuffer: Integer;
  begin
  case Filter.State of
    flsClosed: raise EFilterDriver.Create(SFilterClosed);
    flsClosing: Result := 0;
    flsOpen:
      begin
      if ReadTarget.State = iosClosed then
        begin
        Filter.Close;
        Result := 0;
        end
      else
        Result := ReadTarget.Read(FBufferP^, FBufferSize);
      end;
    else
      Result := 0;
    end;
  FBufStartP := FBufferP;
  FFillP := FBufStartP + Result;
  end;

function TFilterReaderImpl.GetProcessInBuffer: TInProcessor;
  begin
  Result := FProcessInBuffer;
  end;

function TFilterReaderImpl.GetProcessOutBuffer: TOutProcessor;
  begin
  Result := FProcessOutBuffer;
  end;

{ Precondition: StartP must already point into in-buffer (FBufferP)
  Parameters:
  - StartP indicates start of unprocessed portion of buffer
  - Needed indicates requested buffer size
  Returns:
  - Size of allocated buffer (if < Needed then function failed)
  Notes: the following cases are handled:
  - StartP at end of buffer:
    all data have been processed, but more data is welcome
  - StartP < end of buffer:
    the caller did not process all data, and requires more
    to be able to process them, and to reach a valid state }
function TFilterReaderImpl.InProcessor(var StartP: PChar; Needed: Integer): Integer;
  var
    NotProcessed: Integer;
  begin
  NotProcessed := FFillP - StartP;
  Assert(NotProcessed >= 0);
  // fix Needed if necessary
  if NotProcessed >= Needed then Needed := NotProcessed + 1;

  if NotProcessed = 0 then
    begin
    NotProcessed := FillBuffer;  // updates FBufStartP
    // can we provide needed amount from read buffer?
    if NotProcessed >= Needed then
      begin
      StartP := FBufStartP;
      Result := NotProcessed;
      Exit;
      end;
    end
  else
    FBufStartP := StartP;

  // we have unprocessed data:
  // open pending buffer to store unprocessed data and receive needed data
  FProcessInBuffer := PendingProcessor;
  if Needed > FInBufSize then
    begin
    ReallocMem(FInBufP, Needed);
    FInBufSize := Needed;
    end;

  FPendingP := FInBufP;
  while NotProcessed < Needed do
    begin
    Move(FBufStartP^, FPendingP^, NotProcessed);
    Inc(FPendingP, NotProcessed);
    // Inc(FBufStartP, NotProcessed);  not needed - we call FillBuffer anyway
    Dec(Needed, NotProcessed);
    NotProcessed := FillBuffer;
    if NotProcessed < FBufferSize then Break;
    end;

  // move only what we need
  if Needed < NotProcessed then
    begin
    NotProcessed := Needed;
    Needed := 0;
    end
  else
    Dec(Needed, NotProcessed);

  Move(FBufStartP^, FPendingP^, NotProcessed);
  Inc(FPendingP, NotProcessed);
  Inc(FBufStartP, NotProcessed);
  StartP := FInBufP;
  FNeededP := FPendingP + Needed;
  Result := FPendingP - StartP;
  end;

function TFilterReaderImpl.NeededData: Integer;
  begin
  Result := FNeededP - FPendingP;
  end;

{ Precondition:
  - StartP pointing into Buffer parameter of Read
  - no data in waiting buffer
  Parameters:
  - StartP indicates start of free portion of Read buffer argument (out-buffer)
  - Needed indicates requested buffer size
  Returns:
  - Size of allocated buffer (if < Needed then function failed)
  Note: if there are unprocessed data left then this is assumed to mean that
        there was not enough space for a complete chunk of output;
        therefore a separate waiting buffer of at least the needed size
        is opened up and StartP point to it                                   }
function TFilterReaderImpl.OutProcessor(var StartP: PChar; Needed: Integer): Integer;
  var
    NotProcessed: Integer;
  begin
  NotProcessed := FExternalEndP - StartP;
  Assert(NotProcessed >= 0);
  // fix Needed if necessary
  if NotProcessed >= Needed then Needed := NotProcessed + 1;

  FExternalStartP := StartP;

  if NotProcessed = 0 then
    Result := 0
  else
    // if we did not use up all of the out-buffer then we need to
    // open up a waiting buffer to complete a partial output chunk
    begin
    if Needed > FOutBufSize then
      begin
      ReallocMem(FOutBufP, Needed);
      FOutBufSize := Needed;
      end;
    FProcessOutBuffer := WaitingProcessor;
    StartP := FOutBufP;
    FOutStartP := StartP;
    FWaitingP := StartP;
    Result := Needed;
    end;
  end;

{ Precondition: StartP must already point into FInBufP
  Parameters:
  - StartP indicates start of unprocessed portion of buffer
  - Needed indicates requested buffer size
  Returns:
  - Size of allocated buffer (if < Needed then function failed)
  Note: the following cases are handled:
  - StartP at end of buffer:
    all data have been processed, but more is welcome
  - StartP < end of buffer:
    the caller did not process all data, and requires more
    to be able to process them, and to reach a valid state }
function TFilterReaderImpl.PendingProcessor(var StartP: PChar; Needed: Integer): Integer;
  var
    NotProcessed, Rest: Integer;
  begin
  NotProcessed := FPendingP - StartP;
  Assert(NotProcessed >= 0);
  // fix Needed if necessary
  if NotProcessed >= Needed then Needed := NotProcessed + 1;

  Rest := FFillP - FBufStartP;
  if NotProcessed > 0 then
    Move(StartP^, FInBufP^, NotProcessed)  // move to beginning of buffer
  else
    begin
    if Rest = 0 then Rest := FillBuffer;
    if Rest >= Needed then  // we are done processing pending buffer
      begin
      FPendingP := FInBufP;
      FNeededP := FInBufP;
      FProcessInBuffer := InProcessor;
      StartP := FBufStartP;
      Result := Rest;
      Exit;
      end;
    end;

  // we have unprocessed data and/or need more than we can get from FBufferP
  if Needed > FInBufSize then
    begin
    ReallocMem(FInBufP, Needed);
    FInBufSize := Needed;
    end;

  // we already moved data to beginning of buffer
  FPendingP := FInBufP + NotProcessed;
  Dec(Needed, NotProcessed);
  
  while Rest < Needed do
    begin
    Move(FBufStartP^, FPendingP^, Rest);  // empty in-buffer
    Inc(FPendingP, Rest);
    // Inc(FBufStartP, Rest);  not needed - we call FillBuffer anyway
    Dec(Needed, Rest);
    Rest := FillBuffer;
    if Rest < FBufferSize then Break;
    end;

  // move only what we need
  if Needed < Rest then
    begin
    Rest := Needed;
    Needed := 0;
    end
  else
    Dec(Needed, Rest);

  Move(FBufStartP^, FPendingP^, Rest);
  Inc(FPendingP, Rest);
  Inc(FBufStartP, Rest);
  StartP := FInBufP;
  FNeededP := FPendingP + Needed;
  Result := FPendingP - StartP;
  end;

function TFilterReaderImpl.Read(var Buffer; Count: Integer): Integer;
  var
    InSize, OutSize: Integer;
    InStartP, OutStartP: PChar;
    BuffersOnly: Boolean;
  begin
  case State of
    fdsInitialized:
      begin
      if Filter.State = flsClosing then
        raise EFilterDriver.Create(SFilterClosing);
      BuffersOnly := False;
      end;
    fdsBusy:
      raise EFilterReader.Create(SRecursiveRead);
    fdsActive:
      begin
      // if closed then Filter will open on Filter.Process
      BuffersOnly := Filter.State = flsClosed;
      if BuffersOnly and (FOutStartP = FWaitingP) then
        raise EFilterReader.Create(SReaderClosed);
      end
    else
      BuffersOnly := False;
    end;
  FState := fdsBusy;
  
  try
    // check output state (waiting or buffer)
    if @FProcessOutBuffer = @TFilterReaderImpl.WaitingProcessor then
      begin
      // abuse InSize (mis-named here) as "WaitingSize"
      InSize := FWaitingP - FOutStartP;
      if InSize > Count then  // fill read buffer from waiting buffer
        begin
        Move(FOutStartP^, Buffer, Count);
        Inc(FOutStartP, Count);
        Result := Count;
        Exit;
        end
      else       // move rest of waiting data to read buffer
        begin
        Move(FOutStartP^, Buffer, InSize);
        FOutStartP := FWaitingP;
        Result := InSize;
        FProcessOutBuffer := OutProcessor;
        // if Filter is closed then return
        if BuffersOnly or (InSize = Count) then Exit;
        OutStartP := PChar(@Buffer) + InSize;
        OutSize := Count - InSize;
        end;
      end
    else if BuffersOnly then
      begin
      Result := 0;
      Exit;
      end
    else
      begin
      OutStartP := @Buffer;
      OutSize := Count;
      end;

    FExternalStartP := OutStartP;
    FExternalEndP := OutStartP + OutSize;

    // check input state (pending or buffer)
    if @FProcessInBuffer = @TFilterReaderImpl.PendingProcessor then
      begin
      InStartP := FInBufP;
      InSize := FPendingP - InStartP;
      end
    else
      begin
      InStartP := FBufStartP;
      InSize := FFillP - InStartP;
      end;

    Filter.Process(InStartP, InSize, OutStartP, OutSize);

    if @FProcessOutBuffer = @TFilterReaderImpl.WaitingProcessor then
      begin
      FWaitingP := OutStartP;
      OutSize := FExternalEndP - FExternalStartP;
      // re-use InSize (mis-named here) as "WaitingSize"
      InSize := OutStartP - FOutStartP;
      // fill rest of read buffer from waiting buffer
      if InSize > OutSize then
        begin
        Move(FOutStartP^, FExternalStartP^, OutSize);
        Inc(FOutStartP, OutSize);
        Result := Count;
        end
      else       // move rest of waiting data to read buffer
        begin
        Move(FOutStartP^, FExternalStartP^, InSize);
        FOutStartP := FWaitingP;
        Result := InSize + (FExternalStartP - PChar(@Buffer));
        FProcessOutBuffer := OutProcessor;
        end;
      end
    else
      Result := OutStartP - PChar(@Buffer);

    if @FProcessInBuffer = @TFilterReaderImpl.PendingProcessor then
      begin
      Assert(FPendingP >= InStartP);
      if InStartP = FNeededP then   // done with pending buffer
        begin
        FPendingP := FInBufP;
        FNeededP := FInBufP;
        FProcessInBuffer := InProcessor;
        end
      else if InStartP > FInBufP then  // move to beginning of pending buffer
        begin
        InSize := FPendingP - InStartP;
        // re-use OutSize (mis-named here) as "Needed"
        OutSize := FNeededP - FPendingP;
        // move to beginning of buffer
        Move(InStartP^, FInBufP^, InSize);
        FPendingP := FInBufP + InSize;
        FNeededP := FPendingP + OutSize;
        end;
      end
    else
      FBufStartP := InStartP;
  finally
    FState := fdsActive;
    end;
  end;

procedure TFilterReaderImpl.Reset;
  begin
  inherited Reset;
  FProcessInBuffer := InProcessor;
  FProcessOutBuffer := OutProcessor;
  FPendingP := FInBufP;
  FNeededP := FInBufP;
  FOutStartP := FOutBufP;
  FWaitingP := FOutBufP;
  FBufStartP := FBufferP;
  FFillP := FBufferP;
  ReadTarget.Reset;
  end;

procedure TFilterReaderImpl.SetBufferSize(Value: Integer);
  var
    OldBufP: PChar;
    Delta: Integer;
  begin
  if Value <= 0 then raise EFilterReader.Create(SBufferSizeNotPositive);
  OldBufP := FBufferP;
  ReallocMem(FBufferP, Value);
  FBufferSize := Value;
  Delta := FBufferP - OldBufP;
  // adjust relative pointers
  Inc(FBufStartP, Delta);
  Inc(FFillP, Delta);
  end;

procedure TFilterReaderImpl.SetReadTarget(const Value: IReader);
  begin
  if not Assigned(Value) then raise EFilterReader.Create(STargetNil);
  FReadTarget := Value;
  end;

procedure TFilterReaderImpl.GetFillData(out StartP, EndP: PChar);
  begin
  StartP := FBufStartP;
  EndP := FFillP;
  end;

procedure TFilterReaderImpl.GetPendingData(out StartP, EndP: PChar);
  begin
  StartP := FInBufP;
  EndP := FPendingP;
  end;

procedure TFilterReaderImpl.GetWaitingData(out StartP, EndP: PChar);
  begin
  StartP := FOutStartP;
  EndP := FWaitingP;
  end;

{ Precondition: StartP pointing into waiting buffer (FOutBufP)
  Parameters:
  - StartP indicate start of free portion of waiting buffer
  - Needed indicates requested buffer size
  Returns:
  - Size of allocated buffer (if < Needed then function failed)
  Notes:
  - copies as much waiting data to out-buffer as possible
  - if not enough space is available for total amount of uncopied waiting
    data + extra space requested (Needed), then the waiting buffer will be
    extended to the required size and StartP will be set to point to the
    free space in the waiting buffer                                      }
function TFilterReaderImpl.WaitingProcessor(var StartP: PChar; Needed: Integer): Integer;
  var
    WaitingSize, OutSize, WaitingSpace: Integer;
  begin
  WaitingSpace := FOutBufSize - (StartP - FOutBufP);
  Assert(WaitingSpace >= 0);
  // fix Needed if necessary
  if WaitingSpace >= Needed then Needed := WaitingSpace + 1;

  OutSize := FExternalEndP - FExternalStartP;
  WaitingSize := StartP - FOutStartP;
  if WaitingSize >= OutSize then
    begin
    FWaitingP := StartP;
    if OutSize > 0 then  // if space in out-buffer
      begin
      Move(FOutStartP^, FExternalStartP^, OutSize);
      Inc(FOutStartP, OutSize);
      FExternalStartP := FExternalEndP;  // out-buffer full
      end;
    // no additional space created
    Result := WaitingSpace;
    Exit;
    end
  else
    begin
    Move(FOutStartP^, FExternalStartP^, WaitingSize);
    // waiting buffer used up - reset pointers
    FOutStartP := FOutBufP;
    FWaitingP := FOutBufP;
    Inc(FExternalStartP, WaitingSize);
    Dec(OutSize, WaitingSize);
    end;

  // at this point the waiting buffer is cleared

  // if enough space in out-buffer then switch to it
  if Needed <= OutSize then
    begin
    FProcessOutBuffer := OutProcessor;
    StartP := FExternalStartP;
    Result := OutSize;
    end
  else     // stick with waiting buffer
    begin
    if Needed > FOutBufSize then
      begin
      ReallocMem(FOutBufP, Needed);
      FOutBufSize := Needed;
      end;
    StartP := FOutBufP;
    FOutStartP := StartP;
    FWaitingP := StartP;
    // StartP does not change without re-allocating FOutBufP
    Result := Needed;
    end;
  end;

//kw Notes
{ - Why do we flush/fill buffers only when they are full?
    Example: Reader.WaitingProcessor called with Needed > 0:
      we copy all waiting data to out-buffer (= buffer passed top Read);
      still a little room left in out-buffer, nut not enough;
      why don't we return False so that on the next call the out-buffer
      is all available - instead we reserve space in the waiting buffer
      to satisfy the request immediately, even if that requires
      extra copying later (from waiting to ou-buffer)
    - Advantage: we can force the flush/fill amounts to be of fixed size

}

{ TStreamWriter }

procedure TStreamWriter.Close;
  begin
  FState := iosClosed;
  end;

constructor TStreamWriter.Create(Stream: TStream);
  begin
  if Stream = nil then raise EWriter.Create(SStreamNil);
  inherited Create;
  FStream := Stream;
  end;

procedure TStreamWriter.Flush;
  begin
  // do nothing
  end;

procedure TStreamWriter.Open;
  begin
  FState := iosOpen;
  end;

procedure TStreamWriter.Reset;
  begin
  FState := iosInitialized;
  end;

function TStreamWriter.State: TIOState;
  begin
  Result := FState;
  end;

function TStreamWriter.Write(const Buffer; Count: Integer): Integer;
  begin
  if  State = iosInitialized then Open
  else if State <> iosOpen then
    raise EWriter.Create(SWriterNotOpen);
  Result := Stream.Write(Buffer, Count);
  end;

{ TStreamReader }

procedure TStreamReader.Close;
  begin
  FState := iosClosed;
  end;

constructor TStreamReader.Create(Stream: TStream);
  begin
  if Stream = nil then raise EReader.Create(SStreamNil);
  inherited Create;
  FStream := Stream;
  end;

function TStreamReader.Fill: Integer;
  begin
  Result := 0;  // do nothing
  end;

procedure TStreamReader.Open;
  begin
  FState := iosOpen;
  end;

function TStreamReader.Read(var Buffer; Count: Integer): Integer;
  begin
  if  State = iosInitialized then Open
  else if State <> iosOpen then
    raise EReader.Create(SReaderNotOpen);
  Result := Stream.Read(Buffer, Count);
  if CloseOnPartialRead then
    if Result < Count then Close;
  end;

procedure TStreamReader.Reset;
  begin
  FState := iosInitialized;
  end;

function TStreamReader.State: TIOState;
  begin
  Result := FState;
  end;

{ TStringTable }

// does not affect size of hash table
procedure TStringTable.Clear;
  var
    Indx: Integer;
    StringP: Pointer;
  begin
  Indx := 1 shl SizeLog;  // size
  while Indx > 0 do
    begin
    Dec(Indx);
    StringP := Strings[Indx];
    if StringP = Strings then
      FStrings[Indx] := nil
    else if StringP <> nil then
      string(FStrings[Indx]) := '';
    end;
  FCount := 0;
  end;

procedure TStringTable.Collect;
  var
    Indx: Cardinal;
    StringP: Pointer;
    RefCountP: PLongint;
  begin
  Indx := 1 shl SizeLog;  // start from end of table
  while Indx <> 0 do
    begin
    Dec(Indx);
    StringP := Strings[Indx];
    if (StringP <> nil) and (StringP <> Strings) then
      begin
      RefCountP := StringP;
      Dec(RefCountP, 2);
      if RefCountP^ = 1 then
        begin
        string(StringP) := '';
        // mark as deleted
        FStrings[Indx] := Strings;
        Dec(FCount);
        end;
      end;
    end;
  end;

constructor TStringTable.Create(SizeLog: Byte);
  begin
  // FSizeLog = 0 means Size = 1, otherwise SetSizeLog will fail!
  FStrings := AllocMem(SizeOf(Pointer));
  SetSizeLog(SizeLog);
  end;

destructor TStringTable.Destroy;
  begin
  Clear;
  FreeMem(FStrings);
  inherited;
  end;

function TStringTable.Find(Value: PAnsiChar; Len: Integer): Pointer;
  var
    Indx: Integer;
  begin
  if Value = nil then
    begin
    Result := nil;
    Exit;
    end;
  if Len = -1 then Len := StrLen(Value);
  if (Len <> 0) and FindSlot(Slice(PAnsiChars(Value)^, Len), Indx) then
    Result := Strings[Indx]
  else
    Result := nil;
  end;

function TStringTable.Find(const Value: string): Pointer;
  var
    Indx: Integer;
  begin
  if (Value <> '') and FindSlot(Slice(PAnsiChars(Value)^, Length(Value)), Indx) then
    Result := Strings[Indx]
  else
    Result := nil;
  end;

function TStringTable.Find(const Value: array of AnsiChar): Pointer;
  var
    Indx: Integer;
  begin
  if (Length(Value) <> 0) and FindSlot(Value, Indx) then
    Result := Strings[Indx]
  else
    Result := nil;
  end;

function TStringTable.FindSlot(const Key: array of AnsiChar; out Indx: Integer): Boolean;
  label
    NoMatch;
  var
    KeyHash, Mask: Longword;
    DelIndx, CompIndx: Integer;
    StringP: Pointer;
    Step: Byte;
    Log: Byte absolute Step;
  begin
  Result := False;
  KeyHash := StrHash(Key, Length(Key), 0);
  Log := Sizelog;
  Mask := (Longword(1) shl Log) - 1;  // bit mask = 2^SizeLog - 1
  Indx := KeyHash and Mask;
  StringP := Strings[Indx];
  // if slot is empty, then we can return right away
  if StringP = nil then Exit;
  // otherwise, scan for an empty, deleted or duplicate slot
  DelIndx := -1;
  { For probing (after a collision) we need a step size relative prime
    to the hash table size, which is a power of 2. We use double-hashing,
    since we can calculate a second hash value cheaply by taking those bits
    of the first hash value that were discarded (masked out) when the table
    index was calculated: index := hash and mask, where mask = table-size-1.
    The maximum step size must fit into a Byte and be less than table-size/4.
    It must be an odd number, since that is relative prime to a power of 2.  }
  Step := Lo((((KeyHash and not Mask) shr (Log - 1))
    and (Mask shr 2)) or Longword(1));
  repeat
    if StringP = Strings then  // remember first deleted position
      begin
      if DelIndx < 0 then DelIndx := Indx;
      end
    else  // if duplicate found, return its position
      begin
      CompIndx := Length(string(StringP));
      if CompIndx = Length(Key) then
        begin
        while CompIndx >= 0 do
          begin
          Dec(CompIndx);
          if PAnsiChar(StringP)[CompIndx] <> Key[CompIndx] then
            goto NoMatch;
          end;
        Result := True;
        Exit;
        end;
      end;
    NoMatch:
    Indx := Indx - Step;
    if Indx < 0 then Indx := Indx + Integer(Mask) + 1;
    StringP := Strings[Indx];
    until StringP = nil;
  if DelIndx >= 0 then Indx := DelIndx;
  end;

procedure TStringTable.IncCount;
  var
    HalfMask: Longword;
  begin
  Inc(FCount);
  HalfMask := Longword($FFFFFFFF) shl (SizeLog - 1);
  // if table half full, double size
  if Longbool(Count and HalfMask) then SetSizeLog(SizeLog + 1);
  end;

function TStringTable.Intern(const Value: string): Pointer;
  var
    Indx: Integer;
  begin
  if Value = '' then
    begin
    Result :=  nil;
    Exit;
    end;
  if FindSlot(Slice(PAnsiChars(Value)^, Length(Value)), Indx) then
    Result := Strings[Indx]
  else
    begin
    Result := nil;  // initialize for string ref-counting to work properly
    string(Result) := Value;
    FStrings[Indx] := Result;
    IncCount;  // we used new slot
    end;
  end;

function TStringTable.Intern(const Value: array of AnsiChar): Pointer;
  var
    Indx: Integer;
  begin
  if Length(Value) = 0 then
    begin
    Result :=  nil;
    Exit;
    end;
  if FindSlot(Value, Indx) then
    Result := Strings[Indx]
  else
    begin
    Result := nil;  // initialize for string ref-counting to work properly
    SetString(string(Result), PAnsiChar(@Value), Length(Value));
    FStrings[Indx] := Result;
    IncCount;  // we used new slot
    end;
  end;

function TStringTable.Intern(Value: PAnsiChar; Len: Integer): Pointer;
  var
    Indx: Integer;
  begin
  if Value = nil then
    begin
    Result :=  nil;
    Exit;
    end
  else
    begin
    if Len = -1 then Len := StrLen(Value);
    if Len = 0 then
      begin
      Result :=  nil;
      Exit;
      end;
    end;
  if FindSlot(Slice(PAnsiChars(Value)^, Len), Indx) then
    Result := Strings[Indx]
  else
    begin
    Result := nil;  // initialize for string ref-counting to work properly
    SetString(string(Result), Value, Len);
    FStrings[Indx] := Result;
    IncCount;  // we used new slot
    end;
  end;

function TStringTable.Next(var Iter: Cardinal): Pointer;
  var
    Size: Cardinal;
  begin
  Size := 1 shl SizeLog;
  repeat
    if Iter < Size then
      begin
      Result := Strings[Iter];
      Inc(Iter);
      end
    else
      begin
      Result := nil;
      Exit;
      end;
    until (Result <> Strings) and (Result <> nil);
  end;

function TStringTable.Remove(const Value: array of AnsiChar): Boolean;
  var
    Indx: Integer;
    StringP: Pointer;
  begin
  if Length(Value) = 0 then
    begin
    Result := False;
    Exit;
    end;
  Result := FindSlot(Value, Indx);
  if Result then
    begin
    // free string
    StringP := Strings[Indx];
    string(StringP) := '';
    // mark slot as deleted
    FStrings[Indx] := Strings;
    Dec(FCount);
    end;
  end;

type
  TRemoveFunc = function(const Value: array of AnsiChar): Boolean of object;

function TStringTable.Remove(const Value: string): Boolean;
  var
    Rem: TRemoveFunc;
  begin
  // workaround for compiler bug QC #4058
  Rem := Self.Remove;
  Result := Rem(Slice(PAnsiChars(Value)^, Length(Value)));
  end;

function TStringTable.Remove(Value: PAnsiChar; Len: Integer): Boolean;
  var
    Rem: TRemoveFunc;
  begin
  if Value = nil then
    begin
    Result := False;
    Exit;
    end;
  if Len = -1 then Len := StrLen(Value);
  // workaround for compiler bug QC #4058
  Rem := Self.Remove;
  Result := Rem(Slice(PAnsiChars(Value)^, Len));
  end;

procedure TStringTable.SetSizeLog(Value: Byte);
  var
    Indx, NewIndx: Integer;
    StringP: Pointer;
    NewStrings: PPointers;
    NewSize: Cardinal;
    KeyHash, Mask: Longword;
    Step: Byte;
  begin
  NewSize := 1 shl Value;
  if (Value = 0) or (NewSize < (Count shl 1)) then
    raise EStringTable.Create(SStringTableTooSmall);
  Mask := NewSize - 1;
  NewStrings := AllocMem(NewSize * SizeOf(Pointer));
  try
    // re-hash: loop through existing items and insert them into new array
    Indx := 1 shl SizeLog;
    while Indx > 0 do
      begin
      Dec(Indx);
      StringP := Strings[Indx];
      if (StringP <> nil) and (StringP <> Strings) then  // a valid item
        begin
        KeyHash := StrHash(PAnsiChar(StringP), 0);
        NewIndx := KeyHash and Mask;
        // don't have to check for deleted items in NewStrings array
        if NewStrings[NewIndx] <> nil then
          begin
          // see comments to Step in FindSlot()
          Step := Lo((((KeyHash and not Mask) shr (Value - 1))
            and (Mask shr 2)) or Longword(1));
          repeat
            NewIndx := NewIndx - Step;
            if NewIndx < 0 then NewIndx := NewIndx + Integer(NewSize);
            until NewStrings[NewIndx] = nil;
          end;
        NewStrings[NewIndx] := StringP;
        end
      end;
  except
    FreeMem(NewStrings);
    raise;
    end;
  FreeMem(FStrings);
  FStrings := NewStrings;
  FSizeLog := Value;
  end;

function TStringTable.StartIter: Cardinal;
  begin
  Result := 0;
  end;

{ TWideStringTable }

// does not affect size of hash table
procedure TWideStringTable.Clear;
  var
    Indx: Integer;
    StringP: Pointer;
  begin
  Indx := 1 shl SizeLog;  // size
  while Indx > 0 do
    begin
    Dec(Indx);
    StringP := Strings[Indx];
    if StringP = Strings then
      FStrings[Indx] := nil
    else if StringP <> nil then
      WideString(FStrings[Indx]) := '';
    end;
  FCount := 0;
  end;

{$IFDEF LINUX}
procedure TWideStringTable.Collect;
  var
    Indx: Cardinal;
    StringP: Pointer;
    RefCountP: PLongint;
  begin
  Indx := 1 shl SizeLog;  // start from end of table
  while Indx <> 0 do
    begin
    Dec(Indx);
    StringP := Strings[Indx];
    if (StringP <> nil) and (StringP <> Strings) then
      begin
      RefCountP := StringP;
      Dec(RefCountP, 2);
      if RefCountP^ = 1 then
        begin
        WideString(StringP) := '';
        // mark as deleted
        FStrings[Indx] := Strings;
        Dec(FCount);
        end;
      end;
    end;
  end;
{$ENDIF}

constructor TWideStringTable.Create(SizeLog: Byte);
  begin
  // FSizeLog = 0 means Size = 1, otherwise SetSizeLog will fail!
  FStrings := AllocMem(SizeOf(Pointer));
  SetSizeLog(SizeLog);
  end;

destructor TWideStringTable.Destroy;
  begin
  Clear;
  FreeMem(FStrings);
  inherited;
  end;

function TWideStringTable.Find(Value: PWideChar; Len: Integer): Pointer;
  var
    Indx: Integer;
  begin
  if Value = nil then
    begin
    Result := nil;
    Exit;
    end;
  if Len = -1 then Len := WideStrLen(Value);
  if (Len <> 0) and FindSlot(Slice(PWideChars(Value)^, Len), Indx) then
    Result := Strings[Indx]
  else
    Result := nil;
  end;

function TWideStringTable.Find(const Value: WideString): Pointer;
  var
    Indx: Integer;
  begin
  if (Value <> '') and FindSlot(Slice(PWideChars(Value)^, Length(Value)), Indx) then
    Result := Strings[Indx]
  else
    Result := nil;
  end;

function TWideStringTable.Find(const Value: array of WideChar): Pointer;
  var
    Indx: Integer;
  begin
  if (Length(Value) <> 0) and FindSlot(Value, Indx) then
    Result := Strings[Indx]
  else
    Result := nil;
  end;

function TWideStringTable.FindSlot(const Key: array of WideChar; out Indx: Integer): Boolean;
  label
    NoMatch;
  var
    KeyHash, Mask: Longword;
    DelIndx, CompIndx: Integer;
    StringP: Pointer;
    Step: Byte;
    Log: Byte absolute Step;
  begin
  Result := False;
  KeyHash := StrHash(Key, Length(Key), 0);
  Log := Sizelog;
  Mask := (Longword(1) shl Log) - 1;  // bit mask = 2^SizeLog - 1
  Indx := KeyHash and Mask;
  StringP := Strings[Indx];
  // if slot is empty, then we can return right away
  if StringP = nil then Exit;
  // otherwise, scan for an empty, deleted or duplicate slot
  DelIndx := -1;
  { For probing (after a collision) we need a step size relative prime
    to the hash table size, which is a power of 2. We use double-hashing,
    since we can calculate a second hash value cheaply by taking those bits
    of the first hash value that were discarded (masked out) when the table
    index was calculated: index := hash and mask, where mask = table-size-1.
    The maximum step size must fit into a Byte and be less than table-size/4.
    It must be an odd number, since that is relative prime to a power of 2.  }
  Step := Lo((((KeyHash and not Mask) shr (Log - 1))
    and (Mask shr 2)) or Longword(1));
  repeat
    if StringP = Strings then  // remember first deleted position
      begin
      if DelIndx < 0 then DelIndx := Indx;
      end
    else  // if duplicate found, return its position
      begin
      CompIndx := Length(WideString(StringP));
      if CompIndx = Length(Key) then
        begin
        while CompIndx > 0 do
          begin
          Dec(CompIndx);
          if PWideChar(StringP)[CompIndx] <> Key[CompIndx] then
            goto NoMatch;
          end;
        Result := True;
        Exit;
        end;
      end;
    NoMatch:
    Indx := Indx - Step;
    if Indx < 0 then Indx := Indx + Integer(Mask) + 1;
    StringP := Strings[Indx];
    until StringP = nil;
  if DelIndx >= 0 then Indx := DelIndx;
  end;

procedure TWideStringTable.IncCount;
  var
    HalfMask: Longword;
  begin
  Inc(FCount);
  HalfMask := Longword($FFFFFFFF) shl (SizeLog - 1);
  // if table half full, double size
  if Longbool(Count and HalfMask) then SetSizeLog(SizeLog + 1);
  end;

function TWideStringTable.Intern(const Value: WideString): Pointer;
  var
    Indx: Integer;
  begin
  if Value = '' then
    begin
    Result :=  nil;
    Exit;
    end;
  if FindSlot(Slice(PWideChars(Value)^, Length(Value)), Indx) then
    Result := Strings[Indx]
  else
    begin
    Result := nil;  // initialize for string ref-counting to work properly
    WideString(Result) := Value;
    FStrings[Indx] := Result;
    IncCount;  // we used new slot
    end;
  end;

function TWideStringTable.Intern(const Value: array of WideChar): Pointer;
  var
    Indx: Integer;
  begin
  if Length(Value) = 0 then
    begin
    Result :=  nil;
    Exit;
    end;
  if FindSlot(Value, Indx) then
    Result := Strings[Indx]
  else
    begin
    Result := nil;  // initialize for string ref-counting to work properly
    SetString(WideString(Result), PWideChar(@Value), Length(Value));
    FStrings[Indx] := Result;
    IncCount;  // we used new slot
    end;
  end;

function TWideStringTable.Intern(Value: PWideChar; Len: Integer): Pointer;
  var
    Indx: Integer;
  begin
  if Value = nil then
    begin
    Result :=  nil;
    Exit;
    end
  else
    begin
    if Len = -1 then Len := WideStrLen(Value);
    if Len = 0 then
      begin
      Result :=  nil;
      Exit;
      end;
    end;
  if FindSlot(Slice(PWideChars(Value)^, Len), Indx) then
    Result := Strings[Indx]
  else
    begin
    Result := nil;  // initialize for string ref-counting to work properly
    SetString(WideString(Result), Value, Len);
    FStrings[Indx] := Result;
    IncCount;  // we used new slot
    end;
  end;

function TWideStringTable.Next(var Iter: Cardinal): Pointer;
  var
    Size: Cardinal;
  begin
  Size := 1 shl SizeLog;
  repeat
    if Iter < Size then
      begin
      Result := Strings[Iter];
      Inc(Iter);
      end
    else
      begin
      Result := nil;
      Exit;
      end;
    until (Result <> Strings) and (Result <> nil);
  end;

function TWideStringTable.Remove(const Value: array of WideChar): Boolean;
  var
    Indx: Integer;
    StringP: Pointer;
  begin
  if Length(Value) = 0 then
    begin
    Result := False;
    Exit;
    end;
  Result := FindSlot(Value, Indx);
  if Result then
    begin
    // free string
    StringP := Strings[Indx];
    WideString(StringP) := '';
    // mark slot as deleted
    FStrings[Indx] := Strings;
    Dec(FCount);
    end;
  end;

type
  TWideRemoveFunc = function(const Value: array of WideChar): Boolean of object;

function TWideStringTable.Remove(const Value: WideString): Boolean;
  var
    Rem: TWideRemoveFunc;
  begin
  // workaround for compiler bug QC #4058
  Rem := Self.Remove;
  Result := Rem(Slice(PWideChars(Value)^, Length(Value)));
  end;

function TWideStringTable.Remove(Value: PWideChar; Len: Integer): Boolean;
  var
    Rem: TWideRemoveFunc;
  begin
  if Value = nil then
    begin
    Result := False;
    Exit;
    end;
  if Len = -1 then Len := WideStrLen(Value);
  // workaround for compiler bug QC #4058
  Rem := Self.Remove;
  Result := Rem(Slice(PWideChars(Value)^, Len));
  end;

procedure TWideStringTable.SetSizeLog(Value: Byte);
  var
    Indx, NewIndx: Integer;
    StringP: Pointer;
    NewStrings: PPointers;
    NewSize: Cardinal;
    KeyHash, Mask: Longword;
    Step: Byte;
  begin
  NewSize := 1 shl Value;
  if (Value = 0) or (NewSize < (Count shl 1)) then
    raise EStringTable.Create(SStringTableTooSmall);
  Mask := NewSize - 1;
  NewStrings := AllocMem(NewSize * SizeOf(Pointer));
  try
    // re-hash: loop through existing items and insert them into new array
    Indx := 1 shl SizeLog;
    while Indx > 0 do
      begin
      Dec(Indx);
      StringP := Strings[Indx];
      if (StringP <> nil) and (StringP <> Strings) then  // a valid item
        begin
        KeyHash := StrHash(PWideChar(StringP), 0);
        NewIndx := KeyHash and Mask;
        // don't have to check for deleted items in NewStrings array
        if NewStrings[NewIndx] <> nil then
          begin
          // see comments to Step in FindSlot()
          Step := Lo((((KeyHash and not Mask) shr (Value - 1))
            and (Mask shr 2)) or Longword(1));
          repeat
            NewIndx := NewIndx - Step;
            if NewIndx < 0 then NewIndx := NewIndx + Integer(NewSize);
            until NewStrings[NewIndx] = nil;
          end;
        NewStrings[NewIndx] := StringP;
        end
      end;
  except
    FreeMem(NewStrings);
    raise;
    end;
  FreeMem(FStrings);
  FStrings := NewStrings;
  FSizeLog := Value;
  end;

function TWideStringTable.StartIter: Cardinal;
  begin
  Result := 0;
  end;

{ TBufferIO }

procedure TBufferIO.CheckBuffer(BufPtr: Pointer; BufSize: Integer);
  begin
  if (BufPtr = nil) or (BufSize <= 0) then
    raise EIO.Create(SInvalidBuffer);
  end;

procedure TBufferIO.Close;
  begin
  FState := iosClosed;
  end;

constructor TBufferIO.Create;
  begin
  inherited Create;
  FState := iosInitialized;
  end;

constructor TBufferIO.Create(const Buffer; Size: Integer);
  begin
  inherited Create;
  FState := iosInitialized;
  SetBuffer(Buffer, Size);
  end;

procedure TBufferIO.Open;
  begin
  CheckBuffer(FBuffer, FSize);
  FState := iosOpen;
  end;

procedure TBufferIO.Reset;
  begin
  FState := iosInitialized;
  FPosition := 0;
  end;

procedure TBufferIO.SetBuffer(const Buffer; Size: Integer);
  begin
  if FState <> iosInitialized then
    raise EIO.Create(SIOObjectNotReset);
  CheckBuffer(@Buffer, Size);
  FBuffer := @Buffer;
  FSize := Size;
  end;

function TBufferIO.State: TIOState;
  begin
  Result := FState;
  end;

{ TBufferWriter }

procedure TBufferWriter.Flush;
  begin
  // do nothing
  end;

function TBufferWriter.Write(const Buffer; Count: Integer): Integer;
  var
    Delta: Integer;
    DoClose: Boolean;
  begin
  if  State = iosInitialized then
    Open
  else if State <> iosOpen then
    raise EWriter.Create(SWriterNotOpen);
  Delta := Position + Count - FSize;
  if Delta < 0 then
    begin
    Result := Count;
    DoClose := False;
    end
  else if Delta <= Count then
    begin
    Result := Count - Delta;
    DoClose := True;
    end
  else
    raise EWriter.Create(SWriterError);
  Move(Buffer, (PChar(Self.Buffer) + Position)^, Result);
  Inc(FPosition, Result);
  if DoClose then Close;
  end;

{ TBufferReader }

function TBufferReader.Fill: Integer;
  begin
  Result := 0;  // do nothing
  end;

function TBufferReader.Read(var Buffer; Count: Integer): Integer;
  var
    DoClose: Boolean;
  begin
  if  State = iosInitialized then
    Open
  else if State <> iosOpen then
    raise EReader.Create(SReaderNotOpen);
  Result := FSize - Position;
  DoClose := Result <= Count;
  if not DoClose then Result := Count;
  Move((PChar(Self.Buffer) + Position)^, Buffer, Result);
  Inc(FPosition, Result);
  if DoClose then Close;
  end;


end.

