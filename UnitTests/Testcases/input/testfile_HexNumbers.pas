unit testfile_HexNumbers;

interface

const
  HexNumber = $0abcdef9;

implementation

const
  LineFeed = #$0a;
  CarriageReturn = #$0D;
  // This currently doesn't work correctly, should be fixed
  Embedded = 'hello'#$0D#$0a'world'#$0D#$0a;

end.