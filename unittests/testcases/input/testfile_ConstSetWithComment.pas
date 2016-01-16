unit testfile_ConstSetWithComment;

interface

type
  TSomeEnum = (enOne, enTwo, enThree);
  TSomeEnumSet = set of TSomeEnum;

const
  AllSomeEnumSet: TSomeEnumSet = [
  enOne, // some comment
  enTwo,  enThree];

  SomeOtherEnumSet: TSomeEnumSet = [
  enOne, 
    enTwo, // some comment
  enThree
  ];

implementation

end.
