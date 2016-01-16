unit testfile_ConstSetWithComment;

interface

type
  TSomeEnum = (enOne, enTwo, enThree);
  TSomeEnumSet = set of TSomeEnum;

const
  SomeOtherEnumSet: TSomeEnumSet = [
  enOne,
  enTwo,
  enThree // some comment
  ];

implementation

end.
