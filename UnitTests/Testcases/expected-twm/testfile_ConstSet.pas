unit testfile_ConstSet;

interface

type
  TSomeEnum = (enOne, enTwo, enThree);
  TSomeEnumSet = set of TSomeEnum;

const
  AllSomeEnumSet: TSomeEnumSet = [
    enOne,
    enTwo,
    enThree];

implementation

end.
