unit GX_SharedImages;

{$I GX_CondDefine.inc}

interface

uses
  Classes, ImgList, Controls;

type
  TdmSharedImages = class(TDataModule)
    Images: TImageList;
    DisabledImages: TImageList;
  end;

const
  ImageIndexNew = 10;
  ImageIndexExpand = 12;
  ImageIndexContract = 13;
  ImageIndexTrash = 30;
  ImageIndexFunction = 29;
  ImageIndexGear = 28;
  ImageIndexClosedFolder = 21;
  ImageIndexOpenFolder = 22;
  ImageIndexDocument  = 23;
  ImageIndexArrow = 43;
  ImageIndexCheck = 46;
  ImageIndexBlank = 47;
  ImageIndexVisibility = 53;
  ImageIndexMemberType = 48;
  ImageIndexWindow = 57;
  ImageIndexWindows = 58;
  ImageIndexUnit = 59;
  ImageIndexToDoPriority = 71;

function GetSharedImageList: TImageList;

implementation

uses GX_GExperts;

{$R *.dfm}

function GetSharedImageList: TImageList;
begin
  Result := GExpertsInst.GetSharedImages;
end;

end.
