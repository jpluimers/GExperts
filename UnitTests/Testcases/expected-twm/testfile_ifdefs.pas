unit testunit_ifdefs;

interface

implementation

{$IFDEF level1}
{$IFDEF level2}
{$IFDEF level3}
{$IFDEF level4}
{$IFDEF level5}
{$IFDEF level6}
{$IFDEF level7}
{$IFDEF level8}

{$ENDIF level8}
{$ENDIF level7}
{$ENDIF level6}
{$ENDIF level5}
{$ENDIF level4}
{$ENDIF level3}
{$ENDIF level2}
{$ENDIF level1}

end.
