unit testfile_AsmProblem1;

interface

implementation

function ColorMap(Height, Sea: integer): TColor;
begin
  asm
  mov eax,Height
cmp eax,Sea
{jump to section dealing with above sea level}
jg @Above

{ Height is beneath Sea Level }
mov ecx,Sea
{ ecx is depth beneath sea }
sub ecx,eax
{ divide depth by 256 }
shr ecx,8
cmp ecx, 256
{ ecx should not exceed 255 }
jl @NotMaxDepth
mov ecx,255
@NotMaxDepth:
// ecx now holds color
jmp @ColorDone

@Above:
{ eax is height above sea level }
sub eax,Sea
{ divide height by 256 }
shr eax,8
cmp eax,256
{ eax should not exceed 255 }
jl @NotMaxHeight
mov eax,255
@NotMaxHeight:
{ eax now holds green color depth }
{ eax now holds color }
shl eax,8
(* ecx now holds color for
compatibility with beneath
sea level routine *)
mov ecx,eax
@ColorDone:
mov Result,ecx
  end;
end;

end.
