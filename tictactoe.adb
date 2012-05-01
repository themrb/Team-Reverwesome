with Ada.Text_IO;
use Ada.Text_IO;

package body Agent is

	type BoardStates is array (0..9,0..9) of Integer;
	
	currentstate : BoardStates;
	pragma import(cpp, currentstate, "currentcstate");

   procedure Ada_Subroutine is
   begin
      Put("Ada_Subroutine has been invoked from C++.");

	Ada.Text_IO.Put_Line(currentstate(1,1)'Img);
	
   end Ada_Subroutine;

end Agent;