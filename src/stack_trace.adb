with Ada.Text_IO;              use Ada.Text_IO;
with GNAT.Traceback;           use GNAT.Traceback;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;

package Stack_Trace is
   procedure Call_Stack is
      Trace  : Tracebacks_Array (1..1_000);
      Length : Natural;
   begin
      Call_Chain (Trace, Length);
      Put_Line (Symbolic_Traceback (Trace (1..Length)));
   end Call_Stack;
end Test_Stack_Trace;
