with Boards; use Boards;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Configure; use Configure;

package body GameTree is

   --Expand a game tree node's state and return its successor states
   function Expand(state : in GameTree_Type) return ExpandedChildren is
      temp : aliased GameTree_Type;
      Children : ExpandedChildren;
      Counter : TurnsNo := 0;
      toPlay : BoardPoint := NextPlayer(state.state.justWent);
      temptokens : TurnsNo;
   begin

      for i in Dimension'Range loop
         for j in Dimension'Range loop
               Put_Line("Checking " & Dimension'Image(i) & " " & Dimension'Image(j) & "and " & toPlay'Img & " to move");
               temptokens := ValidMove(toPlay,state.state.current_state, i,j);
               if (temptokens > 0) then
                  Put_Line("Found at " & Dimension'Image(i) & " " & Dimension'Image(j));

                  temp := state;
                  temp.state.justWent := NextPlayer(state.state.justWent);
                  AdvanceMove(toPlay, temp.state.current_state, i, j);
                  temp.state.spot := (i,j);
                  temp.state.turns := state.state.turns + 1;
                  temp.state.TokensTaken := temptokens;

                  Children.children(Counter) := temp;
                  Configure.count := Configure.count + 1;
                  Counter := Counter + 1;
               end if;
         end loop;
      end loop;

      Children.branching := Counter;

      if (Counter = 0) then
         Put_Line("We don't have any moves :(");
         Children.nomove := True; 
      end if;
      return Children;
   end Expand;

   function Terminal(board : in GameBoard) return Boolean is
   begin
      for i in Dimension'Range loop
         for j in Dimension'Range loop
               if (ValidMove(White,board, i,j) > 0 or ValidMove(Black,board, i,j) > 0) then
                  return False;
               end if;
         end loop;
      end loop;
      Put_Line("This looks pretty terminal to terminal function.");
      return True;
   end Terminal;
end GameTree;
