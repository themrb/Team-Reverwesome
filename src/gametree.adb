with Boards; use Boards;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Configure; use Configure;
with Features; use Features;

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
            temptokens := ValidMove(toPlay,state.state.current_state, i,j);
            if (temptokens > 0) then
               temp := state;
               temp.state.justWent := NextPlayer(state.state.justWent);
               AdvanceMove(toPlay, temp.state.current_state, i, j);
               temp.state.spot := (i,j);
               temp.state.turnsleft := state.state.turnsleft - 1;
               temp.state.TokensTaken := temptokens;

               --copy and update estimated stability
               temp.state.StableNodes := state.state.StableNodes;
               if CheckStability((i,j), toPlay, state.state.current_state) then
                  temp.state.StableNodes(i,j):= True;
               end if;

               --copy and update estimated stability
               temp.state.InternalNodes := state.state.InternalNodes;
               if CheckInternal((i,j), state.state.current_state) then
                  temp.state.InternalNodes(i,j) := True;
               end if;


               Children.children(Counter) := temp;
               Configure.count := Configure.count + 1;
               Counter := Counter + 1;
            end if;
         end loop;
      end loop;

      Children.branching := Counter;

      if (Counter = 0) then
         --Put_Line("We don't have any moves :(");
         temp := state;
         temp.state.justWent := NextPlayer(state.state.justWent);
         Children := Expand(temp);
         Children.nomove := True;
      end if;
      return Children;
   end Expand;

   function NumMoves(Board : GameBoard; Player : Players) return Natural is
      temp : Natural;
      count : Natural := 0;
   begin
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            temp := ValidMove(player => Player,
                              board  => Board,
                              movex  => i,
                              movey  => j);
            if(temp > 0) then
               count := count + 1;
            end if;
         end loop;
      end loop;
      return count;
   end NumMoves;

   function Terminal(board : in GameBoard) return Boolean is
   begin
      for i in Dimension'Range loop
         for j in Dimension'Range loop
               if (ValidMove(White,board, i,j) > 0 or ValidMove(Black,board, i,j) > 0) then
                  return False;
               end if;
         end loop;
      end loop;
      --Put_Line("This looks pretty terminal to terminal function.");
      return True;
   end Terminal;
end GameTree;
