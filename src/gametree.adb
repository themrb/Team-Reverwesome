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
               temptokens := ValidMove(toPlay,state.state.board, i,j);
               if (temptokens > 0) then

                  temp := state;
                  temp.state.justWent := NextPlayer(state.state.justWent);
                  temp.state.current_state(i,j) := toPlay;
                  temp.state.spot := (i,j);
                  temp.state.turns := state.state.turns + 1;
                  temp.state.TokensTaken := temptokens;

                  Children(Counter) := temp;
                     Configure.count := Configure.count + 1;
                     if(Counter < TurnsNo'Last) then
                        Counter := Counter + 1;
                     end if;
                  end if;
               end if;
         end loop;
      end loop;

      return Children;
   end Expand;

end GameTree;
