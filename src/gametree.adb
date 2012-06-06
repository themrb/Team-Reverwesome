with Boards; use Boards;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Configure; use Configure;
with Features; use Features;

package body GameTree is

   --Expand a game tree node's state and return its successor states
   function Expand(state : in GameTree_Type) return ExpandedChildren is
      -- used as copy-to interchange
      temp : aliased GameTree_Type;
      -- List of nodes to be returned
      Children : ExpandedChildren;
      Counter : TurnsNo := 0;
      -- Whose turn it is
      toPlay : BoardPoint := NextPlayer(state.state.justWent);
      -- Tokens taken (used for greedy)
      temptokens : TurnsNo;
   begin
      -- For all possible positions on the board
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            -- Check if we can actually move here
            temptokens := ValidMove(toPlay,state.state.current_state, i,j);
            if (temptokens > 0) then

               -- Copy all relevant values from the parent
               -- Advance the board state based on the move we're considering
               temp := state;
               temp.state.justWent := NextPlayer(state.state.justWent);
               AdvanceMove(toPlay, temp.state.current_state, i, j);
               temp.state.spot := (i,j);
               temp.state.turnsleft := state.state.turnsleft - 1;
               temp.state.TokensTaken := temptokens;

               -- Check if the latest move changes our state
               temp.state.Current_Phase := state.state.Current_Phase;
               if state.state.Current_Phase = PEarlyGame then
                  -- Check if move is edge piece
                  if i = Dimension'First or i = Dimension'Last or j = Dimension'Last or j = Dimension'First then
                     temp.state.Current_Phase := PMidGame;
                  end if;
               elsif state.state.Current_Phase = PMidGame then
                  if (i = Dimension'First and j = Dimension'First) or (i = Dimension'First and j = Dimension'Last)
                    or (i = Dimension'Last and j = Dimension'First) or (i = Dimension'Last and j = Dimension'Last) then
                     -- Check if move is a corner and we have 2 corners total
                     temp.state.Corners := state.state.Corners + 1;
                     if temp.state.Corners >= 2 then
                        temp.state.Current_Phase := PLateGame;
                     end if;
                  end if;
               end if;

               --copy and update estimated stability
               temp.state.StableNodes := state.state.StableNodes;
               UpdateStability((i,j), state.state.current_state, temp.state.StableNodes);

               --copy and fill out internality memory
               temp.state.InternalNodes := state.state.InternalNodes;
               if CheckInternal((i,j), state.state.current_state) then
                  temp.state.InternalNodes(i,j) := True;
               end if;


               Children.children(Counter) := temp;
               Counter := Counter + 1;
            end if;
         end loop;
      end loop;

      -- Set the branching (number of child nodes)
      Children.branching := Counter;

      if (Counter = 0) then
         -- This play doesn't have any moves
         -- Legal play switches player
         temp := state;
         -- Switch player and expand
         temp.state.justWent := NextPlayer(state.state.justWent);
         Children := Expand(temp);
         -- Set flag that the switch happened
         Children.nomove := True;
      end if;
      return Children;
   end Expand;

   -- Returns the number of possible moves for a player in the current game state
   function NumMoves(Board : GameBoard; Player : Players) return Natural is
      temp : Natural;
      count : Natural := 0;
   begin
      -- Check each board square
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

   -- Check if a given board state is terminal
   function Terminal(board : in GameBoard) return Boolean is
   begin
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            -- If either player ever has a move, then the board is not terminal
            if (ValidMove(White,board, i,j) > 0 or ValidMove(Black,board, i,j) > 0) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Terminal;
end GameTree;
