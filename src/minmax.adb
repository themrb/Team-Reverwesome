with GameTree; use GameTree;
with Boards; use Boards;
with TemporalDifference; use TemporalDifference;
with Ada.Text_IO; use Ada.Text_IO;

package body MinMax is

   procedure NegaMax (Player : BoardPoint; state : in out GameTree_Type;
                      depth : in TurnsNo; outValue : out BoardValue;
                      alpha, beta : in BoardValue;  bestMove : out Place) is
      -- Game node tree children
      -- (possible new states from new moves)
      successors : ExpandedChildren;
      -- Current move and value under consideration
      move : GameTree_Type;
      value : BoardValue;
      -- Alpha and beta values
      a, b : BoardValue;
      -- best move we've seen so far
      best : Place;
      -- current feature set we're using
      Set : FeatureSet;
   begin
      a := alpha;
      b := beta;
      value := BoardValue'First;  -- Set to minimum board-value;

      -- Choose the feature set from the state we're currently in
      Set := PhaseToSet(state.state.Current_Phase);

      -- If the state is terminal, take absolute board values -inf or inf
      outValue := TerminalCheck(state.state.current_state, Player);
      if(outValue /= 0.0) then
         bestMove := (0,0);
         return;
      end if;

      -- Find our successors
      successors := Expand(state);
      -- Initialise the best move
      bestMove := successors.children(0).state.spot;

      -- We hit the bottom of our depth
      if (depth = 0) then
         -- (if doesn't matter what the bestMove is, as long as it's not null, because only root node takes a move)
         bestMove := (0,0);
         -- Check and return the evaluated value of this game state
         outValue := EndBoardValue(Player,state.state, Set);
         return;
      end if;

      --if we don't get a move, switch player and call negamax again from the same perspective
      -- (the opponent moves again)
      if (successors.nomove) then
         state.state.justWent := NextPlayer(state.state.justWent);
         NegaMax(Player, state, depth-1, outValue, a, b, best);
         return;
      end if;

      -- If it's a forced move, just take the move (depth can go down one extra)
      if (successors.branching = 1) then
         NegaMax(Player, successors.children(0), depth, outValue, a, b, best);
         return;
      end if;

      -- Haven't reached the bottom yet, so continue minmaxing
      for i in 1.. (successors.branching-1) loop
         -- Check through all possible moves
         move := successors.children(TurnsNo(i));
         declare
            mValue : BoardValue;
         begin
            -- Continue search deeper, but negate all values
            -- (as they are the values for the opponent)
            NegaMax(NextPlayer(Player), move, depth-1, mValue, -b, -a, best);
            mValue := -mValue;
            -- If the value is better than previously seen values
            -- Take it and the associated move
            if(mValue > value) then
               value := mValue;
               bestMove := move.state.spot;
            end if;
         end;

         if(value >= b) then -- Min sees no way of avoiding max's win
            outValue := value;
            bestMove := move.state.spot;
            return;
         end if;
         if (value > a) then
            a := value;
            bestMove := move.state.spot;
         end if;
      end loop;

      outValue := value;
   end NegaMax;

   -- Pick out a feature set from the game phase
   function PhaseToSet(phase : Game_Phase) return FeatureSet is
      Set : FeatureSet;
   begin
      case phase is
         when PEarlyGame =>
            Set := EarlyGame;
         when PMidGame =>
            Set := MidGame;
         when PLateGame =>
            Set := LateGame;
      end case;

      return Set;
   end PhaseToSet;

   -- Check if a board state is terminal and return appropriate value for if we won or lost
   -- Note this completely ignores / overrides normal board evaluation function
   -- This means we will take a win or loss as absolute
   function TerminalCheck(state : GameBoard; Player : BoardPoint) return BoardValue is
      outValue : BoardValue := 0.0;
   begin
      if (Terminal(state)) then

         --If we win, take it (best possible outcome). If we lose, avoid like the plague!
         declare
            WinningPlayer : BoardPoint;
         begin
            Winner(state,WinningPlayer);
            if WinningPlayer = Player then
               outValue := BoardValue'Last - 1.0;
            else
               outValue := BoardValue'First + 1.0;
            end if;
         end;
      end if;
      return outValue;
   end TerminalCheck;

end MinMax;
