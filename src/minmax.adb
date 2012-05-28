with GameTree; use GameTree;
with Boards; use Boards;
with TemporalDifference; use TemporalDifference;
with Ada.Text_IO; use Ada.Text_IO;

package body MinMax is

   procedure NegaMax (Player : BoardPoint; state : in out GameTree_Type;
                      depth : in TurnsNo; outValue : out BoardValue;
                      alpha, beta : in BoardValue;  bestMove : out Place) is
      successors : ExpandedChildren;
      move : GameTree_Type;
      a, b : BoardValue;
      value : BoardValue;
      best : Place;
      Set : FeatureSet;
   begin
      a := alpha;
      b := beta;
      value := BoardValue'First;  -- Set to minimum board-value;

      Set := PhaseToSet(state.state.Current_Phase);

      if (Player = Blocked or state.state.justWent = Blocked) then
         Put_Line("BAD");
      end if;

      outValue := TerminalCheck(state.state.current_state, Player);
      if(outValue /= 0.0) then
         bestMove := (0,0);
         return;
      end if;

      successors := Expand(state);
      -- This needs to occur /after/ checking if this state is Terminal!
      bestMove := successors.children(0).state.spot;

      if (depth = 0) then
         bestMove := (0,0);
         outValue := EndBoardValue(Player,state.state, Set);
--             -EndBoardValue(NextPlayer(Player),state.state, NumMoves(state.state.current_state,NextPlayer(Player)), Set);
         return;
      end if;

      --if we don't get a move
      if (successors.nomove) then
         state.state.justWent := NextPlayer(state.state.justWent);
         NegaMax(Player, state, depth-1, outValue, a, b, best);
         return;
      end if;

      if (successors.branching = 1) then
         NegaMax(Player, successors.children(0), depth, outValue, a, b, best);
         return;
      end if;

      -- Haven't reached the bottom yet, so continue minmaxing
      for i in 1.. (successors.branching-1) loop
         move := successors.children(TurnsNo(i));
         declare
            mValue : BoardValue;
         begin
            NegaMax(NextPlayer(Player), move, depth-1, mValue, -b, -a, best);
            mValue := -mValue;
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

   function TerminalCheck(state : GameBoard; Player : BoardPoint) return BoardValue is
      outValue : BoardValue := 0.0;
   begin
      if (Terminal(state)) then
         --outValue := EndBoardValue(Player,state.state.current_state, 0, Set);

         --if we win, take it. if we lose, avoid like the plague!
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
