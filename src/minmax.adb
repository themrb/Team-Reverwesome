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
      case state.state.Current_Phase is
         when PEarlyGame =>
            Set := EarlyGame;
         when PMidGame =>
            Set := MidGame;
         when PLateGame =>
            Set := LateGame;
      end case;
      if (Player = Blocked or state.state.justWent = Blocked) then
         Put_Line("BAD");
      end if;

      if (Terminal(state.state.current_state)) then
         bestMove := (0,0);
         --outValue := EndBoardValue(Player,state.state.current_state, 0, Set);

         --if we win, take it. if we lose, avoid like the plague!
         declare
            WinningPlayer : BoardPoint;
         begin
            Winner(state.state.current_state,WinningPlayer);
            if WinningPlayer = Player then
               outValue := BoardValue(5000);
            else
               outValue := BoardValue(-5000);
            end if;
         end;
         return;
      end if;

      a := alpha;
      b := beta;
      value := BoardValue'First;  -- Set to minimum board-value;
      successors := Expand(state);
      -- This needs to occur /after/ checking if this state is Terminal!
      bestMove := successors.children(0).state.spot;

      if (depth = 0) then
         bestMove := (0,0);
         outValue := EndBoardValue(Player,state.state, successors.branching, Set);
         return;
      end if;

      --if we don't get a move
      if (successors.nomove) then
         state.state.justWent := NextPlayer(state.state.justWent);
         NegaMax(Player, state, depth-1, outValue, a, b, best);
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

end MinMax;
