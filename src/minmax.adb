with GameTree; use GameTree;
with Boards; use Boards;
with Ada.Text_IO; use Ada.Text_IO;

package body MinMax is

   procedure Min (Player : BoardPoint; state : in out GameTree_Type; depth : in TurnsNo; outValue : out BoardValue; alpha, beta : in BoardValue; bestMove : out Place) is
      successors : ExpandedChildren := Expand(state);
      move : GameTree_Type;
      a, b : BoardValue;
      value : BoardValue;
      best : Place;
   begin
      a := alpha;
      b := beta;
      value := BoardValue'Last;  -- Set to maximum board-value;
      bestMove := successors.children(1).state.spot;

      if (depth = 0) then         
         EndBoardValue(Player,state.state.current_state,outValue);
         return;
      end if;

      -- Haven't reached the bottom yet, so continue minmaxing
      for i in 1.. (successors.branching) loop
         move := successors.children(TurnsNo(i));
         declare
            maxValue : BoardValue;
         begin
            Max(Player,move, depth-1, maxValue, a, b,best);
            if(maxValue < value) then
               value := maxValue;
               bestMove := move.state.spot;
            end if;
         end;

         if(value <= a) then -- Max sees no way of avoiding min's win
            outValue := value;
            bestMove := move.state.spot;
            return;
         end if;
         if (value < b) then
            b := value;
            bestMove := move.state.spot;
         end if;
      end loop;

      outValue := value;

   end Min;

   procedure Max (Player : BoardPoint; state : in out GameTree_Type; depth : in TurnsNo; outValue : out BoardValue;
                    alpha, beta : in BoardValue;  bestMove : out Place) is
      successors : ExpandedChildren := Expand(state);
      move : GameTree_Type;
      a, b : BoardValue;
      value : BoardValue;
      best : Place;
   begin
      a := alpha;
      b := beta;
      value := BoardValue'First;  -- Set to minimum board-value;
      bestMove := successors.children(1).state.spot;

      if (depth = 0) then         
         EndBoardValue(Player,state.state.current_state,outValue);
         return;
      end if;

      -- Haven't reached the bottom yet, so continue minmaxing
      for i in 1.. (successors.branching) loop
         move := successors.children(TurnsNo(i));
         declare
            maxValue : BoardValue;
         begin
            Max(Player,move, depth-1, maxValue, a, b,best);
            if(maxValue < value) then
               value := maxValue;
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

   end Max;

end MinMax;
