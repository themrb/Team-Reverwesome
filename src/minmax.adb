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
--Put_Line( player'Img);
      if (Player = Blocked or state.state.justWent = Blocked) then
         Put_Line("BAD");
      end if;
      a := alpha;
      b := beta;
      value := BoardValue'Last;  -- Set to maximum board-value;
      bestMove := successors.children(1).state.spot;


      if (depth = 0 or Terminal(state.state.current_state)) then
         --Put_Line(Image(state.state.current_state));
         EndBoardValue(Player,state.state.current_state, successors.branching, outValue);
         return;
      end if;

      --if we don't get a move
      if (successors.nomove = False) then
         state.state.justWent := NextPlayer(state.state.justWent);
         Max(Player,state,depth-1,outValue,a,b,best);
         return;
      end if;

      -- Haven't reached the bottom yet, so continue minmaxing
      for i in 1.. (successors.branching-1) loop
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
--Put_Line( player'Img);
      if (Player = Blocked or state.state.justWent = Blocked) then
         Put_Line("BAD");
      end if;

      if (depth = 0 or Terminal(state.state.current_state)) then
         --Put_Line(TurnsNo'Image(successors.branching));
         --Put_Line(Image(state.state.current_state));
         EndBoardValue(Player,state.state.current_state, successors.branching, outValue);
         return;
      end if;

      --if we don't get a move
      if (successors.nomove = False) then
         state.state.justWent := NextPlayer(state.state.justWent);
         Min(Player,state,depth-1,outValue,a,b,best);
         return;
      end if;

      -- Haven't reached the bottom yet, so continue minmaxing
      for i in 1.. (successors.branching-1) loop
         move := successors.children(TurnsNo(i));
         declare
            maxValue : BoardValue;
         begin
            Min(Player,move, depth-1, maxValue, a, b,best);
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
