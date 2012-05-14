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
      bestMove := successors.children(0).state.spot;
      --Put_Line("Initialising to " & Dimension'Image(bestMove(x)) & "," & Dimension'Image(bestMove(y)));


      if (depth = 0 or Terminal(state.state.current_state)) then
         --Put_Line("Terminal found in min");
         bestMove := (0,0);
         EndBoardValue(Player,state.state.current_state,outValue);
         return;
      end if;

      --if we don't get a move
      if (successors.nomove) then
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
      --Put_Line("Min reports less than alpha " & Dimension'Image(bestMove(x)) & "," & Dimension'Image(bestMove(y)));
            return;
         end if;
         if (value < b) then
            b := value;
            bestMove := move.state.spot;
         end if;
      end loop;

      outValue := value;
      --Put_Line("Min reports " & Dimension'Image(bestMove(x)) & "," & Dimension'Image(bestMove(y)));

   end Min;

   procedure Max (Player : BoardPoint; state : in out GameTree_Type; depth : in TurnsNo; outValue : out BoardValue;
                    alpha, beta : in BoardValue;  bestMove : out Place) is
      successors : ExpandedChildren := Expand(state);
      move : GameTree_Type;
      a, b : BoardValue;
      value : BoardValue;
      best : Place ;
   begin
      a := alpha;
      b := beta;
      value := BoardValue'First;  -- Set to minimum board-value;
      bestMove := successors.children(0).state.spot;
      --Put_Line("Initialising to " & Dimension'Image(bestMove(x)) & "," & Dimension'Image(bestMove(y)));
--Put_Line( player'Img);
      if (Player = Blocked or state.state.justWent = Blocked) then
         Put_Line("BAD");
      end if;

      if (depth = 0 or Terminal(state.state.current_state)) then
         bestMove := (0,0);
         EndBoardValue(Player,state.state.current_state,outValue);
         --Put_Line("Terminal found in max, depth " & TurnsNo'Image(depth) & "value" & BoardValue'Image(outValue));
         return;
      end if;

      --if we don't get a move
      if (successors.nomove) then
         state.state.justWent := NextPlayer(state.state.justWent);
         Min(Player,state,depth-1,outValue,a,b,best);
         return;
      end if;

      -- Haven't reached the bottom yet, so continue minmaxing
      for i in 1.. (successors.branching-1) loop
         move := successors.children(TurnsNo(i));
         declare
            minValue : BoardValue;
         begin
            Min(Player,move, depth-1, minValue, a, b,best);
            --Put_Line("Min returned " & BoardValue'Image(minValue) & "for" & Dimension'Image(bestMove(x)) & "," & Dimension'Image(bestMove(y)));
            if(minValue > value) then
               value := minValue;
               bestMove := move.state.spot;
            end if;
         end;

         if(value >= b) then -- Min sees no way of avoiding max's win
            outValue := value;
            bestMove := move.state.spot;
      --Put_Line("Max reports more than beta " & Dimension'Image(bestMove(x)) & "," & Dimension'Image(bestMove(y)));
            return;
         end if;
         if (value > a) then
            a := value;
            bestMove := move.state.spot;
         end if;
      end loop;

      outValue := value;
      --Put_Line("Max reports " & Dimension'Image(bestMove(x)) & "," & Dimension'Image(bestMove(y)));

   end Max;

      function MonteCarlo (state : GameTree_Type; iterations : Positive) returns Probability is
   begin
      return 0.0;
   end MonteCarlo;

end MinMax;
