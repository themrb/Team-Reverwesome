with GameTree; use GameTree;
with Boards; use Boards;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;

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
         EndBoardValue(Player,state.state.current_state, successors.branching, outValue);
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
         EndBoardValue(Player,state.state.current_state, successors.branching, outValue);
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

   function MonteCarlo (Player : BoardPoint; state : GameTree_Type; iterations : Positive) return Probability is
      Whitewins : Natural := 0;
      Blackwins : Natural := 0;
      Ties : Natural := 0;
      temp : GameTree_Type;
      tempChildren : ExpandedChildren;
      tempWinner : BoardPoint;
      Children : ExpandedChildren := Expand(state);
   begin
      for I in 1..iterations loop
         
         declare
            type Rand_Range is range 0..91;
            package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
            seed : Rand_Int.Generator;
         begin
            Rand_Int.Reset(seed);
            temp := Children.children(Integer(Rand_Int.Random(seed)) mod Children.branching);
         end;

         Single_Iteration:
         loop
            if (Terminal(temp.state.current_state)) then
                  Winner(temp.state.current_state,tempWinner);
                  if (tempWinner = White) then
                     Whitewins := Whitewins + 1;
                  elsif (tempWinner = Black) then
                     Blackwins := Blackwins + 1;
                  else Ties := Ties + 1;
                  end if;
               exit Single_Iteration;
            end if;
            
            tempChildren := Expand(temp);
            declare
               type Rand_Range is range 0..91;
               package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
               seed : Rand_Int.Generator;
            begin
               Rand_Int.Reset(seed);
               temp := tempChildren.children(Integer(Rand_Int.Random(seed)) mod tempChildren.branching);
            end;

         end loop Single_Iteration;
      end loop;

      if (Player = White) then
         return Long_Float(Whitewins) / Long_Float(Whitewins+Blackwins);
      elsif (Player = Black) then
         return Long_Float(Blackwins) / Long_Float(Whitewins+Blackwins);
      end if;
      return 0.0;
   end MonteCarlo;

end MinMax;
