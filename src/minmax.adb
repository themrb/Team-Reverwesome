with GameTree; use GameTree;
with Boards; use Boards;
with TemporalDifference; use TemporalDifference;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;

package body MinMax is

   procedure NegaMax (Player : BoardPoint; state : in out GameTree_Type; depth : in TurnsNo; outValue : out BoardValue; alpha, beta : in BoardValue;  bestMove : out Place) is
      successors : ExpandedChildren;
      move : GameTree_Type;
      a, b : BoardValue;
      value : BoardValue;
      best : Place;
   begin
      if (Player = Blocked or state.state.justWent = Blocked) then
         Put_Line("BAD");
      end if;

      if (depth = 0 or Terminal(state.state.current_state)) then
         bestMove := (0,0);
         EndBoardValue(Player,state.state.current_state, 0, outValue);
         return;
      end if;

      a := alpha;
      b := beta;
      value := BoardValue'First;  -- Set to minimum board-value;
      successors := Expand(state);
      bestMove := successors.children(0).state.spot;

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

   function MonteCarlo (Player : BoardPoint; state : GameTree_Type; iterations : Positive) return Probability is
      Whitewins : Long_Float := 0.0;
      Blackwins : Long_Float := 0.0;
      Ties : Natural := 0;
      temp : GameTree_Type;
      tempChildren : ExpandedChildren;
      tempWinner : BoardPoint;
      Children : ExpandedChildren := Expand(state);
      --random imports
      type Rand_Range is range 0..91;
      package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
      seed : Rand_Int.Generator;
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
                     Whitewins := Whitewins + 1.0;
                  elsif (tempWinner = Black) then
                     Blackwins := Blackwins + 1.0;
                  else Ties := Ties + 1;
                  end if;
               exit Single_Iteration;
            end if;

            tempChildren := Expand(temp);
            Rand_Int.Reset(seed);
            temp := tempChildren.children(Integer(Rand_Int.Random(seed)) mod tempChildren.branching);

         end loop Single_Iteration;
      end loop;

      if (Player = White) then
         Put_Line(Blackwins'Img & ' ' & Whitewins'Img);
         return Whitewins / (Whitewins+Blackwins);
      elsif (Player = Black) then
         Put_Line(Blackwins'Img & ' ' & Whitewins'Img);
         return Blackwins / (Whitewins+Blackwins);
      end if;
      return 0.0;
   end MonteCarlo;

end MinMax;
