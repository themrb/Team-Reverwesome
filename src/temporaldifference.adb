with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Boards;
with GameTree; use GameTree;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;

package body TemporalDifference is

   procedure TD(Current, Next : GameTree_Type; Player : Players) is
      pieceReward : BoardPositionWeights := (others => (others => 0.0));
      mobilityReward : FeatureWeight := 0.0;
      State, NewState : GameBoard;
      Diff : FeatureWeight;
   begin
      State := Current.state.current_state;
      NewState := Next.state.current_state;
      if(Terminal(NewState)) then
         declare
            BlackC, WhiteC : TurnsNo;
            Winner : Players;
         begin
            TokenCount(NewState, WhiteC, BlackC);
            Diff := Float(abs(WhiteC - BlackC));
            if(WhiteC > BlackC) then
               Winner := White;
            else
               Winner := Black;
            end if;
            for i in Dimension'Range loop
               for j in Dimension'Range loop
                  if(NewState(i,j) = Winner) then
                     pieceReward(i,j) := Diff;
                  elsif(NewState(i,j) = NextPlayer(Winner)) then
                     pieceReward(i,j) := -Diff;
                  else
                     pieceReward(i,j) := 0.0;
                  end if;
               end loop;
            end loop;

            if(Player = Winner) then
               mobilityReward := mobilityReward + Diff;
            else
               mobilityReward := mobilityReward - Diff;
            end if;
         end;
      else
         Diff := Float(MonteCarlo(Player, Next, 100)) - 0.5;
         if(not (Diff'Valid or cease)) then
            Put_Line("Monte Carlo did it");
         end if;
         for i in Dimension'Range loop
            for j in Dimension'Range loop
               if(NewState(i,j) = Player) then
                  pieceReward(i,j) := Diff;
               elsif(NewState(i,j) = NextPlayer(Player)) then
                  pieceReward(i,j) := -Diff;
               else
                  pieceReward(i,j) := 0.0;
               end if;
            end loop;
         end loop;

         mobilityReward := mobilityReward + Diff;
      end if;

      declare
         curVal, nextVal : BoardValue;
         nMoves, nNewMoves : TurnsNo;
         newW : FeatureWeight;
      begin
         nMoves := NumMoves(State, Player);
         nNewMoves := NumMoves(NewState, Player);
         curVal := EndBoardValue(Player, State, nMoves);
         nextVal := EndBoardValue(Player, NewState, nNewMoves);

         for i in Dimension'Range loop
            for j in Dimension'Range loop
               newW := pieceWeights(i,j) + alpha * (nextVal - curVal + pieceReward(i,j));

               if(not (newW'Valid or cease)) then
                  Put_Line("*****************");
                  Put_Line(i'Img & j'Img & pieceWeights(i,j)'Img & nextVal'Img
                           & curVal'Img & pieceReward(i,j)'Img);
                  Put_Line("*****************");
                  cease := True;
               end if;

               pieceWeights(i,j) := newW;
            end loop;
         end loop;

         mobilityReward := mobilityReward*FeatureWeight(nMoves - nNewMoves);
         newW := mobilityWeight + alpha * (nextVal - curVal + mobilityReward);
         if(not (newW'Valid or cease)) then
            Put_Line("*****************");
            Put_Line("Mobility" & mobilityWeight'Img & nextVal'Img
                     & curVal'Img & mobilityReward'Img);
            Put_Line("*****************");
            cease := True;
         end if;
         mobilityWeight := newW;
      end;
   end TD;

   function EndBoardValue(Player : BoardPoint; State : GameBoard;
                          NumMoves : Natural) return BoardValue is
      Score : BoardValue;
   begin
      Score := TokenScore(State, Player);
      -- Weighting on the number of available moves
      Score := Score + (FeatureWeight(NumMoves) * mobilityWeight);
      return Score;
   end EndBoardValue;

   function TokenScore(State : in GameBoard; Player: in BoardPoint) return BoardValue is
      Weights : BoardPositionWeights := pieceWeights;
      Score : BoardValue := 0.0;
   begin
      for I in Dimension'Range loop
         for J in Dimension'Range loop
            if State(I,J) = Player then
               Score := Score + Weights(I,J);
            elsif State(I,J) = NextPlayer(Player) then
               Score := Score - Weights(I,J);
            end if;
         end loop;
      end loop;
      return Score;
   end TokenScore;

   procedure TokenCount(State : in GameBoard; WhiteTokens, BlackTokens : out TurnsNo) is
   begin
      BlackTokens := 0;
      WhiteTokens := 0;
        for I in Dimension'Range loop
         for J in Dimension'Range loop
            case State(I,J) is
            when White =>
               WhiteTokens := WhiteTokens + 1;
            when Black =>
               BlackTokens := BlackTokens + 1;
            when others =>
               null;
            end case;
         end loop;
   end loop;
   end TokenCount;

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

   procedure LoadWeights is
      CSV_File : File_Type;
      Line : String(1..255);
      Filename : constant String := "data.csv";
      Line_No : Natural := 0;
      Last : Natural;
      Subs : Slice_Set;
   begin
      Open(CSV_File, In_File, Filename);
      while not End_Of_File (CSV_File) loop
         Get_Line(CSV_File, Line, Last);
         Create(Subs, Line, ",");
         for i in 1..(Slice_Count(Subs)-1) loop
            declare
               Sub : String := Slice(Subs, i);
            begin
               if(Line_No < 5) then
                  Put_Line(Sub);
                  pieceWeights(Line_No, Dimension(i)-1) := Float'Value(Sub (Sub'First .. Sub'First + 11));
               elsif (Line_No = 5 and i = 1) then
                  mobilityWeight := Float'Value(Sub);
               end if;
            end;
         end loop;
         Line_No := Line_No + 1;
      end loop;
      Close(CSV_File);

      for i in Dimension'Range loop
         for j in Dimension'Range loop
            if (pieceWeights(WeightMapping(i), WeightMapping(j)) = 0.0) then
               pieceWeights(i,j)
                 := pieceWeights(WeightMapping(j),WeightMapping(i));
            else
               pieceWeights(i,j)
                 := pieceWeights(WeightMapping(i),WeightMapping(j));
            end if;
         end loop;
      end loop;
   end LoadWeights;

   procedure StoreWeights is
      CSV_File : File_Type;
      Filename : constant String := "data.csv";
      Line_No : Natural := 0;
      Subs : Slice_Set;
      Next_Line : Unbounded_String;
      weightaverage : Float;
   begin
      Create(CSV_File, Out_File, Filename);

      --Line for each pieceweight line
      for i in Dimension range 0.. (Dimension'Last/2) loop
         Next_Line := To_Unbounded_String("");
         for j in Dimension range 0..i loop
            weightaverage := (pieceWeights(i,j)+pieceWeights(Dimension'Last-i,j)+pieceWeights(Dimension'Last-i,Dimension'Last-j)+pieceWeights(i,Dimension'Last-j))/(4.0);
            Next_Line := Next_Line & To_Unbounded_String(Float'Image(weightaverage) & ",");
         end loop;
          Unbounded_IO.Put_Line(CSV_File, Next_Line);
      end loop;

      --Line for mobility weight
      Next_Line := To_Unbounded_String(Float'Image(mobilityWeight));
      Unbounded_IO.Put_Line(CSV_File, Next_Line);
      Close(CSV_File);

   end StoreWeights;

   function WeightMapping(i : Dimension) return Dimension is
   begin
      if(i > 4) then
         return Dimension'Last - i;
      else
         return i;
      end if;
   end;
end TemporalDifference;
