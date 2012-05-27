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
                     pieceReward(i,j) := Diff;
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
         Diff := -(Float(MonteCarlo(NextPlayer(Player), Next, 100)) - 0.5);
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
         nNewMoves := NumMoves(NewState, NextPlayer(Player));
         curVal := EndBoardValue(Player, State, nMoves);
         nextVal := -EndBoardValue(NextPlayer(Player), NewState, nNewMoves);

         Put_Line(curVal'Img & nextVal'Img);

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
                     newW := pieceWeights(i,j) + alpha * (curVal - nextVal + pieceReward(i,j));
                  elsif(NewState(i,j) = NextPlayer(Winner)) then
                     newW := pieceWeights(i,j) - alpha * (curVal - nextVal + pieceReward(i,j));
                  else
                     newW := pieceWeights(i,j);
                  end if;
                  pieceWeights(i,j) := newW;
               end loop;
            end loop;
         end;

         mobilityReward := mobilityReward*FeatureWeight(nMoves - nNewMoves);
         newW := mobilityWeight + alpha * (curVal - nextVal + mobilityReward);
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
      Whitewins : Natural := 0;
      Blackwins : Natural := 0;
      Ties : Natural := 0;
      temp : GameTree_Type;
      tempChildren : ExpandedChildren;
      tempWinner : BoardPoint;
      Children : ExpandedChildren := Expand(state);
      --random imports
      type Rand_Range is range 0..91;
      package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
      seed : Rand_Int.Generator;

      Next_Int : Integer;
      endprobability : Probability;
   begin
      Rand_Int.Reset(seed);
      for I in 1..iterations loop
         Next_Int := Integer(Rand_Int.Random(seed));
         temp := Children.children(Next_Int mod Children.branching);

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
               --Put_Line(Image(temp.state));
               --Put_Line(tempWinner'Img & " wins");
               exit Single_Iteration;
            end if;

            tempChildren := Expand(temp);
            Next_Int := Integer(Rand_Int.Random(seed));
            temp := tempChildren.children(Next_Int mod tempChildren.branching);
         end loop Single_Iteration;
      end loop;

      Put_Line(Player'Img & "'s turn: Black wins " & Blackwins'Img & " times and white wins " & Whitewins'Img & "times");
      if Whitewins = 0 and Blackwins = 0 then
         -- we only saw ties
         return 0.5;
      end if;
      if (Player = White) then
         endprobability := Long_Float(Whitewins) / Long_Float(Whitewins+Blackwins);

         return endprobability;
      elsif (Player = Black) then
         endprobability := Long_Float(Blackwins) / Long_Float(Whitewins+Blackwins);

         return endprobability;
      end if;
      return 0.0;
   end MonteCarlo;

   CSV_File : File_Type;

   procedure CloseFile is
   begin
      Close(CSV_File);
   end CloseFile;

   procedure LoadWeights is
      Filename : constant String := "data.csv";
   begin
      Open(CSV_File, In_File, Filename);
      LoadWeightSet(EarlyGame);
      LoadWeightSet(MidGame);
      LoadWeightSet(LateGame);
      Close(CSV_File);
   end LoadWeights;

   procedure LoadWeightSet(Weights : out FeatureWeights) is
      Subs : GNAT.String_Split.Slice_Set;
      InternalCount : Natural := 0;
   begin
      -- Grab out piece weights first
      for xpoint in Dimension range 0..Dimension'Last/2 loop
         declare
            Line : String := Get_Line(CSV_File);
         begin
            --Put_Line(Line);
            GNAT.String_Split.Create(Subs, Line, ",");
            for i in 1..(Slice_Count(Subs)-1) loop
               declare
                  Sub : String := Slice(Subs, i);
               begin
                  Weights.pieceWeights(xpoint, Dimension(i)-1) := Float'Value(Sub);
               end;
            end loop;
         end;
      end loop;
      -- Grab out peripheral features one by one
      --Put_Line("no raise here");
      declare
         Line : String := Get_Line(CSV_File);
      begin
         --Put_Line(Line);
         Weights.mobilityWeight := Float'Value(Line);
      end;
      --Put_Line("after");



      --Spread out piece weights
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            if (Weights.pieceWeights(WeightMapping(i), WeightMapping(j)) = 0.0) then
               Weights.pieceWeights(i,j)
                 := Weights.pieceWeights(WeightMapping(j),WeightMapping(i));
            else
               Weights.pieceWeights(i,j)
                 := Weights.pieceWeights(WeightMapping(i),WeightMapping(j));
            end if;
         end loop;
      end loop;
   end;

   procedure StoreWeights is
      Filename : constant String := "data.csv";
   begin
      Create(CSV_File, Out_File, Filename);
      StoreWeightSet(EarlyGame);
      StoreWeightSet(MidGame);
      StoreWeightSet(LateGame);
      Close(CSV_File);

   end StoreWeights;

   procedure StoreWeightSet(Weights : FeatureWeights) is
      Subs : Slice_Set;
      Next_Line : Unbounded_String;
      weightaverage : Float;
      divBy : Float := 4.0;
   begin
      --Line for each pieceweight line
      for i in Dimension range 0.. (Dimension'Last/2) loop
         Next_Line := To_Unbounded_String("");
         for j in Dimension range 0..i loop
            weightaverage := pieceWeights(i,j)+pieceWeights(Dimension'Last-i,j)
              +pieceWeights(Dimension'Last-i,Dimension'Last-j)+pieceWeights(i,Dimension'Last-j);
            if (i /= j) then
               weightaverage := weightaverage + pieceWeights(j,i) + pieceWeights(Dimension'Last-j,i)
                 +pieceWeights(Dimension'Last-j,Dimension'Last-i)+pieceWeights(j,Dimension'Last-i);
               divBy := divBy + 4.0;
            end if;
            weightaverage := weightaverage / divBy;
            Next_Line := Next_Line & To_Unbounded_String(Float'Image(weightaverage));
         end loop;
         Unbounded_IO.Put_Line(CSV_File, Next_Line);
      end loop;

      --Line for each peripheral weight
      Unbounded_IO.Put_Line(CSV_File, To_Unbounded_String(Float'Image(mobilityWeight)));
   end StoreWeightSet;

   function WeightMapping(i : Dimension) return Dimension is
   begin
      if(i > 4) then
         return Dimension'Last - i;
      else
         return i;
      end if;
   end;
end TemporalDifference;
