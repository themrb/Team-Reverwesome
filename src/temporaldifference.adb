with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Boards;
with GameTree; use GameTree;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;

package body TemporalDifference is

   procedure TD(History: HistoryType; Player : Players) is
--        deltaWeights : FeatureSet;
--        delt : FeatureWeight;
         ModFeatures : FeatureSet := basicSet;
         Step : Float := 0.0001;
         OldSet : FeatureSet := basicSet;
         StepChange : BoardValue := 0.0;
         Change : BoardValue := 0.0;
   begin
      -- Train Piece Weights
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            for k in History.History'First .. (History.Index-1) loop
               declare
                  NewSet : FeatureSet := OldSet;
                  State : State_Type := History.History(k).state;
                  lambdaSum : Float := 0.0;
                  derivative : Float;
               begin
                  NewSet.piece(i,j) := NewSet.piece(i,j) + step;
                  StepChange := ChangeInValue(Player, State.current_state, OldSet, NewSet, Step);
                  for m in (k+1) .. History.Index loop
                     derivative := EndBoardValue(Player, History.History(m).state.current_state)
                        - EndBoardValue(Player, State.current_state);
                     lambdaSum := lambdaSum + (lambda ** (m-k)) * derivative;
                  end loop;

                  Change := Change + alpha * (StepChange * lambdaSum);
               end;
            end loop;
            ModFeatures.piece(i,j) := ModFeatures.piece(i,j) + Change;
         end loop;
      end loop;

      --- Train Mobility ---
      for k in History.History'First .. (History.Index-1) loop
         declare
            NewSet : FeatureSet := OldSet;
            State : State_Type := History.History(k).state;
            lambdaSum : Float := 0.0;
            derivative : Float;
         begin
            NewSet.mobility := NewSet.mobility + step;
            StepChange := ChangeInValue(Player, State.current_state, OldSet, NewSet, Step);
            for m in (k+1) .. History.Index loop
               derivative := EndBoardValue(Player, History.History(m).state.current_state)
                        - EndBoardValue(Player, State.current_state);
               lambdaSum := lambdaSum + (lambda ** (m-k)) * derivative;
            end loop;

            Change := Change + alpha * (StepChange * lambdaSum);
         end;
      end loop;

      ModFeatures.mobility := ModFeatures.mobility + Change;
      basicSet := ModFeatures;
   end TD;

   function ChangeInValue(Player : Players; Board : GameBoard;
                          OldSet, NewSet : FeatureSet; Step : Float) return BoardValue is
      Current, Updated : BoardValue;
   begin
      Current := EndBoardValue(Player, Board,
                               NumMoves(Board, Player), OldSet);
      Updated := EndBoardValue(Player, Board,
                               NumMoves(Board, Player), NewSet);
      return (Updated - Current)/Step;
   end;

   function EndBoardValue(Player : Players; State : GameBoard; Moves : TurnsNo;
                          Set : FeatureSet) return BoardValue is
      Score : BoardValue;
   begin
      Score := TokenScore(State, Player, Set.piece);
      -- Weighting on the number of available moves
      Score := Score + (FeatureWeight(Moves) * Set.mobility);
      return Score;
   end EndBoardValue;

   function EndBoardValue(Player : Players; State : GameBoard) return BoardValue is
   begin
      return EndBoardValue(Player, State, NumMoves(State, Player), basicSet);
   end EndBoardValue;

   function EndBoardValue(Player : Players; State : GameBoard; Moves : TurnsNo) return BoardValue is
   begin
      return EndBoardValue(Player, State, Moves, basicSet);
   end;

   function TokenScore(State : in GameBoard; Player: in BoardPoint;
                        Weights : BoardPositionWeights) return BoardValue is
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

   procedure LoadWeightSet(Weights : out FeatureSet) is
      Subs : GNAT.String_Split.Slice_Set;
      InternalCount : Natural := 0;
   begin
      -- Grab out piece weights first
      for xpoint in Dimension range 0..(Dimension'Last/2) loop
         declare
            Line : String := Get_Line(CSV_File);
         begin
            --Put_Line(Line);
            GNAT.String_Split.Create(Subs, Line, ",");
            for i in 1..(Slice_Count(Subs)-1) loop
               declare
                  Sub : String := Slice(Subs, i);
               begin
                  Weights.piece(xpoint, Dimension(i)-1) := Float'Value(Sub);
               end;
            end loop;
         end;
      end loop;
      -- Grab out peripheral features one by one
      declare
         Line : String := Get_Line(CSV_File);
      begin
         Put_Line(Line);
         Weights.mobility := Float'Value(Line);
      end;

      --Spread out piece weights
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            if (Weights.piece(WeightMapping(i), WeightMapping(j)) = 0.0) then
               Weights.piece(i,j)
                 := Weights.piece(WeightMapping(j),WeightMapping(i));
            else
               Weights.piece(i,j)
                 := Weights.piece(WeightMapping(i),WeightMapping(j));
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

   procedure StoreWeightSet(Weights : FeatureSet) is
      Subs : Slice_Set;
      Next_Line : Unbounded_String;
      weightaverage : Float;
      divBy : Float := 4.0;
   begin
      --Line for each pieceweight line
      for i in Dimension range 0.. (Dimension'Last/2) loop
         Next_Line := To_Unbounded_String("");
         for j in Dimension range 0..i loop
            weightaverage := Weights.piece(i,j)+Weights.piece(Dimension'Last-i,j)
                 +Weights.piece(Dimension'Last-i,Dimension'Last-j)+Weights.piece(i,Dimension'Last-j);
            if (i /= j) then
               weightaverage := weightaverage + Weights.piece(j,i) + Weights.piece(Dimension'Last-j,i)
                 +Weights.piece(Dimension'Last-j,Dimension'Last-i)+Weights.piece(j,Dimension'Last-i);
               divBy := divBy + 4.0;
            end if;
            weightaverage := weightaverage / divBy;
            Next_Line := Next_Line & To_Unbounded_String(Float'Image(weightaverage));
         end loop;
         Unbounded_IO.Put_Line(CSV_File, Next_Line);
      end loop;

      --Line for each peripheral weight
      Unbounded_IO.Put_Line(CSV_File, To_Unbounded_String(Float'Image(basicSet.mobility)));
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
