with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Boards; use Boards;
with GameTree; use GameTree;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
with Features; use Features;

package body TemporalDifference is

   procedure TD(History: HistoryType; Player : Players; Feedback : Float) is
      -- Modified feature sets.
      ModEarly, ModMid, ModLate : FeatureSet;
      -- Step size to use when taking the 'derivative'.
      Step : Float := 0.0001;
      -- The Feature set at a particular board state during this game.
      OldSet : FeatureSet;
      -- The 'derivative'.
      StepChange : BoardValue := 0.0;
   begin
      ModEarly := EarlyGame;
      ModMid := MidGame;
      ModLate := LateGame;

      -- Train Piece Weights
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            for k in History.History'First .. (History.Index-1) loop
               declare
                  NewSet : FeatureSet;
                  State : State_Type := History.History(k).state;
                  gammaSum : Float := 0.0;
                  temporalDiff : Float;
               begin
                  OldSet := PhaseToSet(State.Current_Phase);
                  NewSet := OldSet;

                  -- Calculate the derivative.
                  NewSet.piece(i,j) := NewSet.piece(i,j) + step;
                  StepChange := ChangeInValue(Player, State, OldSet, NewSet, Step);
                  for m in (k+1) .. History.Index loop
                     -- This value of NewSet has absolutely nothing to do with
                     -- The one above. It's juse useful to have this variable here.
                     NewSet := PhaseToSet(History.History(m).state.Current_Phase);
                     -- Calculate the temporal difference.
                     temporalDiff := EndBoardValue(Player, History.History(m).state, NewSet)
                       - EndBoardValue(Player, State, OldSet);
                     gammaSum := gammaSum + (lambda ** (m-k)) * (temporalDiff);
                  end loop;

                  gammaSum := gammaSum + (lambda ** (History.Index - k)) * Feedback;

                  -- Update the appropriate modified feature set.
                  if(State.Current_Phase = PEarlyGame) then
                     ModEarly.piece(i,j) := ModEarly.piece(i,j) + (alpha * StepChange * gammaSum);
                  elsif(State.Current_Phase = PMidGame) then
                     ModMid.piece(i,j) := ModMid.piece(i,j) + (alpha * StepChange * gammaSum);
                  else
                     ModLate.piece(i,j) := ModLate.piece(i,j) + (alpha * StepChange * gammaSum);
                  end if;
               end;
            end loop;
         end loop;
      end loop;

      -- Train One-Off Weights --
      -- Basically the same as the above --
      for k in History.History'First .. (History.Index-1) loop
         declare
            State : State_Type := History.History(k).state;
            gammaSum : Float := 0.0;
            lambdaInd : array(IndependentWeights) of Float;
            temporalDiff : Float;
         begin
            OldSet := PhaseToSet(State.Current_Phase);

            for i in IndependentWeights loop
               declare
                  NewSet : FeatureSet := OldSet;
               begin
                  NewSet.independent(i) := NewSet.independent(i) + step;
                  StepChange := ChangeInValue(Player, State, OldSet, NewSet, Step);
                  for m in (k+1) .. History.Index loop
                     NewSet := PhaseToSet(History.History(m).state.Current_Phase);
                     temporalDiff := EndBoardValue(Player, History.History(m).state, NewSet)
                       - EndBoardValue(Player, State, OldSet);
                     gammaSum := gammaSum + (lambda ** (m-k)) * (temporalDiff);
                  end loop;

                  gammaSum := gammaSum + (lambda ** (History.Index - k)) * Feedback;
               end;

               lambdaInd(i) := alpha * StepChange * gammaSum;
               gammaSum := 0.0;
            end loop;

            for i in IndependentWeights loop
               if(State.Current_Phase = PEarlyGame) then
                  ModEarly.independent(i) := ModEarly.independent(i) + lambdaInd(i);
               elsif(State.Current_Phase = PMidGame) then
                  ModMid.independent(i) := ModMid.independent(i) + lambdaInd(i);
               else
                  ModLate.independent(i) := ModLate.independent(i) + lambdaInd(i);
               end if;
            end loop;
         end;
      end loop;

      EarlyGame := ModEarly;
      MidGame := ModMid;
      LateGame := ModLate;
   end TD;

   -- Calculate an approximation to the derivative of the eval function.
   function ChangeInValue(Player : Players; Board : State_Type;
                          OldSet, NewSet : FeatureSet; Step : Float) return BoardValue is
      Current, Updated : BoardValue;
   begin
      Current := EndBoardValue(Player, Board,
                                OldSet);
      Updated := EndBoardValue(Player, Board,
                                NewSet);
      return (Updated - Current)/Step;
   end;

   -- Static evaluation function for a game board
   function EndBoardValue(Player : Players; State : State_Type;
                          Set : FeatureSet) return BoardValue is
      Score : BoardValue;
   begin
      -- Find the score of all the player's tokens, minus the opponent's score
      Score := TokenScore(State.current_state, Player, Set.piece);
      -- Consider score from the difference in number of moves, multiplied by mobility weight
      Score := Score + (FeatureWeight(NumMoves(State.current_state, Player)
        - NumMoves(State.current_state, NextPlayer(Player))) * Set.independent(Mobility));

      -- Consider stability score but not in early game
      -- (in early game, no edges have been taken yet)
      if State.Current_Phase /= PEarlyGame then
         declare
            StablePieces : Integer;
            StablePiecesTheirs : Integer;
            StabilityMatrix : InfoMatrix := State.StableNodes;
         begin
            -- Count the number of our stable nodes minux theirs, multiply by stability weight
            CountStability(Player, State.current_state, StabilityMatrix, StablePieces);
            CountStability(NextPlayer(Player), State.current_state, StabilityMatrix, StablePiecesTheirs);
            Score := Score + (FeatureWeight(StablePieces - StablePiecesTheirs) * Set.independent(Stability));
         end;
      end if;
      declare
         InternalPieces : Integer;
         InternalPiecesTheirs : Integer;
      begin
         -- Count the number of our internal nodes minux theirs, multiply by internality weight
         CountInternals(Player, State.current_state, State.InternalNodes, InternalPieces);
         CountInternals(NextPlayer(Player), State.current_state, State.InternalNodes, InternalPiecesTheirs);
         Score := Score + (FeatureWeight(InternalPieces-InternalPiecesTheirs) * Set.independent(Internal));
      end;

      -- Return overall score tally
      return Score;
   end EndBoardValue;

   -- Return token score of a player
   function TokenScore(State : in GameBoard; Player: in BoardPoint;
                        Weights : BoardPositionWeights) return BoardValue is
      Score : BoardValue := 0.0;
   begin
      -- Loop through all positions
      for I in Dimension'Range loop
         for J in Dimension'Range loop
            if State(I,J) = Player then
               -- If we own it, add its weight to our score
               Score := Score + Weights(I,J);
            elsif State(I,J) = NextPlayer(Player) then
               -- If opponent owns it, subtract its weight from our score
               Score := Score - Weights(I,J);
            end if;
         end loop;
      end loop;
      return Score;
   end TokenScore;

   -- Count of raw tokens for bot players
   procedure TokenCount(State : in GameBoard; WhiteTokens, BlackTokens : out TurnsNo) is
   begin
      BlackTokens := 0;
      WhiteTokens := 0;
      -- Check all positions and add as appropriate
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

   -- Find how many tiles a given player owns
   function OwnDiscs(Player : Players; State : in GameBoard) return TurnsNo is
      Discs : TurnsNo := 0;
   begin
      -- Check all positions, add if we own it
      for I in Dimension'Range loop
         for J in Dimension'Range loop
            if (State(I,J) = Player) then
               Discs := Discs + 1;
            end if;
         end loop;
      end loop;
      return Discs;
   end OwnDiscs;

   -- Monte Carlo Predictor
   -- Returns the probability of a particular player winning from a given state
   -- For a given number of iterations, makes random moves for both players until terminal
   -- Returns the percentage of iterations the player won
   function MonteCarlo (Player : BoardPoint; state : GameTree_Type; iterations : Positive) return Probability is
      Whitewins : Natural := 0;
      Blackwins : Natural := 0;
      Ties : Natural := 0;
      temp : GameTree_Type;
      tempChildren : ExpandedChildren;
      tempWinner : BoardPoint;
      Children : ExpandedChildren := Expand(state);
      --Import types from random package
      -- 99991 is a large prime number, this decreases the loss or randomness
      type Rand_Range is range 0..99991;
      package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
      seed : Rand_Int.Generator;

      Next_Int : Integer;
      -- End predictor
      endprobability : Probability;
   begin
      -- Random seed initialisation
      Rand_Int.Reset(seed);

      -- Perform 'iteration' number of games
      for I in 1..iterations loop
         -- Pick random next child
         Next_Int := Integer(Rand_Int.Random(seed));
         temp := Children.children(Next_Int mod Children.branching);

         Single_Iteration:
         loop
            -- Keep picking a child until we hit a terminal state
            -- Then tally up the winner
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
            Next_Int := Integer(Rand_Int.Random(seed));
            temp := tempChildren.children(Next_Int mod tempChildren.branching);
         end loop Single_Iteration;
      end loop;

      -- Debug print
      Put_Line(Player'Img & "'s turn: Black wins " & Blackwins'Img & " times and white wins " & Whitewins'Img & "times");
      if Whitewins = 0 and Blackwins = 0 then
         -- If we only saw ties, we 'half' won
         return 0.5;
      end if;
      -- Return the % of times the given player won
      if (Player = White) then
         endprobability := Long_Float(Whitewins) / Long_Float(Whitewins+Blackwins);

         return endprobability;
      elsif (Player = Black) then
         endprobability := Long_Float(Blackwins) / Long_Float(Whitewins+Blackwins);

         return endprobability;
      end if;

      return 0.0;
   end MonteCarlo;

   -- IO Operations for loading/storing weights
   CSV_File : File_Type;

   -- Load weights procedure
   procedure LoadWeights is
      Filename : constant String := "data.csv";
   begin
      Open(CSV_File, In_File, Filename);

      -- Load each set one by one
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
            -- Split the lines by ',' and store weights in between
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
         Weights.independent(Mobility) := Float'Value(Line);
      end;
      declare
         Line : String := Get_Line(CSV_File);
      begin
         Put_Line(Line);
         Weights.independent(Stability) := Float'Value(Line);
      end;
      declare
         Line : String := Get_Line(CSV_File);
      begin
         Put_Line(Line);
         Weights.independent(Internal) := Float'Value(Line);
      end;

      --Spread out piece weights to full board mapping
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
      -- Store weight sets one by one
      StoreWeightSet(EarlyGame);
      StoreWeightSet(MidGame);
      StoreWeightSet(LateGame);
      Close(CSV_File);

   end StoreWeights;

   procedure StoreWeightSet(Weights : FeatureSet) is
      Subs : Slice_Set;
      Next_Line : Unbounded_String;
      weightaverage : Float;
      -- number of corner symmetries
      divBy : Float := 4.0;
   begin
      --Line for each pieceweight line
      for i in Dimension range 0.. (Dimension'Last/2) loop
         Next_Line := To_Unbounded_String("");
         for j in Dimension range 0..i loop
            -- Condense board by taking average along 4 symmetries, or 8 if non corner
            weightaverage := Weights.piece(i,j)+Weights.piece(Dimension'Last-i,j)
                 +Weights.piece(Dimension'Last-i,Dimension'Last-j)+Weights.piece(i,Dimension'Last-j);
            if (i /= j) then
               weightaverage := weightaverage + Weights.piece(j,i) + Weights.piece(Dimension'Last-j,i)
                 +Weights.piece(Dimension'Last-j,Dimension'Last-i)+Weights.piece(j,Dimension'Last-i);
               -- 4+4 = number of symmetries of non corner pieces
               divBy := divBy + 4.0;
            end if;
            weightaverage := weightaverage / divBy;
            -- Add ',' in between each
            Next_Line := Next_Line & To_Unbounded_String(Float'Image(weightaverage)) & ',';
         end loop;
         -- Write to file
         Unbounded_IO.Put_Line(CSV_File, Next_Line);
      end loop;

      --Line for each peripheral weight
      Unbounded_IO.Put_Line(CSV_File, To_Unbounded_String(Float'Image(Weights.independent(Mobility))));
      Unbounded_IO.Put_Line(CSV_File, To_Unbounded_String(Float'Image(Weights.independent(Stability))));
      Unbounded_IO.Put_Line(CSV_File, To_Unbounded_String(Float'Image(Weights.independent(Internal))));
   end StoreWeightSet;

   -- Map to mirror reflection of board - basically fold coordinates in half
   function WeightMapping(i : Dimension) return Dimension is
   begin
      if(i > 4) then
         return Dimension'Last - i;
      else
         return i;
      end if;
   end;

      -- Pick out a feature set from the game phase
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
end TemporalDifference;
