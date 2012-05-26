with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Boards;
with GameTree; use GameTree;

package body TemporalDifference is

   procedure TD(State, NewState : GameBoard; Player : Players) is
      pieceReward : BoardPositionWeights := (others => (others => 0.0));
      mobilityReward : FeatureWeight := 0.0;
   begin
      if(Terminal(NewState)) then
         declare
            BlackC, WhiteC : TurnsNo;
            Diff : FeatureWeight;
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
      end if;

      declare
         curVal, nextVal : BoardValue;
         nMoves, nNewMoves : TurnsNo;
      begin
         nMoves := NumMoves(State, Player);
         nNewMoves := NumMoves(NewState, NextPlayer(Player));
         curVal := EndBoardValue(Player, State, nMoves);
         nextVal := EndBoardValue(NextPlayer(Player), NewState, nNewMoves);

         for i in Dimension'Range loop
            for j in Dimension'Range loop
               pieceWeights(i,j) := pieceWeights(i,j) + alpha *
                 (nextVal - curVal + pieceReward(i,j));
            end loop;
         end loop;

         mobilityReward := mobilityReward*FeatureWeight(nMoves - nNewMoves);

         mobilityWeight := mobilityWeight + alpha *
           (nextVal - curVal + mobilityReward);
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
                  pieceWeights(Line_No, Dimension(i)-1) := Float'Value(Sub);
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
            pieceWeights(i,j)
              := pieceWeights(WeightMapping(i),WeightMapping(j));
         end loop;
      end loop;
   end LoadWeights;

   function WeightMapping(i : Dimension) return Dimension is
   begin
      if(i > 4) then
         return Dimension'Last - i;
      else
         return i;
      end if;
   end;
end TemporalDifference;
