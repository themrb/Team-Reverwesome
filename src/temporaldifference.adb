with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;

package body TemporalDifference is

   procedure TD(State : GameBoard; NumMoves : Natural) is

   begin
      null;
   end TD;

   procedure EndBoardValue(Player : BoardPoint; State : GameBoard;
                           NumMoves : Natural; Score : out BoardValue) is
   begin
      Score := 0.0;
      TokenScore(State, Player, pieceWeights, Score);
      -- Weighting on the number of available moves
      Score := Score + (FeatureWeight(NumMoves) * mobilityWeight);
   end EndBoardValue;


   procedure TokenScore(State : GameBoard; Player: in BoardPoint;
                        Weights: in BoardPositionWeights; Score: in out BoardValue) is
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
   end TokenScore;

   procedure TokenCount(State : GameBoard; WhiteTokens : out TurnsNo; BlackTokens : out TurnsNo) is
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

   procedure StoreWeights is
      CSV_File : File_Type;
      Filename : constant String := "data2.csv";
      Line_No : Natural := 0;
      Subs : Slice_Set;
      Next_Line : Unbounded_String;
   begin
      Create(CSV_File, Out_File, Filename);

      --Line for each pieceweight line
      for i in reverse Dimension range 0.. (Dimension'Last/2) loop
         Next_Line := To_Unbounded_String("");
         for j in reverse Dimension range 0..(Dimension'Last/2-i) loop
            Next_Line := Next_Line & To_Unbounded_String(Float'Image(pieceWeights(i,j)) & ",");
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
