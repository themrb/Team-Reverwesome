with Ada.Text_IO; use Ada.Text_IO;
with Exceptions; use Exceptions;
with Ada.Task_Attributes;
with Configure; use Configure;

package body Workers is

   task body Explorer is
      current, parent : GameTree_Type;
      value, a, b : BoardValue;
      move : Place; -- Not needed;
   begin
      loop
         toExplore.all.Next(current, parent, a, b);
         Negamax(NextPlayer(my_player), current, Configure.depth - 1, value, -b, -a, move);
         value := - value;
         toExplore.all.Report(current, parent, value);
         delay 0.0;
      end loop;
   end Explorer;

   ----------------------

   protected body BeingExplored is

      procedure OneOffInit is
      begin
         Ada.Numerics.Float_Random.Reset(fSeed);
         RandInt.Reset(iSeed);
      end;

      procedure Initialise (parent : in GameTree_Type) is
      begin
         root := parent;
         index := TurnsNo'First;
         alpha := BoardValue'First;
         beta := BoardValue'Last;
         value := BoardValue'First; -- Set to minimum board-value;
         more := True;
         Preprocessing;
      end Initialise;

      entry GetResult(move : out Place) when not more is
      begin
         move := best;
      end GetResult;

      -- Gets the next GameTree node to explore.
      entry Next(node, parent : out GameTree_Type; a, b : out BoardValue)
         when more is
      begin
         node := boards(index);
         a := alpha;
         b := beta;
         parent := root;
         if(index < children.branching - 1) then
            index := index + 1;
         else
            more := False;
         end if;

      end Next;

      procedure Report(board, parent : in GameTree_Type; bValue : in BoardValue) is
      begin
         -- This is basically a chunk of the 'Max' part of Minmax
         if(parent = root) then -- Have to make sure result is still relevant
            if(bValue > value) then
               value := bValue;
               best := board.state.spot;
            end if;

            if(value >= beta) then -- min sees no way of avoiding max's win
               best := board.state.spot;
               more := False;
            end if;
            if(value > alpha) then
               alpha := value;
               best := board.state.spot;
            end if;
         end if;
      end Report;

      procedure Preprocessing is
      begin
         -- Shouldn't encounter a Terminal state

         children := Expand(root);
         boards := children.children;
         best := boards(TurnsNo'First).state.spot;
         -- Should hopefully never need to Depth Check.

         -- Also, never encounter a 'no-move' scenario at this level.

         if (children.branching = 1) then
            more := False;
            return;
         end if;

         declare
            randomI : Integer;
            randomF : Float;
         begin
            randomF := Ada.Numerics.Float_Random.Random(fSeed);
            if(randomF < Configure.epsilon) then
               randomI := Integer(RandInt.Random(iSeed)) mod (children.branching);
               best := boards(randomI).state.spot;
               more := False;
            end if;
         end;
      end Preprocessing;
   end BeingExplored;


end Workers;
