with Boards; use Boards;
with MinMax; use MinMax;
with GameTree; use GameTree;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package Workers is
   package RandInt is new Ada.Numerics.Discrete_Random(TurnsNo);

   -- Protected object which manages data about and including the set of nodes
   -- currently being explored, or which are going to be explored in this pass.
   protected type BeingExplored is
      -- Allows explorer task to get the next node to be explored.
      entry Next(node, parent : out GameTree_Type; a, b : out BoardValue);

      -- Allows player task to retrieve the result of exploring the game-tree.
      entry GetResult(move : out Place);

      -- Allows explorer tasks to report what they found.
      procedure Report(board, parent : in GameTree_Type; bValue : in BoardValue);

      -- Initialise the set to explore. Note that we do this for every turn.
      procedure Initialise (parent : in GameTree_Type);

      procedure OneOffInit;
   private
      root : GameTree_Type;
      best : Place;
      children : ExpandedChildren;
      boards : ChildrenArray;
      index : TurnsNo := TurnsNo'First;
      more : Boolean := False; -- Any more boards to check?
      alpha, beta, value : BoardValue;

      fSeed : Ada.Numerics.Float_Random.Generator;
      iSeed : RandInt.Generator;

      procedure Preprocessing;         -- Look ahead to see if we have any terminals.
   end BeingExplored;

   -- Task which actually explores the game-tree.
   task type Explorer(toExplore : access BeingExplored) is
   end Explorer;

end Workers;
