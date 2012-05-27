with Boards; use Boards;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

package GameTree is

   type GameTree_Type;
   type GameTree_Access is access all GameTree_Type;

   -- Representation of a game tree node
   type GameTree_Type is record
      state : State_Type;
      expanded : Boolean := False;
   end record;

   type ChildrenArray is array(TurnsNo) of GameTree_Type;

   type HistoryType is record
      History : ChildrenArray;
      Index  : TurnsNo := 0;
   end record;

   -- Information on the game state
   type ExpandedChildren is record
      branching : TurnsNo;
      nomove : Boolean := False;
      children : ChildrenArray;
   end record;

   function Expand(state : in GameTree_Type) return ExpandedChildren;

   function NumMoves(Board : GameBoard; Player : Players) return Natural;

   function Terminal(board : in GameBoard) return Boolean;

end GameTree;
