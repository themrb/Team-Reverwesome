with Boards; use Boards;

package Features is

   function CheckStability(move : Place; player : BoardPoint; board : GameBoard) return Boolean;
   function CheckInternal(move : Place; board : GameBoard) return Boolean;

end Features;
