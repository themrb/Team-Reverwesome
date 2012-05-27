with Boards; use Boards;

package Features is

   procedure CountStability(player : Players; board : GameBoard; stabmatrix : in out InfoMatrix; StablePieces : out Integer);
   procedure CountInternals(player : Players; board : GameBoard; internalmatrix : in out InfoMatrix; InternalPieces: out Integer);
   function CheckStability(move : Place; player : BoardPoint; board : GameBoard) return Boolean;
   function CheckInternal(move : Place; board : GameBoard) return Boolean;

end Features;
