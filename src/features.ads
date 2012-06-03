with Boards; use Boards;

package Features is

   procedure BuildStability(board : GameBoard; StabMatrix : out InfoMatrix);
   procedure CountStability(player : Players; board : GameBoard; stabmatrix : InfoMatrix; StablePieces : out Integer);
   procedure CountInternals(player : Players; board : GameBoard; internalmatrix : InfoMatrix; InternalPieces: out Integer);
   procedure UpdateStability(move : Place; board : GameBoard; StabMatrix : in out InfoMatrix);
   function CheckStability(move : Place; player : BoardPoint; board : GameBoard; StabMatrix : InfoMatrix) return Boolean;
   function CheckInternal(move : Place; board : GameBoard) return Boolean;

end Features;
