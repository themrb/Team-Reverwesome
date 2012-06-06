with Boards; use Boards;

package Features is

   -- Build the stability of a game board from scratch
   procedure BuildStability(board : GameBoard; StabMatrix : out InfoMatrix);
   -- Count the number of stable pieces for a given player
   procedure CountStability(player : Players; board : GameBoard; stabmatrix : InfoMatrix; StablePieces : out Integer);
   -- Update stability matrix given a move (evaluate effect of new move on stability)
   procedure UpdateStability(move : Place; board : GameBoard; StabMatrix : in out InfoMatrix);
   -- Check the stability of a particular place for a given player
   function CheckStability(move : Place; player : BoardPoint; board : GameBoard; StabMatrix : InfoMatrix) return Boolean;

   -- Count the number of internal nodes
   procedure CountInternals(player : Players; board : GameBoard; internalmatrix : InfoMatrix; InternalPieces: out Integer);
   -- Check if a node is internal
   function CheckInternal(move : Place; board : GameBoard) return Boolean;

end Features;
