with GameTree; use GameTree;
with Boards; use Boards;
with TemporalDifference; use TemporalDifference;

package MinMax is
   -- NegaMax search procedure
   procedure NegaMax (Player : BoardPoint; state : in out GameTree_Type;  depth : in  TurnsNo; outValue : out BoardValue;
                      alpha, beta : in BoardValue;  bestMove : out Place);

   -- Pick out feature set given game phase
   function PhaseToSet(phase : Game_Phase) return FeatureSet;

   -- Board evaluation for terminals - returns -inf or inf
   function TerminalValue(state : GameBoard; Player : BoardPoint) return BoardValue;

end MinMax;
