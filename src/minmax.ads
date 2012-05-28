with GameTree; use GameTree;
with Boards; use Boards;
with TemporalDifference; use TemporalDifference;

package MinMax is
   procedure NegaMax (Player : BoardPoint; state : in out GameTree_Type;  depth : in  TurnsNo; outValue : out BoardValue;
                      alpha, beta : in BoardValue;  bestMove : out Place);

   function PhaseToSet(phase : Game_Phase) return FeatureSet;

   function TerminalCheck(state : GameBoard; Player : BoardPoint) return BoardValue;

end MinMax;
