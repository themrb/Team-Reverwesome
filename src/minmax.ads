with GameTree; use GameTree;
with Boards; use Boards;

package MinMax is
   procedure NegaMax (Player : BoardPoint; state : in out GameTree_Type;  depth : in  TurnsNo; outValue : out BoardValue;
                  alpha, beta : in BoardValue;  bestMove : out Place);

   subtype Probability is Long_Float;

   function MonteCarlo (Player : BoardPoint; state : GameTree_Type; iterations : Positive) return Probability;

end MinMax;
