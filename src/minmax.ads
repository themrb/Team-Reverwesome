with GameTree; use GameTree;
with Boards; use Boards;

package MinMax is

   procedure Min (Player: BoardPoint; state : in out GameTree_Type;  depth : in  TurnsNo; outValue : out BoardValue;
                  alpha, beta : in BoardValue;  bestMove : out Place);
   procedure Max (Player : BoardPoint; state : in out GameTree_Type;  depth : in  TurnsNo; outValue : out BoardValue;
                  alpha, beta : in BoardValue;  bestMove : out Place);

   subtype Probability is Long_Float range 0.0 .. 1.0;

   function MonteCarlo (state : GameTree_Type; iterations : Positive) return Probability;

end MinMax;
