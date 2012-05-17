with Boards; use Boards;

package TemporalDifference is
   procedure TD(State : GameBoard; NumMoves : Natural);

   subtype FeatureWeight is BoardValue'Base;
   type BoardPositionWeights is array(Dimension, Dimension) of FeatureWeight;
--     defaultCorner : constant FeatureWeight := 846; -- ~(92^2)/10
--     defaultWeights : constant BoardPositionWeights := (0 => (0 => defaultCorner,
--                                                              9=> defaultCorner,
--                                                              others => 1),
--                                                        9 => (0 => defaultCorner,
--                                                              9=> defaultCorner,
--                                                              others => 1),
--                                                        others => (others => 1)
--                                                       );
--     defaultMobility : constant FeatureWeight := 92;

   procedure TokenScore(State : GameBoard; Player: in BoardPoint;
                        Weights: in BoardPositionWeights; Score: in out BoardValue);

   procedure TokenCount(State : GameBoard; WhiteTokens : out TurnsNo; BlackTokens : out TurnsNo);

   procedure EndBoardValue(Player : BoardPoint; State : GameBoard;
                           NumMoves : Natural; Score : out BoardValue);

   procedure LoadWeights;

   function WeightMapping(i : Dimension) return Dimension;

   pieceWeights : BoardPositionWeights := (others => (others => 1.0));
   mobilityWeight : FeatureWeight := 1.0;

end TemporalDifference;
