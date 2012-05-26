with Boards; use Boards;

package TemporalDifference is
   procedure TD(State, NewState : GameBoard; Player : Players);

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

   function TokenScore(State : in GameBoard; Player: in BoardPoint) return BoardValue;

   procedure TokenCount(State : in GameBoard; WhiteTokens, BlackTokens : out TurnsNo);

   function EndBoardValue(Player : BoardPoint; State : GameBoard;
                           NumMoves : Natural) return BoardValue;

   procedure LoadWeights;

   function WeightMapping(i : Dimension) return Dimension;

   pieceWeights : BoardPositionWeights := (others => (others => 1.0));
   mobilityWeight : FeatureWeight := 1.0;

   epsilon : Float := 0.1;
   alpha : Float := 0.1;

end TemporalDifference;
