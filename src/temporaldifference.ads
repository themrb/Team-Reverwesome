with Boards; use Boards;
with GameTree; use GameTree;

package TemporalDifference is
   procedure TD(History: HistoryType; Player : Players; Feedback : Float);

   -- Numerical weight on features
   subtype FeatureWeight is BoardValue'Base;
   -- Array for weight on each board position
   type BoardPositionWeights is array(Dimension, Dimension) of FeatureWeight;

   -- Set of feature weights
   type FeatureSet is record
      piece : BoardPositionWeights;
      mobility : FeatureWeight;
      stability : FeatureWeight;
      internal : FeatureWeight;
   end record;

   -- Number of non-position weights the program has
   subtype numIndWeights is Natural range 0..2;

   -- Returns the token score of a player on a given board
   -- NOTE: This takes position weights into account
   function TokenScore(State : in GameBoard; Player: in BoardPoint;
                        Weights : BoardPositionWeights) return BoardValue;

   -- Returns the raw number of tokens of both players
   procedure TokenCount(State : in GameBoard; WhiteTokens, BlackTokens : out TurnsNo);

   -- Static evaluation function of a board, given feature weights
   function EndBoardValue(Player : Players; State : State_Type;
                          Set : FeatureSet) return BoardValue;

   -- Gives a change in value based on difference and step size, as part of TD lambda
   function ChangeInValue(Player : Players; Board : State_Type; OldSet, NewSet : FeatureSet; Step : Float) return BoardValue;

   -- IO operations for loading and storing to csv file

   procedure CloseFile;
   -- Loads all weights
   procedure LoadWeights;
   -- Loads a set of weights (used for each phase)
   procedure LoadWeightSet(Weights : out FeatureSet);
   -- Stores all weights
   procedure StoreWeights;
   -- Stores a set of weights (used for each phase)
   procedure StoreWeightSet(Weights : FeatureSet);

   function WeightMapping(i : Dimension) return Dimension;

   -- Counts number of tiles a given player owns
   function OwnDiscs(Player : Players; State : in GameBoard) return TurnsNo;

   -- Monte Carlo probability function
   subtype Probability is Long_Float;
   function MonteCarlo (Player : BoardPoint; state : GameTree_Type; iterations : Positive) return Probability;

   -- Static values to use in TD lambda algorithm
   lambda : Float := 0.9;
   alpha : Float := 0.00001;

   cease : Boolean := False;

   -- Sets of feature weights for each game phase
   EarlyGame : FeatureSet;
   MidGame : FeatureSet;
   LateGame : FeatureSet;

end TemporalDifference;
