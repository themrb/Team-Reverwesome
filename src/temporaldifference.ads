with Boards; use Boards;
with GameTree; use GameTree;

package TemporalDifference is
   procedure TD(History: HistoryType; Player : Players; Feedback : Float);

   subtype FeatureWeight is BoardValue'Base;
   type BoardPositionWeights is array(Dimension, Dimension) of FeatureWeight;

   type FeatureSet is record
      piece : BoardPositionWeights;
      mobility : FeatureWeight;
      stability : FeatureWeight;
      internal : FeatureWeight;
   end record;

   subtype numIndWeights is Natural range 0..2;

--     basicSet : FeatureSet := ((others => (others => 0.0)), 1.0);

   function TokenScore(State : in GameBoard; Player: in BoardPoint;
                        Weights : BoardPositionWeights) return BoardValue;

   procedure TokenCount(State : in GameBoard; WhiteTokens, BlackTokens : out TurnsNo);

   function EndBoardValue(Player : Players; State : State_Type; Moves : TurnsNo;
                          Set : FeatureSet) return BoardValue;

   function EndBoardValue(Player : Players; State : State_Type; Set: FeatureSet) return BoardValue;

   function ChangeInValue(Player : Players; Board : State_Type; OldSet, NewSet : FeatureSet; Step : Float) return BoardValue;

   procedure CloseFile;
   procedure LoadWeights;
   procedure LoadWeightSet(Weights : out FeatureSet);
   procedure StoreWeights;
   procedure StoreWeightSet(Weights : FeatureSet);

   function WeightMapping(i : Dimension) return Dimension;

   subtype Probability is Long_Float;

   function OwnDiscs(Player : Players; State : in GameBoard) return TurnsNo;
   function MonteCarlo (Player : BoardPoint; state : GameTree_Type; iterations : Positive) return Probability;

   lambda : Float := 0.9;
   alpha : Float := 0.0001;

   cease : Boolean := False;

   EarlyGame : FeatureSet;
   MidGame : FeatureSet;
   LateGame : FeatureSet;

end TemporalDifference;
