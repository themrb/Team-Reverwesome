package Boards is

   type BoardValue is new Integer;
   type FinalValue is new Integer range -1..1;
   type Dimension is new Natural range 0 .. 9;

   subtype FeatureWeight is BoardValue'Base;
   type BoardPositionWeights is array(Dimension, Dimension) of FeatureWeight;
   defaultCorner : constant FeatureWeight := 846; -- ~(92^2)/10
   defaultWeights : constant BoardPositionWeights := (0 => (0 => defaultCorner,
                                                            9=> defaultCorner,
                                                            others => 1),
                                                      9 => (0 => defaultCorner,
                                                            9=> defaultCorner,
                                                            others => 1),
                                                      others => (others => 1)
                                                     );
   defaultMobility : constant FeatureWeight := 92;

   type CBoardState is array (Dimension,Dimension) of Integer;

   type BoardPoint is (Empty, White, Black, Blocked);
   for BoardPoint use (Empty => 0, White => 1, Black => 2, Blocked => 3);

   type GameBoard is array(Dimension, Dimension) of BoardPoint;

   type Coordinate is (x, y);
   type Place is array(Coordinate) of Dimension;

   subtype TurnsNo is Natural range 0 .. 92;

   -- Information on the game state
   type State_Type is record
      justWent : BoardPoint := Blocked;
      TokensTaken : TurnsNo := 0;
      spot : Place;
      turns : TurnsNo := 0;
      current_state : GameBoard;
   end record;

   procedure EndBoardValue(Player : BoardPoint; State : GameBoard;
                           NumMoves : Natural; Score : out BoardValue);

   function NextPlayer(player : BoardPoint) return BoardPoint;

   procedure TokenScore(State : GameBoard; Player: in BoardPoint;
                        Weights: in BoardPositionWeights; Score: in out BoardValue);

--   function AdvanceMove(state : State_Type; move : Place) return State_Type;

   function ValidMove(player : BoardPoint; board : in GameBoard; movex : in Dimension; movey : in Dimension) return Natural;

   procedure AdvanceMove(player : BoardPoint; board : in out GameBoard; movex : Dimension; movey : Dimension);

   function Image(state : State_Type) return String;

   function Image(spot : Place) return String;

   function Image(board : GameBoard) return String;

end Boards;