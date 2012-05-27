package Boards is

   subtype BoardValue is Float;
   type FinalValue is new Integer range -1..1;
   subtype Dimension is Natural range 0 .. 9;

   type CBoardState is array (Dimension,Dimension) of Integer;

   type BoardPoint is (Empty, White, Black, Blocked);
   for BoardPoint use (Empty => 0, White => 1, Black => 2, Blocked => 3);
   subtype Players is BoardPoint range White .. Black;

   type GameBoard is array(Dimension, Dimension) of BoardPoint;
   type InfoMatrix is array(Dimension, Dimension) of Boolean;
   pragma Pack(InfoMatrix);

   EmptyMatrix : constant InfoMatrix := (others => (others => False));

   type Coordinate is (x, y);
   type Place is array(Coordinate) of Dimension;

   subtype TurnsNo is Natural range 0 .. 96;

   -- Information on the game state
   type State_Type is record
      justWent : BoardPoint := Blocked;
      TokensTaken : TurnsNo := 0;
      spot : Place;
      turnsleft : TurnsNo := 0;
      current_state : GameBoard;
      StableNodes : InfoMatrix;
      InternalNodes : InfoMatrix;
   end record;

   function NextPlayer(player : BoardPoint) return BoardPoint;

   procedure Winner(State : GameBoard; Winner : out BoardPoint);

   function ValidMove(player : BoardPoint; board : in GameBoard; movex : in Dimension; movey : in Dimension) return Natural;

   procedure AdvanceMove(player : BoardPoint; board : in out GameBoard; movex : Dimension; movey : Dimension);

   function Image(state : State_Type) return String;

   function Image(spot : Place) return String;

   function Image(board : GameBoard) return String;

end Boards;
