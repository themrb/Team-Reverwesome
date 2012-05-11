package Boards is

   type BoardValue is new Integer range -92..92;
   type FinalValue is new Integer range -1..1;
   type Dimension is new Natural range 0 .. 9;

   type BoardPoint is (Empty, White, Black, Blocked);

   type Board_Type is array(Dimension, Dimension) of BoardPoint;

   type Coordinate is (x, y);
   type Place is array(Coordinate) of Dimension;

   subtype TurnsNo is Natural range 0 .. 92;

   -- Information on the game state
   type State_Type is record
      justWent : BoardPoint;
      TokensTaken : TurnsNo;
      spot : Place;
      turns : TurnsNo;
      current_state : Board_Type;
   end record;

   Empty_Board : constant State_Type := (Empty, (1,1,1), 0, (others => (others => (others => False))), (others => (others => (others => False))));

   function NextPlayer(prev : BoardPoint) return BoardPoint;

   function AdvanceMove(state : State_Type; move : Place) return State_Type;

   function ValidMove(player : BoardPoint; board : in BoardState; movex : in Dimension; movey : in Dimension) return Natural;

   function Terminal(state : in State_Type) return Boolean;

   function Image(state : State_Type) return String;

   function Image(spot : Place) return String;

   function Image(board : Board_Type) return String;

end Boards;
