package Boards is

   -- Board Evaluation Value
   subtype BoardValue is Float;
   -- Terminal value of a board
   type FinalValue is new Integer range -1..1;

   -- Dimension length of the board
   subtype Dimension is Natural range 0 .. 9;
   -- Different board point types
   type BoardPoint is (Empty, White, Black, Blocked);
   for BoardPoint use (Empty => 0, White => 1, Black => 2, Blocked => 3);
   -- Different types of players
   subtype Players is BoardPoint range White .. Black;

   -- Board representation, two dimensional array of boardpoints
   type GameBoard is array(Dimension, Dimension) of BoardPoint;
   -- C++ board state storage type
   type CBoardState is array (Dimension,Dimension) of Integer;

   -- Information matrix for stability and internality tracking
   type InfoMatrix is array(Dimension, Dimension) of Boolean;
   -- Pack into bit shifting operations
   pragma Pack(InfoMatrix);
   -- Empty information matrix
   EmptyMatrix : constant InfoMatrix := (others => (others => False));

   -- Cooordinate on a game board
   type Coordinate is (x, y);
   type Place is array(Coordinate) of Dimension;

   -- Total number of possible turns
   subtype TurnsNo is Natural range 0 .. 96;

   -- Game phase information
   type Game_Phase is (PEarlyGame, PMidGame, PLateGame);

   -- Information on the game state
   type State_Type is record
      -- Player who just went
      -- (initialised to blocked as it needs to be set after creation)
      justWent : BoardPoint := Blocked;
      -- The number of tokens the previous move took
      TokensTaken : TurnsNo := 0;
      -- Coordinates of the previous move
      spot : Place;
      -- Number of possible turns left
      turnsleft : TurnsNo := 0;
      -- Current board information
      current_state : GameBoard;
      -- Current Game Phase
      Current_Phase : Game_Phase;
      -- Number of corners taken (for phase tracking)
      Corners : Natural := 0;
      -- Board Stability information
      StableNodes : InfoMatrix;
      -- Board Internality information
      InternalNodes : InfoMatrix;
   end record;

   -- Switch player
   function NextPlayer(player : BoardPoint) return BoardPoint;

   -- Determine winner of a board
   procedure Winner(State : GameBoard; Winner : out BoardPoint);

   -- Check if x,y is a valid move
   function ValidMove(player : BoardPoint; board : in GameBoard; movex : in Dimension; movey : in Dimension) return Natural;

   -- Advance the board state given a move
   procedure AdvanceMove(player : BoardPoint; board : in out GameBoard; movex : Dimension; movey : Dimension);

   -- ToString for a state
   function Image(state : State_Type) return String;

   -- ToString for a place
   function Image(spot : Place) return String;

   --ToString for a board
   function Image(board : GameBoard) return String;

end Boards;
