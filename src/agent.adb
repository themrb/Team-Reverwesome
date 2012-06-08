with Ada.Text_IO; use Ada.Text_IO;
with Boards; use Boards;
with GameTree; use GameTree;
with MinMax; use MinMax;
with Configure; use Configure;
with Exceptions; use Exceptions;
with TemporalDifference; use TemporalDifference;
with Features; use Features;
with Workers; use Workers;

package body Agent is

   -- Shared memory for C++ and Ada
   -- Imported using compiler directives

   -- C++ array of the board
   ccurrentstate : CBoardState;
   -- Our colour
   cplayercolour : Integer;
   -- Instruct C++ which move to make next
   cnextmovey : Integer;
   cnextmovex : Integer;
   -- Previous move made (given from server message)
   cprevmovey : Integer;
   cprevmovex : Integer;
   -- The winner of a game
   cwinner : Integer;
   -- Timing constraints
   TimeLeft : Long_Float;
   PrevTimeLeft : Long_Float;
   pragma import(cpp, ccurrentstate, "currentcstate");
   pragma import(cpp, cplayercolour, "playercolour");
   pragma import(cpp, cnextmovey, "nextmovey");
   pragma import(cpp, cnextmovex, "nextmovex");
   pragma import(cpp, cprevmovey, "prevmovey");
   pragma import(cpp, cprevmovex, "prevmovex");
   pragma import(cpp, cwinner, "cwinner");
   pragma import(cpp, TimeLeft, "timeleft");

   -- Ada representation of board state and next move choice
   currentstate : GameBoard;
   move : Place;

   task body Main is
      History : HistoryType;
      toExplore : aliased BeingExplored;
      workers : array(Natural range 1..Configure.workerTasks) of Explorer(toExplore'Access);
      -- Whether time has been previously restricted
      BumpedDown : Boolean := False;
   begin
      accept Initialise  do
         -- Essentially free time to 'boot up' Ada's systems

         -- Initialise Game Phase
         CurrentGamePhase := PEarlyGame;
         -- Load feature weights
         LoadWeights;

         -- Set player colour
         if (cplayercolour = 1) then
            my_player := White;
         elsif (cplayercolour = 2) then
            my_player := Black;
         end if;

         -- Initialise explorers
         toExplore.OneOffInit;
      end Initialise;

      -- Selectively accept either NewMove or EndGame in a loop
      Main_Loop:
      loop
         select
            accept NewMove  do
               declare
                  treeroot : GameTree_Type;
                  turnsleft : TurnsNo := 0;
               begin
                  -- Asked to choose a new move

                  -- Print timing information
                  Put_Line("We have " & TimeLeft'Img & " seconds left");
                  -- Read in CPP values for the board
                  for I in Dimension'Range loop
                     for J in Dimension'Range loop
                        currentstate(I,J) := BoardPoint'Val(ccurrentstate(I,J));
                        if (currentstate(I,J) = Empty) then
                           turnsleft := turnsleft + 1;
                        end if;
                     end loop;
                  end loop;

                  -- Print current board state
                  Put_Line(Image(currentstate));

                  -- Check game state and transition if required
                  PhaseTransition(CurrentGamePhase, currentstate, CurrentCorners);

                  -- Initialise game tree
                  treeroot.state.justWent := NextPlayer(my_player);
                  treeroot.state.current_state := currentstate;
                  treeroot.state.turnsleft := turnsleft;

                  -- Find initial stability
                  Put_Line("Finding stability");
                  BuildStability(currentstate, treeroot.state.StableNodes);

                  -- Find initial internality
                  treeroot.state.InternalNodes := EmptyMatrix;
                  for i in Dimension'Range loop
                     for j in Dimension'Range loop
                        if (CheckInternal((i,j),currentstate)) then
                           treeroot.state.InternalNodes(i,j) := True;
                        end if;
                     end loop;
                  end loop;

                  --Set game phase
                  treeroot.state.Current_Phase := CurrentGamePhase;

                  --Save the state we're in
                  History.History(History.Index) := treeroot;
                  History.Index := History.Index + 1;

--                    Configure.depth := 6;
--                    if TimeLeft < 5.0 then
--                       Configure.depth := 2;
--                    elsif TimeLeft < 15.0 then
--                       Configure.depth := 4;
--                    elsif (turnsleft < 13) then
--                       Configure.depth := 12;
--                    elsif TimeLeft < 30.0 or (PrevTimeLeft - TimeLeft) > 5.0 then
--                       Configure.depth := 5;
--                       BumpedDown := True;
--                    elsif PrevTimeLeft - TimeLeft < 3.0 and not BumpedDown and TimeLeft > 50.0 then
--                       Configure.depth := 8;
--                    else
--                       Configure.depth := 7;
--                    end if;

                  -- Print depth information
                  PrevTimeLeft := TimeLeft;
                  Put_Line("Depth at " & Configure.depth'Img);

                  -- Send explorer tasks in
                  toExplore.Initialise(treeroot);
                  toExplore.GetResult(move);

                  -- Evaluate greediness of current move and print choice
                  declare
                     temppieces : Natural := ValidMove(my_player, currentstate, move(x), move(y));
                  begin
                     Put_Line("We'll get " & TurnsNo'Image(temppieces) & "for moving at" & Dimension'Image(move(x)) & "," & Dimension'Image(move(y)));
                  end;
                  -- Pass back next move values to C++
                  cnextmovey := Integer(move(x));
                  cnextmovex := Integer(move(y));
               end;

               -- Print out board from after the move is made
               AdvanceMove(my_player, currentstate, move(x), move(y));
               Put_Line("After moving:");
               Put_Line(Image(currentstate));

               -- Transition the game phase if our last move just pushed us over
               if CurrentGamePhase = PEarlyGame then
                  -- If an edge has been taken, transition to midgame
                  if move(x) = Dimension'First or move(x) = Dimension'Last or move(y) = Dimension'Last or move(y) = Dimension'First then
                     CurrentGamePhase := PMidGame;
                  end if;
               elsif CurrentGamePhase = PMidGame then
                  -- If two corners have been taken, transition to endgame
                  if (move(x) = Dimension'First and move(y) = Dimension'First) or (move(x) = Dimension'First and move(y) = Dimension'Last)
                    or (move(x) = Dimension'Last and move(y) = Dimension'First) or (move(x) = Dimension'Last and move(y) = Dimension'Last) then
                     CurrentCorners := CurrentCorners + 1;
                     if CurrentCorners >= 2 then
                        CurrentGamePhase := PLateGame;
                     end if;
                  end if;
               end if;
            end NewMove;
         or
            accept GameEnd  do
               -- Learn from the game history
               declare
                  feedback : Float := 0.0;
                  Board : GameBoard := History.History(History.Index - 1).state.current_state;
                  GameWinner : Players;
               begin

                  -- Check who won
                  if (cwinner = 1) then
                     GameWinner := White;
                  elsif (cwinner = 2) then
                     GameWinner := Black;
                  end if;

                  -- Decide if we won
                  declare
                     WhiteCount, BlackCount : TurnsNo;
                  begin
                     TokenCount(Board, WhiteCount, BlackCount);
                     if(GameWinner = my_player) then
                        feedback := abs(Float(WhiteCount - BlackCount));
                     elsif (GameWinner = NextPlayer(my_player)) then
                        feedback := -abs(Float(WhiteCount - BlackCount));
                     end if;
                  end;

                  Put_Line("Feedback : " & feedback'Img);
                  -- Call learning algorithm
                  TD(History, my_player, feedback);
                  -- Store our updated weights
                  StoreWeights;
               end;

               -- Shut down the workers
               for i in workers'Range loop
                  abort workers(i);
               end loop;
            end GameEnd;
            exit Main_Loop;
            -- Exit main loop, terminating the task
         end select;
      end loop Main_Loop;
   end Main;

   procedure StartUp is
   begin
      -- Boot up task on the heap
      MainTask := new Main;
      -- Ask main to initialise
      MainTask.Initialise;
   end StartUp;

   procedure Ada_Subroutine is
   begin
      -- Ask to make new move
      MainTask.NewMove;

   exception
      when E : others =>
         Show_Exception (E);
   end Ada_Subroutine;

   procedure GameEnd is
   begin
      -- Ask to terminate
      MainTask.GameEnd;
   end GameEnd;

   -- Check game state and transition if required
   -- Comprehensive state check rather than using last move
   procedure PhaseTransition(CurrentGamePhase : in out Game_Phase; State : in GameBoard;
                             Corners : in out Natural) is
   begin
      if CurrentGamePhase = PEarlyGame then
         -- If an edge has been taken, transition to midgame
         for x in Dimension'Range loop
            if State(x,Dimension'First) = Black or State(x,Dimension'First) = White
              or State(x,Dimension'Last) = Black or State(x,Dimension'Last) = White
              or State(Dimension'First,x) = Black or State(Dimension'First,x) = White
              or State(Dimension'Last,x) = Black or State(Dimension'Last,x) = White then
               CurrentGamePhase := PMidGame;
            end if;
         end loop;
      elsif CurrentGamePhase = PMidGame then
         -- If two corners have been taken, transition to endgame
         Corners := 0;
         if ((State(Dimension'First, Dimension'First) = Black) or (State(Dimension'First, Dimension'First) = White)) then
            Corners := Corners + 1;
         end if;
         if State(Dimension'Last, Dimension'Last) = Black or State(Dimension'Last, Dimension'Last) = White then
            Corners := Corners + 1;
         end if;
         if State(Dimension'First, Dimension'Last) = Black or State(Dimension'First, Dimension'Last) = White then
            Corners := Corners + 1;
         end if;
         if State(Dimension'Last, Dimension'First) = Black or State(Dimension'Last, Dimension'First) = White then
            Corners := Corners + 1;
         end if;
         if Corners >= 2 then
            CurrentGamePhase := PLateGame;
         end if;
      end if;
   end PhaseTransition;

end Agent;
