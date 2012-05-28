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

   ccurrentstate : CBoardState;
   cplayercolour : Integer;
   cnextmovey : Integer;
   cnextmovex : Integer;
   cprevmovey : Integer;
   cprevmovex : Integer;
   cwinner : Integer;
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

   currentstate : GameBoard;
   move : Place;

   task body Main is
      History : HistoryType;
      toExplore : aliased BeingExplored;
      workers : array(Natural range 1..Configure.workerTasks) of Explorer(toExplore'Access);
      BumpedDown : Boolean := False;
   begin
      accept Initialise  do

         CurrentGamePhase := PEarlyGame;
         LoadWeights;

         if (cplayercolour = 1) then
            my_player := White;
         elsif (cplayercolour = 2) then
            my_player := Black;
         end if;

         toExplore.OneOffInit;

         --Put_Line("not weights or players");
         -- spawn explorer tasks as required
         -- this is basically our set up to be ready to play the game.
         -- the clock hasn't started yet - free time
      end Initialise;

      Main_Loop:
      loop
         select
            accept NewMove  do
               declare
                  treeroot : GameTree_Type;
                  turnsleft : TurnsNo := 0;
               begin
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

                  Put_Line(Image(currentstate));

                  PhaseTransition(CurrentGamePhase, currentstate, CurrentCorners);

                  -- Initialise game tree
                  treeroot.state.justWent := NextPlayer(my_player);
                  treeroot.state.current_state := currentstate;
                  treeroot.state.turnsleft := turnsleft;

                  treeroot.state.StableNodes := EmptyMatrix;
                  treeroot.state.InternalNodes := EmptyMatrix;
                  treeroot.state.Current_Phase := CurrentGamePhase;

                  for i in Dimension'Range loop
                     for j in Dimension'Range loop
                        if (CheckStability((i,j),my_player,currentstate)) then
                           treeroot.state.StableNodes(i,j) := True;
                        end if;
                        if (CheckInternal((i,j),currentstate)) then
                           treeroot.state.InternalNodes(i,j) := True;
                        end if;
                     end loop;
                  end loop;

                  History.History(History.Index) := treeroot;
                  History.Index := History.Index + 1;

                  toExplore.Initialise(treeroot);
                  toExplore.GetResult(move);

                  Configure.depth := 4;
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
                  PrevTimeLeft := TimeLeft;
                  Put_Line("Depth at " & Configure.depth'Img);

                  declare
                     temppieces : Natural := ValidMove(my_player, currentstate, move(x), move(y));
                  begin
                     Put_Line("We'll get " & TurnsNo'Image(temppieces) & "for moving at" & Dimension'Image(move(x)) & "," & Dimension'Image(move(y)));
                  end;
                  cnextmovey := Integer(move(x));
                  cnextmovex := Integer(move(y));
               end;
               AdvanceMove(my_player, currentstate, move(x), move(y));
               Put_Line("After moving:");
               Put_Line(Image(currentstate));
               if CurrentGamePhase = PEarlyGame then
                  if move(x) = Dimension'First or move(x) = Dimension'Last or move(y) = Dimension'Last or move(y) = Dimension'First then
                     CurrentGamePhase := PMidGame;
                  end if;
               elsif CurrentGamePhase = PMidGame then
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
               declare
                  feedback : Float := 0.0;
                  Board : GameBoard := History.History(History.Index - 1).state.current_state;
                  GameWinner : Players;
               begin

                  if (cwinner = 1) then
                     GameWinner := White;
                  elsif (cwinner = 2) then
                     GameWinner := Black;
                  end if;

                  if(GameWinner = my_player) then
                     feedback := 1.0;
                  elsif (GameWinner = NextPlayer(my_player)) then
                     feedback := -1.0;
                  end if;

                  Put_Line("Feedback : " & feedback'Img);
                  TD(History, my_player, feedback);
                  StoreWeights;
               end;

               for i in workers'Range loop
                  abort workers(i);
               end loop;
            end GameEnd;
            exit Main_Loop;
         end select;
      end loop Main_Loop;
   end Main;

   procedure StartUp is
   begin
      MainTask := new Main;
      MainTask.Initialise;
   end StartUp;

   procedure Ada_Subroutine is
   begin
      MainTask.NewMove;

   exception
      when E : others =>
         Show_Exception (E);
   end Ada_Subroutine;

   procedure GameEnd is
   begin
      MainTask.GameEnd;
   end GameEnd;

   procedure GreedyMove(board : in GameBoard; xmove : out Dimension; ymove : out Dimension) is
   begin
      null;
   end GreedyMove;

   procedure PhaseTransition(CurrentGamePhase : in out Game_Phase; State : in GameBoard;
                             Corners : in out Natural) is
   begin
      if CurrentGamePhase = PEarlyGame then
         for x in Dimension'Range loop
            if State(x,Dimension'First) = Black or State(x,Dimension'First) = White
              or State(x,Dimension'Last) = Black or State(x,Dimension'Last) = White
              or State(Dimension'First,x) = Black or State(Dimension'First,x) = White
              or State(Dimension'Last,x) = Black or State(Dimension'Last,x) = White then
               CurrentGamePhase := PMidGame;
            end if;
         end loop;
      elsif CurrentGamePhase = PMidGame then
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
