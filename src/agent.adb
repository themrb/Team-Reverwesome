with Ada.Text_IO;
use Ada.Text_IO;
with Boards; use Boards;
with GameTree; use GameTree;
with MinMax; use MinMax;
with Configure; use Configure;
with Exceptions; use Exceptions;
with TemporalDifference; use TemporalDifference;
with GNAT.Traceback;           use GNAT.Traceback;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;
with Features; use Features;

package body Agent is

   ccurrentstate : CBoardState;
   cplayercolour : Integer;
   cnextmovey : Integer;
   cnextmovex : Integer;
   cprevmovey : Integer;
   cprevmovex : Integer;
   cwinner : Integer;
   pragma import(cpp, ccurrentstate, "currentcstate");
   pragma import(cpp, cplayercolour, "playercolour");
   pragma import(cpp, cnextmovey, "nextmovey");
   pragma import(cpp, cnextmovex, "nextmovex");
   pragma import(cpp, cprevmovey, "prevmovey");
   pragma import(cpp, cprevmovex, "prevmovex");
   pragma import(cpp, cwinner, "cwinner");

   currentstate : GameBoard;
   move : Place;

   task body Main is
      History : HistoryType;
   begin
      accept Initialise  do

         CurrentGamePhase := PEarlyGame;
         LoadWeights;

         if (cplayercolour = 1) then
            my_player := White;
         elsif (cplayercolour = 2) then
            my_player := Black;
         end if;

      exception
         when E : others =>
            Show_Exception (E);
            CloseFile;
            LoadWeights;

         --Put_Line("not weights or players");
         -- spawn explorer tasks as required
         -- this is basically our set up to be ready to play the game.
         -- the clock hasn't started yet - free time
      end Initialise;

      Main_Loop:
      loop
         select
            accept NewMove  do
--                 Put_Line(cprevmovex'Img & " " & cprevmovey'Img);
--                 if not (cprevmovex < 0) then
--                    declare
--                       cx : Dimension := Dimension(cprevmovex);
--                       cy : Dimension := Dimension(cprevmovey);
--                    begin
--                       null;
--                    end;
--                 end if;


               declare
                  treeroot : GameTree_Type;
                  value : BoardValue;
                  turnsleft : TurnsNo := 0;
               begin
                  -- Read in CPP values for the board
                  for I in Dimension'Range loop
                     for J in Dimension'Range loop
                        currentstate(I,J) := BoardPoint'Val(ccurrentstate(I,J));
                        if (currentstate(I,J) = Empty) then
                           turnsleft := turnsleft + 1;
                        end if;
                     end loop;
                  end loop;

                  if CurrentGamePhase = PEarlyGame then
                     for x in Dimension'Range loop
                        if currentstate(x,Dimension'First) = Black or currentstate(x,Dimension'First) = White
                          or currentstate(x,Dimension'Last) = Black or currentstate(x,Dimension'Last) = White
                          or currentstate(Dimension'First,x) = Black or currentstate(Dimension'First,x) = White
                          or currentstate(Dimension'Last,x) = Black or currentstate(Dimension'Last,x) = White then
                           CurrentGamePhase := PMidGame;
                        end if;
                     end loop;
                  elsif CurrentGamePhase = PMidGame then
                     CurrentCorners := 0;
                     if currentstate(Dimension'First, Dimension'First) = Black or currentstate(Dimension'First, Dimension'First) = White then
                        CurrentCorners := CurrentCorners + 1;
                     elsif currentstate(Dimension'Last, Dimension'Last) = Black or currentstate(Dimension'Last, Dimension'Last) = White then
                        CurrentCorners := CurrentCorners + 1;
                     elsif currentstate(Dimension'First, Dimension'Last) = Black or currentstate(Dimension'First, Dimension'Last) = White then
                        CurrentCorners := CurrentCorners + 1;
                     elsif currentstate(Dimension'Last, Dimension'First) = Black or currentstate(Dimension'Last, Dimension'First) = White then
                        CurrentCorners := CurrentCorners + 1;
                     end if;
                     if CurrentCorners >= 2 then
                        CurrentGamePhase := PLateGame;
                     end if;
                  end if;


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

                  if (turnsleft < 13) then
                     NegaMax(my_player, treeroot, 12, value, BoardValue'First, BoardValue'Last, move);
                  else
                     NegaMax(my_player, treeroot, 5, value, BoardValue'First, BoardValue'Last, move);
                  end if;

                  declare
                     temppieces : Natural := ValidMove(my_player, currentstate, move(x), move(y));
                  begin
                     Put_Line("We'll get " & TurnsNo'Image(temppieces) & "for moving at" & Dimension'Image(move(x)) & "," & Dimension'Image(move(y)));
                  end;
                  cnextmovey := Integer(move(x));
                  cnextmovex := Integer(move(y));
               end;
            end NewMove;
            AdvanceMove(my_player, currentstate, move(x), move(y));
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
         or
            accept GameEnd  do
               declare
                  feedback : Float := 0.0;
                  Board : GameBoard := History.History(History.Index - 1).state.current_state;
                  GameWinner : Players;
               begin
                  if(my_player = White) then
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

                     Put_Line(cwinner'Img);
                     Put_Line(my_player'Img);

                     Put_Line("Feedback : " & feedback'Img);
                     TD(History, my_player, feedback);
                     StoreWeights;
                  end if;
               end;
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
      Put("asking main for a move");
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

end Agent;
