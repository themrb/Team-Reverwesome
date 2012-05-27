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
   pragma import(cpp, ccurrentstate, "currentcstate");
   pragma import(cpp, cplayercolour, "playercolour");
   pragma import(cpp, cnextmovey, "nextmovey");
   pragma import(cpp, cnextmovex, "nextmovex");

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

                  -- Initialise game tree
                  treeroot.state.justWent := NextPlayer(my_player);
                  treeroot.state.current_state := currentstate;
                  treeroot.state.turnsleft := turnsleft;

                  treeroot.state.StableNodes := EmptyMatrix;
                  treeroot.state.InternalNodes := EmptyMatrix;

                  for I in Dimension'Range loop
                     for J in Dimension'Range loop
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

                  if (turnsleft < 16) then
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
               for x in Dimension'Range loop
                  if currentstate(x,Dimension'First) = Black or currentstate(x,Dimension'First) = White
                    or currentstate(x,Dimension'Last) = Black or currentstate(x,Dimension'Last) = White
                    or currentstate(x,Dimension'Last) = Black or currentstate(x,Dimension'First) = White
                    or currentstate(x,Dimension'First) = Black or currentstate(x,Dimension'Last) = White then
                     CurrentGamePhase := PMidGame;
                  end if;
               end loop;
            end if;
         or
            accept GameEnd  do
               if(my_player = White) then
                  TD(History, my_player);
                  StoreWeights;
               end if;
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
