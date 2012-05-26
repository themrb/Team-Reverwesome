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

package body Agent is

   ccurrentstate : CBoardState;
   cplayercolour : Integer;
   cnextmovey : Integer;
   cnextmovex : Integer;
   pragma import(cpp, ccurrentstate, "currentcstate");
   pragma import(cpp, cplayercolour, "playercolour");
   pragma import(cpp, cnextmovey, "nextmovey");
   pragma import(cpp, cnextmovex, "nextmovex");

   task body Main is

   begin
      accept Initialise  do
         --Put_Line("is it weights?");
         LoadWeights;

         if (cplayercolour = 1) then
            player := White;
         elsif (cplayercolour = 2) then
            player := Black;
         end if;

         --Put_Line("not weights or players");
         -- spawn explorer tasks as required
         -- this is basically our set up to be ready to play the game.
         -- the clock hasn't started yet - free time
      end Initialise;

      Main_Loop:
      loop
         select
            accept NewMove  do
               --Put("no storage error at start");
               declare
                  currentstate : GameBoard;
                  treeroot : GameTree_Type;
                  move : Place;
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
                  treeroot.state.justWent := NextPlayer(player);
                  treeroot.state.current_state := currentstate;
                  treeroot.state.turnsleft := turnsleft;

                  if (turnsleft < 16) then
                     NegaMax(player, treeroot, 15, value, BoardValue'First, BoardValue'Last, move);
                  else
                     NegaMax(player, treeroot, 5, value, BoardValue'First, BoardValue'Last, move);
                  end if;

                  declare
                     temppieces : Natural := ValidMove(player, currentstate, move(x), move(y));
                  begin
                     Put_Line("We'll get " & TurnsNo'Image(temppieces) & "for moving at" & Dimension'Image(move(x)) & "," & Dimension'Image(move(y)));
                  end;
                  cnextmovey := Integer(move(x));
                  cnextmovex := Integer(move(y));

                  declare
                     newState : GameBoard := currentstate;
                  begin
                     AdvanceMove(player, newState, move(x), move(y));
                     TD(currentstate, newState, player);
                  end;
               end;
            end NewMove;
         or
            accept GameEnd  do
               StoreWeights;
               -- also free time
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
