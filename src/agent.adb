with Ada.Text_IO;
use Ada.Text_IO;
with Boards; use Boards;
with GameTree; use GameTree;
with MinMax; use MinMax;
with Configure; use Configure;
with Exceptions; use Exceptions;

package body Agent is

	ccurrentstate : CBoardState;
	cplayercolour : Integer;
	cnextmovey : Integer;
	cnextmovex : Integer;
	pragma import(cpp, ccurrentstate, "currentcstate");
	pragma import(cpp, cplayercolour, "playercolour");
	pragma import(cpp, cnextmovey, "nextmovey");
	pragma import(cpp, cnextmovex, "nextmovex");

	procedure Ada_Subroutine is
		currentstate : GameBoard;
		player : BoardPoint;
      treeroot : GameTree_Type;
      move : Place;
      value : BoardValue;
      turnsleft : TurnsNo := 0;
	begin
		Put("Ada_Subroutine has been invoked from C++.");
		Ada.Text_IO.Put_Line("");

		if (cplayercolour = 1) then
			player := White;
		elsif (cplayercolour = 2) then
			player := Black;
		end if;

		for I in Dimension'Range loop
			for J in Dimension'Range loop
				currentstate(I,J) := BoardPoint'Val(ccurrentstate(I,J));
            if (currentstate(I,J) = Empty) then
               turnsleft := turnsleft + 1;
            end if;
			end loop;
			--Ada.Text_IO.Put_Line("");
		end loop;

      if (ValidMove(player,currentstate,0,0) > 0) then
         cnextmovey := 0;
         cnextmovex := 0;
         return;
      elsif (ValidMove(player,currentstate,0,9) > 0) then
         cnextmovey := 0;
         cnextmovex := 9;
         return;
      elsif (ValidMove(player,currentstate,9,9) > 0) then
         cnextmovey := 9;
         cnextmovex := 9;
         return;
      elsif (ValidMove(player,currentstate,9,0) > 0) then
         cnextmovey := 9;
         cnextmovex := 0;
         return;
      end if;
         

      treeroot.state.justWent := NextPlayer(player);
      treeroot.state.current_state := currentstate;
      treeroot.state.turnsleft := turnsleft;

      --Put_Line("No storage error yet");
      if (turnsleft < 17) then
         Max(player, treeroot, 15, value, BoardValue'First, BoardValue'Last, move);
      else
         declare
            tempprob : Probability;
            bestprob : Probability := 0.0;
            bestmove : Place;
            children : ExpandedChildren := Expand(treeroot);
         begin
            for I in 0 .. children.branching-1 loop
               tempprob := MonteCarlo(player,children.children(I),200);
               if (tempprob >= bestprob) then
                  bestmove := children.children(I).state.spot;
                  bestprob := tempprob;
               end if;
            end loop;
            Put_Line("Probability of winning : " & Long_Float'Image(bestprob));
            move := bestmove;
         end;
      end if;

      --Put_Line("No storage error after max");
      --Put_Line("testing monte carlo " & Long_Float'Image(MonteCarlo(player,treeroot,100)));
      declare
         temppieces : Natural := ValidMove(player, currentstate, move(x), move(y));
      begin
         Put_Line("We'll get " & TurnsNo'Image(temppieces) & "for moving at" & Dimension'Image(move(x)) & "," & Dimension'Image(move(y)));
      end;
      cnextmovey := Integer(move(x));
      cnextmovex := Integer(move(y));

-- 		Move_Loop:
-- 		for I in Dimension range Dimension'Range loop
-- 			for J in Dimension range Dimension'Range loop
-- 				piecestaken := ValidMove(player, currentstate, I, J);
-- 				if (piecestaken > 0) then
-- 					if (piecestaken > bestpiecestaken) then
-- 						bestpiecestaken := piecestaken;
-- 						cnextmovey := Integer(I);
-- 						cnextmovex := Integer(J);
-- 						Put_Line("Seen move " & I'Img &J'Img);
-- 						--PrintBoard(currentstate);
-- 					end if;
-- 				end if;
-- 			end loop;
-- 		end loop Move_Loop;
	end Ada_Subroutine;

	procedure GreedyMove(board : in GameBoard; xmove : out Dimension; ymove : out Dimension) is
	begin
		null;
	end GreedyMove;

end Agent;
