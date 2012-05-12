with Ada.Text_IO;
use Ada.Text_IO;
with Boards; use Boards;
with GameTree; use GameTree;
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
		piecestaken : Natural;
		bestpiecestaken : Natural := 0;
      treeroot : GameTree_Type;
      move : Place;
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
			end loop;
			--Ada.Text_IO.Put_Line("");
		end loop;

      treeroot.state.justWent := NextPlayer(player);
      treeroot.state.current_state := currentstate;

		Move_Loop:
		for I in Dimension range Dimension'Range loop
			for J in Dimension range Dimension'Range loop
				piecestaken := ValidMove(player, currentstate, I, J);
				if (piecestaken > 0) then
					if (piecestaken > bestpiecestaken) then
						bestpiecestaken := piecestaken;
						cnextmovey := Integer(I);
						cnextmovex := Integer(J);
						Put_Line("Seen move " & I'Img &J'Img);
						--PrintBoard(currentstate);
					end if;
				end if;
			end loop;
		end loop Move_Loop;
	end Ada_Subroutine;

	procedure GreedyMove(board : in GameBoard; xmove : out Dimension; ymove : out Dimension) is
	begin
		null;
	end GreedyMove;

end Agent;
