with Ada.Text_IO;
use Ada.Text_IO;

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
		currentstate : BoardState;
		player : BoardPoint;
		piecestaken : Natural;
		bestpiecestaken : Natural := 0;
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
		
		--PrintBoard(currentstate);
		
		Move_Loop:
		for I in Dimension range Dimension'Range loop
			for J in Dimension range Dimension'Range loop
				piecestaken := ValidMove(player, currentstate, I, J);
				if (piecestaken > 0) then
					if (piecestaken > bestpiecestaken) then
						bestpiecestaken := piecestaken;
						cnextmovey := Integer(I)-1;
						cnextmovex := Integer(J)-1;
						Put_Line("Seen move " & I'Img &J'Img);
						--PrintBoard(currentstate);
					end if;
				end if;
			end loop;
		end loop Move_Loop;
		
		
	end Ada_Subroutine;

	function OtherPlayer(player : BoardPoint) return BoardPoint is
	begin
		if (player = White) then
			return Black;
		elsif (player = Black) then
			return White;
		end if;
		return Empty;
	end OtherPlayer;

	procedure GreedyMove(board : in BoardState; xmove : out Dimension; ymove : out Dimension) is
	begin
		null;
	end GreedyMove;

	function ValidMove(player : BoardPoint; board : in BoardState; movex : in Dimension; movey : in Dimension) return Natural is
		HitOpponent : Boolean;
		Opponent : BoardPoint := OtherPlayer(player);
		x : Dimension;
		y : Dimension;
		moveroom : Dimension;
		yroom : Dimension;
		xroom : Dimension;
		totalpieces : Natural := 0;
		pieces : Natural;
	begin
		--Check square occupancy
		if (board(movex,movey) /= Empty) then
			return totalpieces;
		end if;
		--Put("Opponent" & Opponent'Img & "player" & player'Img);
		--straight right
		HitOpponent := False;
		pieces := 0;
		--Put_Line("Starting right");
		Right_Loop :
		for xpoint in Dimension range 1 .. (Dimension'Last - movex) loop
			declare
				difference : Dimension := Dimension'Last - movex;
			begin
				null;
				--Put(difference'Img);
			end;
			y := movey;
			x := movex + xpoint;
			if board(x,y) = Empty then
				--Put_Line("Hit empty " & x'Img &y'Img);
				exit Right_Loop;
			elsif board(x,y) = Blocked then
				exit Right_Loop;
			elsif board(x,y) = player then
				--PrintBoard(board);
				--Put_Line("Hit player " & x'Img &y'Img & board(x,y)'Img);
				if (HitOpponent) then
					--Put_Line("Straight right " & movex'Img &movey'Img);
					totalpieces := totalpieces + pieces;
				else
					--Put_Line("Hit player too early " & x'Img &y'Img);
					exit Right_Loop;
				end if;
			elsif board(x,y) = Opponent then
				pieces := pieces + 1;
				--Put_Line("Hit opponent " & x'Img &y'Img);
				HitOpponent := true;
			end if;
		end loop Right_Loop;
		--straight left
		HitOpponent := False;
		pieces := 0;
		for xpoint in Dimension range 1 .. (movex-1) loop
			y := movey;
			x := movex - xpoint;
			if board(x,y) = Empty then
				exit;
			elsif board(x,y) = Blocked then
				exit;
			elsif board(x,y) = player then
				if (HitOpponent) then
					--Put("Straight left " & movex'Img &movey'Img);
					totalpieces := totalpieces + pieces;
				else exit;
				end if;
			elsif board(x,y) = Opponent then
				pieces := pieces + 1;
				HitOpponent := true;
			end if;
		end loop;
		--straight up
		HitOpponent := False;
		pieces := 0;
		for ypoint in Dimension range 1 .. (Dimension'Last - movey) 		loop
			y := movey+ypoint;
			x := movex;
			if board(x,y) = Empty then
				exit;
			elsif board(x,y) = Blocked then
				exit;
			elsif board(x,y) = player then
				if (HitOpponent) then
					--Put("Straight up " & x'Img &y'Img);
					totalpieces := totalpieces + pieces;
				else exit;
				end if;
			elsif board(x,y) = Opponent then
				pieces := pieces + 1;
				HitOpponent := true;
			end if;
		end loop;
		--straight down
		HitOpponent := False;
		pieces := 0;
		for ypoint in Dimension range 1 .. (movey-1) loop
			y := movey - ypoint;
			x := movex;
			if board(x,y) = Empty then
				exit;
			elsif board(x,y) = Blocked then
				exit;
			elsif board(x,y) = player then
				if (HitOpponent) then
					--Put("Straight down " & x'Img &y'Img);
					totalpieces := totalpieces + pieces;
				else exit;
				end if;
			elsif board(x,y) = Opponent then
				pieces := pieces + 1;
				HitOpponent := true;
			end if;
		end loop;


		--straight NE
		if (not(movex = Dimension'Last or movey = Dimension'Last)) then
		HitOpponent := False;
		pieces := 0;
		--if (y
		yroom := Dimension'Last - movey;
		xroom := Dimension'Last - movex;
		if (yroom > xroom) then
			moveroom := xroom;
		else
			moveroom := yroom;
		end if;			
		for epoint in Dimension range 1 .. moveroom loop
			y := movey + epoint;
			x := movex + epoint;
			if board(x,y) = Empty then
				exit;
			elsif board(x,y) = Blocked then
				exit;
			elsif board(x,y) = player then
				if (HitOpponent) then
					--Put("Straight down " & x'Img &y'Img);
					totalpieces := totalpieces + pieces;
				else exit;
				end if;
			elsif board(x,y) = Opponent then
				pieces := pieces + 1;
				HitOpponent := true;
			end if;
		end loop;
		end if;
		--straight NW
		if (not(movex = Dimension'First or movey = Dimension'Last)) then
			yroom := Dimension'Last - movey;
			xroom := movex-1;
			if (yroom > xroom) then
				moveroom := xroom;
			else
				moveroom := yroom;
			end if;	
			HitOpponent := False;
			pieces := 0;
			for epoint in Dimension range 1 .. moveroom loop
				y := movey + epoint;
				x := movex - epoint;
				if board(x,y) = Empty then
					exit;
				elsif board(x,y) = Blocked then
					exit;
				elsif board(x,y) = player then
					if (HitOpponent) then
						--Put("Straight down " & x'Img &y'Img);
					totalpieces := totalpieces + pieces;
					else exit;
					end if;
				elsif board(x,y) = Opponent then
					pieces := pieces + 1;
					HitOpponent := true;
				end if;
			end loop;
		end if;
		--straight SW
		if (not(movex = Dimension'First or movey = Dimension'First)) then
		yroom := movey-1;
		xroom := movex-1;
		if (yroom > xroom) then
			moveroom := xroom;
		else
			moveroom := yroom;
		end if;	
		HitOpponent := False;
		pieces := 0;
		for epoint in Dimension range 1 .. moveroom loop
			y := movey - epoint;
			x := movex - epoint;
			if board(x,y) = Empty then
				exit;
			elsif board(x,y) = Blocked then
				exit;
			elsif board(x,y) = player then
				if (HitOpponent) then
					--Put("Straight down " & x'Img &y'Img);
					totalpieces := totalpieces + pieces;
				else exit;
				end if;
			elsif board(x,y) = Opponent then
				pieces := pieces + 1;
				HitOpponent := true;
			end if;
		end loop;
		end if;
		--straight SE
		if (not(movey = Dimension'First or movex = Dimension'Last)) then
		yroom := movey-1;
		xroom := Dimension'Last - movex;
		if (yroom > xroom) then
			moveroom := xroom;
		else
			moveroom := yroom;
		end if;	
		HitOpponent := False;
		pieces := 0;
		for epoint in Dimension range 1 .. moveroom loop
			y := movey - epoint;
			x := movex + epoint;
			if board(x,y) = Empty then
				exit;
			elsif board(x,y) = Blocked then
				exit;
			elsif board(x,y) = player then
				if (HitOpponent) then
					--Put("Straight down " & x'Img &y'Img);
					totalpieces := totalpieces + pieces;
				else exit;
				end if;
			elsif board(x,y) = Opponent then
				pieces := pieces + 1;
				HitOpponent := true;
			end if;
		end loop;
		end if;
		return totalpieces;
	end ValidMove;
	
	procedure PrintBoard(board : in BoardState) is
	begin
		for J in Dimension'Range loop
			for I in Dimension'Range loop
				case board(I,J) is
					  when Empty =>
						 Put(" .");
					  when White =>
						 Put(" w");
					  when Black =>
						 Put(" b");
					  when Blocked =>
						 Put(" *");
					  when others =>
						 Put_Line ("+F+");
				end case;
			end loop;
			Ada.Text_IO.Put_Line("");
	end loop;
	end PrintBoard;

end Agent;