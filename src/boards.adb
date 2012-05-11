with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;

package body Boards is

   -- Passes the move onto the next player
   function NextPlayer(prev : BoardPoint) return BoardPoint is
   begin
      if (player = White) then
         return Black;
      elsif (player = Black) then
         return White;
      end if;
      return Empty;
   end OtherPlayer;

   procedure TokenCount(State : BoardState, WhiteTokens : out TurnsNo; BlackTokens : out TurnsNo) is
   begin
      BlackTokens := 0;
      WhiteTokens := 0;
        for I in Dimension'Range loop
         for J in Dimension'Range loop
            case state.current_state(I,J) is
            when White =>
               WhiteTokens := WhiteTokens + 1;
            when Black =>
               BlackTokens := BlackTokens + 1;
            when others =>
               null;
            end case;
         end loop;
   end TokenCount;

   -- Advances a game state, given a move, returning the resulting state
   function AdvanceMove(state : State_Type; move : Place) return State_Type is
      placedPiece : Cell := NextPlayer(state.justWent);
      newState : State_Type := state;
   begin
      if (placedPiece = X) then
         newState.current_stateX(move(x), move(y), move(z)) := True;
      elsif (placedPiece = O) then
         newState.current_stateO(move(x), move(y), move(z)) := True;
      end if;
      newState.turns := newState.turns + 1;
      newState.justWent := placedPiece;
      newState.spot := move;
      return newState;
   end AdvanceMove;

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

   -- Print function
   function Image(state : State_Type) return String is
      temp : Unbounded_String;
   begin
      temp := "[" & Cell'Image(state.justWent) & "," & Image(state.spot)
        & "," & Natural'Image(state.turns) & "]";
      for I in Dimension'Range loop
         for J in Dimension'Range loop
            case state.current_state(I,J) is
            when Empty =>
               temp := temp & " .";
            when White =>
               temp := temp & " w";
            when Black =>
               temp := temp & " b";
            when Blocked =>
               temp := temp & " *";
            when others =>
               temp := temp & "+F+";
            end case;
         end loop;
         return To_String(temp);
      end;

      -- Print function
      function Image(spot : Place) return String is
      begin
         return "(" & Dimension'Image(spot(x)) & "," & Dimension'Image(spot(y))
           & "," & Dimension'Image(spot(z)) & ")";
      end;

      -- Print function
      function Image(board : Board_Type) return String is
      temp : Unbounded_String;
   begin
      temp := "[" & Cell'Image(state.justWent) & "," & Image(state.spot)
        & "," & Natural'Image(state.turns) & "]";
      for I in Dimension'Range loop
         for J in Dimension'Range loop
            case state.currentstate(I,J) is
            when Empty =>
               temp := temp & " .";
            when White =>
               temp := temp & " w";
            when Black =>
               temp := temp & " b";
            when Blocked =>
               temp := temp & " *";
            when others =>
               temp := temp & "+F+";
            end case;
         end loop;

         return To_String(temp);
      end;

      -- Checks if a given game state is terminal (the last player to move has won)
      function Terminal(state : in State_Type) return Boolean is
         currentstate : Board_Type;
      begin
         -- There cannot be a win in less than 4 moves
         if(state.turns < 4) then
            return False;
         end if;

         -- Decide which player state to check
         if (state.justWent = X) then
            currentstate := state.current_stateX;
         elsif (state.justWent = O) then
            currentstate := state.current_stateO;
         end if;

         -- Check for straight line wins in each of the planes
         for i in Dimension'Range loop
            xrow := xrow and currentstate(i,state.spot(y), state.spot(z)) = True;
            yrow := yrow and currentstate(state.spot(x),i, state.spot(z)) = True;
            zrow := zrow and currentstate(state.spot(x),state.spot(y), i) = True;
         end loop;
         if (xrow or yrow or zrow) then
            return True;
         end if;

         -- The following 3 blocks check for diagonals in each of the planes.

         if state.spot(x) = state.spot(y) then
            for i in Dimension'Range loop
               zdiag := zdiag and currentstate(i,i,state.spot(z)) = True;
            end loop;
         elsif state.spot(x) + state.spot(y) = 3 then
            for i in Dimension'Range loop
               zdiag := zdiag and currentstate(i,3-i,state.spot(z)) = True;
            end loop;
         else
            zdiag := False;
         end if;

         if(zdiag) then return True; end if;

         if state.spot(y) = state.spot(z) then
            for i in Dimension'Range loop
               xdiag := xdiag and currentstate(state.spot(x),i,i) = True;
            end loop;
         elsif state.spot(z) + state.spot(y) = 3 then
            for i in Dimension'Range loop
               xdiag := xdiag and currentstate(state.spot(x),i,3-i) = True;
            end loop;
         else
            xdiag := False;
         end if;

         if(xdiag) then return True; end if;

         if state.spot(x) = state.spot(z) then
            for i in Dimension'Range loop
               ydiag := ydiag and currentstate(i,state.spot(y),i) = True;
            end loop;
         elsif state.spot(x) + state.spot(z) = 3 then
            for i in Dimension'Range loop
               ydiag := ydiag and currentstate(i,state.spot(y),3-i) = True;
            end loop;
         else
            ydiag := False;
         end if;

         if(ydiag) then return True; end if;

         --Check all crosscorner diagonals

         if(state.spot(x) = state.spot(y) and state.spot(x) = state.spot(z)) then
            for i in Dimension'Range loop
               corner := corner and currentstate(i,i,i) = True;
            end loop;
         elsif(state.spot(x) = (3-state.spot(y)) and state.spot(x) = state.spot(z)) then
            for i in Dimension'Range loop
               corner := corner and currentstate(i,3-i,i) = True;
            end loop;
         elsif(state.spot(x) = state.spot(y) and state.spot(x) = (3-state.spot(z))) then
            for i in Dimension'Range loop
               corner := corner and currentstate(i,i,3-i) = True;
            end loop;
         else
            for i in Dimension'Range loop
               corner := corner and currentstate(3-i,i,i) = True;
            end loop;
         end if;

         return corner;

      end Terminal;

   end Boards;
