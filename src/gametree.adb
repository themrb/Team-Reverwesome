with Boards; use Boards;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Configure; use Configure;

package body GameTree is

   --Expand a game tree node's state and return its successor states
   function Expand(state : in GameTree_Type) return ExpandedChildren is
      temp : aliased GameTree_Type;
      Children : ExpandedChildren;\
   begin

      for i in Dimension'Range loop
         for j in Dimension'Range loop
            for k in Dimension'Range loop
               -- if we don't already have something sitting there
               if(not state.state.current_stateX(i,j,k) and not state.state.current_stateO(i,j,k)) then
                  temp := state;
                  temp.state.justWent := NextPlayer(state.state.justWent);

                  if (temp.state.justWent = X) then
                     temp.state.current_stateX(i,j,k) := True;
                  elsif (temp.state.justWent = O) then
                     temp.state.current_stateO(i,j,k) := True;
                  end if;

                  temp.state.spot := (i,j,k);
                  temp.state.turns := state.state.turns + 1;

                  if((temp.state.turns) /= state.state.turns + 1) then
                     Put_Line(temp.state.turns'Img & state.state.turns'Img);
                  end if;

                  -- Check if corner or inner nodes, and if so, add to front of
                  -- queue
                  if(((i = 0 or i = 3) and (j = 0 or j = 3) and (k = 0 or k = 3)) or
                    ((i = 1 or i = 2) and (j = 1 or j = 2) and (k = 1 or k = 2))) then
                     Children(frontCounter) := temp;
                     Configure.count := Configure.count + 1;
                     if(frontCounter < Children_Range'Last) then
                        frontCounter := frontCounter + 1;
                     end if;
                  else -- Otherwise, back of the line!
                     Children(backCounter) := temp;
                     Configure.count := Configure.count + 1;
                     if(backCounter > Children_Range'First) then
                        backCounter := backCounter - 1;
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
      end loop;

      return Children;
   end Expand;

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

end GameTree;
