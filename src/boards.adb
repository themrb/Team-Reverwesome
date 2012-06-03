with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;

with TemporalDifference; use TemporalDifference;

package body Boards is

   -- Function to quickly switch the player
   function NextPlayer(player : BoardPoint) return BoardPoint is
   begin
      if (player = White) then
         return Black;
      elsif (player = Black) then
         return White;
      end if;
      return Empty;
   end NextPlayer;

   -- Find the winner of a terminal state
   -- Tokens added up, whoever has the most, wins
   -- Terminal must be checked seperately
   procedure Winner(State : GameBoard; Winner : out BoardPoint) is
      BlackTokens : TurnsNo;
      WhiteTokens : TurnsNo;
   begin
      TokenCount(State,WhiteTokens,BlackTokens);
      -- Choose whoever has the most tokens
      if (WhiteTokens > BlackTokens) then
         Winner := White;
      elsif (WhiteTokens < BlackTokens) then
         Winner := Black;
      else Winner := Empty;
      end if;
   end Winner;

   -- Check if a given square is a valid move or not
   -- Returns 0 for invalid move, or the number of tokens a move will flip
   function ValidMove(player : BoardPoint; board : in GameBoard; movex : in Dimension; movey : in Dimension) return Natural is
      -- Whether we ran into the opponent yet
      HitOpponent : Boolean;
      -- Opponent's colour
      Opponent : BoardPoint := NextPlayer(player);

      -- Current x,y being inspected
      x : Dimension;
      y : Dimension;
      -- How much room along a direction line we have
      moveroom : Dimension;
      yroom : Dimension;
      xroom : Dimension;
      -- Total tiles that would be flipped
      totalpieces : Natural := 0;
      -- Temp piece counter
      pieces : Natural;
   begin
      --Check square occupancy - square must be empty
      if (board(movex,movey) /= Empty) then
         return totalpieces;
      end if;

      -- -- The Basic Idea -- --
      -- There are 8 lines going left, right, up, down, NE, SW, NW, SE
      -- 1. Find how much space we have to move along these lines
      -- 2. Check if we could take pieces from moving along these lines
      --      - We must hit a series of at least one opponent pieces
      --      - Then we must hit a piece of our own
      --      - We add all the pieces in between as flippable

      -- Straight Right Line
      HitOpponent := False;
      pieces := 0;
      Right_Loop :
      for xpoint in Dimension range 1 .. (Dimension'Last - movex) loop
         -- Check if we could take pieces from moving along this line
         y := movey;
         x := movex + xpoint;
         if board(x,y) = Empty then
            -- Cannot take pieces along this line
            exit Right_Loop;
         elsif board(x,y) = Blocked then
            -- Cannot take pieces along this line
            exit Right_Loop;
         elsif board(x,y) = player then
            if (HitOpponent) then
               -- If there are opponent pieces beforehand
               -- We can take these pieces
               totalpieces := totalpieces + pieces;
            else
               -- Hit ourselves before the opponent
               -- Cannot take pieces along this line
               exit Right_Loop;
            end if;
         elsif board(x,y) = Opponent then
            -- We must hit a series of at least one opponent pieces
            pieces := pieces + 1;
            HitOpponent := true;
         end if;
      end loop Right_Loop;

      -- Straight Left Line
      HitOpponent := False;
      pieces := 0;
      for xpoint in Dimension range 1 .. (movex) loop
         y := movey;
         x := movex - xpoint;
         if board(x,y) = Empty then
            exit;
         elsif board(x,y) = Blocked then
            exit;
         elsif board(x,y) = player then
            if (HitOpponent) then
               totalpieces := totalpieces + pieces;
            else exit;
            end if;
         elsif board(x,y) = Opponent then
            pieces := pieces + 1;
            HitOpponent := true;
         end if;
      end loop;

      -- Straight Up Line
      HitOpponent := False;
      pieces := 0;
      for ypoint in Dimension range 1 .. (Dimension'Last - movey)       loop
         y := movey+ypoint;
         x := movex;
         if board(x,y) = Empty then
            exit;
         elsif board(x,y) = Blocked then
            exit;
         elsif board(x,y) = player then
            if (HitOpponent) then
               totalpieces := totalpieces + pieces;
            else exit;
            end if;
         elsif board(x,y) = Opponent then
            pieces := pieces + 1;
            HitOpponent := true;
         end if;
      end loop;

      -- Straight Down Line
      HitOpponent := False;
      pieces := 0;
      for ypoint in Dimension range 1 .. (movey) loop
         y := movey - ypoint;
         x := movex;
         if board(x,y) = Empty then
            exit;
         elsif board(x,y) = Blocked then
            exit;
         elsif board(x,y) = player then
            if (HitOpponent) then
               totalpieces := totalpieces + pieces;
            else exit;
            end if;
         elsif board(x,y) = Opponent then
            pieces := pieces + 1;
            HitOpponent := true;
         end if;
      end loop;

      --Straight NE Line
      if (not(movex = Dimension'Last or movey = Dimension'Last)) then
         -- So long as we have room to move (not on the very edge)
         HitOpponent := False;
         pieces := 0;
         -- Find room along each axis
         yroom := Dimension'Last - movey;
         xroom := Dimension'Last - movex;
         -- Diagonal length is the max of these
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
                  totalpieces := totalpieces + pieces;
               else exit;
               end if;
            elsif board(x,y) = Opponent then
               pieces := pieces + 1;
               HitOpponent := true;
            end if;
         end loop;
      end if;

      -- Straight NW Line
      if (not(movex = Dimension'First or movey = Dimension'Last)) then
         yroom := Dimension'Last - movey;
         xroom := movex;
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
                  totalpieces := totalpieces + pieces;
               else exit;
               end if;
            elsif board(x,y) = Opponent then
               pieces := pieces + 1;
               HitOpponent := true;
            end if;
         end loop;
      end if;

      -- Straight SW Line
      if (not(movex = Dimension'First or movey = Dimension'First)) then
         yroom := movey;
         xroom := movex;
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
                  totalpieces := totalpieces + pieces;
               else exit;
               end if;
            elsif board(x,y) = Opponent then
               pieces := pieces + 1;
               HitOpponent := true;
            end if;
         end loop;
      end if;

      -- Straight SE Line
      if (not(movey = Dimension'First or movex = Dimension'Last)) then
         yroom := movey;
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
                  totalpieces := totalpieces + pieces;
               else exit;
               end if;
            elsif board(x,y) = Opponent then
               pieces := pieces + 1;
               HitOpponent := true;
            end if;
         end loop;
      end if;

      -- Return number of pieces we could actually take
      return totalpieces;
   end ValidMove;

   -- Advances the board state given a chosen move
   -- WARNING: Check for ValidMove first before using this procedure
   procedure AdvanceMove(player : BoardPoint; board : in out GameBoard; movex : Dimension; movey : Dimension) is
      HitOpponent : Boolean;
      Opponent : BoardPoint := NextPlayer(player);
      x : Dimension;
      y : Dimension;
      moveroom : Dimension;
      yroom : Dimension;
      xroom : Dimension;
   begin
      -- -- The Basic Idea -- --
      -- This is very similar to ValidMove - except instead of counting, we flip
      -- There are 8 lines going left, right, up, down, NE, SW, NW, SE
      -- 1. Find how much space we have to move along these lines
      -- 2. Check if we could take pieces from moving along these lines
      --      - We must hit a series of at least one opponent pieces
      --      - Then we must hit a piece of our own
      --      - We go backwards and flip all the pieces in between


      --Set square as owned by us
      board(movex,movey) := player;

      -- Straight right line conquest
      HitOpponent := False;
      Right_Loop :
      for xpoint in Dimension range 1 .. (Dimension'Last - movex) loop
         y := movey;
         x := movex + xpoint;
         if board(x,y) = Empty then
            -- Cannot take pieces along this line
            exit Right_Loop;
         elsif board(x,y) = Blocked then
            -- Cannot take pieces along this line
            exit Right_Loop;
         elsif board(x,y) = player then
            if (HitOpponent) then
               -- Hit our piece after hitting opponents
               -- We need to claim all pieces from the start up to this point
               for xpoint in Dimension range 1 .. (Dimension'Last - movex) loop
                  y := movey;
                  x := movex + xpoint;
                  if board(x,y) = Opponent then
                     board(x,y) := Player;
                  else exit Right_Loop;
                  end if;
               end loop;
            else
               -- Hit ourselves first - cannot take pieces along this line
               exit Right_Loop;
            end if;
         elsif board(x,y) = Opponent then
            -- Found an opponent piece in between
            HitOpponent := true;
         end if;
      end loop Right_Loop;

      --straight left line conquest
      HitOpponent := False;
      Left_Loop :
      for xpoint in Dimension range 1 .. (movex) loop
         y := movey;
         x := movex - xpoint;
         if board(x,y) = Empty then
            exit;
         elsif board(x,y) = Blocked then
            exit;
         elsif board(x,y) = player then
            if (HitOpponent) then
               for xpoint in Dimension range 1 .. (movex) loop
                  y := movey;
                  x := movex - xpoint;
                  if board(x,y) = Opponent then
                     board(x,y) := Player;
                  else exit Left_Loop;
                  end if;
               end loop;
            else exit;
            end if;
         elsif board(x,y) = Opponent then
            HitOpponent := true;
         end if;
      end loop Left_Loop;

      --straight up line conquest
      HitOpponent := False;
      Up_Loop :
      for ypoint in Dimension range 1 .. (Dimension'Last - movey)       loop
         y := movey+ypoint;
         x := movex;
         if board(x,y) = Empty then
            exit;
         elsif board(x,y) = Blocked then
            exit;
         elsif board(x,y) = player then
            if (HitOpponent) then
               for ypoint in Dimension range 1 .. (Dimension'Last - movey)       loop
                  y := movey+ypoint;
                  x := movex;
                  if board(x,y) = Opponent then
                     board(x,y) := Player;
                  else exit Up_Loop;
                  end if;
               end loop;
            else exit;
            end if;
         elsif board(x,y) = Opponent then
            HitOpponent := true;
         end if;
      end loop Up_Loop;

      --straight down line conquest
      HitOpponent := False;
      Down_Loop :
      for ypoint in Dimension range 1 .. (movey) loop
         y := movey - ypoint;
         x := movex;
         if board(x,y) = Empty then
            exit;
         elsif board(x,y) = Blocked then
            exit;
         elsif board(x,y) = player then
            if (HitOpponent) then
               for ypoint in Dimension range 1 .. (movey) loop
                  y := movey - ypoint;
                  x := movex;
                  if board(x,y) = Opponent then
                     board(x,y) := Player;
                  else exit Down_Loop;
                  end if;
               end loop;
            else exit;
            end if;
         elsif board(x,y) = Opponent then
            HitOpponent := true;
         end if;
      end loop Down_Loop;

      --straight NE line conquest
      if (not(movex = Dimension'Last or movey = Dimension'Last)) then
         HitOpponent := False;
         yroom := Dimension'Last - movey;
         xroom := Dimension'Last - movex;
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         NE_Loop :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey + epoint;
            x := movex + epoint;
            if board(x,y) = Empty then
               exit;
            elsif board(x,y) = Blocked then
               exit;
            elsif board(x,y) = player then
               if (HitOpponent) then
                  for epoint in Dimension range 1 .. moveroom loop
                     y := movey + epoint;
                     x := movex + epoint;
                     if board(x,y) = Opponent then
                        board(x,y) := Player;
                     else exit NE_Loop;
                     end if;
                  end loop;
               else exit;
               end if;
            elsif board(x,y) = Opponent then
               HitOpponent := true;
            end if;
         end loop NE_Loop;
      end if;
      --straight NW line conquest
      if (not(movex = Dimension'First or movey = Dimension'Last)) then
         yroom := Dimension'Last - movey;
         xroom := movex;
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         HitOpponent := False;
         NW_Loop :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey + epoint;
            x := movex - epoint;
            if board(x,y) = Empty then
               exit;
            elsif board(x,y) = Blocked then
               exit;
            elsif board(x,y) = player then
               if (HitOpponent) then
                  for epoint in Dimension range 1 .. moveroom loop
                     y := movey + epoint;
                     x := movex - epoint;
                     if board(x,y) = Opponent then
                        board(x,y) := Player;
                     else exit NW_Loop;
                     end if;
                  end loop;
               else exit;
               end if;
            elsif board(x,y) = Opponent then
               HitOpponent := true;
            end if;
         end loop NW_Loop;
      end if;

      --straight SW line conquest
      if (not(movex = Dimension'First or movey = Dimension'First)) then
         yroom := movey;
         xroom := movex;
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         HitOpponent := False;
         SW_Loop :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey - epoint;
            x := movex - epoint;
            if board(x,y) = Empty then
               exit;
            elsif board(x,y) = Blocked then
               exit;
            elsif board(x,y) = player then
               if (HitOpponent) then
                  for epoint in Dimension range 1 .. moveroom loop
                     y := movey - epoint;
                     x := movex - epoint;
                     if board(x,y) = Opponent then
                        board(x,y) := Player;
                     else exit SW_Loop;
                     end if;
                  end loop;
               else exit;
               end if;
            elsif board(x,y) = Opponent then
               HitOpponent := true;
            end if;
         end loop SW_Loop;
      end if;

      --straight SE line conquest
      if (not(movey = Dimension'First or movex = Dimension'Last)) then
         yroom := movey;
         xroom := Dimension'Last - movex;
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         HitOpponent := False;
         SE_Loop :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey - epoint;
            x := movex + epoint;
            if board(x,y) = Empty then
               exit;
            elsif board(x,y) = Blocked then
               exit;
            elsif board(x,y) = player then
               if (HitOpponent) then
                  for epoint in Dimension range 1 .. moveroom loop
                     y := movey - epoint;
                     x := movex + epoint;
                     if board(x,y) = Opponent then
                        board(x,y) := Player;
                     else exit SE_Loop;
                     end if;
                  end loop;
               else exit;
               end if;
            elsif board(x,y) = Opponent then
               HitOpponent := true;
            end if;
         end loop SE_Loop;
      end if;
   end AdvanceMove;

   -- 'ToString' function for states
   function Image(state : State_Type) return String is
      temp : Unbounded_String;
   begin
      temp := temp & "[" & BoardPoint'Image(state.justWent) & "," & Image(state.spot)
        & "," & Natural'Image(state.turnsleft) & "]";
      for J in Dimension'Range loop
         for I in Dimension'Range loop
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
         temp := temp & Ada.Characters.Latin_1.LF;
      end loop;
      return To_String(temp);
   end Image;

   -- 'ToString' function for places
   function Image(spot : Place) return String is
   begin
      return "(" & Dimension'Image(spot(x)) & "," & Dimension'Image(spot(y))
        & "," & ")";
   end;

   -- 'ToString' function for boards
   function Image(board : GameBoard) return String is
      temp : Unbounded_String;
   begin
      for J in Dimension'Range loop
         for I in Dimension'Range loop
            case board(I,J) is
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
         temp := temp & Ada.Characters.Latin_1.LF;
      end loop;

      return To_String(temp);
   end;

end Boards;
