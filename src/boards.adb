with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;

package body Boards is

   -- Passes the move onto the next player
   function NextPlayer(player : BoardPoint) return BoardPoint is
   begin
      if (player = White) then
         return Black;
      elsif (player = Black) then
         return White;
      else
         Put_Line("Dude, the previous player was " & player'Img);
      end if;
      return Empty;
   end NextPlayer;

   procedure EndBoardValue(Player : BoardPoint; State : GameBoard;
                           NumMoves : Natural; Score : out BoardValue) is
   begin
      Score := 0;
      TokenScore(State, Player, defaultWeights, Score);
      -- Weighting on the number of available moves
      Score := Score + (FeatureWeight(NumMoves) * defaultMobility);
   end EndBoardValue;


   procedure TokenScore(State : GameBoard; Player: in BoardPoint;
                        Weights: in BoardPositionWeights; Score: in out BoardValue) is
   begin
      for I in Dimension'Range loop
         for J in Dimension'Range loop
            if State(I,J) = Player then
               Score := Score + Weights(I,J);
            elsif State(I,J) = NextPlayer(Player) then
               Score := Score - Weights(I,J);
            end if;
         end loop;
      end loop;
   end TokenScore;

   function ValidMove(player : BoardPoint; board : in GameBoard; movex : in Dimension; movey : in Dimension) return Natural is
      HitOpponent : Boolean;
      Opponent : BoardPoint := NextPlayer(player);
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
      for ypoint in Dimension range 1 .. (Dimension'Last - movey)       loop
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

   procedure AdvanceMove(player : BoardPoint; board : in out GameBoard; movex : Dimension; movey : Dimension) is
      HitOpponent : Boolean;
      Opponent : BoardPoint := NextPlayer(player);
      x : Dimension;
      y : Dimension;
      moveroom : Dimension;
      yroom : Dimension;
      xroom : Dimension;
   begin
      --Check square occupancy
      board(movex,movey) := player;
      --straight right
      HitOpponent := False;
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
               for xpoint in Dimension range 1 .. (Dimension'Last - movex) loop
                  y := movey;
                  x := movex + xpoint;
                  if board(x,y) = Opponent then
                     board(x,y) := Player;
                  else exit Right_Loop;
                  end if;
               end loop;
            else
               --Put_Line("Hit player too early " & x'Img &y'Img);
               exit Right_Loop;
            end if;
         elsif board(x,y) = Opponent then
            --Put_Line("Hit opponent " & x'Img &y'Img);
            HitOpponent := true;
         end if;
      end loop Right_Loop;
      --straight left
      HitOpponent := False;
      Left_Loop :
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
      for xpoint in Dimension range 1 .. (movex-1) loop
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
      --straight up
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
               --Put("Straight up " & x'Img &y'Img);
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
      --straight down
      HitOpponent := False;
      Down_Loop :
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
      for ypoint in Dimension range 1 .. (movey-1) loop
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


      --straight NE
      if (not(movex = Dimension'Last or movey = Dimension'Last)) then
      HitOpponent := False;
      --if (y
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
               --Put("Straight down " & x'Img &y'Img);
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
      --straight NW---------------------------------------------------------
      if (not(movex = Dimension'First or movey = Dimension'Last)) then
         yroom := Dimension'Last - movey;
         xroom := movex-1;
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
                  --Put("Straight down " & x'Img &y'Img);
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
               --Put("Straight down " & x'Img &y'Img);
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
               --Put("Straight down " & x'Img &y'Img);
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

   -- Print function
   function Image(state : State_Type) return String is
      temp : Unbounded_String;
   begin
      temp := temp & "[" & BoardPoint'Image(state.justWent) & "," & Image(state.spot)
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
         temp := temp & "\n";
      end loop;
      return To_String(temp);
   end Image;

      -- Print function
   function Image(spot : Place) return String is
   begin
      return "(" & Dimension'Image(spot(x)) & "," & Dimension'Image(spot(y))
        & "," & ")";
   end;

   -- Print function
   function Image(board : GameBoard) return String is
      temp : Unbounded_String;
   begin
      for I in Dimension'Range loop
         for J in Dimension'Range loop
            --Put_Line(Dimension'Image(I) & " " & Dimension'Image(J));
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
