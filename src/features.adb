with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;
With Boards; use Boards;

with TemporalDifference; use TemporalDifference;

package body Features is

   procedure BuildStability(board : GameBoard; StabMatrix : out InfoMatrix) is
      StabilityChanged : Boolean := True;
   begin
      StabMatrix := EmptyMatrix;
      -- While loop while we are still finding stability changes
      -- (This is because stability can depend on stability of neighbouring nodes)
      while StabilityChanged loop
         Put_Line("Looping!");
         StabilityChanged := False;
         for i in Dimension'Range loop
            for j in Dimension'Range loop
               -- Don't evaluate for empty, blocked or if we already know is stable
               if board(i,j) /= Empty and board(i,j) /= Blocked
                 and not StabMatrix(i,j) then
                  if CheckStability((i,j),board(i,j),board, StabMatrix) then
                     StabMatrix(i,j) := True;
                     StabilityChanged := True;
                  end if;
               end if;
            end loop;
         end loop;
      end loop;
   end BuildStability;

   -- Count the number of stable nodes we have
   procedure CountStability(player : Players; board : GameBoard; stabmatrix : in InfoMatrix; StablePieces : out Integer) is

   begin
      StablePieces := 0;
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            if board(i,j) = player then
               if stabmatrix(i,j) then
                  StablePieces := StablePieces + 1;
               end if;
            end if;
         end loop;
      end loop;
   end CountStability;

   procedure CountInternals(player : Players; board : GameBoard; internalmatrix : in InfoMatrix; InternalPieces: out Integer) is
   begin
      InternalPieces := 0;
      for i in Dimension'Range loop
         for j in Dimension'Range loop
            if board(i,j) = player then
               if internalmatrix(i,j) then
                  InternalPieces := InternalPieces + 1;
               else
                  if (CheckInternal((i,j),board)) then
--                       internalmatrix(i,j) := True;
                     InternalPieces := InternalPieces + 1;
                  end if;
               end if;
            end if;
         end loop;
      end loop;
   end CountInternals;

   procedure UpdateStability(move : Place; board : GameBoard; StabMatrix : in out InfoMatrix) is
      columnfull : Boolean := True;
      rowfull : Boolean := True;
      NWSEfull : Boolean := True;
      SWNEfull : Boolean := True;

      moveroom : Dimension;
      yroom : Dimension;
      xroom : Dimension;
      movey : Dimension := (move(y));
      movex : Dimension := (move(x));
      x : Dimension;
      y : Dimension;
   begin
      -- Check all 8 lines on 4 different directions, to see if we filled them
      -- We only need to check the new status of the stability in the place we just moved
      -- or a direction, if we filled a direction

      if CheckStability(move, board(movex,movey), board, StabMatrix) then
         StabMatrix(movex,movey) := True;
      end if;

      --Check straight right full

      -- Y is static
      y := movey;
      Right_Loop :
      for xpoint in Dimension range 1 .. (Dimension'Last - movex) loop
         -- Move along the row
         x := movex + xpoint;
         if board(x,y) = Empty then
            -- direction is not full, we hit empty
            rowfull := False;
            exit Right_Loop;
         elsif board(x,y) = Blocked then
            --line is full, we hit blocked first
            exit Right_Loop;
         end if;
      end loop Right_Loop;

      if rowfull then
         --straight left, only if left-right direction is full so far
         Left_Loop :
         for xpoint in Dimension range 1 .. (movex) loop
            x := movex - xpoint;
            if board(x,y) = Empty then
               -- direction is not full, we hit empty
               rowfull := False;
               exit Left_Loop;
            elsif board(x,y) = Blocked then
               --direction is full, we hit blocked
               exit Left_Loop;
            end if;
         end loop Left_Loop;
      end if;

      --straight up
      x := movex;
      Up_Loop :
      for ypoint in Dimension range 1 .. (Dimension'Last - movey)       loop
         y := movey+ypoint;
         if board(x,y) = Empty then
            columnfull := False;
            exit Up_Loop;
         elsif board(x,y) = Blocked then
            exit Up_Loop;
         end if;
      end loop Up_Loop;

      if columnfull then
         --straight down if needed
         Down_Loop :
         for ypoint in Dimension range 1 .. (movey) loop
            y := movey - ypoint;
            if board(x,y) = Empty then
               columnfull := False;
               exit Down_Loop;
            elsif board(x,y) = Blocked then
               exit Down_Loop;
            end if;
         end loop Down_Loop;
      end if;

      --straight NE
      yroom := Dimension'Last - movey;
      xroom := Dimension'Last - movex;
      -- Check which axis has the least room, to see how long the diagonal is
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
            SWNEfull := False;
            exit NE_Loop;
         elsif board(x,y) = Blocked then
            exit NE_Loop;
         end if;
      end loop NE_Loop;

      if SWNEfull then
         --straight SW
         yroom := movey;
         xroom := movex;
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         SW_Loop :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey - epoint;
            x := movex - epoint;
            if board(x,y) = Empty then
               SWNEfull := False;
               exit SW_Loop;
            elsif board(x,y) = Blocked then
               exit SW_Loop;
            end if;
         end loop SW_Loop;
      end if;

      --straight NW
      yroom := Dimension'Last - movey;
      xroom := movex;
      -- Check which axis has the least room, to see how long the diagonal is
      if (yroom > xroom) then
         moveroom := xroom;
      else
         moveroom := yroom;
      end if;
      NW_Loop :
      for epoint in Dimension range 1 .. moveroom loop
         y := movey + epoint;
         x := movex - epoint;
         if board(x,y) = Empty then
            NWSEfull := False;
            exit NW_Loop;
         elsif board(x,y) = Blocked then
            exit NW_Loop;
         end if;
      end loop NW_Loop;

      if NWSEfull then
         --straight SE
         yroom := movey;
         xroom := Dimension'Last - movex;
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         SE_Loop :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey - epoint;
            x := movex + epoint;
            if board(x,y) = Empty then
               NWSEfull := False;
               exit SE_Loop;
            elsif board(x,y) = Blocked then
               exit SE_Loop;
            end if;
         end loop SE_Loop;
      end if;

      -- If any direction is now full, check the stability of all its members
      if rowfull then
         --straight right
         y := movey;
         Right_Loop_Stability_Check :
         for xpoint in Dimension range 1 .. (Dimension'Last - movex) loop
            -- Move along the row
            x := movex + xpoint;
            if board(x,y) = Blocked then
               --we hit a blocked point so we can finish checking
               exit Right_Loop_Stability_Check;
            elsif not StabMatrix(x,y) then
               -- If we haven't already found it to be stable
               StabMatrix(x,y) := CheckStability(move, board(x,y), board, StabMatrix);
            end if;
         end loop Right_Loop_Stability_Check;

         --straight left
         Left_Loop_Stability_Check :
         for xpoint in Dimension range 1 .. (movex) loop
            x := movex - xpoint;
            if board(x,y) = Blocked then
               --we hit a blocked point so we can finish checking
               exit Left_Loop_Stability_Check;
            elsif not StabMatrix(x,y) then
               -- If we haven't already found it to be stable
               StabMatrix(x,y) := CheckStability(move, board(x,y), board, StabMatrix);
            end if;
         end loop Left_Loop_Stability_Check;
      end if;

      if columnfull then
         --straight up
         x := movex;
         Up_Loop_Stability_Check :
         for ypoint in Dimension range 1 .. (Dimension'Last - movey)       loop
            y := movey+ypoint;
            if board(x,y) = Blocked then
               exit Up_Loop_Stability_Check;
            elsif not StabMatrix(x,y) then
               -- If we haven't already found it to be stable
               StabMatrix(x,y) := CheckStability(move, board(x,y), board, StabMatrix);
            end if;
         end loop Up_Loop_Stability_Check;
         --straight down if needed
         Down_Loop_Stability_Check :
         for ypoint in Dimension range 1 .. (movey) loop
            y := movey - ypoint;
            if board(x,y) = Blocked then
               exit Down_Loop_Stability_Check;
            elsif not StabMatrix(x,y) then
               -- If we haven't already found it to be stable
               StabMatrix(x,y) := CheckStability(move, board(x,y), board, StabMatrix);
            end if;
         end loop Down_Loop_Stability_Check;
      end if;

      if SWNEfull then
         --straight NE
         yroom := Dimension'Last - movey;
         xroom := Dimension'Last - movex;
         -- Check which axis has the least room, to see how long the diagonal is
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;

         NE_Loop_Stability_Check :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey + epoint;
            x := movex + epoint;
            if board(x,y) = Blocked then
               exit NE_Loop_Stability_Check;
            elsif not StabMatrix(x,y) then
               -- If we haven't already found it to be stable
               StabMatrix(x,y) := CheckStability(move, board(x,y), board, StabMatrix);
            end if;
         end loop NE_Loop_Stability_Check;
         --straight SW
         yroom := movey;
         xroom := movex;
         -- Check which axis has the least room, to see how long the diagonal is
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         SW_Loop_Stability_Check :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey - epoint;
            x := movex - epoint;
            if board(x,y) = Blocked then
               exit SW_Loop_Stability_Check;
            elsif not StabMatrix(x,y) then
               -- If we haven't already found it to be stable
               StabMatrix(x,y) := CheckStability(move, board(x,y), board, StabMatrix);
            end if;
         end loop SW_Loop_Stability_Check;
      end if;

      if NWSEfull then
         --straight NW
         yroom := Dimension'Last - movey;
         xroom := movex;
         -- Check which axis has the least room, to see how long the diagonal is
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         NW_Loop_Stability_Check :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey + epoint;
            x := movex - epoint;
            if board(x,y) = Blocked then
               exit NW_Loop_Stability_Check;
            elsif not StabMatrix(x,y) then
               -- If we haven't already found it to be stable
               StabMatrix(x,y) := CheckStability(move, board(x,y), board, StabMatrix);
            end if;
         end loop NW_Loop_Stability_Check;

         --straight SE
         yroom := movey;
         xroom := Dimension'Last - movex;
         -- Check which axis has the least room, to see how long the diagonal is
         if (yroom > xroom) then
            moveroom := xroom;
         else
            moveroom := yroom;
         end if;
         SE_Loop_Stability_Check :
         for epoint in Dimension range 1 .. moveroom loop
            y := movey - epoint;
            x := movex + epoint;
            if board(x,y) = Blocked then
               exit SE_Loop_Stability_Check;
            elsif not StabMatrix(x,y) then
               -- If we haven't already found it to be stable
               StabMatrix(x,y) := CheckStability(move, board(x,y), board, StabMatrix);
            end if;
         end loop SE_Loop_Stability_Check;
      end if;

   end UpdateStability;

   function CheckStability(move : Place; player : BoardPoint; board : GameBoard; StabMatrix : InfoMatrix) return Boolean is

      Opponent : BoardPoint := NextPlayer(player);

      columnfull : Boolean := True;
      rowfull : Boolean := True;
      NWSEfull : Boolean := True;
      SWNEfull : Boolean := True;

      moveroom : Dimension;
      yroom : Dimension;
      xroom : Dimension;
      movey : Dimension := (move(y));
      movex : Dimension := (move(x));
      x : Dimension;
      y : Dimension;
   begin

      -- ROW direction stable check
      if (movex = Dimension'Last or movex = Dimension'First) then
         -- Edge neighbour, this line is stable
         null;
      elsif (StabMatrix(movex-1,movey) and board(movex-1,movey) = player)
        or (StabMatrix(movex+1,movey) and board(movex+1,movey) = player)
        or (board(movex-1,movey) = Blocked)
        or (board(movex+1,movey) = Blocked)then
         -- Stable node next to us of our colour, we're stable on this direction
         null;
      else

         --straight right
         y := movey;

         Right_Loop :
         for xpoint in Dimension range 1 .. (Dimension'Last - movex) loop
            x := movex + xpoint;
            if board(x,y) = Empty then
               rowfull := False;
               exit Right_Loop;
            elsif board(x,y) = Blocked then
               --direction is full, we hit blocked first
               exit Right_Loop;
            end if;
         end loop Right_Loop;

         if rowfull then
            --straight left, only if required
            Left_Loop :
            for xpoint in Dimension range 1 .. (movex-1) loop
               x := movex - xpoint;
               if board(x,y) = Empty then
                  rowfull := False;
                  exit Left_Loop;
               elsif board(x,y) = Blocked then
                  --direction is full, we hit blocked first
                  exit Left_Loop;
               end if;
            end loop Left_Loop;
         end if;
      end if;


      if (movey = Dimension'Last or movey = Dimension'First) then
         -- Edge neighbour, this line is stable
         null;
      elsif (StabMatrix(movex,movey-1) and board(movex,movey-1) = player)
        or (StabMatrix(movex,movey+1) and board(movex,movey+1) = player)
        or (board(movex,movey-1) = Blocked)
        or (board(movex,movey+1) = Blocked) then
         -- Stable node next to us of our colour, we're stable on this direction
         null;
      else
         --straight up
         x := movex;
         Up_Loop :
         for ypoint in Dimension range 1 .. (Dimension'Last - movey)       loop
            y := movey+ypoint;
            if board(x,y) = Empty then
               columnfull := False;
               exit Up_Loop;
            elsif board(x,y) = Blocked then
               --direction is full, we hit blocked first
               exit Up_Loop;
            end if;
         end loop Up_Loop;

         if columnfull then
            --straight down if needed
            Down_Loop :
            for ypoint in Dimension range 1 .. (movey) loop
               y := movey - ypoint;
               if board(x,y) = Empty then
                  columnfull := False;
                  exit Down_Loop;
               elsif board(x,y) = Blocked then
                  --direction is full, we hit blocked first
                  exit Down_Loop;
               end if;
            end loop Down_Loop;
         end if;
      end if;

      -- DIAGONALS check
      if (movex = Dimension'Last or movex = Dimension'First
          or movey = Dimension'Last or movey = Dimension'First) then
         --we're at least an edge piece, so both diagonals are stable
         null;
      else
         -- NESW direction check
         if (StabMatrix(movex+1,movey-1) and board(movex+1,movey-1) = player)
           or (StabMatrix(movex-1,movey+1) and board(movex-1,movey+1) = player)
           or (board(movex+1,movey-1) = Blocked)
           or (board(movex-1,movey+1) = Blocked) then
            -- Stable node next to us of our colour, we're stable on this direction
            null;
         else
            --straight NE
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
                  SWNEfull := False;
                  exit NE_Loop;
               elsif board(x,y) = Blocked then
                  --direction is full, we hit blocked first
                  exit NE_Loop;
               end if;
            end loop NE_Loop;

            if SWNEfull then
               --straight SW
               yroom := movey;
               xroom := movex;
               if (yroom > xroom) then
                  moveroom := xroom;
               else
                  moveroom := yroom;
               end if;
               SW_Loop :
               for epoint in Dimension range 1 .. moveroom loop
                  y := movey - epoint;
                  x := movex - epoint;
                  if board(x,y) = Empty then
                     SWNEfull := False;
                     exit SW_Loop;
                  elsif board(x,y) = Blocked then
                     --direction is full, we hit blocked first
                     exit SW_Loop;
                  end if;
               end loop SW_Loop;
            end if;
         end if;

         -- NWSE direction check
         if (StabMatrix(movex-1,movey-1) and board(movex-1,movey-1) = player)
           or (StabMatrix(movex+1,movey+1) and board(movex+1,movey+1) = player)
           or (board(movex-1,movey-1) = Blocked)
           or (board(movex+1,movey+1) = Blocked) then
            -- Stable node next to us of our colour, we're stable on this direction
            null;
         else
            --straight NW
            yroom := Dimension'Last - movey;
            xroom := movex;
            if (yroom > xroom) then
               moveroom := xroom;
            else
               moveroom := yroom;
            end if;
            NW_Loop :
            for epoint in Dimension range 1 .. moveroom loop
               y := movey + epoint;
               x := movex - epoint;
               if board(x,y) = Empty then
                  NWSEfull := False;
                  exit NW_Loop;
               elsif board(x,y) = Blocked then
                  --direction is full, we hit blocked first
                  exit NW_Loop;
               end if;
            end loop NW_Loop;

            if NWSEfull then
               --straight SE
               yroom := movey;
               xroom := Dimension'Last - movex;
               if (yroom > xroom) then
                  moveroom := xroom;
               else
                  moveroom := yroom;
               end if;
               SE_Loop :
               for epoint in Dimension range 1 .. moveroom loop
                  y := movey - epoint;
                  x := movex + epoint;
                  if board(x,y) = Empty then
                     NWSEfull := False;
                     exit SE_Loop;
                  elsif board(x,y) = Blocked then
                     --direction is full, we hit blocked first
                     exit SE_Loop;
                  end if;
               end loop SE_Loop;
            end if;
         end if;

      end if;

      return columnfull and rowfull and SWNEfull and NWSEfull;
   end CheckStability;

   function CheckInternal(move : Place; board : GameBoard) return Boolean is
   begin
      if move(x) = Dimension'Last then
         if move(y) = Dimension'Last then
            return (board(move(x)-1,move(y)) /= Empty and board(move(x)-1,move(y)-1) /= Empty and board(move(x),move(y)-1) /= Empty);
         elsif move(y) = Dimension'First then
            return (board(move(x)-1,move(y)) /= Empty and board(move(x)-1,move(y)+1) /= Empty and
                      board(move(x),move(y)+1) /= Empty);
         else
            return (board(move(x)-1,move(y)) /= Empty and board(move(x)-1,move(y)-1) /= Empty and board(move(x)-1,move(y)+1) /= Empty and board(move(x),move(y)-1) /= Empty and
                      board(move(x),move(y)+1) /= Empty);
         end if;
      elsif move(x) = Dimension'First then
         if move(y) = Dimension'Last then
                  return (board(move(x),move(y)-1) /= Empty and
                board(move(x)+1,move(y)) /= Empty and board(move(x)+1,move(y)-1) /= Empty);
         elsif move(y) = Dimension'First then
                  return (board(move(x),move(y)+1) /= Empty and board(move(x)+1,move(y)) /= Empty and board(move(x)+1,move(y)+1) /= Empty);
         else
            return (board(move(x),move(y)-1) /= Empty and
                      board(move(x),move(y)+1) /= Empty and board(move(x)+1,move(y)) /= Empty and board(move(x)+1,move(y)-1) /= Empty and board(move(x)+1,move(y)+1) /= Empty);
         end if;
      elsif move(y) = Dimension'First then
         return (board(move(x)-1,move(y)) /= Empty and board(move(x)-1,move(y)+1) /= Empty and
                   board(move(x),move(y)+1) /= Empty and board(move(x)+1,move(y)) /= Empty and board(move(x)+1,move(y)+1) /= Empty);
      elsif move(y) = Dimension'Last then
         return (board(move(x)-1,move(y)) /= Empty and board(move(x)-1,move(y)-1) /= Empty and board(move(x),move(y)-1) /= Empty and
                   board(move(x)+1,move(y)) /= Empty and board(move(x)+1,move(y)-1) /= Empty);
      else
         --Put_Line(move(x)'Img & "," & move(y)'Img);
         return (board(move(x)-1,move(y)) /= Empty and board(move(x)-1,move(y)-1) /= Empty and board(move(x)-1,move(y)+1) /= Empty and board(move(x),move(y)-1) /= Empty and
                board(move(x),move(y)+1) /= Empty and board(move(x)+1,move(y)) /= Empty and board(move(x)+1,move(y)-1) /= Empty and board(move(x)+1,move(y)+1) /= Empty);
      end if;
   end CheckInternal;

end Features;
