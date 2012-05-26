with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;
With Boards; use Boards;

with TemporalDifference; use TemporalDifference;

package body Features is

   function CheckStability(move : Place; board : GameBoard) return Boolean is

      columnfull : Boolean := True;
      rowfull : Boolean := True;
      NWSEfull : Boolean := True;
      SWNEfull : Boolean := True;

      columnstable : Boolean := False;
      rowstable : Boolean := False;
      NWSEstable : Boolean := False;
      SWNEstable : Boolean := False;

      x : Dimension;
      y : Dimension;
      moveroom : Dimension;
      yroom : Dimension;
      xroom : Dimension;
      movey : Dimension := move(y);
      movex : Dimension := move(x);
   begin

      if (x = Dimension'Last or x = Dimension'First) then
         -- left or right is edge, we're stable
         rowstable := True;
      else
         --straight right
         y := movey;
         --Put_Line("Starting right");

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
         for ypoint in Dimension range 1 .. (movey-1) loop
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

      --straight NE
      if (not(movex = Dimension'Last or movey = Dimension'Last)) then
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
      end if;

      if SWNEfull then
         --straight SW
         if (not(movex = Dimension'First or movey = Dimension'First)) then
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


      --straight NW
      if (not(movex = Dimension'First or movey = Dimension'Last)) then
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
      end if;

      if NWSEfull then
         --straight SE
         if (not(movey = Dimension'First or movex = Dimension'Last)) then
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

      if columnfull then
         columnstable := True;
      else


      return True;
   end CheckStability;

end Features;
