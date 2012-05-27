with Boards; use Boards;

package Agent is

   task type Main is
      entry Initialise;
      entry NewMove;
      entry GameEnd;
   end Main;

   type MainAccess is access all Main;
   MainTask : MainAccess;

   procedure StartUp;
   procedure Ada_Subroutine;
   procedure GameEnd;
   pragma export(CPP, StartUp );
   pragma export(CPP, Ada_Subroutine );
   pragma export(CPP, GameEnd );

   my_player : BoardPoint;

   CurrentGamePhase : Game_Phase;
   CurrentCorners : Natural := 0;

end Agent;
