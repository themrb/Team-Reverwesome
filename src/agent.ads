with Boards; use Boards;

package Agent is

   -- Main computation task
   task type Main is
      entry Initialise;
      -- Start up
      entry NewMove;
      -- Choose next move
      entry GameEnd;
      -- Evaluate learning and terminate
   end Main;

   -- Main task creation and accessing
   -- (Task created on heap to ensure persistence)
   type MainAccess is access all Main;
   MainTask : MainAccess;

   -- Procedures for C++ to call to direct Ada
   -- Exported to C++ with compiler directives
   procedure StartUp;
   procedure Ada_Subroutine;
   procedure GameEnd;
   pragma export(CPP, StartUp );
   pragma export(CPP, Ada_Subroutine );
   pragma export(CPP, GameEnd );

   -- Check game state and transition if required
   procedure PhaseTransition(CurrentGamePhase : in out Game_Phase;
                             State : in GameBoard; Corners : in out Natural);

   -- Current game phase and number of corners that have been taken
   CurrentGamePhase : Game_Phase;
   CurrentCorners : Natural := 0;

end Agent;
