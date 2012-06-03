with Boards; use Boards;

package Configure is
   -- Player colour
   my_player : BoardPoint;
   -- Total depth to search to
   depth : TurnsNo := 6;
   -- Number of worker tasks: Warning, should be set to one less than number of available cores
   workerTasks : Natural := 4;
   -- Initialisation of worker tasks
   initialised : Boolean := False;
   -- For epsilon-greedy move selection;
   epsilon : Float := 0.15;
end Configure;
