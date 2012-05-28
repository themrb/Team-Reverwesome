with Boards; use Boards;

package Configure is
   -- Total depth to search to
   depth : TurnsNo := 6;
   -- Number of worker tasks: Warning, should be set to one less than number of available cores
   workerTasks : Natural := 2;
   count : Natural := 0;
   initialised : Boolean := False;

   epsilon : Float := 0.1; -- For epsilon-greedy move selection;
   my_player : BoardPoint;
end Configure;
