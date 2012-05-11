package Agent is


	for BoardPoint use (Empty => 0, White => 1, Black => 2, Blocked => 3);
	type CBoardState is array (Dimension,Dimension) of Integer;
	type BoardState is array (Dimension,Dimension) of BoardPoint;

	procedure Ada_Subroutine;
	procedure PrintBoard(board : in BoardState);
	pragma export(CPP, Ada_Subroutine );

end Agent;