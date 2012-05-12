package Agent is

<<<<<<< HEAD
=======
	for BoardPoint use (Empty => 0, White => 1, Black => 2, Blocked => 3);
	type CBoardState is array (Dimension,Dimension) of Integer;
	type BoardState is array (Dimension,Dimension) of BoardPoint;

>>>>>>> 468b1c0f22e90d1fb3184eefef1b44b519eb79ca
	procedure Ada_Subroutine;
	pragma export(CPP, Ada_Subroutine );

end Agent;
