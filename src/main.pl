% True if [X,Y] represents a valid point on the grid
point([X,Y]) :- X >= 0, X < 6, Y >= 0, Y < 6.

% True if the list represents a valid Car object 
car([P1,P2]) :- point(P1), point(P2).
car([P1,P2,P3]) :- point(P1), point(P2), point(P3).

% True if Sign is 1 for positive values of N, -1 for negative values of N, and 0 otherwise.
sign(N, Sign) :- N > 0, Sign is 1.
sign(N, Sign) :- N < 0, Sign is -1.
sign(N, Sign) :- N == 0, Sign is 0.

% Compares the x-coordinates between two points. True if Result is 1 and the x-coordinate of 
% the first point is greater than the second, Result is -1 and the second x-coordinate is greater 
% than the first, or Result is 0 and both coordinates are equal.
compareX([X1,_], [X2,_], Result) :- Dx is X2 - X1, sign(Dx, Result).

% Compares the y-coordinates between two points. True if Result is 1 and the y-coordinate of 
% the first point is greater than the second, Result is -1 and the second y-coordinate is greater 
% than the first, or Result is 0 and both coordinates are equal.
compareY([_,Y1], [_,Y2], Result) :- Dy is Y2 - Y1, sign(Dy, Result).

% True if the given direction vector corresponds with the given Car
direction([P1,P2|_], [X, 0]) :- compareX(P2, P1, X), X \= 0.
direction([P1,P2|_], [0, Y]) :- compareY(P2, P1, Y), Y \= 0.

% True if the [X1,Y1] is [X,Y] translated by the vector [Dx,Dy]
translated([X,Y], [Dx,Dy], [X1,Y1]) :- X1 is X + Dx, Y1 is Y + Dy.

% True if each point in the second list is equal to the corresponding point in the first
% list, but scaled by Delta.
translatedPoints([P], Delta, [P1]) :- translated(P, Delta, P1), point(P1).
translatedPoints([P|T], Delta, [P1|T1]) :- 	translated(P, Delta, P1), 
											translatedPoints(T, Delta, T1),
											point(P1).

% True if [X,Y] is equal to [X1,Y1] but scaled by [Sx, Sy]
scaledVector([X,Y], [Sx,Sy], [X1,Y1]) :- X1 is X * Sx, Y1 is Y * Sy.											

% True if [Dx,Dy] is a valid representation of a move vector
delta([Dx,Dy]) :- Dx == 0 ; Dy == 0.

% True if Result is a translation of Car by the specified vector relative to Car's direction
translatedCar(Car, RelativeDelta, Result) :- 	delta(RelativeDelta),
												direction(Car, Dir),
												scaledVector(Dir, RelativeDelta, AbsoluteDelta), 
												translatedPoints(Car, AbsoluteDelta, Result).

% True if Result is a valid translation of the car at CarIndex of Carr by the specified vector relative 
% to the car's direction and constrained by the other cars on the grid
validMove(Cars, CarIndex, RelativeDelta, [Result|OtherCars]) :-  nth0(CarIndex, Cars, Car),
													deleteAtIndex(CarIndex, Cars, OtherCars),
													translatedCar(Car, RelativeDelta, Result), 		
													allSpacesAreUnoccupied(OtherCars, Result).

% True if Result is equal to List with the element at Index removed
deleteAtIndex(Index, List, Result) :- deleteAtIndex(0, Index, List, Result).

deleteAtIndex(_, _, [], []).
deleteAtIndex(CurrentIndex, Index, [H|T], [H|Result]) :- CurrentIndex \= Index, NextIndex is CurrentIndex + 1, deleteAtIndex(NextIndex, Index, T, Result).
deleteAtIndex(CurrentIndex, Index, [_|T], T) :- CurrentIndex == Index. 

% True if Point is not occupied by any of the specified cars
isUnoccupiedSpace([], _).
isUnoccupiedSpace([Car|Cars], Point) :- \+ member(Point, Car), isUnoccupiedSpace(Cars, Point).

% True if all of the given points are unoccupied by any of the specified cars
allSpacesAreUnoccupied(_, []).
allSpacesAreUnoccupied(Cars, [Point|Points]) :- isUnoccupiedSpace(Cars, Point), allSpacesAreUnoccupied(Cars, Points).