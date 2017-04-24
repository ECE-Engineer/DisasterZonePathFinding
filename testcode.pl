/*
Just getting used to the language.
*/

/*
%%% FACTS
raining.
john_is_cold.
fred_lost_his_car_keys.
john_Forgot_His_Raincoat.
peter_footballer.

%%% Testing FACTS
% raining.
% not_raining.
*/

/*
%%% FACTS WITH ARGUMENTS
eats(fred,oranges).
eats(fred,t_bone_steaks).
eats(tony,apples).
eats(john,apples).
eats(john,grapefruit).

%%% Testing FACTS WITH ARGUMENTS
% eats(fred,oranges).
% eats(mike,apples).
*/

/*
%%% Variables And Unification
eats(fred,mangoes).
loves(john,mary).
loves(fred,hobbies).
tape(1,van_morrison,astral_weeks,madam_george).
tape(2,beatles,sgt_pepper,a_day_in_the_life).
tape(3,beatles,abbey_road,something).
tape(4,rolling_stones,sticky_fingers,brown_sugar).
tape(5,eagles,hotel_california,new_kid_in_town).

%%% Testing Variables And Unification
% eats(fred,What).
% loves(john,Who).
% loves(arnold,Who).
% loves(fred,Who).
% tape(5,Artist,Album,Fave_Song).
% tape(4,rolling_stones,sticky_fingers,Song).
*/

/*
%%% RULES
mortal(X) :- human(X).
human(socrates).
fun(X) :- red(X), car(X).
fun(X) :- blue(X), bike(X).
fun(ice_cream).
car(vw_beatle).
car(ford_escort).
bike(harley_davidson).
red(vw_beatle).
red(ford_escort).
blue(harley_davidson).

%%% Testing RULES
% mortal(socrates).
% mortal(P).
% fun(harley_davidson).
% fun(What).
*/

/*
%%% SEARCH
eats(fred,pears).
eats(fred,t_bone_steak).
eats(fred,apples).
hold_party(X) :- birthday(X), happy(X).
birthday(tom).
birthday(fred).
birthday(helen).
happy(mary).
happy(jane).
happy(helen).
fun(X) :- red(X), car(X).
fun(X) :- blue(X), bike(X).
red(apple_1).
red(block_1).
red(car_27).
car(desoto_48).
car(edsel_57).
blue(flower_3).
blue(glass_9).
blue(honda_81).
bike(iris_8).
bike(my_bike).
bike(honda_81).

%%% Testing SEARCH
% eats(fred,FoodItem).
% hold_party(Who).
% fun(What).
*/

/*
%%% RECURSION
on_route(rome).
on_route(Place):-
	move(Place,Method,NewPlace),
	on_route(NewPlace).
move(home,taxi,halifax).
move(halifax,train,gatwick).
move(gatwick,plane,rome).

parent(john,paul).
parent(paul,tom).
parent(tom,mary).
ancestor(X,Y):- parent(X,Y).

%%% Testing RECURSION
% on_route(home).
% on_route(halifax).
% ancestor(john,tom).
% ancestor(paul,tom).
*/

/*
%%% LISTS
p([H|T], H, T).
on(Item,[Item|Rest]).
on(Item,[DisregardHead|Tail]):-
	on(Item,Tail).
append([],List,List).
append([Head|Tail],List2,[Head|Result]):-
	append(Tail,List2,Result).
sift([],[]).
sift([X|T],[X|Result]):- X > 6,
	sift(T,Result).
sift([ThrowAway|Tail],Result):-
	sift(Tail,Result).

delete_all([],Z,[]).
delete_all([X|T],Z,[X|Result]):- X \= Z,
	delete_all(T,Z,Result).
delete_all([ThrowAway|Tail],Z,Result):-
	delete_all(Tail,Z,Result).

replace_all([],A,B,[]).
replace_all([A|Tail],A,B,[B|Tail2]):-
	replace_all(Tail,A,B,Tail2).
replace_all([X|T],A,B,[X|T2]):- X \= A,
	replace_all(T,A,B,T2).

%%% Testing LISTS
% p([a,b,c], X, Y).
% p([a], X, Y).
% p([], X, Y).
% on(apples,[pears, tomatoes, apples, grapes]).
% on(apples,[london_buckingham_palace, paris_eiffel_tower, york_minster, pisa_leaning_tower, athens_parthenon]).
% append([a,b,c],[one,two,three],Result).
% sift([1,12,3,14,5,8], Result).
% delete_all([a,b,a,c,a,d],a,Result).
% delete_all([a,b,a,c,a,d],b,Result).
% delete_all([a,b,a,c,a,d],prolog,Result).
% replace_all([a,b,a,c,a,d],a,mike,Result).
*/












% True if [X,Y] represents a valid point on the grid
point([X,Y]) :- X >= 0, X < 6, Y >= 0, Y < 6.

%%% TESTS FOR THE METHOD
% point([5,5]).
% point([6,5]).

% True if the list represents a valid Car object 
car([P1,P2]) :- point(P1), point(P2).
car([P1,P2,P3]) :- point(P1), point(P2), point(P3).

%%% TESTS FOR THE METHOD
% car([[5,5], [5,4]]).
% car([[5,5], [5,4], [5,3]]).

%%% Create a method that takes a single position and a list of positions to see if the position is already on the list.
wasHereAlready(Item, [Front|Tail]) :- Item = Front ; wasHereAlready(Item, Tail).

%%% TESTS FOR THE METHOD
% wasHereAlready([0,0], [[1,1],[2,2],[3,3],[4,4],[5,5]]).
% wasHereAlready([1,1], [[1,1],[2,2],[3,3],[4,4],[5,5]]).
% wasHereAlready([1,2], [[1,1],[2,2],[3,3],[4,4],[5,5]]).
% wasHereAlready([5,5], [[1,1],[2,2],[3,3],[4,4],[5,5]]).

%%% Create a method that takes a position and a list of cars to see if any of the cars in the list occupy that position given.
carTakenSpot(_,[]) :- false.
carTakenSpot([X,Y],[H|Tail]):-
	wasHereAlready([X,Y],H) ; carTakenSpot([X,Y],Tail).

%%% TESTS FOR THE METHOD
% carTakenSpot([1,1], [[[0,0],[0,1]],[[1,2],[2,2]],[[2,3],[3,3]],[[3,4],[4,4]],[[4,5],[5,5]]]).
% carTakenSpot([1,1], [[[0,1],[1,1]],[[1,2],[2,2]],[[2,3],[3,3]],[[3,4],[4,4]],[[4,5],[5,5]]]).

%%% Create a method to add a value to each value in the point list
addPoint([X,Y], [Sx,Sy], [X1,Y1]) :- X1 is X + Sx, Y1 is Y + Sy.

%%% TESTS FOR THE METHOD
% addPoint([5,5],[1,1], Result).
% addPoint([5,5],[-1,-1], Result).
% addPoint([2,2],[0,1], CurrentPosition).
% addPoint([5,5],[0,1], CurrentPosition).
% addPoint([2,2],[0,1], CurrentPosition) , point(CurrentPosition).
% addPoint([5,5],[0,1], CurrentPosition) , point(CurrentPosition).

%%% Create a method that appends items to a list
append([],List,List).
append([Head|Tail],List2,[Head|Result]):-
	append(Tail,List2,Result).

%%% TESTS FOR THE METHOD
% Result = [b,b,b,b,b].
% append([a],Result, Result).
% append([a],[b,c,b,c,b], Result).
% append([[5,5]],[[1,1],[2,2],[3,3],[4,4]], Result).
% append([[5,5]],[[1,1],[2,2],[3,3],[4,4]], Result) , wasHereAlready([5,5], Result).
% append([[5,5]],[[1,1],[2,2],[3,3],[4,4]], Result) , wasHereAlready([0,0], Result).
% append([[3,4]],[[1,1],[2,2],[3,3],[4,4]], Somelist) , addPoint([3,4],[0,1], Somepoint).
% append([[3,4]],[[1,1],[2,2],[3,3],[4,4]], Somelist) , addPoint([3,4],[0,1], Somepoint), point(Somepoint).
% append([[3,4]],[[1,1],[2,2],[3,3],[4,4]], Somelist) , addPoint([3,4],[0,1], Somepoint), point(Somepoint), not(wasHereAlready(Somepoint, Somelist)).
% append([[3,4]],[[1,1],[2,2],[3,3],[4,4]], Somelist) , addPoint([3,4],[0,1], Somepoint), point(Somepoint), not(wasHereAlready(Somepoint, Somelist)), not(carTakenSpot(Somepoint, [])).

%%% Create a method to determine if there is a path through the grid, from start to finish.
isPath(CurrentPosition, EndPosition, CarList, BookeepingList) :- CurrentPosition = EndPosition ;
	append([CurrentPosition], BookeepingList, BookeepingList1) ,
	addPoint(CurrentPosition,[0,1], CurrentPosition), point(CurrentPosition) ,
	not(wasHereAlready(CurrentPosition, BookeepingList1)) ,
	not(carTakenSpot(CurrentPosition, CarList)) ,
	isPath(CurrentPosition, EndPosition, CarList, BookeepingList1) ;
	append([CurrentPosition], BookeepingList1, BookeepingList2) ,
	addPoint(CurrentPosition,[1,0], CurrentPosition), point(CurrentPosition) ,
	not(wasHereAlready(CurrentPosition, BookeepingList2)) ,
	not(carTakenSpot(CurrentPosition, CarList)) ,
	isPath(CurrentPosition, EndPosition, CarList, BookeepingList2) ;
	append([CurrentPosition], BookeepingList2, BookeepingList3) ,
	addPoint(CurrentPosition,[0,-1], CurrentPosition), point(CurrentPosition) ,
	not(wasHereAlready(CurrentPosition, BookeepingList3)) ,
	not(carTakenSpot(CurrentPosition, CarList)) ,
	isPath(CurrentPosition, EndPosition, CarList, BookeepingList3) ;
	append([CurrentPosition], BookeepingList3, BookeepingList4) ,
	addPoint(CurrentPosition,[-1,0], CurrentPosition), point(CurrentPosition) ,
	not(wasHereAlready(CurrentPosition, BookeepingList4)) ,
	not(carTakenSpot(CurrentPosition, CarList)) ,
	isPath(CurrentPosition, EndPosition, CarList, BookeepingList4).





%%% TESTS FOR THE METHOD
% isPath([0,0], [0,0], [], []).
% isPath([0,0], [0,1], [], []).-------------------this should pass!!
% isPath([0,0], [1,1], [], []).-------------------this should pass!!
% isPath([0,0], [6,6], [], []).----------haven't gotten to this yet!
% isPath([0,0], [6,6], [], []).----------haven't gotten to this yet!
% isPath([0,0], [6,6], [], []).----------haven't gotten to this yet!
% 
% 
% 
% could the issue be with the variable in the square brackets
% could the issue be how i am storing the value on the list >>> BookeepingList    when im using the append function
% 
% 
% 
% 
% 






%%%%%%%%%%%%%%%%carTakenSpot(wasHereAlready(point(addPoint(CurrentPosition,[-1,0], CurrentPosition)), BookeepingList), CarList).


%%% Create a method that returns the list of moves of the solution path.

% USE THE BOOKEEPING LIST after seeing if a path exists
% by going through all the points in the list to see 
% if they have a path to the solution and 
% if they don't then delete them from the list!!!
%%%%%% the leftover list will by the path   (((MAKE SURE TO ITERATE OVER THE LIST IN ORDER)))!!!







