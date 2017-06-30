% Signature: noDups(List, WithoutDups) /2
% Purpose: that succeeds if and only if WithoutDups contains the same 
noDups(List, WithoutDups) :- noDupsRecursion(List, WithoutDups).

% The basis of recursion 
noDupsRecursion([], []). 

% Signature: noDupsRecursion(List, WithoutDups) /2
% Purpose: that succeeds if and only if WithoutDups contains the same elements of List with duplications removed.
noDupsRecursion([X|List], [X|WithoutDups]):-
		removeX(X, List, NewList), noDups(NewList, WithoutDups).

% Signature: removeX(element, List, NewList) /3
% Purpose: remove every element X from List, and retuen it as NewList. 
% The basis of recursion 
removeX(X, [], []).	
% if the elemnts is the appering first in the list
removeX(X, [X | List], NewList) :- removeX(X, List, NewList).
% else the elemnts isn't appering first in the list
removeX(X, [Y | List], [Y | NewList]) :- X \= Y, removeX(X, List, NewList).