-module(building_lists).
-export().

% Building Lists in Natural Order:
% Teh most efficient wa to build a list is to add elements to the head of an
% existing list, so we often see code with this kind of pattern:

% some_function([H|T], ..., Result, ....) ->
% 	H1 = .... H .....,
% 	some_function(T, ...., [H1|Result], ...);
% some_function([], ..., Result, ....) ->
% 	{..., Result, ...}.

% This code walks down a list extracting the head of the list H and computes
% some value based on this function (we can call this H1); it then adds H1
% to the output list Result

% When the input list is exhausted, the final clause matches, and the output
% variable Result is returned from the function
% The elements in Result are in the opposite order as the elements in the 
% original list, which may or may not be a problem, but if they are in the
% wrong order, they can easily be reversed in the final step

% 1. Always add elements to a list head
% 2. Taking the elements from the head of an InputList and adding them
% 	head first to an OutputList results in the OutputList having the reverse
% 	order of the InputList
% 3. If the order matters, then call list:reverse/1, which is highly optimized
% 4. Avoid going against these recommendations

% Note: Whenever you want to reverse a list, you should call lists:reverse and
% Nothing else. if you look in the source code for the module lists, you'll
% find a definition of reverse. However, the definition is simply used for
% illustration.
% The compiler, which it finds a call to lists:reverse, calls a more efficient
% internal version of the function

% If you ever see code like the following, it should set warning bells sounding
% in your brain -- this is very inefficient and acceptable only if List is
% short:
% List ++ [H]

% Even though ++ might lead to inefficient code, there is a trade-off between
% clarity and performance. Using ++ might lead to a clearer program without
% performance problems
% The best thing to do is first write your programs as clearly as possible
% and then, if there are performance problems, measure before making any
% optimizations
