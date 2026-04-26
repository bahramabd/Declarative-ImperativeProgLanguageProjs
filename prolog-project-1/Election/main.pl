:- module(main, [is_vote_wasted/2, is_candidate_elected/2, candidate_count_from_city/3, all_parties/1, all_candidates_from_party/2, all_elected_from_party/2, election_rate/2, council_percentage/2, alternative_debate_setups/2]).
:- [kb].

% DO NOT CHANGE THE UPPER CONTENT, WRITE YOUR CODE AFTER THIS LINE
is_vote_wasted(City,PoliticalParty) :- not(elected(City,PoliticalParty,ElectedRepresentativeCount)).

is_candidate_elected(X , Y):-candidate(X, Y, Z, N), elected(Z,Y,F), N=<F.

bir(X,Y):- candidate(X,A,Y,B).
candidate_count_from_city([],A,0).
candidate_count_from_city([H|T],A,B):- 
(bir(H,A) -> candidate_count_from_city(T,A,C), B is C+1 ; candidate_count_from_city(T,A,B)).

all_parties(Bu):- findall(Y,party(Y,_),Bu). 

all_candidates_from_party(X,Y):- findall(Z,candidate(Z,X,_,_),Y).

all_elected_from_party(X,Y):- findall( Z,is_candidate_elected(Z,X),Y).

election_rate(X,Y):-all_candidates_from_party(X,A), all_elected_from_party(X,B), length(A,Bir), length(B,Iki), 
Y is Iki/Bir, format('~2f',[Y]).  

council_percentage(X,Y):- all_elected_from_party(_,A), all_elected_from_party(X,B), 
    length(A,Bir), length(B,Iki), Y is Iki/Bir, format('~2f',[Y]).
    

mem(X,[X|_]).
mem(X,[_|T]):- mem(X,T).
lo(S,Y):-string_chars(S,Y).
bu([],Y,Y).
bu([H|T],Y,K):- party(P,H), candidate(A,P,_,_), not(mem(A,Y)), bu(T,[A|Y],K).
alternative_debate_setups(X,Y):- lo(X,T), bu(T,[],Y).
    


    

