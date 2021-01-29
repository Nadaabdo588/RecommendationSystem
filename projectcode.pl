offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).

possibleSubset(L,R):-
    subset(L,R1),
   permutation(R1,R).

subset([],[]).
subset([H|T],[H|S]):-
    subset(T,S).
subset([H|T],S):-
    subset(T,S).


 isEmpty([]).

 choosePreferences(L,R):-
	choosePreferences(L,[],R).

choosePreferences([],C,C).
choosePreferences([activity(L)|T],C,R):-
	helper(L,[],C1),
	\+isEmpty(C1),
	append(C,[activity(C1)],C2),
	choosePreferences(T,C2,R).

choosePreferences([H|T],C,R):-
		\+ H = activity(X),
		append(C,[H],C1),
		choosePreferences(T,C1,R).


choosePreferences([H|T],C,R):-
		choosePreferences(T,C,R).

helper([],C,C).
helper([H|T],C,R):-
	append(C,[H],C1),
	helper(T,C1,R).
helper([H|T],C,R):-	helper(T,C,R).
preferenceSatisfaction(Offer, Customer, ChosenPrefs, S):-
	f(Offer, Customer, ChosenPrefs, S).
f(_,_,[],0).
f(O,C,[accommodation(X)|T],S):-
	offerAccommodation(O,X),
	customerPreferredAccommodation(C,X,R),
	f(O,C,T,S1),
	S is S1+R.
f(O,C,[accommodation(X)|T],S):-
	\+offerAccommodation(O,X),
	f(O,C,T,S).
f(O,C,[means(X)|T],S):-
	offerMean(O,X),
	customerPreferredMean(C,X,R),
	f(O,C,T,S1),
	S is S1+R.
f(O,C,[means(X)|T],S):-
	\+offerMean(O,X),
	f(O,C,T,S).
f(offer(D,Ac,Co,Vf,Vt,P,Du,N),C,[activity(X)|T],S):-
	f1(Ac,X,C,R),
	f(offer(D,Ac,Co,Vf,Vt,P,Du,N),C,T,S1),
	S is S1+R.
f(O,C,[dest(X)|T],S):-
	f(O,C,T,S).
f(O,C,[budget(X)|T],S):-
	f(O,C,T,S).
f(O,C,[period(X,Y)|T],S):-
	f(O,C,T,S).
f1(_,[],_,0).
f1(Ac,[H|T],C,S):-
	member(H,Ac),
	customerPreferredActivity(C,H,R),
	f1(Ac,T,C,S1),
	S is R+S1.
f1(Ac,[H|T],C,S):-
	\+member(H,Ac),
	f1(Ac,T,C,S).


overlapPeriod(period(Y1o-M1o-D1o,Y2o-M2o-D2o),period(Y1c-M1c-D1c,Y2c-M2c-D2c)):-
	C1 is D1c+M1c*30+Y1c*365,
	O1 is Y1o*365 + M1o*30+D1o,
	C2 is D2c+M2c*30+Y2c*365,
	O2 is Y2o*365 + M2o*30+D2o,
	overlap(C1,C2,O1,O2).

overlap(C1,C2,O1,O2):-
	\+(C1>O2 ; C2<O1).

getOffer(L,O):-
    offerMean(offer(D,A,C,VF,VT,P,DU,NOG),_),
    getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),L),
    O=offer(D,A,C,VF,VT,P,DU,NOG).
getOffer1(O,[]).
getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),[dest(X)|T]):-
	X=D,
    getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),T).


getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),[activity(X)|T]):-
	possibleSubset(A,X),
    getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),T).



getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),[means(M)|T]):-
	offerMean(offer(D,A,C,VF,VT,P,DU,NOG),M),
    getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),T).



getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),[accommodation(AC)|T]):-
	offerAccommodation(offer(D,A,C,VF,VT,P,DU,NOG),AC),
    getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),T).



 getOffer1(offer(D,A,C,VF,VT,period(Y1-M1-D1,Y2-M2-D2),DU,NOG),[period(Y3-M3-D3,Y4-M4-D4)|T]):-
    overlapPeriod(period(Y1-M1-D1,Y2-M2-D2),period(Y3-M3-D3,Y4-M4-D4)),
    getOffer1(offer(D,A,C,VF,VT,period(Y1-M1-D1,Y2-M2-D2),DU,NOG),T).


getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),[budget(B)|T]):-
		B>=C,
        getOffer1(offer(D,A,C,VF,VT,P,DU,NOG),T).






recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
	choosePreferences(Prefs,ChosenPrefs),
	getOffer(ChosenPrefs, O).


recommendOffer(Customers,PreferenceList, O, CustomersChosen):-
	offerMean(O,_),
	helpR(Customers,PreferenceList,O,C1),
	sortStr(C1,C2),
	O=offer(A,B,C,D,E,F,G,N),
	choosecustomers(Customer,C2,N),
        getCustomers(Customer,CustomersChosen).


helpR([],[],_,[]).
helpR([C1|T1],[P1|T2],Offer,Ch):-
	preferenceSatisfaction(Offer,C1,P1,S),
	helpR(T1,T2,Offer,Ch1),
	Ch=[(C1,S)|Ch1].

getCustomers([],[]).
getCustomers([(C,S)|T],[C|L]):-
	getCustomers(T,L).
choosecustomers(L,[H|T],1):-
	howMany([H|T], K),
	getN([H|T],K,L2),
	combination(1, L2,L).
choosecustomers([],[],_).
choosecustomers(L,[H|T],N):-
	N>1,
	howMany([H|T],N1),
	N1=<N,
	N2 is N-1,
	choosecustomers(L1,T,N2),
	L=[H|L1].
choosecustomers(L,[H|T],N):-
	N>1,
	howMany([H|T],N1),
	N1>N,
	getN([H|T],N1,L1),
	combination(N,L1,L).







howMany([(C,S)|T],N):-
	howM([(C,S)|T],S,0,N).
howM([(C,S)|T],S,N1,N):-
	N2 is N1+1,
	howM(T,S,N2,N).
howM([],_,N1,N1).
howM([(C,S)|T],K,N1,N1):-
	K\=S.

getN([H|T],1,[H]).
getN([H|T],N,L):-
	N>1,
	N1 is N-1,
	getN(T,N1,L1),
	L =[H|L1].




sortStr(L,R):-
    sortAcc(L,[],R).

insertTosorted([],(N,S),[(N,S)]).
insertTosorted([(N,S)|T],(N2,S2),[(N,S)|R]):-
    S2=<S,
    insertTosorted(T,(N2,S2),R).
insertTosorted([(N,S)|T],(N2,S2),[(N2,S2),(N,S)|T]):-
    S2>S.
sortAcc([],R,R).
sortAcc([H|T],Acc,R):-
    insertTosorted(Acc,H,NewAcc),
    sortAcc(T,NewAcc,R).



combination(0, _, []) :-
    !.
combination(N, L, [V|R]) :-
    N > 0,
    NN is N - 1,
    unknown(V, L, Rem),
    combination(NN, Rem, R).

unknown(X,[X|L],L).
unknown(X,[_|L],R) :-
    unknown(X,L,R).
