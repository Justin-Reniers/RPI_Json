// Agent z_one in project Zeuthen Strategy.mas2j

//z_one is similar to z_two, and in fact, mostly copied and pasted (including this very text!). There is one important difference:
//A deal, defined as [[list],[list]], consists of two task allocations, one for each agent.
//Z_one uses the LEFT list as its set, and the right list as Z_two's. You are free to define a deal in other ways, however.
//Depending on how you implement the Zeuthen strategy, you might end up adding functionalities to one agent and not to the other.
//Always make sure that when you copy and paste code from one agent to the other, that you make the necessary adjustments (such as
//switching which side the agent has to use).

//Hint: Jason is not widely used on the internet, so code examples mostly exist
//on the Jason site itself. However, the beliefs are mostly prolog: take examples
//from prolog code, then check on http://jason.sourceforge.net/api/ if their prolog
//equivalents exist.
//Further, you should not need to use the environment/java part of this code.
//You should be able to finish all assignments by adjusting z_one and z_two.

/* Initial beliefs and rules */
//I remember where the post office is.
postoffice(a).

//I remember the following routes and their cost:
//Unless you want to make a graph search algorithm (not recommended),
//these beliefs will likely be unused.
route(a,b,2).
route(a,c,3).
route(b,c,4).
route(b,d,6).
route(b,e,5).
route(e,f,4).
route(c,f,3).

//I know the costs of all sub-tasks. Keep in mind that every possible combination
//is written here: if you want to add more, you will have to write down all
//new possible costs.
//It is recommended to keep lists in alphabetic order. Jason does not see
//[b,c] as the same as [c,b]. This can cause bugs. If necessary, use the function
// .sort(List,SortedList) to make a list[c,b] a sorted list [b,c].
cost([],0).
cost([b],4).
cost([c],6).
cost([d],16).
cost([e],14).
cost([f],12).
cost([b,c],9).
cost([b,d],16).
cost([b,e],14).
cost([b,f],15).
cost([c,d],21).
cost([c,e],17).
cost([c,f],12).
cost([d,e],26).
cost([d,f],27).
cost([e,f],17).
cost([b,c,d],21).
cost([b,c,e],17).
cost([b,c,f],17).
cost([b,d,e],26).
cost([b,d,f],27).
cost([b,e,f],17).
cost([c,d,e],27).
cost([c,d,f],27).
cost([c,e,f],17).
cost([d,e,f],27).
cost([b,c,d,e],27).
cost([b,c,d,f],27).
cost([b,c,e,f],17).
cost([b,d,e,f],27).
cost([c,d,e,f],27).
cost([b,c,d,e,f],27).

//I remember my task set. During experiments, make sure to adjust the task.
originalTask([b,c,f]).

//I remember that I proposed a new deal this round. 1 means I did, 0 means I didn't,
//2 means we haven't started proposing deals yet.
iPropose(2).

//I remember that I the other agent proposed a new deal this round. 1 means he did, 0 means he didn't,
//2 means we haven't started proposing deals yet.
theyPropose(2).

//Checking if two task sets are indeed valid re-distribution. This code requires
// having the total task set (b,c,d,e,f for example). You will need a way for totalTask
//to get the total task set when agents need to calculate the total task set by themselves.
validDistribution(OneSide,OtherSide) :-
	checkTotalTask(OneSide,OtherSide,[b,c,d,e,f]).  //Adjust [b,c,d,e,f] with a totalTask belief later on in the assignment.
	//uniqueSets(OneSide,OtherSide, []).

checkTotalTask(Task1, Task2, TotalTask) :-
	union(Task1,Task2, X) &
	.sort(X,SortedX) &
	SortedX = TotalTask.

union([], L, L).
union([Head|L1tail], L2, L3) :-
        member(Head, L2) &
        union(L1tail, L2, L3).
union([Head|L1tail], L2, [Head|L3tail]) :-
        union(L1tail, L2, L3tail).

//Checking if two sets are unique. 
uniqueSets([],_,[]).
uniqueSets(_,[],[]).
uniqueSets([Head1|OneSide], Otherside, [Head1|Result]):-
	member(Head1,Otherside) &
	uniqueSets(OneSide,Otherside, Result).
uniqueSets([_|OneSide], Otherside, Result) :-
	uniqueSets(OneSide, OtherSide, Result).

utility(Deal, Task, Util) :-
	cost(Task, CT) &
	cost(Deal, CD) &
	Util = (CT-CD).	
	
//I know when a task is individual rational.
indiRatio(Side, SOT) :-//enter your code here.
	utility(Side, SOT, Util) &
	Util >= 0.
	
//I know when a deal is pareto optimal:
paretoOptimal(MyTask, TheirTask) :-
	.findall(Task, cost(T, _), AllTasks) &
	.delete(MyTask, AllTasks, RestTasks) &
	lookAllTasks(MyTask, TheirTask, RestTasks).
//Enter your code here. Consider adding more functions to
//solve this problem. For example, given a task, which addresses will the other agent have to do?
//Hint: .findall function might be useful here. (See below for details)

//Checks if our a task is not dominated by any other task that is a viable other 
//task according to two conditions:
//1) The Task in RestTasks is at least as good as Task for every agent.
//2) The Task in RestTasks is strictly better for at least one agent.
lookAllTasks(_, _, []).
lookAllTasks(MyTask, TheirTask, [DealTask|Tail]) :-
	originalTask(OT) &
	theirOriginalTask(TOT) &
	utility(MyTask, OT, OTUtil) &
	utility(TheirTask, TOT, TOTUtil) &
	not checkBetterUtility(OTUtil, TOTUtil, DealTask) &
	checkOptimality(MyTask, TheirTask, Tail).

checkBetterUtility(OOTUtil, OTOTUtil, DealTask) :-
	originalTask(OT) &
	theirOriginalTask(TOT) &
	findTheirDeal(DealTask, TheirDealTask) &
	utility(DealTask, OT, OTUtil) &
	utility(TheirDealTask, TOT, TOTUtil) &
	OTUtil >= OOTUtil &
	TOTUtil >= OTOTUtil &
	(OTUtil > OOTUtil | TOTUtil > OTOTUtil).
	
//Calculates what Task the other agent gets if we have this DealTask.	
findTheirDeal(DealTask, TheirDealTask) :-
	.difference([b,c,d,e,f], DealTask, TheirDealTask).
	
	

//I know what a deal I can offer up for negotiations, is like.
//If you want to check if you did a part correct, for example, validDistribution,
//comment the other parts out. 
goodDeal([MySide,TheirSide]) :-
	cost(MySide,_) &
	cost(TheirSide,_) &
	validDistribution(MySide,TheirSide) &
	originalTask(OT) &
	theirOriginalTask(TOT) & //The agent should have received this info from the other agent.
	indiRatio(MySide,OT) & //I am not going to consider deals worse than the conflict deal.
	indiRatio(TheirSide,TOT) & //The other agent is always going to refuse deals worse than the conflict deal. No point in considering them.
	paretoOptimal(MySide,TheirSide). 
	
//I can find all possible deals for negotiations.
setOfDeals(SetOfDeals) :-
	.findall(Deal, goodDeal(Deal),SetOfDeals).
	// The function .findall(Answer, belief(Nonsense,Answer), Answers) finds all
	//potential solutions to belief(Nonsense,Answer) and puts them in a list of Answers.
	//The first argument (Answer) states which part of the belief we want all solution of.
	//The second argument is the belief that we attempt to unify with the believes we have,
	//using the parameters 'Nonsense' and 'Answer'.
	//In this particular belief, we ask for all potential deals that are correct and put them
	//in the list SetOfDeals.
	
//I can sort a set of good deals so that the deals that are best for me, come first and slowly decline to less profitable deals.
sortedSet([[MySide,TheirSide]|OtherDeals],SetOfSortedDeals) :-
	sortSet(OtherDeals,[[MySide,TheirSide]|OtherDeals],[MySide,TheirSide],SetOfSortedDeals).

//I can sort a set of deals. This is accomplished using selection sort.
//No more deals left to try and sort. We are done.
sortSet([],[],_,SetOfSortedDeals) :- SetOfSortedDeals =[].
//Went through the list and found the lowest cost. Placing it on the spot, remove
//it from the to be sorted list, and continue with remainder.
sortSet([],ToBeSortedDeals,Deal,[X|Y]) :-
	X=Deal &
	.delete(Deal,ToBeSortedDeals,ToBeSorted) &
	sortSet(ToBeSorted,ToBeSorted,[],Y).
//Starting anew, assuming for now that lowest cost comes from the first deal in the list.
sortSet([Deal|OtherDeals],ToBeSorted,[],SetOfSortedDeals) :-
	sortSet(OtherDeals,ToBeSorted,Deal,SetOfSortedDeals).
//We found a deal with a lower cost: remembering it so we can compare it with the deals after it.
sortSet([[MySide,TheirSide]|OtherDeals],ToBeSorted,[CurMyHigh,CurTheirHigh],SetOfSortedDeals) :-
	cost(MySide,MyCheckCost) &
	cost(CurMyHigh,CurMyCost) &
	MyCheckCost < CurMyCost &
	sortSet(OtherDeals,ToBeSorted,[MySide,TheirSide],SetOfSortedDeals).
//No new deal with a lower cost, so our current assumed best remains the best for now.
sortSet([[MySide,TheirSide]|OtherDeals],ToBeSorted,CurBestDeal,SetOfSortedDeals) :-
	sortSet(OtherDeals,ToBeSorted,CurBestDeal,SetOfSortedDeals).

//Returns the head of a list.
head([H|_], H).	

//Returns last element of a list.
last([X], X).
last([_|Z], Y) :-
	last(Z, Y).
	
//
willingnessToRisk(OUtil, TUtil, Willingness) :-
	Willingness = (OUtil - TUtil)/OUtil.

/* Initial goals */
//I hate the deal I have been given. I want a better one! Perhaps I can ask z_two...
!getBetterDeal.




/* Plans */
//Initial conversation with the other agent. If I do not know their Original Task,
//then I have not yet asked. So I will ask for that and remember their answer. I will show
//their original task to the user as well. After this, I find all good deals and sort them and
//print the answer to the user. I then wait for further negotiation.

//This function requires you to finish the beliefs the agent has. If you have completed
//everything correctly, then you should see the following pop up:
//Agent 1 offers following deals  [[[c,f],[b,d,e]],[[d],[b,c,e,f]],[[b,d],[c,e,f]],[[c,e,f],[b,d]],[[b,c,e,f],[d]]]
//This is the negotiation set: you completed the first part of the assignment.

//It is recommended to first finish negotiations when you get to this point. After,
//you will need to come back to this function to create a way to reason what the total
//task set is given originalTask and theirOriginalTask, as well converse about the costs and remember these.
+!getBetterDeal
	: not theirOriginalTask(Task)
	<-
	.send(z_two, askOne, originalTask(TheirTask), originalTask(Answer)); //Asking the other agent what their original task is.
	//Note that here, we only want the Answer, whereas this function would normally return 'originalTask(Answer)[source: z_two]
	//By specifying originalTask in the return, we can seperate Answer from the rest and make a new belief with it.
	+theirOriginalTask(Answer);
	.print("Agent 2 told Agent 1 their task was ", Answer);
	?setOfDeals(Deals); //Finding all good deals, but they are unsorted.
	?sortedSet(Deals,SortedSet); //All good deals are now sorted.
	.print("Agent 1 offers following deals ", SortedSet);
	+theSetOfNegotiationDeals(SortedSet); //Remember the current negotiation deals.
	!getBetterDeal.

agentWillingness(OT, OurProposal, TheirProposal, Willingness) :-
	utility(OurProposal, OT, OurPropUtility) &
	utility(TheirProposal, OT, TheirPropUtility) &
	Willingness = ((OurPropUtility - TheirPropUtility) / OurPropUtility).

updateNegotiationSet(D1, [], TOT, UpdatedNegotiationSet) :-
	UpdatedNegotiationSet = [[]].
updateNegotiationSet(D1, [D2|Tail], TOT, UpdatedNegotiationSet) :-
	last(D1, D1A2) &
	last(D2, D2A2) &
	utility(D1A2, TOT, UD1A2) &
	utility(D2A2, TOT, UD2A2) &
	UD2A2 < UD1A2 &
	updateNegotiationSet(D1, Tail, TOT, UpdatedNegotiationSet).
updateNegotiationSet(D1, [D2|Tail], TOT, UpdatedNegotiationSet) :-
	last(D1, D1A2) &
	last(D2, D2A2) &
	utility(D1A2, TOT, UD1A2) &
	utility(D2A2, TOT, UD2A2) &
	UD2A2 >= UD1A2 &
	UpdatedNegotiationSet = [D2|Tail].

compareLists([], [], Condition) :-
	Condition = true.
compareLists([], _, Condition) :-
	Condition = true.
compareLists([L1Head|L1Tail], List2, Condition) :-
	member(L1Head, List2) &
	compareLists(L1Tail, List2).

theirUtility(TOT, D2A2, D1A2) :-
	cost(TOT, CTOT) &
	cost(D2A2, CD2A2) &
	cost(D1A2, CD1A2) &
	((CTOT-CD1A2) >= (CTOT-CD2A2)).	
	
myUtility(OT, D1A1, D2A1) :-
	cost(OT, COT) &
	cost(D1A1, CD1A1) &
	cost(D2A1, CD2A1) &
	((COT-CD2A1) >= (COT-CD1A1)).
	
+!getBetterDeal
	: theirOriginalTask(TOT) & theSetOfNegotiationDeals([D1|Tail])
	<-
	-+ourDealTask(D1);
	.wait(1000);
	.send(z_two, askOne, ourDealTask(TheirTask), ourDealTask([D2A1|ListD2A2]));
	+theirDealTask([D2A1|ListD2A2]);
	?originalTask(OT);
	?utility(D2A1, OT, UD2A1);
	?head(D1, D1A1);
	?utility(D1A1, OT, UD1A1);
	?head(ListD2A2, D2A2);
	?last(D1, D1A2);
	.send(z_two, askOne, iPropose(TheyProposed), iPropose(TheyProposed));
	-+theyPropose(TheyProposed);
	if (theirUtility(TOT, D2A2, D1A2) & TheyProposed == 0) {
		.print("Deal A1: ", D1A1, D1A2);
	} if (myUtility(OT, D1A1, D2A1) & TheyProposed == 1) {
		.print("Deal A2: ", D2A1, D2A2);
	} else {
		?agentWillingness(OT, D1A1, D2A1, Agent1Willingness);
		?agentWillingness(TOT, D2A2, D1A2, Agent2Willingness);
		if (Agent1Willingness <= Agent2Willingness) {
			?updateNegotiationSet(D1, Tail, TOT, UpdatedNegotiationSet);
			-theSetOfNegotiationDeals([D1|Tail]);
			+theSetOfNegotiationDeals(UpdatedNegotiationSet);
			-+iPropose(1);
			!getBetterDeal
		} else {
			-+iPropose(0);
			!getBetterDeal
		}
	}
	.
