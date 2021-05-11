{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Data.Maybe (fromMaybe)
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    state :: s,
    parent :: Node s a,
    hasParent :: Int,
    adancime :: Int,
    copii :: [Node s a],
    action :: a,
    cost :: Float
}

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    Node state1 parent1 hasParent1 adancime1 copii1 action1 cost1 ==
        Node state2 parent2 hasParent2 adancime2 copii2 action2 cost2 = 
        state1 == state2

instance Ord s => Ord (Node s a) where
    Node state1 parent1 hasParent1 adancime1 copii1 action1 cost1 <=
        Node state2 parent2 hasParent2 adancime2 copii2 action2 cost2 = 
        state1 <= state2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState nod = state nod

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent nod = if (hasParent nod) == 1 then Just (parent nod)
                else Nothing

nodeDepth :: Node s a -> Int
nodeDepth nod = adancime nod

nodeChildren :: Node s a -> [Node s a]
nodeChildren nod = copii nod

nodeHeuristic :: Node s a -> Float
nodeHeuristic nod = cost nod

nodeAction :: Node s a -> Maybe a
nodeAction node = if hasParent node == 1 then Just (action node)
                    else Nothing

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

buildTree :: (ProblemState s a, Eq s) => Int -> Node s a -> (a , s) -> Node s a
buildTree adanc tata nxt = Node (snd nxt) tata 1 adanc (foldl (\ac pr -> ac ++ [buildTree (adanc + 1) (Node (snd nxt) tata 1 adanc undefined (fst nxt) 0) pr]) [] (successors (snd nxt))) (fst nxt) 0

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = Node initialState undefined 0 0 (foldl (\ac pr -> ac ++ [buildTree 1 (createStateSpace initialState) pr]) [] (successors initialState)) undefined 0

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = foldl (\ac child -> if S.member (state child) visited then ac else ac ++ [child]) [] (copii node)

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = let
                cst = h (state node) + fromIntegral (adancime node)
                ans = PQ.lookup node frontier
                lastCst = case ans of
                        Just y -> y
                        Nothing -> -1.0
                in if lastCst == -1.0 then PQ.insert node cst frontier
                    else if cst < lastCst then let
                        newPq = PQ.delete node frontier
                        in PQ.insert node cst newPq
                        else frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = let
    validNodes = suitableSuccs node visited
    in if null validNodes then frontier
        else foldl insertSucc frontier validNodes

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

emptyNode :: Node s a
emptyNode = Node undefined undefined (-1) (-1) undefined undefined (-1)

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = if PQ.null frontier then emptyNode
    else let
        pair = deleteFindMin frontier
        nd = fst pair
        pqNew = snd pair
        newVisited = S.insert (state nd) visited
        newPq = insertSuccs nd pqNew newVisited
        in if not $ isGoal (state nd) then
                astar' newVisited newPq
                else nd

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = let
    pq = PQ.empty
    pqLast = insertSucc pq initialNode
    st = S.empty
    in astar' st pqLast

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode = let
    nodes = takeWhile (\nod -> hasParent nod == 1) $ iterate (\nd -> parent nd) goalNode
    in reverse $ foldl (\ac el -> ac ++ [(action el, state el)]) [] nodes