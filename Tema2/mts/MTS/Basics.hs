{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe (fromMaybe)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Game = Game {
    targets :: [Target],
    targetsPos :: [Position],
    obstacles :: [Position],
    gateways :: [Position],
    hunter :: Position,
    connections :: [(Position, Position)],
    rows :: Int,
    columns :: Int
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String

gameAsString game = init (foldl (\accu x -> accu ++ foldl (\ac y -> 
                    if y == 0 || y == columns game - 1 || x == 0 || x == rows game - 1 || elem (x, y) (obstacles game)
                        then ac ++ "@"
                        else if x == fst (hunter game) && y == snd (hunter game)
                            then ac ++ "!"
                            else if elem (x, y) (targetsPos game)
                                then ac ++ "*"
                                else if elem (x, y) (gateways game)
                                    then ac ++ "#"
                                    else ac ++ " "
                    ) "" (take (columns game) [0..]) ++ "\n" ) "" (take (rows game) [0..]))

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game

emptyGame n m = let
        hunt = (1,  1)
        obst = [(i, j) | i <- [0, n - 1], j <- [0..m - 1]] ++
                    [(i, j) | i <- [1..n - 2], j <- [0, m - 1]]
        in Game [] [] obst [] hunt [] n m

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter pos game 
    | fst pos < 0 || fst pos >= rows game || snd pos < 0 || snd pos >= columns game = game
    | elem pos (obstacles game) || elem pos (targetsPos game) || elem pos (gateways game) = game
    | otherwise = Game (targets game) (targetsPos game) (obstacles game) (gateways game) pos (connections game) (rows game) (columns game)

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget beh pos game 
    | fst pos < 0 || fst pos >= rows game || snd pos < 0 || snd pos >= columns game = game
    -- | elem pos (obstacles game) || elem pos (targetsPos game) || elem pos (gateways game) || pos == hunter game = game
    | otherwise = Game (targets game ++ [Target pos beh]) (targetsPos game ++ [pos]) (obstacles game) (gateways game) (hunter game) (connections game) (rows game) (columns game)

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway  (pos1, pos2) game
    | fst pos1 < 0 || fst pos1 >= rows game || snd pos1 < 0 || snd pos1 >= columns game = game
    | fst pos2 < 0 || fst pos2 >= rows game || snd pos2 < 0 || snd pos2 >= columns game = game
    | otherwise = Game (targets game) (targetsPos game) (obstacles game) (gateways game ++ [pos1] ++ [pos2]) (hunter game) (connections game ++ [(pos1, pos2)] ++ [(pos2, pos1)]) (rows game) (columns game)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game
    | fst pos < 0 || fst pos >= rows game || snd pos < 0 || snd pos >= columns game = game
    -- | elem pos (obstacles game) || elem pos (targetsPos game) || elem pos (gateways game) || pos == hunter game = game
    | otherwise = Game (targets game) (targetsPos game) (obstacles game ++ [pos]) (gateways game) (hunter game) (connections game) (rows game) (columns game)

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game
    | elem pos (obstacles game) || elem pos (targetsPos game) || pos == hunter game = Nothing
    | elem pos (gateways game) = Just (snd (head (filter (\x -> fst x == pos) (connections game))))
    | otherwise = Just pos
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

getTargetMove :: Behavior -> Position -> Position -> Game -> Target

getTargetMove behave end pos game = let 
    newPos = attemptMove end game
    ans = fromMaybe pos newPos
    in if ans == pos && elem pos (gateways game) then
        Target (snd (head (filter (\x -> fst x == pos) (connections game)))) behave
        else Target ans behave

goEast :: Behavior
goEast pos game = getTargetMove goEast ( fst pos, snd pos + 1) pos game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos game = getTargetMove goWest ( fst pos, snd pos - 1) pos game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos game = getTargetMove goNorth ( fst pos - 1, snd pos) pos game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos game = getTargetMove goSouth ( fst pos + 1, snd pos) pos game

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

getBounceMove :: Behavior -> Position -> Position -> Game -> Target

getBounceMove behave end pos game = let 
    newPos = attemptMove end game
    ans = fromMaybe pos newPos
    in Target ans behave

bounce :: Int -> Behavior
bounce value pos game
    | value == 1 = let
        nextPos = fromMaybe pos (attemptMove (fst pos + 1, snd pos) game)
        in if nextPos == pos then
            if fromMaybe pos (attemptMove (fst pos - 1, snd pos) game) == pos && elem pos (gateways game) then
                Target (snd (head (filter (\x -> fst x == pos) (connections game)))) (bounce (-1))
                else getBounceMove (bounce (-1)) (fst pos - 1, snd pos) pos game
            else getBounceMove (bounce 1) (fst pos + 1, snd pos) pos game
    | otherwise = let
        nextPos = fromMaybe pos (attemptMove (fst pos - 1, snd pos) game)
        in if nextPos == pos then
            if fromMaybe pos (attemptMove (fst pos + 1, snd pos) game) == pos && elem pos (gateways game) then
                Target (snd (head (filter (\x -> fst x == pos) (connections game)))) (bounce 1)
                else getBounceMove (bounce 1) (fst pos + 1, snd pos) pos game
            else getBounceMove (bounce (-1)) (fst pos - 1, snd pos) pos game

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

moveTargets :: Game -> Game
moveTargets game = let
    obsta = obstacles game
    gate = gateways game
    hunt = hunter game
    conn = connections game
    r = rows game
    c = columns game
    tar = foldl (\acc x -> acc ++ [((behavior x) (position x) game)]) [] $ targets game
    tarPos = foldl (\acc x -> acc ++ [position x]) [] tar
    in Game tar tarPos obsta gate hunt conn r c

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled pos target
    | (fst pos + 1, snd pos) == (position target) || (fst pos, snd pos + 1) == (position target) ||
        (fst pos - 1, snd pos) == (position target) || (fst pos, snd pos - 1) == (position target) = True
    | otherwise = False
{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

moveToDirection :: Direction -> Position -> Game -> Position

moveToDirection dir pos game
    | dir == North = fromMaybe pos (attemptMove (fst pos - 1, snd pos) game)
    | dir == South = fromMaybe pos (attemptMove (fst pos + 1, snd pos) game)
    | dir == East = fromMaybe pos (attemptMove (fst pos, snd pos + 1) game)
    | dir == West = fromMaybe pos (attemptMove (fst pos, snd pos - 1) game)
    | otherwise = pos

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir move game = let
    hnt = moveToDirection dir (hunter game) game
    trg = filter (\x -> (isTargetKilled hnt x) == False) (targets game)
    tarPos = foldl (\acc x -> acc ++ [position x]) [] trg
    obsta = obstacles game
    gate = gateways game
    conn = connections game
    r = rows game
    c = columns game
    gm = Game (targets game) (targetsPos game) obsta gate hnt conn r c
    in if move == False
        then gm
        else let
            gm2 = Game trg tarPos obsta gate hnt conn r c
            newGame = moveTargets gm2
            trg2 = filter (\x -> (isTargetKilled hnt x) == False) (targets newGame)
            tarPos2 = foldl (\acc x -> acc ++ [position x]) [] trg2
            in Game trg2 tarPos2 obsta gate hnt conn r c

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = null (targets game)  

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined

getTargetFromPos :: Position -> Game -> Maybe Target
getTargetFromPos pos game
    | elem (fst pos - 1, snd pos) (targetsPos game) = Just $ head $ filter (\x -> position x == (fst pos - 1, snd pos)) (targets game)
    | elem (fst pos + 1, snd pos) (targetsPos game) = Just $ head $ filter (\x -> position x == (fst pos + 1, snd pos)) (targets game)
    | elem (fst pos, snd pos + 1) (targetsPos game) = Just $ head $ filter (\x -> position x == (fst pos, snd pos + 1)) (targets game)
    | elem (fst pos, snd pos - 1) (targetsPos game) = Just $ head $ filter (\x -> position x == (fst pos, snd pos - 1)) (targets game)
    | otherwise = Nothing

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North, advanceGameState North False game),
                        (South, advanceGameState South False game),
                        (West, advanceGameState West False game),
                        (East, advanceGameState East False game)]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game
        | elem (fst (hunter game) - 1, snd (hunter game)) (targetsPos game) = True
        | elem (fst (hunter game) + 1, snd (hunter game)) (targetsPos game) = True
        | elem (fst (hunter game), snd (hunter game) + 1) (targetsPos game) = True
        | elem (fst (hunter game), snd (hunter game) - 1) (targetsPos game) = True
        | otherwise = areTargetsLeft game

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game = let
        target = fromMaybe (Target (0, 0) goEast) (getTargetFromPos (hunter game) game)
        in if (position target) == (0, 0) then
            0.0
            else hEuclidean (position target) (hunter game)

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
