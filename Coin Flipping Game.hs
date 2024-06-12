
gcd1 :: Int -> Int -> Int  -- Kiszámítja két szám legnagyobb közös osztóját 
gcd1 a b
    | b == 0 = abs a  --abs függvény abszolút értéket számít ki egy számról
    | otherwise = gcd1 b (a `mod` b) -- függvény a maradékot számítja ki két egész szám osztásakor


hasSolution :: Int -> Int -> Bool
hasSolution c f
  | c <= 0 || f <= 0 = False -- Mindkét szám pozitívnak kell lennie
  | f > c = False -- f nem lehet nagyobb, mint c
  | even f = False -- f páratlan kell legyen
  | gcd1 c f == 1 = True -- c és f relatív prímek
  | otherwise = False  --False

initialState :: Int -> Int -> ([Bool], Int)  ---- Kezdeti állapot létrehozása
initialState c f = (take c (repeat False), f)  --repeat: Az repeat függvény egy végtelen listát hoz létre, amely ugyanazt az elemet tartalmazza ismételve.
                                                --take: A take függvény egy meghatározott számú elemet vesz egy listából. 
                                                --A bemeneti lista elemeinek az első n darabját tartja meg, a többit elhagyja.


type Coins = [Bool]
type State = (Coins, Int)
type History = [State]

goalState :: State -> Bool  -- Célállapot ellenőrzése
goalState (coins, _) = all (== True) coins  -- Az all függvény egy logikai feltételt értékel ki minden elemre egy listában, és igaz értéket ad vissza,
                                            -- ha a feltétel minden elemre igaz.
                                            --Tehát a goalState (coins, _) = all (== True) coins kifejezés azt jelenti, 
                                            --hogy a goalState függvény a coins listában ellenőrzi, hogy minden érme igaz-e (a True-val egyenlő). Ha minden érme igaz, 
                                            --akkor a goalState függvény True értéket ad vissza, jelezve, hogy elérte a célt (az összes érme az igaz állapotban van). Ellenkező esetben, 
                                            --ha bármelyik érme hamis, a goalState függvény False értéket ad vissza, jelezve, hogy még nem érte el a célt.

type Flips = [Bool]

isLegalFlip :: State -> Flips -> Bool 
isLegalFlip (coins, f) flips =   
  length coins == length flips && countTrue flips == f 
  where
    countTrue :: [Bool] -> Int
    countTrue = length . filter (== True)

applyFlips :: [Bool] -> Flips -> [Bool] 
applyFlips coins flips = zipWith (\coin flip -> flip /= coin) coins flips

flipCoins :: State -> Flips -> State  
flipCoins (coins, f) flips
  | isLegalFlip (coins, f) flips = (applyFlips coins flips, f)
  | otherwise = error "Invalid flips!"    

generateFlips :: Int -> Int -> [Flips]                       
generateFlips c f
   | f == 0 = [take c (repeat False)] -- Flip sorozat generálása, ha f értéke 0
   | f == c = [take f (repeat True)]   -- Flip sorozat generálása, ha f értéke c
   | otherwise = [True : flip | flip <- generateFlips (c - 1) (f - 1)] ++ [False : flip | flip <- generateFlips (c - 1) f]

nextStates :: State -> [State]
nextStates (coins, f) =
  let flips = generateFlips (length coins) f
      legalFlips = filter (isLegalFlip (coins, f)) flips
  in map (\flip -> flipCoins (coins, f) flip) legalFlips -- Lehetséges következő állapotok generálása

solution :: [History] -> History
solution [] = []  -- Üres előzmények listája esetén nincs megoldás
solution (h:hs)
  | goalState (head h) = h  -- Az első előzmény első állapota célállapot
  | otherwise = solution (hs ++ map (:h) (nextStates (head h))) -- Rekurzív megoldás generálása

solve :: Int -> Int -> [Coins]
solve c f
  | not (hasSolution c f) = error "No solution"
  | otherwise = map fst (solution [[initialState c f]])



--Meghivasok:
--gcd 12 9 == 3
--hasSolution 5 3 == True
--initialState 5 3 == ([False,False,False,False,False],3)
--goalState ([True, True, True, True, True], 3) == True
--isLegalFlip ([False, True, True, True, True], 3) [False, True, False, True, True] == True
--flipCoins ([False, False, False], 1)  [True, False, False] == ([True,False,False],1)
--generateFlips 3 1 == [[True,False,False],[False,True,False],[False,False,True]]
--nextStates ([False, False, False], 1) == [([True,False,False],1),([False,True,False],1),([False,False,True],1)]
--solution [[initialState 3 1]] ==  [([True,True,True],1),([True,True,False],1),([True,False,False],1),([False,False,False],1)] 
--solve 3 1 == [[True,True,True],[True,True,False],[True,False,False],[False,False,False]]







