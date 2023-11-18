import Game
import JavaScript
import Data.Char

-- Task 1
---- a)
emptyMap :: Int -> Room ()
emptyMap n = replicate n (replicate n (Left None))

---- b)
initGameConfig :: GameConfig ()
initGameConfig = GameConfig {
    rooms       = [("start", emptyMap 10)],
    actions     = (\x -> Null),
    actionItems = []
}

---- c)
q1 :: IO ()
q1 = outputConfig initGameConfig (\x -> "")

-- Task 2
---- a)
mapBuilder :: (Char -> Either MapItem a) -> [String] -> Room a
mapBuilder _ [] = []
mapBuilder func (row : rest) = (map func row) : (mapBuilder func rest) 
{-
The following would generate a 3 × 3 empty room with a tree in the middle:
    example = mapBuilder (\x -> case x of 'T' -> Left TreeA; _ -> Left None)
        ["---"
        ,"-T-"
        ,"---"]
-}

---- b)
charToMapItem :: Char -> Either MapItem a
charToMapItem char = Left (case char of
        'H' -> Home
        'A' -> TreeA
        'B' -> TreeB
        '$' -> doorNorth
        '%' -> doorSouth
        '&' -> doorEast
        '*' -> doorWest
        x  -> if ((isDigit x) && (((digitToInt x) >= 0) && ((digitToInt x) <= 4)))
            then Rock (toInteger (digitToInt x))
            else None)

rock0 = Left (Rock 0)
rock1 = Left (Rock 1)
rock2 = Left (Rock 2)
rock3 = Left (Rock 3)
rock4 = Left (Rock 4)

doorNorth = Door {target = "north", requirements = [Orange]}
doorSouth = Door {target = "start", requirements = [Orange]}
doorEast  = Door {target = "east", requirements = [Blue, Yellow]}
doorWest  = Door {target = "north", requirements = [Blue, Yellow]}

q2StringMap :: [String]
q2StringMap = ["H-01-A"
              ,"-A-B--"
              ,"2--313"
              ,"-AA--2"
              ,"-3--1-"
              ,"4-B--A"]

q2Example :: Room a
q2Example = mapBuilder charToMapItem q2StringMap

---- c)
------ i)
expand :: a -> [a] -> [a]
expand _ [] = []
expand element (head : tail) =
    (foldr (:) [element] [head]) ++ (expand element tail)
{-
e.g.
    expand '-' "HDT" = "H-D-T-"
-}

------ ii)
dilate :: Room a -> Room a
dilate [] = []
dilate (row : rest) = 
    [expand (Left None) row] ++ [replicate (2 * (length row)) (Left None)] ++ dilate rest

---- d)
dialatedQ2Example :: Room a
dialatedQ2Example = dilate q2Example

q2GameConfig :: GameConfig ()
q2GameConfig = GameConfig {
    rooms       = [("start", dialatedQ2Example)],
    actions     = (\x -> Null),
    actionItems = []
}

q2 :: IO ()
q2 = outputConfig q2GameConfig (\x -> "")

-- Task 3
---- a)
data ActionItem =
      Key {keyColor :: KeyColor}
    | Snack {snackType :: SnackType} -- Three snack types: Candies, Apple, and Berries (Task 5)
    | Coin {coinColor :: CoinColor}
    deriving Show

data CoinColor = Copper | Gold
  deriving Show

data SnackType = Candies | Apple | Berries -- Added two more sources of energy (Task 5)
  deriving Show

---- b)
actionItemName :: ActionItem -> String
actionItemName (Key color) = case color of
    Orange -> "keyorange"
    Yellow -> "keyyellow"
    Blue   -> "keyblue"
actionItemName (Snack snackType) = case snackType of
    Candies -> "food"
    Apple   -> "apple"   -- Added by me (Task 5)
    Berries -> "berries" -- Added by me (Task 5)
actionItemName (Coin color) = case color of
    Copper -> "coincopper"
    Gold   -> "coingold"

---- c)
charToItem :: Char -> Either MapItem ActionItem
charToItem char = case char of
    'O' -> Right Key {keyColor = Orange}
    'Y' -> Right Key {keyColor = Yellow}
    'U' -> Right Key {keyColor = Blue}
    'S' -> candiesR
    'R' -> berriesR -- Added by me (Task 5)
    'P' -> appleR   -- Added by me (Task 5)
    'C' -> coinCopperR
    'G' -> coinGoldR
    itm -> charToMapItem itm

candies  = Snack {snackType = Candies}
candiesR = Right candies
apple    = Snack {snackType = Apple}
appleR   = Right apple
berries  = Snack {snackType = Berries}
berriesR = Right berries

coinGold    = Coin {coinColor = Gold}
coinCopper  = Coin {coinColor = Copper}
coinGoldR   = Right coinGold
coinCopperR = Right coinCopper


---- d)
jsId :: JSExpr
jsId = Function ["input"] (Return (Var "input"))

---- e)
updatePlayerData :: [(String, JSExpr -> JSExpr)] -> JSExpr
updatePlayerData [(element, func)] = case element of
    "energy"    -> Function ["pdata"] (Return (Object [(change func "energy"), (dontChange "score"), (dontChange "inventory")]))
    "score"     -> Function ["pdata"] (Return (Object [(dontChange "energy"), (change func "score"), (dontChange "inventory")]))
    "inventory" -> Function ["pdata"] (Return (Object [(dontChange "energy"), (dontChange "score"), (change func "inventory")]))
    _           -> Function ["pdata"] (Return (Object [(dontChange "energy"), (dontChange "score"), (dontChange "inventory")]))
{-
For example:
    updatePlayerData [("score", \x -> BinOp "+" x (Num 1))]

 produce an AST value equivalent to the JavaScript code:
    function (pdata) {
        return {energy: pdata.energy, score: pdata.score + 1, inventory: pdata.inventory}
    }
-}

change :: (JSExpr -> JSExpr) -> String -> (Id, JSExpr)
change func attribute = (attribute, (func (Property "pdata" attribute)))

dontChange :: String -> (Id, JSExpr)
dontChange attribute = (attribute, (Property "pdata" attribute))

---- f)
myActions :: ActionItem -> JSExpr
myActions item = case item of
    Coin Gold     -> updatePlayerData [("score", increment 2)]
    Coin Copper   -> updatePlayerData [("score", increment 1)]
    Snack Candies -> updatePlayerData [("energy", increment 10)]
    Snack Apple   -> updatePlayerData [("energy", increment 5)] -- An Apple restores half the energy Candies do (Task 5)
    Snack Berries -> updatePlayerData [("energy", increment 3)] -- Berries restore only 3 energy (Task 5)
    _             -> updatePlayerData [("inventory", \x -> Null)]

increment :: Integer -> (JSExpr -> JSExpr)
increment i = \x -> BinOp "+" x (Num i)

---- g)
q3 :: IO ()
q3 = outputConfig q3GameConfig actionItemName

mapBuilder' :: (Char -> Either MapItem ActionItem) -> [String] -> Room ActionItem
mapBuilder' _ [] = []
mapBuilder' func (row : restOfMap) = (map func row) : (mapBuilder func restOfMap) 

q3StringMap :: [String]
q3StringMap = ["HS01GA"
              ,"CAUBSC"
              ,"2G-313"
              ,"-AA-S2"
              ,"S3G-1-"
              ,"4GBSCA"]
              
q3Example :: Room ActionItem
q3Example = mapBuilder' charToItem q3StringMap

dialatedQ3Example :: Room ActionItem
dialatedQ3Example = dilate q3Example

allActionItems :: [ActionItem]
allActionItems = [Coin Gold, Coin Copper, Snack Candies, Snack Apple, Snack Berries, Key Orange, Key Yellow, Key Blue]

q3GameConfig :: GameConfig ActionItem
q3GameConfig = GameConfig {
    rooms       = [("start", dialatedQ3Example)],
    actions     = myActions,
    actionItems = allActionItems
}

-- Task 4
---- a)
insertAt :: Either MapItem item -> (Int, Int) -> Room item -> Room item
insertAt item (x, y) (currentRow : rest) = if x == 0
    then (insertAtColumn item y currentRow) : rest
    else currentRow : (insertAt item ((x - 1), y) rest)
{-
e.g.
    insertAt (Right Food) (1, 0) [[Left None, Left None], [Left None, Left None]]
    = [[Left None,Left None],[Right Food,Left None]] 
-}

insertAtColumn :: Either MapItem item -> Int -> [Either MapItem item] -> [Either MapItem item]
insertAtColumn item y (currentCol : rest) = if y == 0
    then item : rest
    else currentCol : (insertAtColumn item (y - 1) rest)

insertMultipleAt :: [Either MapItem item] -> [(Int, Int)] -> Room item -> Room item
insertMultipleAt (item : restItems) (coords : restCoords) room =
    if (length restItems) == 0
        then insertAt item coords room
        else insertMultipleAt restItems restCoords (insertAt item coords room)

---- b)
firstStringMap :: [String]
firstStringMap = ["R$01GG"
                 ,"CAUBRC"
                 ,"2G--32"
                 ,"RAA-S-"
                 ,"S3G-1O"
                 ,"0GBPCA"]

firstRoom :: Room ActionItem
firstRoom = insertMultipleAt 
    ([coinGoldR, rock1, rock0] ++ (replicate 2 berriesR) ++ (replicate 3 rock4) ++ (replicate 3 rock3) ++ (replicate 3 (Left TreeA)))
    [(1, 11), (4, 7), (5, 9), (0, 11), (5, 1), (3, 8), (3, 9), (5, 11), (0, 5), (4, 9), (5, 10), (5, 3), (5, 4), (6, 3)]
    (dilate (mapBuilder' charToItem firstStringMap))

secondStringMap :: [String]
secondStringMap =  ["0RCCCA"
                   ,"C10SGG"
                   ,"GG-B1G"
                   ,"-AA-SY"
                   ,"0-B-A-"
                   ,"-%-03-"]

secondRoom :: Room ActionItem
secondRoom = insertMultipleAt 
    ([(Left doorEast), (Left doorSouth), berriesR] ++ (replicate 2 appleR) ++ (replicate 2 candiesR) ++ (replicate 2 (Left TreeB)) ++ (replicate 3 (Left TreeA)) ++ (replicate 3 rock1) ++ (replicate 2 rock2))
    [(8, 11), (11, 2), (7, 10) ,(10, 1), (11, 0), (10, 0), (10, 3), (6, 3), (7, 3), (7, 4), (7, 5), (0, 11), (2, 3), (10, 5), (10, 7), (1, 3), (4, 9)] 
    (dilate (mapBuilder' charToItem secondStringMap))

thirdStringMap :: [String]
thirdStringMap = ["GGRAAH"
                 ,"CBBG0C"
                 ,"2C-3-3"
                 ,"-AA-P2"
                 ,"*-1-4-"
                 ,"S430CS"]

thirdRoom :: Room ActionItem
thirdRoom = insertMultipleAt
    ([rock1] ++ (replicate 3 rock0) ++ (replicate 3 (Left TreeA)) ++ (replicate 2 (Left TreeB)) ++ (replicate 3 coinCopperR) ++ (replicate 3 berriesR))
    [(10, 3), (10, 5), (5, 10), (5, 11), (0, 7), (5, 3), (6, 3), (1, 4), (2, 3), (3, 1), (6, 11), (11, 11), (2, 7), (10, 1), (11, 5)]    
    (dilate (mapBuilder' charToItem thirdStringMap))

---- c)
myActions' :: ActionItem -> JSExpr
myActions' item = case item of
    Key Orange  -> updatePlayerData [("inventory", collect "Orange")]
    Key Yellow  -> updatePlayerData [("inventory", collect "Yellow")]
    Key Blue    -> updatePlayerData [("inventory", collect "Blue")]
    _           -> myActions item

collect :: String -> (JSExpr -> JSExpr)
collect keyColor = \x -> Apply (Var "cons") [(JString keyColor), x]

---- d)
q4GameConfig :: GameConfig ActionItem
q4GameConfig = GameConfig {
    rooms       = [("start", firstRoom), ("north", secondRoom), ("east", thirdRoom)],
    actions     = myActions',
    actionItems = allActionItems
}

q4 :: IO ()
q4 = outputConfig q4GameConfig actionItemName

-- Task 5
q5 :: IO ()
q5 = q4

---- 1)
------ I added apples to the game. They act similarly to
------ freddos, but restore only 5 evergy (instead of 10).
------ I had to edit nk465.hs and style.css

---- 2)
------ I added blueberries to the game. They act similarly to apples
------ and freddos, but restore only 5 evergy (instead of 10).
------ I had to edit the nk465.hs and style.css

---- 2)
------ I added instructions under the game board.
------ I had to edit game.html and style.css

---- 3)
------ I allowed players to use awsd/AWSD for movement.
------ I had to edit the game.js

---- 4)
------ I made treeA visualize with a trunk in the game.
------ I had to edit (resize) the treeA.png