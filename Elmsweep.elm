module Elmsweep where

import Signal
import Time as T
import Time (Time)
import String
import List as L
import Set as S
import Set (Set)
import Random as R
import Html as H
import Html (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy as HL
import Array as A
import Array (Array)
import Trampoline as TR
import Trampoline (Trampoline)

main: Signal Html
main = Signal.map display
       (Signal.foldp update defaultGame
        (Signal.mergeMany [Signal.map InitGame initGame,
                           clicks (T.timestamp (Signal.subscribe commands)),
                           times]))

-- INPUTS

port initGame: Signal (Int, Int, Int, Int)

commands: Signal.Channel Command
commands = Signal.channel Restart

times: Signal Command
times = Signal.map Timer (T.every (1 * T.second))

click: (Time, Command) -> Command
click (t, cmd) = case cmd of
                   (Click _ cs) -> (Click (Just t) cs)
                   _ -> cmd

clicks: Signal (Time, Command) -> Signal Command
clicks s = Signal.map click s

-- MODEL

type Command
    = Click (Maybe Time) ClickCommand
    | Timer Time
    | Restart
    | InitGame (Int, Int, Int, Int)

type ClickCommand
    = Flag Int
    | Show Int


type Status
    = Play
    | Won
    | Lost

show: Int -> Command
show i = Click Nothing (Show i)

flag: Int -> Command
flag i = Click Nothing (Flag i)

type alias Cell =
    {flag: Bool, seen: Bool, safe: Bool, adj: Int}
type Board = Board Int Int (Array Cell)
type alias GameState = {timeStart: Time, timeReached: Time,
                        board: Board, seed: R.Seed, nmine: Int, status: Status}

makeBoard: Int -> Int -> List Bool -> Board
makeBoard w h mines =
    let rows = split w mines
        three n ls = L.drop (n - 1) (L.take (n + 2) ls)
        adjMines x y bo =
            L.length (fst (L.partition (identity)
                           (L.concat (three y (L.map (three x) bo)))))
        makeCell x y mine =
            Cell False False (not mine) (adjMines x y rows)
        bo = L.indexedMap (\y row ->
                           L.indexedMap (\x mine ->
                                         makeCell x y mine) row) rows
    in Board w h (A.fromList (L.concat bo))

makeNewGame: Int -> Int -> Int -> Int -> GameState
makeNewGame w h n seed =
    GameState 0 0 (makeNewBoard w h) (R.initialSeed seed) n Play

makeNewBoard: Int -> Int -> Board
makeNewBoard w h = makeBoard w h (L.repeat (w * h) True)

defaultGame: GameState
defaultGame = makeNewGame 0 0 0 0

randomMineIndices: Int -> R.Generator Int -> R.Seed -> (List Int, R.Seed)
randomMineIndices n gen seed =
    let go m seed0 acc =
        if | m == n -> (S.toList acc, seed0)
           | otherwise -> let (i, seed1) = R.generate gen seed0
                          in if | S.member i acc -> go m seed1 acc
                                | otherwise -> go (m + 1) seed1 (S.insert i acc)
    in go 0 seed S.empty

randomMines: Int -> Int -> Int -> R.Seed -> (R.Seed, List Bool)
randomMines w h n seed =
    let gen = R.int 0 (w * h - 1)
        (ns, seed0) = randomMineIndices n gen seed
        bs = L.indexedMap (\i b -> L.member i ns) (L.repeat (w * h) False)
    in (seed0, bs)

randomGame: GameState -> GameState
randomGame gs =
    let bo = gs.board
        w = width bo
        h = height bo
        (seed, mines) = randomMines w h gs.nmine gs.seed
    in {gs | board <- makeBoard w h mines
           , timeStart <- 0
           , timeReached <- 0
           , seed <- seed
           , status <- Play}

width: Board -> Int
width (Board w _ _) = w

height: Board -> Int
height (Board _ h _) = h

adjacent i (Board w h arr) =
    let x = rem i w
        y = i // w
        arrayIndex w (x, y) = x + y * w
        adj x y =
            [(x + 1, y), (x + 1, y + 1), (x, y + 1), (x - 1, y + 1), (x - 1, y),
             (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
    in L.map (arrayIndex w)
           (L.filter (\(x0, y0) -> x0 >= 0 && x0 < w && y0 >= 0 && y0 < h)
           (adj x y))

gameDone: GameState -> Bool
gameDone gs = gs.status /= Play

gameWon: GameState -> Bool
gameWon gs = gs.status == Won

gameLost: GameState -> Bool
gameLost gs = gs.status == Lost

status: Board -> Status
status (Board _ _ arr) =
    let won =
            A.foldl (\c r -> r && ((c.safe && (not c.flag)) ||
                                   ((not c.safe) && c.flag))) True arr
        lost = A.foldl (\c r -> r || ((not c.safe) && c.seen)) False arr
    in if | won -> Won
          | lost -> Lost
          | otherwise -> Play

-- DISPLAY

display: GameState -> Html
display gs =
    let (Board w h arr) = gs.board
        won = gameWon gs
        lost = gameLost gs
        done = won || lost
        flagsUsed bo = A.length (A.filter (\c -> c.flag) bo)
        flag = toString (if | gs.timeReached == 0 -> gs.nmine
                            | otherwise -> gs.nmine - flagsUsed arr)
        time = toString (round (T.inSeconds (gs.timeReached - gs.timeStart)))
        rows = split w (A.toIndexedList arr)
    in if gs.nmine == 0 then H.text "" else
      H.div [HA.class "esw-table"]
           [H.div [HA.class "esw-status"]
                [screen time, statusScreen (statusString won lost), screen flag],
            H.div [HA.class "esw-rows"]
                (L.map
                  -- Lazy updating of rows.
                      (\row -> HL.lazy
                       (\row0 -> H.div [HA.class "esw-row esw-noselect"]
                        (L.map
                         (\(i, c) -> button i done c) row0)) row) rows)]

screen: String -> Html
screen str =
    H.div [HA.class "esw-screen"] [H.span [HA.class "esw-noselect"] [H.text str]]

statusScreen: String -> Html
statusScreen str =
    H.div [HA.class "esw-status",
         HE.onClick (Signal.send commands Restart)]
        [H.div [HA.class "esw-rotate-90"]
             [H.span [HA.class "esw-noselect"] [H.text str]]]

button: Int -> Bool -> Cell -> Html
button i done c =
    let str = fieldString done c
        down = str /= ""
    in H.div [HA.class (if down then "esw-cell esw-cell-down"
                  else "esw-cell esw-cell-up"),
            HE.onClick (Signal.send commands (show i)),
            HE.messageOn "contextmenu" (Signal.send commands (flag i))]
           [H.span [fieldClass str]
                 [H.text (if str == "0" || str == "B" || str == "W" ||
                             str == "F" || str == "X" then "" else str)]]

statusString: Bool -> Bool -> String
statusString won lost = if | won -> ":D"
                           | lost -> ":("
                           | otherwise -> ":)"

fieldString: Bool -> Cell -> String
fieldString done c =
    case (c.seen, c.safe, c.flag, done, c.adj) of
      (True,  True,  _,     _,    adj) -> toString adj
      (True,  False, _,     _,     _) -> "X"
      (False, True,  True,  True,  _) -> "W"
      (False, True,  True,  False, _) -> "F"
      (False, True,  False, _,     _) -> ""
      (False, False, True,  _,     _) -> "F"
      (False, False, False, True,  _) -> "B"
      (False, False, False, False, _) -> ""
      _                               -> "U"

fieldClass: String -> H.Attribute
fieldClass str = if | str == "B" -> HA.class "esw-noselect esw-bomb"
                    | str == "F" -> HA.class "esw-noselect esw-flag"
                    | str == "W" -> HA.class "esw-noselect esw-wrong"
                    | str == "X" -> HA.class "esw-noselect esw-explode"
                    | otherwise -> HA.class "esw-noselect"

-- UPDATE

update: Command -> GameState -> GameState
update cmd gs =
    if gameDone gs then
        case cmd of
          Restart -> randomGame gs
          InitGame (w, h, n, seed) -> randomGame (makeNewGame w h n seed)
          _ -> gs
    else case cmd of
           Click (Just t) (Show i) ->
               let gs0 = if gs.timeStart == 0 then
                             {gs | timeReached <- t
                                 , timeStart <- t}
                         else gs
                   bo0 = updateClick (Show i) i gs0.board
               in {gs0 | board <- bo0
                       , status <- status bo0}
           Click t (Flag i) -> let bo0 = updateClick (Flag i) i gs.board
                               in {gs | board <- bo0
                                      , status <- status bo0}
           Restart -> randomGame gs
           InitGame (w, h, n, seed) -> randomGame (makeNewGame w h n seed)
           Timer t -> if gs.timeStart == 0 then gs
                      else {gs | timeReached <- t}
           _ -> gs

updateClick: ClickCommand -> Int -> Board -> Board
updateClick cmd i bo =
    case cmd of
      Show _ -> updateShow i bo
      Flag _ -> updateCell i
                (\c -> {c | flag <- not c.flag && not c.seen}) bo

updateShowCascadeCell: Int -> Board -> (Board, List Int)
updateShowCascadeCell i bo =
    case getCell i bo of
      Just c -> if not c.seen && not c.flag then
                    if c.adj == 0 then
                        (showCell i bo, adjacent i bo)
                    else (showCell i bo, [])
                else  (bo, [])
      Nothing -> (bo, [])

updateShowCascade: List Int -> Set Int -> Board -> Trampoline Board
updateShowCascade acc set bo =
     case acc of
       i::cc -> let (bo0, acc0) = updateShowCascadeCell i bo
                    acx = L.filter (\i0 -> S.member i0 set) acc0
                    (bo1, acc1) =
                        L.foldl (\i0 (bo2, acc2) ->
                                 case updateShowCascadeCell i0 bo2 of
                                   (bo3, acc3) -> (bo3, L.append acc3 acc2))
                                 (bo0, []) acx
                    accn = L.append acc0 acc1
                    new = S.diff (S.fromList accn) set
                in TR.Continue (\() -> updateShowCascade
                                               (L.append cc (S.toList new))
                                               (S.union new set) bo1)
       [] -> TR.Done bo

updateShow: Int -> Board -> Board
updateShow i bo =
    let (ok, n) = case getCell i bo of
                    Just c -> (not c.seen && not c.flag, c.adj)
                    Nothing -> (False, 0)
    in if ok then
           if n == 0 then
               TR.trampoline (updateShowCascade (adjacent i bo) S.empty (showCell i bo))
           else showCell i bo
       else bo

updateCell: Int -> (Cell -> Cell) -> Board -> Board
updateCell i f bo =
    case bo of
         Board w h arr ->
             case A.get i arr of
               Just c -> Board w h (A.set i (f c) arr)
               Nothing -> bo

getCell: Int -> Board -> Maybe Cell
getCell i (Board w h arr) = A.get i arr

showCell: Int -> Board -> Board
showCell i bo =
    updateCell i (\c -> if not c.flag then {c | seen <- True} else c) bo

-- UTIL

split: Int -> List a -> List (List a)
split n lls = case lls of (l::ls) -> L.take n lls :: (split n (L.drop n lls))
                          [] -> []