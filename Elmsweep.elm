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
import Maybe as M

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
    | ShowAdjacent Int

type Status
    = Play
    | Won
    | Lost

flag: Int -> Command
flag i = Click Nothing (Flag i)

show: Int -> Command
show i = Click Nothing (Show i)

showAdjacent: Int -> Command
showAdjacent i = Click Nothing (ShowAdjacent i)

type alias Cell = {flag: Bool, seen: Bool, safe: Bool, adj: Int}
type Board = Board Int Int (Array Cell)
type alias GameState = {timeStart: Time, timeReached: Time,
                        board: Board, seed: R.Seed, nmine: Int, status: Status}
type alias Pos = Int

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

adjacent: Pos -> Board -> List Pos
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
            A.foldl (\c r -> r && ((c.safe && c.seen) ||
                                   ((not c.safe) && (not c.seen)))) True arr
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
                         (\(i, c) -> button i done won c) row0)) row) rows)]

screen: String -> Html
screen str =
    H.div [HA.class "esw-screen"] [H.span [HA.class "esw-noselect"] [H.text str]]

statusScreen: String -> Html
statusScreen str =
    H.div [HA.class "esw-status",
         HE.onClick (Signal.send commands Restart)]
        [H.div [HA.class "esw-rotate-90"]
             [H.span [HA.class "esw-noselect"] [H.text str]]]

button: Pos -> Bool -> Bool -> Cell -> Html
button i done won c =
    let str = fieldString done won c
        down = str /= ""
    in H.div [HA.class (if down then "esw-cell esw-cell-down"
                  else "esw-cell esw-cell-up"),
            HE.onClick (Signal.send commands (show i)),
            HE.messageOn "dblclick" (Signal.send commands (showAdjacent i)),
            HE.messageOn "contextmenu" (Signal.send commands (flag i))]
           [H.span [fieldClass str]
                 [H.text (if str == "0" || str == "B" || str == "W" ||
                             str == "F" || str == "X" then "" else str)]]

statusString: Bool -> Bool -> String
statusString won lost = if | won -> ":D"
                           | lost -> ":("
                           | otherwise -> ":)"

fieldString: Bool -> Bool -> Cell -> String
fieldString done won c =
    case (c.seen, c.safe, c.flag, done, c.adj) of
      (True,  True,  _,     _,    adj) -> toString adj
      (True,  False, _,     _,     _) -> "X"
      (False, True,  True,  True,  _) -> "W"
      (False, True,  True,  False, _) -> "F"
      (False, True,  False, _,     _) -> ""
      (False, False, True,  _,     _) -> "F"
      (False, False, False, True,  _) -> if won then "F" else "B"
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
           Click t (ShowAdjacent i) ->
               if canShowAdjacent i gs.board then
                   let bo0 = updateClick (ShowAdjacent i) i gs.board
                   in {gs | board <- bo0
                          , status <- status bo0}
               else gs
           Restart -> randomGame gs
           InitGame (w, h, n, seed) -> randomGame (makeNewGame w h n seed)
           Timer t -> if gs.timeStart == 0 then gs
                      else {gs | timeReached <- t}
           _ -> gs

canShowAdjacent: Pos -> Board -> Bool
canShowAdjacent p bo =
    case getCell p bo of
      Just c -> L.length (L.filter (\(p, c0) -> c0.flag) (adjacentCells p bo))
                == c.adj
      Nothing -> False

updateClick: ClickCommand -> Pos -> Board -> Board
updateClick cmd p bo =
    case cmd of
      Show _ -> updateShow [p] bo
      Flag _ -> updateCell p
                (\c -> {c | flag <- not c.flag && not c.seen}) bo
      ShowAdjacent _ -> updateShow (L.filter
                                    (\p -> case getCell p bo of
                                             Just c -> not (c.seen || c.flag)
                                             _ -> False) (adjacent p bo)) bo

updateShowCascadeCell: Pos -> Board -> (Board, List Pos)
updateShowCascadeCell p bo =
    case getCell p bo of
      Just c -> if not c.seen && not c.flag then
                    if c.adj == 0 then
                        (showCell p bo, adjacent p bo)
                    else (showCell p bo, [])
                else  (bo, [])
      Nothing -> (bo, [])

updateShowCascade: List Pos -> Set Pos -> Board -> Trampoline Board
updateShowCascade acc set bo =
     case acc of
       p::cc -> let (bo0, acc0) = updateShowCascadeCell p bo
                    acx = L.filter (\p0 -> S.member p0 set) acc0
                    (bo1, acc1) =
                        L.foldl (\p0 (bo2, acc2) ->
                                 case updateShowCascadeCell p0 bo2 of
                                   (bo3, acc3) -> (bo3, L.append acc3 acc2))
                                 (bo0, []) acx
                    accn = L.append acc0 acc1
                    new = S.diff (S.fromList accn) set
                in TR.Continue (\() -> updateShowCascade
                                               (L.append cc (S.toList new))
                                               (S.union new set) bo1)
       [] -> TR.Done bo

updateShow: List Pos -> Board -> Board
updateShow ps bo =
    let cs = L.filter (\(p, c) -> not c.seen && not c.flag)
             (L.filterMap (\p -> M.map ((,) p) (getCell p bo)) ps)
        adj = L.concat (L.map (\(p, c) -> if c.adj == 0
                                          then adjacent p bo else []) cs)
        bo0 = L.foldl (\(p, _) bo1 -> showCell p bo1) bo cs
        done = L.any (\(p, c) -> not c.safe) cs
    in if done then bo0
       else TR.trampoline (updateShowCascade (S.toList (S.fromList adj))
                                             S.empty bo0)

adjacentCells: Pos -> Board -> List (Pos, Cell)
adjacentCells p bo = L.filterMap (\p0 -> M.map ((,) p0) (getCell p0 bo))
                     (adjacent p bo)

updateCell: Pos -> (Cell -> Cell) -> Board -> Board
updateCell p f bo =
    case bo of
         Board w h arr ->
             case A.get p arr of
               Just c -> Board w h (A.set p (f c) arr)
               Nothing -> bo

getCell: Pos -> Board -> Maybe Cell
getCell p (Board w h arr) = A.get p arr

showCell: Pos -> Board -> Board
showCell p bo =
    updateCell p (\c -> if not c.flag then {c | seen <- True} else c) bo

-- UTIL

split: Int -> List a -> List (List a)
split n lls = case lls of (l::ls) -> L.take n lls :: (split n (L.drop n lls))
                          [] -> []
