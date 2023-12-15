
module Stairs = 
    type Move = 
        | Down
        | Up
    type Stair = int
    type Index = int
    let takeStep (current:Stair) dir =
        match dir with  
            | Up -> current + 1
            | Down -> current - 1
    let betweenConstraint (min: Stair) (max: Stair) takeStep = 
        let f (current: Stair) (dir:Move): Stair = 
            let a:Stair = takeStep current dir
            if a < min then
                min
            elif a > max then
                max
            else
                a
        f
    let getIncorrectMoveIndexes startWith moves :Index list =
        moves
        |> List.scan takeStep startWith
        |> List.mapi (fun i v -> i, v)
        |> List.filter (fun (i, v) -> v = -1)
        |> List.map fst
    let getVisitedStairs  (startWith:Stair) withConstraints (moves: Move list) = 
        moves
        |> List.scan (takeStep |> withConstraints ) startWith
    let getValidMoves  (startWith:Stair) withConstraints (moves: Move list) = 
        moves
        |> getVisitedStairs startWith withConstraints
        |> List.distinct 
open Stairs
let charToMove = function
    |'+' -> [Up]
    |'-' -> [Down]
    |_ -> []
let stringToMoves input = 
    input
    |> List.ofSeq
    |> List.collect charToMove
let input = "-+-"
let moves = 
    input
    |> stringToMoves
let incorrectMoveIndexes = 
    moves
    |> getIncorrectMoveIndexes 0
let visitedStairs = 
    moves
    |> getVisitedStairs 0 (betweenConstraint 0 100)
