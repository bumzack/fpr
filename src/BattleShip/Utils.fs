module Utils

open Domain

let convertToDirection (d: string) =
    match d.ToUpper().[0] with
    | 'N' -> Some(Direction.North)
    | 'E' -> Some(Direction.East)
    | 'S' -> Some(Direction.South)
    | 'W' -> Some(Direction.West)
    | _ -> None


let mapToCoord (x, y) =
    { X = x
      Y = y }

let mapToShip (c, l, d) =
    { pos = c
      length = l
      direction = d }


// https://stackoverflow.com/questions/42820232/f-convert-a-char-to-int
let charToInt c = int c - int 'A'

let intToChar i =
    let character = [ 'A' .. 'Z' ]
    character.[i]


let createCoordList (s: Ship) =
    let tmp = [ 0 .. s.length - 1 ]

    let fields =
        match s.direction with
        | North ->
            let tmp = tmp |> List.map (fun y -> (s.pos.Y + y))
            let min = tmp |> List.min
            if min < 0 then
                []
            else
                let fields = tmp |> List.map (fun y -> mapToCoord (s.pos.X, y))
                fields
        | South ->
            let tmp = tmp |> List.map (fun y -> (s.pos.Y - y))
            let fields = tmp |> List.map (fun y -> mapToCoord (s.pos.X, y))
            fields
        | East ->
            let idx = charToInt s.pos.X
            let tmp = tmp |> List.map (fun x -> (idx + x))
            let fields = tmp |> List.map (fun y -> mapToCoord (intToChar y, s.pos.Y))
            fields
        | West ->
            let idx = charToInt s.pos.X
            let tmp = tmp |> List.map (fun x -> (idx - x))

            let min = tmp |> List.min
            if min < 0 then
                []
            else
                let fields = tmp |> List.map (fun y -> mapToCoord (intToChar y, s.pos.Y))
                fields

    fields
