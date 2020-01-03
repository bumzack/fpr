module ConsoleHelper

open Domain

// convert a Field to a string
let toString f: string =
    match f with
    | NotAttempted -> " "
    | Attempted fs ->
        match fs with
        | Water -> "w"
        | Hit -> "h"


// http://www.fssnip.net/5u/title/String-explode-and-implode
let implode (xs: string list) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    sb.ToString()

let horizontallLine (size: int) =
    let cnt = (size + 2) * 3

    let line =
        [ for a in 1 .. cnt do
            yield "-" ]
    implode line


// https://stackoverflow.com/questions/42820232/f-convert-a-char-to-int
// A == 1
// B == 2 etc
let inline charToInt c = int c - int 'A'

// TODO: man that's ugly
let drawShip (board: Board) =
    let size_squared = board.Size * board.Size
    let fields = [ 0 .. size_squared-1 ]

    let mapCoordToIndex (sp: ShipPoint) =
        let x = (charToInt sp.Coord.X)
        let idx = (sp.Coord.Y-1) * board.Size + x
        idx

    let shipsPoints =
            match board.Ships.Length with
            | 0 -> []
            | 1 -> board.Ships.[0].Points
            | 2 -> List.append board.Ships.[0].Points  board.Ships.[1].Points
            | _ -> failwith "too many ships"

    if shipsPoints.Length > 0 then

        let shipIndizes = List.map mapCoordToIndex shipsPoints
        let shipIndizes = List.sort shipIndizes

        let mapShips idx =
            if List.contains idx shipIndizes then 's'
            else ' '

        let fieldsAsChars = List.map mapShips fields

        // good old for loop and string concatenation    ¯\_(ツ)_/¯

        let mutable lines = List.empty
        lines <- List.append lines [ horizontallLine (board.Size) ]
        for y = 0 to board.Size - 1 do
            let idx = y * board.Size
            let mutable l = []

            for x = 0 to board.Size - 1 do
                // why fields.[]   and not fields[]  it's a list, not an array?
                // TODO: that's the only difference to drawBoards for a fields list
                let a = fieldsAsChars.[idx + x].ToString()
                let b = " | " + a
                // append  list to list ¯\_(ツ)_/¯
                l <- List.append l [ b ]
            //
            l <- List.append l [ " | " ]
            let l1 = implode l
            lines <- List.append lines [ l1 ]
            lines <- List.append lines [ horizontallLine (board.Size) ]

        lines |> List.iter (fun x -> printfn "%s " x)


// TODO: man that's ugly
let drawBoard (board: Board) =
    // good old for loop and string concatenation    ¯\_(ツ)_/¯
    let mutable lines = List.empty
    lines <- List.append lines [ horizontallLine (board.Size) ]
    for y = 0 to board.Size - 1 do
        let idx = y * board.Size
        let mutable l = []

        let fields = board.Fields

        for x = 0 to board.Size - 1 do
            // why fields.[]   and not fields[]  it's a list, not an array?
            let a = toString (fields.[idx + x])
            let b = " | " + a
            // append  list to list ¯\_(ツ)_/¯
            l <- List.append l [ b ]
        //
        l <- List.append l [ " | " ]
        let l1 = implode l
        lines <- List.append lines [ l1 ]
        lines <- List.append lines [ horizontallLine (board.Size) ]

    // print to console
    // https://stackoverflow.com/questions/2519458/f-how-to-print-full-list-console-writeline-prints-only-first-three-elements
    //https://stackoverflow.com/questions/19469252/convert-integer-list-to-a-string
    // printf("lines =   %A") lines.ToString
    lines |> List.iter (fun x -> printfn "%s " x)

let drawBoards (g: Game) =
    printfn ("my board")
    drawBoard (g.HumanBoard)
    printfn ("")
    printfn ("my attempts at the opponents board")
    drawBoard (g.ComputerBoard)
    printfn ("")

let drawShips(g: Game) =
    printfn ("human ships")
    drawShip (g.HumanBoard)
    printfn ("")
    printfn ("computer ships")
    drawShip (g.ComputerBoard)
    printfn ("")
