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




// TODO: man that's ugly
let drawShips (board: Board) =
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

