module Domain

open System
open System.Linq.Expressions

type FieldStatus =
    | Water
    | Hit

// field is one of field on the 5x5 board
type Field =
    | NotAttempted
    | Attempted of FieldStatus

// a ship consists of 2 or more points which can be either Hit or not hit
type ShipPointStatus =
    | NotHit
    | ShipHit

type Coord = {
    X: char
    Y: int
}

type ShipPoint =
    { Coord: Coord
      PointStatus: ShipPointStatus }

type ShipStatus =
    | Alive
    | Destroyed

type Ship =
    { Points: ShipPoint list
      Status: ShipStatus }


type Board =
    { Fields: Field list
      Size: int
      ShipsDestroyed: int
      Ships: Ship list }

type Player =
    | Human
    | Computer


type GameStatus =
    | WonBy of Player
    | Running


type Game =
    { P1Board: Board
      P2Board: Board
      Status: GameStatus
      Turn: Player
      Size: int }

// TODO remove
let initnewGame (size: int, humanShips: Ship list, computerSHips: Ship list): Game =

    let cntFields = size * size

    let fieldList1 =
        [ for a in 1 .. cntFields do
            yield (NotAttempted) ]


    // TODO: change to NotAttempted: just for debugging purposes
    let fieldList2 =
        [ for a in 1 .. cntFields do
            yield (Attempted Water) ]

    let board1 =
        { Fields = fieldList1
          Size = size
          ShipsDestroyed = 0
          Ships = humanShips }

    let board2 =
        { Fields = fieldList2
          Size = size
          ShipsDestroyed = 0
          Ships = computerSHips }




    let g =
        { P1Board = board1
          P2Board = board2
          Status = Running
          Turn = Human
          Size = size }

    g



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
    drawBoard (g.P1Board)
    printfn ("")
    printfn ("my attempts at opponents board")
    drawBoard (g.P2Board)





// TODO remove
type State = int


// TODO adapt/cleanup
// Message is a command entered by the user
type Message =
    | Increment
    | Decrement
    | IncrementBy of int
    | DecrementBy of int


// TODO remove
let init(): State =
    0

let update (msg: Message) (model: State): State =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1
    | IncrementBy x -> model + x
    | DecrementBy x -> model - x
