module ConsoleHelper

open Domain

// String representation of a Field
let fieldToAttemptStatus (field: Field): string =
    match field.AttemptStatus with
    | NotAttempted -> " "
    | Attempted ->
        match field.ShipStatus with
        | Water -> "~"
        | Ship -> "o"
        | ShipHit -> "x"

let fieldToShipStatus (field: Field): string =
    match field.ShipStatus with
    | Water -> " "
    | Ship -> "o"
    | ShipHit -> "x"

// Horizontal Line output
let newHorizontalLine (size: int): string = String.replicate size "-"

let createFieldStringRow (index: int) (fieldStringRowArray: string []): string =
    let character = [ '1' .. '9' ].[index]
    fieldStringRowArray
    |> Array.map (fun str -> " " + str + " |")
    |> String.concat ""
    |> (+) "|"
    |> (+) (" " + string character + " ")

// Horizontal Line output
let horizontallLine (size: int) =
    let cnt = (size + 2) * 3
    String.replicate cnt "-"


let createBoardStringList (board: Board, fieldStrings: List<string>) =
    let boardCharacterRange = getCharacterRange board.Size
    let guiHorizontalLine = [ "   " + (horizontallLine board.Size) ]

    let guiHorizontalLabels =
        [ (boardCharacterRange
           |> List.map (fun  c -> "  " + string c + " ")
           |> String.concat ""
           |> (+) "   ") ]

    let fieldStringRows =
        fieldStrings
        |> Seq.chunkBySize board.Size
        |> Seq.toList
        |> List.mapi createFieldStringRow

    let output =
        List.foldBack (fun element accumulator ->
            accumulator
            |> List.append guiHorizontalLine
            |> List.append [ element ]) fieldStringRows []
        |> (@) guiHorizontalLine
        |> (@) guiHorizontalLabels
        |> (@) [ "\n" ]
        |> String.concat "\n"

    output

let drawBoardWithGui (board: Board, fieldStrings: List<string>): unit =
    printfn "%s" ( createBoardStringList(board, fieldStrings))

let drawBoard (board: Board, fieldTransformer: Field -> string): unit =
    let fieldStrings = board.Fields |> List.map fieldTransformer
    drawBoardWithGui (board, fieldStrings)

let drawBoardStringList (board: Board, fieldTransformer: Field -> string) =
    let fieldStrings = board.Fields |> List.map fieldTransformer
    createBoardStringList (board, fieldStrings)

let drawBoardWithShipStatusVisible (board: Board): unit =
    let fieldStrings = board.Fields |> List.map fieldToShipStatus
    drawBoardWithGui (board, fieldStrings)

let drawHumanBoard (game: Game): unit = drawBoard (game.HumanBoard, fieldToAttemptStatus)

let drawHumanBoardStringList (game: Game) =
    drawBoardStringList (game.HumanBoard, fieldToAttemptStatus)


let drawComputerBoard (game: Game): unit = drawBoard (game.ComputerBoard, fieldToAttemptStatus)

let drawComputerBoardStringList (game: Game) =
    drawBoardStringList (game.ComputerBoard, fieldToAttemptStatus)

// Used for ShowShips command
let drawShips (game: Game) =
    printfn ""
    printfn "   You"
    drawBoardWithShipStatusVisible game.HumanBoard
    printfn ""
    printfn "   Opponent"
    drawBoardWithShipStatusVisible game.ComputerBoard

// Draw game status after every move
let drawBoards (game: Game) =
    let b1 = drawHumanBoardStringList game
    let b1 =  "\n" + "     You              T  " + "\n" + b1
    let b2 = drawComputerBoardStringList game
    let b2 =  "\n" + " Opponent  " + "\n" + b2

    let output = Utils.mergeStrings (b1, b2) |> String.concat "\n"
    printfn "%s" output
