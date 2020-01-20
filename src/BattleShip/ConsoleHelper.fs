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
    let character = [ 'A' .. 'Z' ].[index]
    fieldStringRowArray
    |> Array.map (fun str -> " " + str + " |")
    |> String.concat ""
    |> (+) "|"
    |> (+) (" " + string character + " ")

// Horizontal Line output
let horizontallLine (size: int) =
    let cnt = (size + 2) * 3
    String.replicate cnt "-"

let drawBoardWithGui (board: Board, fieldStrings: List<string>): unit =
    let boardCharacterRange = getCharacterRange board.Size
    let guiHorizontalLine = [ "   " + (horizontallLine board.Size) ]

    let guiHorizontalLabels =
        [ (boardCharacterRange
           |> List.mapi (fun index character -> "  " + string (index + 1) + " ")
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

    printfn "%s" output

let drawBoard (board: Board, fieldTransformer: Field -> string): unit =
    let fieldStrings = board.Fields |> List.map fieldTransformer
    drawBoardWithGui (board, fieldStrings)

let drawBoardWithShipStatusVisible (board: Board): unit =
    let fieldStrings = board.Fields |> List.map fieldToShipStatus
    drawBoardWithGui (board, fieldStrings)

let drawHumanBoard (game: Game): unit = drawBoard (game.HumanBoard, fieldToAttemptStatus)

let drawComputerBoard (game: Game): unit = drawBoard (game.ComputerBoard, fieldToAttemptStatus)

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
    printfn ""
    printfn "   You"
    drawHumanBoard game
    printfn ""
    printfn "   Opponent"
    drawComputerBoard game
