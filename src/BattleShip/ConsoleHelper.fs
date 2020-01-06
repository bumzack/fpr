module ConsoleHelper

open Domain

// String representation of a Field
let fieldToString (field: Field): string =
    match field.AttemptStatus with
    | NotAttempted -> " "
    | Attempted fieldStatus ->
        match fieldStatus with
        | Water -> "w"
        | Hit -> "h"

// String representation of a ShipPoint
let shipPointToString (shipPoint: ShipPoint): string =
    match shipPoint.PointStatus with
    | NotHit -> "o"
    | ShipHit -> "x"

// Horizontal Line output
let horizontallLine (size: int) =
    let cnt = (size + 2) * 3
    String.replicate cnt "-"

// Draw the status of all Fields of a Board
let drawFieldStatus (board: Board) =
    let boardCharacterRange = getCharacterRangeForBoard board // Possible character values
    let stringFields = List.map fieldToString board.Fields

    printfn " "
    printf "   "
    for character in boardCharacterRange do
        printf "  %c " character
    printfn " "

    printfn "   %s" (horizontallLine (board.Size))
    for i = 1 to board.Size do
        printf " %i " i
        for j = 1 to board.Size do
            printf "| %s " stringFields.[i + j - 2]
        printf "|\n"
        printfn "   %s" (horizontallLine (board.Size))

// Draw the status of all ShipPoints of a Board
let drawShipPointStatus (board: Board) =
    let boardCharacterRange = getCharacterRangeForBoard board // Possible character values
    let boardShipPointCoords = getShipPointCoordsForBoard board // All existing ShipPoints

    printfn " "
    printf "   "
    for character in boardCharacterRange do
        printf "  %c " character
    printfn " "

    printfn "   %s" (horizontallLine (board.Size))
    for j = 1 to board.Size do
        printf " %i " j
        for i = 1 to board.Size do
            let tempCoord =
                { X = boardCharacterRange.[i - 1]
                  Y = j }

            let shipPointCoordExists = List.contains tempCoord boardShipPointCoords

            match shipPointCoordExists with
            | true ->
                let shipPoint = getShipPointByCoordForBoard (tempCoord, board)
                printf "| %s " (shipPointToString shipPoint)
            | false -> printf "|   "

        printf "|\n"
        printfn "   %s" (horizontallLine (board.Size))

// Draw the Field and ShipPoint status for the provided Board
let drawBoardStatus (board: Board) =
    drawShipPointStatus board
    drawFieldStatus board

// Used for ShowShips command
let drawShips (g: Game) =
    printfn ""
    printfn "   You"
    drawFieldStatus g.HumanBoard
    drawShipPointStatus g.HumanBoard
    printfn ""
    printfn "   Opponent"
    drawFieldStatus g.ComputerBoard
    drawShipPointStatus g.ComputerBoard
    printfn ""

// Draw game status after every move
let drawBoards (g: Game) =
    printfn ""
    printfn "   You"
    drawShipPointStatus g.HumanBoard
    drawFieldStatus g.HumanBoard
    printfn ""
    printfn "   Opponent"
    drawFieldStatus g.ComputerBoard
    printfn ""
