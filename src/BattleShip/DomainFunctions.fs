module DomainFunctions

open Domain
open Utils

// Return a list of all Fields with FieldAttemptStatus = NotAttempted
let getNotAttemptedFieldsForBoard (board: Board): Field list =
    board.Fields |> List.filter (fun field -> field.AttemptStatus = NotAttempted)

// Add a new ShipPoint at the provided Coord to the provided Board
let addShipPointAtCoordToBoard (coord: Coord, board: Board): Board =
    let newFields =
        board.Fields
        |> List.map (fun field ->
            match field.Coord with
            | { X = xValue; Y = yValue } when xValue = coord.X && yValue = coord.Y ->
                match field.ShipStatus with
                | Water -> { field with ShipStatus = Ship }
                | _ -> field
            | _ -> field)
    { board with Fields = newFields }

// Return a list of Fields with ShipStatus = Ship
let getRemainingShipsForBoard (board: Board): Field list =
    board.Fields |> List.filter (fun field -> field.ShipStatus = Ship)

// Check if the provided coordinate is even on the board
let isValidGameCoord (game: Game, coord: Coord): bool =
    let isValidBoardCharacter = ConsoleHelper.getCharacterRange game.Size |> List.contains coord.X
    let isValidBoardInteger = coord.Y >= 1 && coord.Y <= game.Size
    isValidBoardInteger && isValidBoardCharacter

let rec helper (board: Board, coords: Coord list) =
    match coords with
    | head :: tail ->
        let newboard = addShipPointAtCoordToBoard (head, board)
        helper (newboard, tail)
    | [] -> board

let addShipPointsToBoard (board: Board, s: Ship) =
    let shipCoordinates = createCoordList (s)
    let newBoard = helper (board, shipCoordinates)
    newBoard

// Initialize a new Field at the given Coord
let initNewField (coord: Coord): Field =
    { Coord = coord
      AttemptStatus = NotAttempted
      ShipStatus = Water }

// Create a list of all Coords for the given size
let createCoordsForSize (size: int): Coord list =
    let coordCharacters = ConsoleHelper.getCharacterRange size
    [ for i in 1 .. size do
        for character in coordCharacters ->
            { X = character
              Y = i } ]

let randomCoord (size: int): Coord =
    let emptyFields = createCoordsForSize (size)
    let coord = emptyFields.[System.Random().Next(0, emptyFields.Length - 1)]
    coord

let isCoordNotOnBoard (size: int, c: Coord) =
    let idx = charToInt c.X
    let res = idx >= size || c.Y > size
    res

let shipOnBoard (s: Ship, size: int) =
    let shipCoords = createCoordList (s)

    match shipCoords.Length with
    | 0 -> false
    | _ ->
        let curriedIsCoordNotOnBoard c s = isCoordNotOnBoard (c, s)
        let coordsNotOnBoard = List.filter (curriedIsCoordNotOnBoard size) shipCoords
        coordsNotOnBoard.Length = 0

let previousCharacter (character: char): char =
    (char)((int)character - 1)

let nextCharacter (character: char): char =
    (char)((int)character + 1)

let addDisallowedCoords (coords: Coord list): Coord list =
    let westCoords = coords |> List.map (fun coord -> { coord with X = previousCharacter coord.X })
    let eastCoords = coords |> List.map (fun coord -> { coord with X = nextCharacter coord.X })
    let northCoords = coords |> List.map (fun coord -> { coord with Y = coord.Y - 1 })
    let southCoords = coords |> List.map (fun coord -> { coord with Y = coord.Y + 1 })

    (Set.ofList coords)
        |> Set.union (Set.ofList westCoords)
        |> Set.union (Set.ofList eastCoords)
        |> Set.union (Set.ofList northCoords)
        |> Set.union (Set.ofList southCoords)
        |> Set.toList

let shipCollidesWithExistingShip (ship: Ship, board: Board): bool =
    let existingShipCoords = getRemainingShipsForBoard board |> List.map (fun field -> field.Coord) |> addDisallowedCoords
    let shipCoords = createCoordList ship
    let intersection = Set.intersect (Set.ofList existingShipCoords) (Set.ofList shipCoords) |> Set.toList
    intersection.Length > 0

let rec createRandomShip (board: Board, length: int): Board =
    let pos = randomCoord (board.Size)
    let direction = randomDirection()
    let ship = mapToShip (pos, length, direction)

    match shipOnBoard (ship, board.Size) with

    | true ->
        match shipCollidesWithExistingShip (ship, board) with
        | true ->
            createRandomShip (board, length)
        | false ->
            let newboard = addShipPointsToBoard (board, ship)
            let ships = ship :: board.Ships
            let newboard2 = { newboard with Ships = ships }
            newboard2
    | false ->
        createRandomShip (board, length)

let rec createRandomShips (board: Board, ships: int list): Board =
    match ships with
    | head :: tail ->
        let newboard = createRandomShip (board, head)
        createRandomShips (newboard, tail)
    | [] -> board

// Initialize a new Game
let initNewGame (size: int): Game =
    let requiredShips = [ 2; 3 ]

    let newFieldList = createCoordsForSize size |> List.map initNewField

    let humanBoard =
        { Fields = newFieldList
          Size = size
          Ships = [] }

    let tmpComputerBoard =
        { Fields = newFieldList
          Size = size
          Ships = [] }

    let computerBoard = createRandomShips (tmpComputerBoard, requiredShips)

    { HumanBoard = humanBoard
      ComputerBoard = computerBoard
      Status = SetupShips 0
      Size = size
      RequiredShips = requiredShips }

// Computer makes a random move
let computerMove (humanBoard: Board): Coord =
    let notAttemptedFields = getNotAttemptedFieldsForBoard humanBoard
    let randomNumberGenerator = System.Random()
    let randomNumber = randomNumberGenerator.Next(0, notAttemptedFields.Length)
    notAttemptedFields.[randomNumber].Coord

let hitField (coord: Coord) (field: Field): Field =
    match field.AttemptStatus with
    | NotAttempted ->
        match field.Coord with
        | { X = xValue; Y = yValue } when xValue = coord.X && yValue = coord.Y ->
            match field.ShipStatus with
            | Ship ->
                { field with
                      AttemptStatus = Attempted
                      ShipStatus = ShipHit }
            | Water -> { field with AttemptStatus = Attempted }
            | _ -> field
        | _ -> field
    | _ -> field

let allShipsDestroyed (board: Board) =
    let remainig = getRemainingShipsForBoard (board)
    remainig.Length = 0

// can be used for both boards - so for human entered coordinates or random created coords for the computer attempts
let hitOnBoard (board: Board, c: Coord) =
    let newFields = board.Fields |> List.map (hitField c)
    let newBoard = { board with Fields = newFields }
    let hit = (getRemainingShipsForBoard board).Length > (getRemainingShipsForBoard newBoard).Length
    (newBoard, hit)



let getHitShipsForBoard (board: Board): Field list =
    board.Fields |> List.filter (fun field -> field.ShipStatus = ShipHit)

let isShipSunk (board: Board, s: Ship) =
    let shipCoordinates = createCoordList s
    let hitShipCoords = getHitShipsForBoard board |> List.map (fun field -> field.Coord)
    let len = shipCoordinates.Length

    let intersection = Set.intersect (Set.ofList hitShipCoords) (Set.ofList shipCoordinates) |> Set.toList
    intersection.Length = len


// https://stackoverflow.com/questions/2889961/f-insert-remove-item-from-list
let rec remove i l =
    match i, l with
    | 0, x :: xs -> xs
    | i, x :: xs -> x :: remove (i - 1) xs
    | i, [] -> failwith "index out of range"

let shipSunk (board: Board) =
    let curriedIsShipSunk b s = isShipSunk (b, s)

    // list bools
    let sunkShips = List.map (curriedIsShipSunk board) board.Ships

    // list where bool = true
    let isAnyHit = sunkShips |> List.filter (fun b -> b)

    if isAnyHit.Length > 0 then

        let sunkShipIdx = sunkShips |> Seq.findIndex (fun b -> b)
        let ships = remove sunkShipIdx board.Ships
        let newBoard = { board with Ships = ships }
        (newBoard, true)
    else
        (board, false)




let shipSunkComputerBoard (game: Game) =
    let (newComputerBoard, shipSunk) = shipSunk (game.ComputerBoard)

    let newGame2 =
        match shipSunk with
        | true ->
            { game with ComputerBoard = newComputerBoard }
        | false ->
            game
    (newGame2, shipSunk)

let shipSunkHumanBoard (game: Game) =
    let (newHumanBoard, shipSunk) = shipSunk (game.HumanBoard)

    let newGame2 =
        match shipSunk with
        | true ->
            { game with HumanBoard = newHumanBoard }
        | false ->
            game
    (newGame2, shipSunk)



let rec runComputerLoop (game: Game) =
    let computerMoveCoord = computerMove game.HumanBoard
    let (newHumanBoard, computerHasHit) = hitOnBoard (game.HumanBoard, computerMoveCoord)
    let newGame = { game with HumanBoard = newHumanBoard }

    let newGame2 =
        match computerHasHit with
        | false ->
            printfn ""
            printfn "The computer tried at %c%i and missed" computerMoveCoord.X computerMoveCoord.Y
            newGame
        | true ->
            let (newGame2, shipSunk) = shipSunkHumanBoard (newGame)

            printfn ""
            if shipSunk then
                printfn "The computer hit at %c%i" computerMoveCoord.X computerMoveCoord.Y
                printf "\n ohh nooo!  the computer sunk a ship\n"
            else
                printfn "The computer hit at %c%i" computerMoveCoord.X computerMoveCoord.Y

            printfn ""
            printfn "The computer tried at %c%i and made a hit! " computerMoveCoord.X computerMoveCoord.Y
            newGame2

    printfn ""

    ConsoleHelper.drawBoards newGame2

    match computerHasHit with
    | false ->
        newGame2
    | true ->
        runComputerLoop (newGame2)


let tryHitAt (game: Game, humanMoveCoord: Coord) =
    match game.Status with
    | Running ->
        let newGame =
            match isValidGameCoord (game, humanMoveCoord) with
            | true ->
                let (newComputerBoard, humanHasHit) = hitOnBoard (game.ComputerBoard, humanMoveCoord)

                match humanHasHit with
                | true ->
                    let newGame = { game with ComputerBoard = newComputerBoard }
                    let (newGame2, shipSunk) = shipSunkComputerBoard (newGame)

                    ConsoleHelper.drawBoards newGame2
                    printfn ""
                    if shipSunk then
                        printfn "You hit at %c%i" humanMoveCoord.X humanMoveCoord.Y
                        printf "\nGREAT!   you sunk a ship\n"
                    else
                        printfn "You hit at %c%i" humanMoveCoord.X humanMoveCoord.Y

                    printfn "Try again !"
                    printfn ""

                    newGame2

                | false ->
                    printfn ""
                    printfn "You missed at %c%i" humanMoveCoord.X humanMoveCoord.Y
                    printfn ""
                    printfn "now it's the computers turn"
                    let newGame = { game with ComputerBoard = newComputerBoard }
                    let newGame = runComputerLoop (newGame)
                    newGame
            | false ->
                printfn "The given coordinate is not valid!"
                game

        if (allShipsDestroyed (newGame.HumanBoard)) then { newGame with Status = WonBy Computer }
        elif (allShipsDestroyed (newGame.ComputerBoard)) then { newGame with Status = WonBy Human }
        else newGame

    | _ ->
        printfn "game not in status 'Running' but in status '%A'!" game.Status
        printfn "finish setting up your ships to play"
        game

let addShipToBoard (game: Game, s: Ship, board: Board) =
    match game.Status with
    | SetupShips idx when idx < game.RequiredShips.Length ->
        if s.length = game.RequiredShips.[idx] then
            let newBoard = addShipPointsToBoard (board, s)
            let ships = s :: newBoard.Ships
            let newBoard2 = { newBoard with Ships = ships }
            Some(newBoard2)
        else
            printfn "Your ship has length %i but should have length %i - try again!" s.length game.RequiredShips.[idx]
            None
    | _ ->
        ConsoleHelper.drawBoards game
        None

// Cheat code - Show all ships
let showShips (game: Game) =
    ConsoleHelper.drawShips (game)
    game

let setShip (game: Game, s: Ship): Game =
    match shipOnBoard (s, game.Size) with
    | true ->
        match game.Status with
        | SetupShips shipIdx when shipIdx < game.RequiredShips.Length ->
            match shipCollidesWithExistingShip (s, game.HumanBoard) with
            | true ->
                printfn ("ship cannot be placed on the board because there is already another ship - try again!")
                game
            | false ->
                let humandboard = addShipToBoard (game, s, game.HumanBoard)
                if humandboard.IsSome then
                    let newGame = { game with HumanBoard = humandboard.Value }
                    let newShipIdx = shipIdx + 1
                    if newShipIdx >= newGame.RequiredShips.Length then { newGame with Status = Running }
                    else { newGame with Status = SetupShips newShipIdx }
                else
                    game
        | _ ->
            printfn ("enough ships - lets play!")
            printfn ("use command 'Try' and coordinates like A2  and destroy the computers ships")
            game

    | false ->
        printfn ("ship does not fit on the board - try again!")
        game

let update (msg: Message) (game: Game): Game =
    match msg with
    | Set s -> setShip (game, s)
    | Try c -> tryHitAt (game, c)
    | ShowShips -> showShips (game)
