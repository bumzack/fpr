module DomainFunctions

open Domain
open Utils

// Return a list of all Fields with FieldAttemptStatus = NotAttempted
let getNotAttemptedFieldsForBoard (board: Board): List<Field> =
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
let getRemainingShipsForBoard (board: Board): List<Field> =
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
let createCoordsForSize (size: int): List<Coord> =
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

let shipCollidesWithExistingShip (ship: Ship, board: Board): bool =
    let existingShipCoords = getRemainingShipsForBoard board |> List.map (fun field -> field.Coord)
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
            addShipPointsToBoard (board, ship)
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
    let requiredShips = [ 2; 2 ]

    let newFieldList = createCoordsForSize size |> List.map initNewField

    let humanBoard =
        { Fields = newFieldList
          Size = size }

    let tmpComputerBoard =
        { Fields = newFieldList
          Size = size }

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

let rec runComputerLoop (game: Game) =
    let computerMoveCoord = computerMove game.HumanBoard
    let (newHumanBoard, computerHasHit) = hitOnBoard (game.HumanBoard, computerMoveCoord)

    match computerHasHit with
    | false ->
        printfn ""
        printfn "The computer tried at %c%i and missed" computerMoveCoord.X computerMoveCoord.Y
    | true ->
        printfn ""
        printfn "The computer tried at %c%i and made a hit! " computerMoveCoord.X computerMoveCoord.Y

    printfn ""
    let newGame = { game with HumanBoard = newHumanBoard }
    ConsoleHelper.drawBoards newGame

    match computerHasHit with
    | false ->
        newGame
    | true ->
        runComputerLoop (newGame)

let tryHitAt (game: Game, humanMoveCoord: Coord) =
    let newGame =
        match isValidGameCoord (game, humanMoveCoord) with
        | true ->
            let (newComputerBoard, humanHasHit) = hitOnBoard (game.ComputerBoard, humanMoveCoord)

            match humanHasHit with
            | true ->
                let newGame = { game with ComputerBoard = newComputerBoard }
                ConsoleHelper.drawBoards newGame
                printfn ""
                printfn "You hit at %c%i" humanMoveCoord.X humanMoveCoord.Y
                printfn "You have another go!"
                printfn ""

                newGame

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

let addShipToBoard (game: Game, s: Ship, board: Board) =
    match game.Status with
    | SetupShips idx when idx < game.RequiredShips.Length ->
        if s.length = game.RequiredShips.[idx] then
            let newBoard = addShipPointsToBoard (board, s)
            Some(newBoard)
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
