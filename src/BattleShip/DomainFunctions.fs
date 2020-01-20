module DomainFunctions

open Domain
open Utils

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
    let coordCharacters = getCharacterRange size
    [ for character in coordCharacters do
        for i in 1 .. size ->
            { X = character
              Y = i } ]



let rec addShipsToBoard (board: Board, ships: Ship list) =
    match ships with
    | head :: tail ->
        let newBoard = addShipPointsToBoard (board, head)
        addShipsToBoard (newBoard, tail)
    | [] -> board


let setRandomShipForBoard (board: Board): Board =
    let emptyFields = getEmptyFieldsForBoard board
    let coord = emptyFields.[System.Random().Next(0, emptyFields.Length - 1)].Coord
    addShipPointAtCoordToBoard (coord, board)

let randomCoord (size: int): Coord =
    let emptyFields = createCoordsForSize (size)
    let coord = emptyFields.[System.Random().Next(0, emptyFields.Length - 1)]
    coord



let isCoordNotOnBoard (size: int, c: Coord) =
    let idx = charToInt c.X
    let res = idx > size || c.Y > size
    res

let shipOnBoard (s: Ship, size: int) =
    let shipCoords = createCoordList (s)

    match shipCoords.Length with
    | 0 -> false
    | _ ->
        let curriedIsCoordNotOnBoard c s = isCoordNotOnBoard(c, s)
        let coordsNotOnBoard = List.filter (curriedIsCoordNotOnBoard size) shipCoords
        coordsNotOnBoard.Length = 0

let rec createRandomShip (size: int, len: int) =
    let pos = randomCoord (size)
    let direction = randomDirection()
    let ship = mapToShip (pos, len, direction)

    match shipOnBoard (ship, size) with
    | true -> ship
    | false -> createRandomShip (len, size)


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

    // currying for the first time https://stackoverflow.com/questions/50105745/list-map-with-multiple-parameters-in-f
    let curriedCreateRandomShip s l = createRandomShip (s, l)
    let randomShips = List.map (curriedCreateRandomShip size) requiredShips
    let computerBoard = addShipsToBoard (tmpComputerBoard, randomShips)


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

let iterateFields (coord: Coord) (field: Field): Field =
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
    let newFields = board.Fields |> List.map (iterateFields c)
    let newBoard = { board with Fields = newFields }
    let hit = (getRemainingShipsForBoard board).Length > (getRemainingShipsForBoard newBoard).Length
    (newBoard, hit)

let rec runComputerLoop (game: Game) =

    let computerMoveCoord = computerMove game.HumanBoard
    let (newHumanBoard, computerHasHit) = hitOnBoard (game.HumanBoard, computerMoveCoord)

    // System.Console.Clear()

    match computerHasHit with
    | false -> printfn "The computer tried at %c%i and missed" computerMoveCoord.X computerMoveCoord.Y
    | true -> printfn "The computer tried at %c%i and made a hit! " computerMoveCoord.X computerMoveCoord.Y

    printfn ""
    ConsoleHelper.drawBoards game

    match computerHasHit with
    | false -> game
    | true ->
        let newGame = { game with HumanBoard = newHumanBoard }
        runComputerLoop (newGame)

let tryHitAt (game: Game, humanMoveCoord: Coord) =
    let newGame =
        match isValidGameCoord (game, humanMoveCoord) with
        | true ->
            let (newComputerBoard, humanHasHit) = hitOnBoard (game.ComputerBoard, humanMoveCoord)

            match humanHasHit with
            // if it is a hit  then return and the human can try again
            | true ->
                let newGame = { game with ComputerBoard = newComputerBoard }
                // System.Console.Clear()
                ConsoleHelper.drawBoards newGame
                printfn ""
                printfn "You hit at %c%i" humanMoveCoord.X humanMoveCoord.Y
                printfn "You have another go!"
                printfn ""
                newGame

            // if it is a miss; then it is the computers turn, then let the computer randomly choose an action until a miss occurs
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
    // System.Console.Clear()
    ConsoleHelper.drawShips (game)
    game


let setShip (game: Game, s: Ship): Game =

    // TODO: check if ship does not collide with other ships
    match shipOnBoard (s, game.Size) with
    | true ->
        match game.Status with
        | SetupShips shipIdx when shipIdx < game.RequiredShips.Length ->
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
