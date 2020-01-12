module DomainFunctions

open Domain

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

// Create 4 random ship coordinates for the computer player
let createRandomCoords (size: int): List<Coord> =
    let coordCharacters = getCharacterRange size
    let randomNumberGenerator = System.Random()
    let xRand = randomNumberGenerator.Next(0, coordCharacters.Length)
    let yRand = randomNumberGenerator.Next(0, (size - 1))
    let xValue = coordCharacters.[xRand]
    let yValue = yRand

    let coord1 =
        { X = xValue
          Y = yValue }

    let coord2 =
        { X = coordCharacters.[randomNumberGenerator.Next(0, coordCharacters.Length)]
          Y = randomNumberGenerator.Next(0, (size - 1)) }

    let coord3 =
        { X = coordCharacters.[randomNumberGenerator.Next(0, coordCharacters.Length)]
          Y = randomNumberGenerator.Next(0, (size - 1)) }

    let coord4 =
        { X = coordCharacters.[randomNumberGenerator.Next(0, coordCharacters.Length)]
          Y = randomNumberGenerator.Next(0, (size - 1)) }

    coord1 :: coord2 :: coord3 :: [ coord4 ]

let setShipForFieldAtCoord (coords: List<Coord>) (field: Field): Field =
    match field.Coord with
    | { X = xValue; Y = yValue } when xValue = coords.[0].X && yValue = coords.[0].Y -> { field with ShipStatus = Ship }
    | { X = xValue; Y = yValue } when xValue = coords.[1].X && yValue = coords.[1].Y -> { field with ShipStatus = Ship }
    | { X = xValue; Y = yValue } when xValue = coords.[2].X && yValue = coords.[2].Y -> { field with ShipStatus = Ship }
    | { X = xValue; Y = yValue } when xValue = coords.[3].X && yValue = coords.[3].Y -> { field with ShipStatus = Ship }
    | _ -> field

let setRandomShipForBoard (board: Board): Board =
    let emptyFields = getEmptyFieldsForBoard board
    let coord = emptyFields.[System.Random().Next(0, emptyFields.Length - 1)].Coord
    addShipPointAtCoordToBoard (coord, board)

// Initialize a new Game
let initNewGame (size: int): Game =
    let newFieldList = createCoordsForSize size |> List.map initNewField

    let humanBoard =
        { Fields = newFieldList
          Size = size }

    let computerBoard =
        { Fields = newFieldList
          Size = size }
        |> setRandomShipForBoard
        |> setRandomShipForBoard
        |> setRandomShipForBoard
        |> setRandomShipForBoard

    { HumanBoard = humanBoard
      ComputerBoard = computerBoard
      Status = SetupShips 1
      Size = size }

// Computer makes a random move
let computerMove (humanBoard: Board): Coord =
    let notAttemptedFields = getNotAttemptedFieldsForBoard humanBoard
    let randomNumberGenerator = System.Random()
    let randomNumber = randomNumberGenerator.Next(0, notAttemptedFields.Length)
    notAttemptedFields.[randomNumber].Coord

let iterateFields (board: Board, coord: Coord) (field: Field): Field =
    match field.AttemptStatus with
    | NotAttempted ->
        match field.Coord with
        | { X = xValue; Y = yValue } when xValue = coord.X && yValue = coord.Y ->
            match field.ShipStatus with
            | Ship ->
                { field with
                      AttemptStatus = Attempted
                      ShipStatus = DestroyedShip }
            | Water -> { field with AttemptStatus = Attempted }
            | _ -> field
        | _ -> field
    | _ -> field

// can be used for both boards - so for human entered coordinates or random created coords for the computer attempts
let hitOnBoard (board: Board, c: Coord) =
    let newFields = board.Fields |> List.map (iterateFields (board, c))
    { board with Fields = newFields }

let tryHitAt (game: Game, humanMoveCoord: Coord) =
    if isValidGameCoord (game, humanMoveCoord) then
        // if it is a hit  then return and the human can try again
        let newComputerBoard = hitOnBoard (game.ComputerBoard, humanMoveCoord)

        // if it is a miss; then it is the computers turn, then let the computer randomly choose an action until a miss occurs
        let computerMoveCoord = computerMove game.HumanBoard
        let newHumanBoard = hitOnBoard (game.HumanBoard, computerMoveCoord)

        let newGame =
            { game with
                  ComputerBoard = newComputerBoard
                  HumanBoard = newHumanBoard }

        System.Console.Clear()
        printfn "%A" computerMoveCoord
        ConsoleHelper.drawBoards newGame

        newGame

    else
        printfn "The given coordinate is not valid!"
        game

let addShip (game: Game, coord: Coord): Game =
    match game.Status with
    | SetupShips value when value <= 3 ->
        let newGame =
            { game with
                  HumanBoard = addShipPointAtCoordToBoard (coord, game.HumanBoard)
                  Status = SetupShips(value + 1) }

        System.Console.Clear()
        printfn ""
        printfn "   You"
        ConsoleHelper.drawHumanBoard newGame
        newGame
    | SetupShips 3 ->
        let newGame =
            { game with
                  HumanBoard = addShipPointAtCoordToBoard (coord, game.HumanBoard)
                  Status = SetupShips 4 }

        System.Console.Clear()
        ConsoleHelper.drawBoards newGame
        newGame
    | SetupShips 4 ->
        let newGame =
            { game with
                  HumanBoard = addShipPointAtCoordToBoard (coord, game.HumanBoard)
                  Status = Running }

        System.Console.Clear()
        ConsoleHelper.drawBoards newGame
        newGame
    | _ ->
        ConsoleHelper.drawBoards game
        game

// Cheat code - Show all ships
let showShips (game: Game) =
    ConsoleHelper.drawShips (game)
    game

// Human player sets new ShipPoints on the provided Coord
let set (game: Game, coord: Coord): Game =
    match isValidGameCoord (game, coord) with
    | true -> addShip (game, coord)
    | false ->
        printfn ("Invalid coordinate %c%d") coord.X coord.Y
        printfn ("Try again!")
        game

let update (msg: Message) (game: Game): Game =
    match msg with
    | Set c -> set (game, c)
    | Try c -> tryHitAt (game, c)
    | ShowShips -> showShips (game)
