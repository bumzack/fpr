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

let iterateFields (coord: Coord) (field: Field): Field =
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
    let newFields = board.Fields |> List.map (iterateFields c)
    let newBoard = { board with Fields = newFields }
    let hit = (getRemainingShipsForBoard board).Length > (getRemainingShipsForBoard newBoard).Length
    (newBoard, hit)

let tryHitAt (game: Game, humanMoveCoord: Coord) =
    match isValidGameCoord (game, humanMoveCoord) with
    | true ->
        let (newComputerBoard, humanHasHit) = hitOnBoard (game.ComputerBoard, humanMoveCoord)

        match humanHasHit with
        // if it is a hit  then return and the human can try again
        | true ->
            let newGame = { game with ComputerBoard = newComputerBoard }
            System.Console.Clear()
            ConsoleHelper.drawBoards newGame
            printfn ""
            printfn "You hit at %c%i" humanMoveCoord.X humanMoveCoord.Y
            printfn "You have another go!"
            printfn ""
            newGame

        // if it is a miss; then it is the computers turn, then let the computer randomly choose an action until a miss occurs
        | false ->
            let computerMoveCoord = computerMove game.HumanBoard
            let (newHumanBoard, computerHasHit) = hitOnBoard (game.HumanBoard, computerMoveCoord)

            let newGame =
                { game with
                      ComputerBoard = newComputerBoard
                      HumanBoard = newHumanBoard }

            System.Console.Clear()
            ConsoleHelper.drawBoards newGame
            printfn ""
            printfn "You missed at %c%i" humanMoveCoord.X humanMoveCoord.Y
            printfn "The computer tried at %c%i" computerMoveCoord.X computerMoveCoord.Y
            printfn ""
            newGame
    | false ->
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
    System.Console.Clear()
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
