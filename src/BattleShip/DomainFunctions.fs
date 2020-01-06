module DomainFunctions

open Domain
open Ships

// Initialize a new Game
let initNewGame (size: int): Game =
    let coordCharacters = getCharacterRange size
    let computerShips = createRandomShips size

    let newFieldList =
        [ for character in coordCharacters do
            for i in 1 .. size ->
                { X = character
                  Y = i } ]
        |> List.map (fun coord ->
            { Coord = coord
              AttemptStatus = NotAttempted })

    { HumanBoard =
          { Fields = newFieldList
            Size = size
            ShipPoints = [] }
      ComputerBoard =
          { Fields = newFieldList
            Size = size
            ShipPoints = computerShips }
      Status = SetupShips 1
      Size = size }

// Computer makes a random move
let computerMove (humanBoard: Board): Coord =
    let notAttemptedFields = getNotAttemptedFieldsForBoard humanBoard
    let randomNumberGenerator = System.Random()
    let randomNumber = randomNumberGenerator.Next(0, notAttemptedFields.Length)
    notAttemptedFields.[randomNumber].Coord

let iterateShipPoints (coord: Coord) (shipPoint: ShipPoint) =
    match shipPoint.Coord = coord && shipPoint.PointStatus = NotHit with
    | true -> { shipPoint with PointStatus = ShipHit }
    | false -> shipPoint

let iterateFields (board: Board, coord: Coord) (field: Field): Field =
    if field.Coord = coord && field.AttemptStatus = NotAttempted then
        match boardHasShipPointAtCoord (board, coord) with
        | true -> { field with AttemptStatus = Attempted Hit }
        | false -> { field with AttemptStatus = Attempted Water }
    else
        field

// can be used for both boards - so for human entered coordinates or random created coords for the computer attempts
let hitOnBoard (board: Board, c: Coord) =
    let newShipPoints = board.ShipPoints |> List.map (iterateShipPoints c)
    let newFields = board.Fields |> List.map (iterateFields (board, c))
    { board with
          ShipPoints = newShipPoints
          Fields = newFields }

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
        ConsoleHelper.drawBoards newGame

        newGame

    else
        printfn "The given coordinate is not valid!"
        game

// Set a new ShipPoint on board
let setShipPoint (game: Game, coord: Coord): Game =
    match game.Status with
    | SetupShips value when value <= 3 ->
        let newGame =
            { game with
                  HumanBoard =
                      addShipPointToBoard
                          ({ Coord = coord
                             PointStatus = NotHit }, game.HumanBoard)
                  Status = SetupShips(value + 1) }

        System.Console.Clear()
        printfn ""
        printfn "   You"
        ConsoleHelper.drawShipPointStatus newGame.HumanBoard
        newGame
    | SetupShips 3 ->
        let newGame =
            { game with
                  HumanBoard =
                      addShipPointToBoard
                          ({ Coord = coord
                             PointStatus = NotHit }, game.HumanBoard)
                  Status = SetupShips 4 }

        System.Console.Clear()
        ConsoleHelper.drawBoards newGame
        newGame
    | SetupShips 4 ->
        let newGame =
            { game with
                  HumanBoard =
                      addShipPointToBoard
                          ({ Coord = coord
                             PointStatus = NotHit }, game.HumanBoard)
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
    | true ->
        printfn ("New ship at %c%d") coord.X coord.Y
        setShipPoint (game, coord)
    | false ->
        printfn ("Invalid coordinate %c%d") coord.X coord.Y
        printfn ("Try again!")
        game

let update (msg: Message) (game: Game): Game =
    match msg with
    | Set c -> set (game, c)
    | Try c -> tryHitAt (game, c)
    | ShowShips -> showShips (game)
