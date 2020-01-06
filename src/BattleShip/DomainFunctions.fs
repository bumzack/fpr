module DomainFunctions

open Domain
open Ships

let initNewGame (size: int): Game =
    let coordCharacters = getCharacterRange size
    let computerShips = createRandomShips (size)

    let newFieldList =
        [ for character in coordCharacters do
            for i in 1 .. size ->
                { X = character
                  Y = i } ]
        |> List.map (fun coord ->
            { Coord = coord
              AttemptStatus = NotAttempted })

    let g =
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

    g

// Computer makes a random move
let computerMove (humanBoard: Board): Coord =
    let notAttemptedFields = getNotAttemptedFieldsForBoard humanBoard
    let randomNumberGenerator = System.Random()
    let randomNumber = randomNumberGenerator.Next(0, notAttemptedFields.Length)
    notAttemptedFields.[randomNumber].Coord

let iterateShipPoints (coord: Coord) (shipPoint: ShipPoint) =
    if shipPoint.Coord = coord then
        printfn "Hit"
        { shipPoint with PointStatus = ShipHit }
    else
        shipPoint

let iterateFields (board: Board, coord: Coord) (field: Field): Field =
    if field.Coord = coord && field.AttemptStatus = NotAttempted then
        match boardHasShipPointAtCoord (board, coord) with
        | true ->
            let newAttemptStatus = Attempted Hit
            printfn "Hit"
            { field with AttemptStatus = newAttemptStatus }
        | false ->
            let newAttemptStatus = Attempted Water
            printfn "Water"
            { field with AttemptStatus = newAttemptStatus }
    else
        field

// can be used for both boards - so for human entered coordinates or random created coords for the computer attempts
let hitOnBoard (board: Board, c: Coord) =
    let newShipPoints = board.ShipPoints |> List.map (iterateShipPoints c)

    let newFields = board.Fields |> List.map (iterateFields (board, c))

    let newBoard =
        { board with
              ShipPoints = newShipPoints
              Fields = newFields }

    newBoard

let tryHitAt (game: Game, humanMoveCoord: Coord) =
    // TODO
    // update the fields and ships accordingly

    // if it is a hit  then return and the human can try again
    // if it is a miss; then it is the computers turn, then let the computer randomly choose an action until a miss occurs
    if isValidGameCoord (game, humanMoveCoord) then
        let newComputerBoard = hitOnBoard (game.ComputerBoard, humanMoveCoord)
        let computerMoveCoord = computerMove game.HumanBoard
        let newHumanBoard = hitOnBoard (game.HumanBoard, computerMoveCoord)

        let newGame =
            { game with
                  ComputerBoard = newComputerBoard
                  HumanBoard = newHumanBoard }

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

        ConsoleHelper.drawBoards newGame
        newGame
    | _ ->
        ConsoleHelper.drawBoards game
        game

let showShips (game: Game) =
    ConsoleHelper.drawShips (game)
    game

// Human player sets new ShipPoints on the provided Coord
let set (game: Game, coord: Coord): Game =
    if (isValidGameCoord (game, coord)) then
        printfn ("New ship at %c%d") coord.X coord.Y
        setShipPoint (game, coord)
    else
        printfn ("Invalid coordinate %c%d") coord.X coord.Y
        printfn ("Try again!")
        game

let update (msg: Message) (game: Game): Game =
    match msg with
    | Set c -> set (game, c)
    | Try c -> tryHitAt (game, c)
    | ShowShips -> showShips (game)
