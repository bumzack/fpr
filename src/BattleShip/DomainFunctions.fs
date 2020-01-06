module DomainFunctions

open Domain

let initNewGame (size: int, computerShips: Ship list): Game =
    let coordCharacters = getCharacterRange size

    let newCoordList =
        [ for character in coordCharacters do
            for i in 1 .. size ->
                { X = character
                  Y = i } ]

    let newFieldList = newCoordList |> List.map createNewFieldFromCoord

    let board1 =
        { Fields = newFieldList
          Size = size
          ShipsDestroyed = 0
          Ships = [] }

    let board2 =
        { Fields = newFieldList
          Size = size
          ShipsDestroyed = 0
          Ships = computerShips }

    let g =
        { HumanBoard = board1
          ComputerBoard = board2
          Status = SetupShips 1
          Size = size }

    g

// Computer makes a random move
let computerMove (humanBoard: Board): Coord =
    let notAttemptedFields = getNotAttemptedFieldsForBoard humanBoard
    let randomNumberGenerator = System.Random()
    let randomNumber = randomNumberGenerator.Next(0, notAttemptedFields.Length)
    notAttemptedFields.[randomNumber].Coord

let iterateShipPoints (c: Coord) (shipPoint: ShipPoint) =
    if shipPoint.Coord = c then
        printfn "Hit"
        { shipPoint with PointStatus = ShipHit }
    else
        shipPoint

let iterateShips (c: Coord) (ship: Ship): Ship = { ship with Points = List.map (iterateShipPoints c) ship.Points }

let iterateFields (board: Board, coord: Coord) (field: Field): Field =
    if field.Coord = coord && field.AttemptStatus = NotAttempted then
        match boardHasShipPointAtCoord (board, coord) with
        | true ->
            printfn "Hit"
            { field with AttemptStatus = Attempted Hit }
        | false ->
            printfn "Water"
            { field with AttemptStatus = Attempted Water }
    else
        field

// can be used for both boards - so for human entered coordinates or random created coords for the computer attempts
let hitOnBoard (board: Board, c: Coord) =
    let newShips = board.Ships |> List.map (iterateShips c)

    let newFields = board.Fields |> List.map (iterateFields (board, c))

    let newBoard =
        { board with
              Ships = newShips
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

let createShipFromCoordPair (cp: CoordPair) =
    let p1 =
        { Coord = cp.c1
          PointStatus = NotHit }

    let p2 =
        { Coord = cp.c2
          PointStatus = NotHit }

    let ship =
        { Points = [ p1; p2 ]
          Status = Alive }

    ship

// TODO: simply - a lot of duplicated code
let setShipCoordinates (game: Game, cp: CoordPair) =
    match game.Status with
    | SetupShips 1 ->
        let ship = createShipFromCoordPair cp

        let board =
            { game.HumanBoard with
                  Ships = [ ship ]
                  ShipsDestroyed = 0 }

        let new_game =
            { game with
                  HumanBoard = board
                  Status = SetupShips 2 }

        new_game

    | SetupShips 2 ->
        let ship = createShipFromCoordPair cp
        let ships = List.append game.HumanBoard.Ships [ ship ]
        let board = { game.HumanBoard with Ships = ships }

        let new_game =
            { game with
                  HumanBoard = board
                  Status = Running }
        ConsoleHelper.drawBoards new_game
        new_game

    | _ ->
        ConsoleHelper.drawBoards game
        game

let showShips (game: Game) =
    ConsoleHelper.drawShips (game)
    game

// Human player sets new ShipPoints a provided CoordPair if CoordPair is valid
let set (game: Game, cp: CoordPair) =
    if (isValidGameCoord (game, cp.c1)) && (isValidGameCoord (game, cp.c2)) && (isValidShipPair (game, cp)) then
        printfn ("New ship at (%c%d, %c%d)") cp.c1.X cp.c1.Y cp.c2.X cp.c2.Y
        setShipCoordinates (game, cp)
    else
        printfn ("Invalid coordinates (%c%d, %c%d)") cp.c1.X cp.c1.Y cp.c2.X cp.c2.Y
        printfn ("Try again!")
        game

let update (msg: Message) (game: Game): Game =
    match msg with
    | Set cp -> set (game, cp)
    | Try c -> tryHitAt (game, c)
    | ShowShips -> showShips (game)
