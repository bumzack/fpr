module DomainFunctions

open Domain

let initNewGame (size: int, computerShips: Ship list): Game =

    let cntFields = size * size

    let fieldList1 =
        [ for a in 1 .. cntFields do
            yield (NotAttempted) ]

    let fieldList2 =
        [ for a in 1 .. cntFields do
            yield (NotAttempted) ]

    let board1 =
        { Fields = fieldList1
          Size = size
          ShipsDestroyed = 0
          Ships = [] }

    let board2 =
        { Fields = fieldList2
          Size = size
          ShipsDestroyed = 0
          Ships = computerShips }

    let g =
        { HumanBoard = board1
          ComputerBoard = board2
          Status = SetupShips 1
          Size = size }

    g

let iterateShipPoints =
    fun (c: Coord) (shipPoint: ShipPoint) ->
        if shipPoint.Coord = c then
            printfn "Hit"
            { shipPoint with PointStatus = ShipHit }
        else
            shipPoint

let computerMove (humanBoard: Board): Coord =
    // Find Unattempted Fields --> for this we will probably need to extend the Field Type with a Coord
    // Randomly choose one of them and return its Coord
    let coord =
        { X = 'A'
          Y = 1 }
    coord

let iterateShips = fun (c: Coord) (ship: Ship) -> { ship with Points = List.map (iterateShipPoints c) ship.Points }

// can be used for both boards - so for human entered coordinates or random created coords for the computer attempts
let hitOnBoard (board: Board, c: Coord) =
    let newShips = List.map (iterateShips c) board.Ships
    let newBoard = { board with Ships = newShips }
    newBoard

// the human has entered a coordinate -> try to find out if
// it is a hit or not
let tryHitAt (game: Game, c: Coord) =
    // TODO
    // check if c is a valid coordinate depending on the size of the board
    // otherwise do nothing

    // update the fields and ships accordingly

    // if it is a hit  then return and the human can try again
    // if it is a miss; then it is the computers turn, then let the computer randomly choose an action until a miss occurs
    let newComputerBoard = hitOnBoard (game.ComputerBoard, c)
    let newHumanBoard = hitOnBoard (game.HumanBoard, c)

    let newGame =
        { game with
              ComputerBoard = newComputerBoard
              HumanBoard = newHumanBoard }
    newGame

// Filter function for Coord value tuples
let filterInvalidCoordTuples (gameSize: int) (coordTuple: int * int): bool =
    match coordTuple with
    | (x, y) when x >= 0 && y >= 1 && x < gameSize && y < gameSize + 1 -> true
    | _ -> false

let mapValidCoordTuples (game: Game) (x: int, y: int): Coord =
    let boardCharacterRange = Domain.getCharacterRangeForBoard game.HumanBoard
    { X = boardCharacterRange.[x]
      Y = y }

// Check if CoordPair is valid
let isValidShipPair (game: Game, coords: CoordPair) =
    let boardCharacterRange = Domain.getCharacterRangeForBoard game.HumanBoard
    let positionCharacter = coords.c1.X
    let characterIndex = List.findIndex (fun element -> positionCharacter = element) boardCharacterRange
    let positionInteger = coords.c1.Y

    let allTuples =
        [ (characterIndex, positionInteger - 1)
          (characterIndex, positionInteger + 1)
          (characterIndex - 1, positionInteger)
          (characterIndex + 1, positionInteger) ]

    allTuples
    |> List.filter (filterInvalidCoordTuples game.Size)
    |> List.map (mapValidCoordTuples game)
    |> List.contains coords.c2

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


// the human has entered a coordinate -> try to find out if
// it is a hit or not
let set (game: Game, cp: CoordPair) =
    if not (isValidShipPair (game, cp)) then
        printfn ("this is not a valid pair of coordinates for a ship p1 = %c%d, p2 = %c%d") cp.c1.X cp.c1.Y cp.c2.X
            cp.c2.Y
        printfn ("please try again")
        game
    else
        printfn ("Added new ship at coord  p1 = %c%d, p2 = %c%d") cp.c1.X cp.c1.Y cp.c2.X cp.c2.Y
        setShipCoordinates (game, cp)

let update (msg: Message) (game: Game): Game =
    match msg with
    | Set cp -> set (game, cp)
    | Try c -> tryHitAt (game, c)
    | ShowShips -> showShips (game)
