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

// can be used for both boards - so for human entered coordinates or random created coords for the computer attempts
let hitOnBoard (board: Board, c: Coord) =
    // or maybe even better a tuple (board, true) so the caller knows if there was a hit or not
    (board, true)

// the human has entered a coordinate -> try to find out if
// it is a hit or not
let tryHitAt (game: Game, c: Coord) =

    // TODO: remove printfn
    printfn ("tryhitat coord = %A") c

    // TODO
    // check if c is a valid coordinate depending on the size of the board
    // otherwise do nothing

    // update the fields and ships accordingly

    // if it is a hit  then return and the human can try again
    // if it is a miss; then it is the computers turn, then let the computer randomly choose an action until a miss occurs

    // we return the new game
    game


// TODO: implement
let isValidShipPair (cp: CoordPair) =
    true


let createShipFromCoordPair (cp: CoordPair ) =
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
        let board =
            { game.HumanBoard with Ships = ships }
        let new_game = { game with HumanBoard = board
                                   Status = Running  }
        new_game

    | _ -> game



let showShips (game: Game) =
    ConsoleHelper.drawShips(game)
    game


// the human has entered a coordinate -> try to find out if
// it is a hit or not
let set (game: Game, cp: CoordPair) =
    if not (isValidShipPair cp) then
        printfn("this is not a valid pair of coordinates for a ship p1 = %c%d, p2 = %c%d") cp.c1.X cp.c1.Y cp.c2.X cp.c2.Y
        printfn("please try again")
        game
    else
        printfn("added new ship at coord  p1 = %c%d, p2 = %c%d") cp.c1.X cp.c1.Y cp.c2.X cp.c2.Y
        setShipCoordinates (game, cp)

let update (msg: Message) (game: Game): Game =
    match msg with
    | Set cp -> set (game, cp)
    | Try c -> tryHitAt (game, c)
    | ShowShips -> showShips (game)
