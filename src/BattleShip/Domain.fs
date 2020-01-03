module Domain

type FieldStatus =
    | Water
    | Hit

// field is one of field on the 5x5 board
type Field =
    | NotAttempted
    | Attempted of FieldStatus

// a ship consists of 2 or more points which can be either Hit or not hit
type ShipPointStatus =
    | NotHit
    | ShipHit

type Coord =
    { X: char
      Y: int }

type CoordPair =
    { c1: Coord
      c2: Coord }


type ShipPoint =
    { Coord: Coord
      PointStatus: ShipPointStatus }

type ShipStatus =
    | Alive
    | Destroyed

type Ship =
    { Points: ShipPoint list
      Status: ShipStatus }


type Board =
    { Fields: Field list
      Size: int
      ShipsDestroyed: int
      Ships: Ship list }

type Player =
    | Human
    | Computer


type GameStatus =
    | WonBy of Player
    | Running
    | SetupShips of int

type Game =
    { HumanBoard: Board
      ComputerBoard: Board
      Status: GameStatus
      Size: int }

let initNewGame (size: int, computerShips: Ship list): Game =

    let cntFields = size * size

    //TODO: 1.. or 0.. ?! guess 0..
    let fieldList1 =
        [ for a in 1 .. cntFields do
            yield (NotAttempted) ]


    // TODO: change to NotAttempted: just for debugging purposes
    let fieldList2 =
        [ for a in 1 .. cntFields do
            yield (Attempted Water) ]

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
    // otherwise do nothing and leave game.Turn

    // update the fields and ships accordingly

    // if it is a hit, then leave game.Turn as it is and the current player can try again
    // if it is a miss; then switch game.Turn

    // if it is the computers turn, then let the computer randomly choose an action



    // we return the new game
    game


// TODO: implement
let isValidShipPair (cp: CoordPair) =
    true


// TODO: simply - a lot of duplicated code
let setShipCoordinates (game: Game, cp: CoordPair) =
    match game.Status with
    | SetupShips 1 ->
        let p1 =
            { Coord = cp.c1
              PointStatus = NotHit }

        let p2 =
            { Coord = cp.c2
              PointStatus = NotHit }

        let ship =
            { Points = [ p1; p2 ]
              Status = Alive }

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
        let p1 =
            { Coord = cp.c1
              PointStatus = NotHit }
        let p2 =
            { Coord = cp.c2
              PointStatus = NotHit }

        let ship =
            { Points = [ p1; p2 ]
              Status = Alive }

        let ships = List.append game.HumanBoard.Ships [ ship ]
        let board =
            { game.HumanBoard with Ships = ships }
        let new_game = { game with HumanBoard = board }
        new_game

    | _ -> game


// the human has entered a coordinate -> try to find out if
// it is a hit or not
let set (game: Game, cp: CoordPair) =

    // TODO: remove printfn
    printfn ("set coord1 = %A") cp.c1
    printfn ("set coord2 = %A") cp.c2

    if not (isValidShipPair cp) then game
    else setShipCoordinates (game, cp)


// Message is a command entered by the user
type Message =
    | Set of CoordPair
    | Try of Coord

let update (msg: Message) (game: Game): Game =
    match msg with
    | Set cp -> set (game, cp)
    | Try c -> tryHitAt (game, c)
