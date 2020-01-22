module Domain

type FieldShipStatus =
    | Water
    | Ship
    | ShipHit

type FieldAttemptStatus =
    | NotAttempted
    | Attempted

type Coord =
    { X: char
      Y: int }

type Field =
    { Coord: Coord
      AttemptStatus: FieldAttemptStatus
      ShipStatus: FieldShipStatus }


type Player =
    | Human
    | Computer

type GameStatus =
    | WonBy of Player
    | Running
    | SetupShips of int

type Direction =
    | North
    | East
    | South
    | West

type Ship = {
    length: int
    pos: Coord
    direction: Direction
}

type Board =
    { Fields: Field list
      Size: int
      Ships: Ship list }

type Game =
    { HumanBoard: Board
      ComputerBoard: Board
      Status: GameStatus
      Size: int
      RequiredShips: int list }

type Message =
    | Set of Ship
    | Try of Coord
    | ShowShips
