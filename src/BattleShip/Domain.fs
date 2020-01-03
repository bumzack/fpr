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



// Message is a command entered by the user
type Message =
    | Set of CoordPair
    | Try of Coord
    | ShowShips
