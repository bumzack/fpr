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

// Get Coord of ShipPoint
let shipPointToCoord (shipPoint: ShipPoint): Coord = shipPoint.Coord

// Check if ShipPoint has givenn Coord
let shipPointHasCoord (coord: Coord) (shipPoint: ShipPoint) = shipPoint.Coord = coord

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

// Get character Range for provided board
let getCharacterRangeForBoard (board: Board): List<char> = [ 'A' .. 'Z' ].[0..(board.Size - 1)]

// Get a list of all shipPoints for the given board
let getShipPointsForBoard (board: Board): List<ShipPoint> = board.Ships.[0].Points @ board.Ships.[1].Points

// Get list of all Coord for all ShipPoints of provided board
let getShipPointCoordsForBoards (board: Board): List<Coord> =
    (getShipPointsForBoard board) |> List.map shipPointToCoord

// Return ShipPoint with the provided Coord from the provided Board
let getShipPointByCoordForBoard (coord: Coord, board: Board): ShipPoint =
    List.find (shipPointHasCoord coord) (getShipPointsForBoard board)

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
