module Domain


type FieldStatus =
    | Water
    | Hit

type FieldAttemptStatus =
    | NotAttempted
    | Attempted of FieldStatus

type Coord =
    { X: char
      Y: int }

// field is one of field on the 5x5 board
type Field =
    { Coord: Coord
      AttemptStatus: FieldAttemptStatus }

// a ship consists of 2 or more points which can be either Hit or not hit
type ShipPointStatus =
    | NotHit
    | ShipHit

type ShipPoint =
    { Coord: Coord
      PointStatus: ShipPointStatus }

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
      ShipPoints: ShipPoint list }

// Get character Range for provided board
let getCharacterRange (length: int): List<char> = [ 'A' .. 'Z' ].[0..(length - 1)]

// Get list of all Coord for all ShipPoints of provided board
let getShipPointCoordsForBoard (board: Board): List<Coord> =
    board.ShipPoints |> List.map (fun shipPoint -> shipPoint.Coord)

// Check if the provided board has a ShipPoint at the provided Coord
let boardHasShipPointAtCoord (board: Board, coord: Coord): bool =
    getShipPointCoordsForBoard board |> List.contains coord

// Return ShipPoint with the provided Coord from the provided Board
let getShipPointByCoordForBoard (coord: Coord, board: Board): ShipPoint =
    List.find (shipPointHasCoord coord) board.ShipPoints

// Return a list of all Fields with FieldAttemptStatus = NotAttempted
let getNotAttemptedFieldsForBoard (board: Board): List<Field> =
    board.Fields |> List.filter (fun field -> field.AttemptStatus = NotAttempted)

// Add a new ShipPoint to the provided board
let addShipPointToBoard (shipPoint: ShipPoint, board: Board): Board =
    { board with ShipPoints = board.ShipPoints |> List.append [ shipPoint ] }

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

// Check if the provided coordinate is even on the board
let isValidGameCoord (game: Game, coord: Coord): bool =
    let isValidBoardCharacter = getCharacterRange game.Size |> List.contains coord.X
    let isValidBoardInteger = coord.Y >= 1 && coord.Y <= game.Size
    isValidBoardInteger && isValidBoardCharacter

// Filter function for Coord value tuples
let filterInvalidCoordTuples (gameSize: int) (coordTuple: int * int): bool =
    match coordTuple with
    | (x, y) when x >= 0 && y >= 1 && x < gameSize && y < gameSize + 1 -> true
    | _ -> false

// Map Coord value Tuple to Coord
let mapValidCoordTuples (game: Game) (x: int, y: int): Coord =
    let boardCharacterRange = getCharacterRange game.Size
    { X = boardCharacterRange.[x]
      Y = y }

// Message is a command entered by the user
type Message =
    | Set of Coord
    | Try of Coord
    | ShowShips
