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

type CoordPair =
    { c1: Coord
      c2: Coord }

// field is one of field on the 5x5 board
type Field =
    { Coord: Coord
      AttemptStatus: FieldAttemptStatus }

// Create a new Field from the provided Coord
let createNewFieldFromCoord (coord: Coord): Field =
    { Coord = coord
      AttemptStatus = NotAttempted }

// a ship consists of 2 or more points which can be either Hit or not hit
type ShipPointStatus =
    | NotHit
    | ShipHit

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
let getCharacterRange (length: int): List<char> = [ 'A' .. 'Z' ].[0..(length - 1)]
let getCharacterRangeForBoard (board: Board): List<char> = [ 'A' .. 'Z' ].[0..(board.Size - 1)]

// Get a list of all shipPoints for the given board
let getShipPointsForBoard (board: Board): List<ShipPoint> = board.Ships.[0].Points @ board.Ships.[1].Points

// Get list of all Coord for all ShipPoints of provided board
let getShipPointCoordsForBoard (board: Board): List<Coord> =
    (getShipPointsForBoard board) |> List.map shipPointToCoord

// Check if the provided board has a ShipPoint at the provided Coord
let boardHasShipPointAtCoord (board: Board, coord: Coord): bool =
    getShipPointCoordsForBoard board |> List.contains coord

// Return ShipPoint with the provided Coord from the provided Board
let getShipPointByCoordForBoard (coord: Coord, board: Board): ShipPoint =
    List.find (shipPointHasCoord coord) (getShipPointsForBoard board)

let getNotAttemptedFieldsForBoard (board: Board): List<Field> =
    board.Fields |> List.filter (fun field -> field.AttemptStatus = NotAttempted)

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
    let boardCharacterRange = getCharacterRangeForBoard game.HumanBoard
    { X = boardCharacterRange.[x]
      Y = y }

// Check if CoordPair is valid
let isValidShipPair (game: Game, coords: CoordPair) =
    let boardCharacterRange = getCharacterRangeForBoard game.HumanBoard
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

// Message is a command entered by the user
type Message =
    | Set of CoordPair
    | Try of Coord
    | ShowShips
