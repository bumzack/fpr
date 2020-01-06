module Ships

open Domain
open System

// TODO: dummy implementation replace with ships created at random positions
let createRandomShipPoint (size: int, chars: char list) =
    let rnd = System.Random()
    { Coord =
          { X = chars.[rnd.Next(1, size + 1) - 1]
            Y = rnd.Next(1, size + 1) }
      PointStatus = NotHit }

let createRandomShips (size: int): List<ShipPoint> =
    let validBoardCharacters = getCharacterRange size

    let ships =
        [ createRandomShipPoint (size, validBoardCharacters)
          createRandomShipPoint (size, validBoardCharacters)
          createRandomShipPoint (size, validBoardCharacters)
          createRandomShipPoint (size, validBoardCharacters) ]

    ships
