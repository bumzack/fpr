module Ships

open Domain

// TODO: dummy implementation replace with ships created from user input
let readShipsFromHuman() =
    let c =
        { X = 'A'
          Y = 2 }
    let p11 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'A'
          Y = 3 }
    let p12 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'A'
          Y = 3 }
    let p13 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'D'
          Y = 4 }
    let p14 =
        { Coord = c
          PointStatus = NotHit }

    let ship11 =
        { Points = [ p11; p12 ]
          Status = Alive }

    let ship12 =
        { Points = [ p13; p14 ]
          Status = Alive }

    let ships = [ ship11; ship12 ]

    ships



// TODO: dummy implementation replace with ships created at random positions
let createRandomShips() =
    let c =
        { X = 'A'
          Y = 3 }

    let p21 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'C'
          Y = 3 }
    let p22 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'E'
          Y = 4 }
    let p23 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'F'
          Y = 4 }
    let p24 =
        { Coord = c
          PointStatus = NotHit }


    let ship21 =
        { Points = [ p21; p22 ]
          Status = Alive }

    let ship22 =
        { Points = [ p23; p24 ]
          Status = Alive }

    let ships = [ ship21; ship22 ]

    ships
