module Ships

open Domain
open System

// TODO: dummy implementation replace with ships created at random positions
let createRandomShip (size: int, rnd: Random, chars: char list) =
    // ugly hack unless there is a google search result which provides an int->ASCII conversion function

    // TODO: if size = 5 are these valid coordinates?   1..5 ?
    let x_rnd = rnd.Next(1, size+1) - 1        // for indexing into the char list
    let y_rnd = rnd.Next(1, size+1)

    let c1 =
        { X = chars.[x_rnd]
          Y = y_rnd }

    // N/E/S/W     :-)
    let direction = rnd.Next(0, 4)

    // create second point for the ship in either of these 4 directions
    let c2 =
        match direction with
        | 0 ->            // North
            if y_rnd - 1 >= 0 then
                // if possible go north
                let c2 =
                    { X = chars.[x_rnd]
                      Y = y_rnd - 1 }
                c2
            else
                // or else go south
                let c2 =
                    { X = chars.[x_rnd]
                      Y = y_rnd + 1 }
                c2
        | 1 ->            // East
            if x_rnd + 1 < size then
                let c2 =
                    { X = chars.[x_rnd + 1]
                      Y = y_rnd }
                c2
            else
                let c2 =
                    { X = chars.[x_rnd - 1]
                      Y = y_rnd }
                c2
        | 2 ->            // South
            if y_rnd + 1 <   size then
                let c2 =
                    { X = chars.[x_rnd]
                      Y = y_rnd + 1 }
                c2
            else
                let c2 =
                    { X = chars.[x_rnd]
                      Y = y_rnd - 1 }
                c2
        | 3 ->            // west
            if x_rnd + 1 >= 0 then
                let c2 =
                    { X = chars.[x_rnd + 1]
                      Y = y_rnd }
                c2
            else
                let c2 =
                    { X = chars.[x_rnd - 1]
                      Y = y_rnd }
                c2
        | _ -> failwith "our random logic is flawed"

    let p1 =
        { Coord = c1
          PointStatus = NotHit }


    let p2 =
        { Coord = c2
          PointStatus = NotHit }

    let ship =
        { Points = [ p1; p2 ]
          Status = Alive }

    ship


let createRandomShips (size: int, rnd: Random) =
    // ugly hack unless there is a google search result which provides an int->ASCII conversion function
    let chars = [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N' ]

    // TODO: check if ships touch or overlap!
    let ship1 = createRandomShip (size, rnd, chars)
    let ship2 = createRandomShip (size, rnd, chars)


    // TODO: remove
    printfn ("create random ships %A    %A") ship1 ship2

    let ships = [ ship1; ship2 ]

    ships
