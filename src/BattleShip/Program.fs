open System

[<EntryPoint>]
let main argv =

    printfn "================================="
    printfn "Welcome to BattleShip!"
    printfn "Press CTRL+C to stop the program."
    printfn "=================================x"
    printfn ""

    let size = 5
    // initialize in Main to avoid duplicates         https://stackoverflow.com/questions/6062191/f-getting-a-list-of-random-numbers
    let rnd = System.Random()

    // let humanShips = Ships.readShipsFromHuman()
    let randomComputerShips = Ships.createRandomShips(5, rnd)

    let game = Domain.initNewGame (5, randomComputerShips)

    ConsoleHelper.drawBoards(game)

    Repl.loop game
    0 // return an integer exit code
