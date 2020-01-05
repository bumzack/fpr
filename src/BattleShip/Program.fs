open System

[<EntryPoint>]
let main argv =

    printfn "   ================================="
    printfn "   Welcome to BattleShip!"
    printfn "   Press CTRL+C to stop the program."
    printfn "   ================================="
    printfn ""

    let size = 5
    // initialize in Main to avoid duplicates         https://stackoverflow.com/questions/6062191/f-getting-a-list-of-random-numbers
    let rnd = System.Random()

    // let humanShips = Ships.readShipsFromHuman()
    let randomComputerShips = Ships.createRandomShips(size, rnd)

    let game = DomainFunctions.initNewGame (size, randomComputerShips)

    printfn "   Please choose where you want to put your ships"
    printfn "   (e.g.: Set A1 A2)"
    printfn ""
    ConsoleHelper.drawFieldStatus game.HumanBoard
    printfn ""

    Repl.loop game
    0 // return an integer exit code
