[<EntryPoint>]
let main argv =
    printfn "================================="
    printfn "Welcome to BattleShip!"
    printfn "Press CTRL+C to stop the program."
    printfn "=================================x"
    printfn ""

    let humanShips = Ships.readShipsFromHuman()
    let randomComputerShips = Ships.createRandomShips()

    let game = Domain.initNewGame (5, humanShips, randomComputerShips)

    Domain.drawBoards(game)

    Repl.loop game
    0 // return an integer exit code
