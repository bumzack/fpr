[<EntryPoint>]
let main argv =
    printfn "================================="
    printfn "Welcome to BattleShip!"
    printfn "Press CTRL+C to stop the program."
    printfn "=================================x"
    printfn ""

    let humanShips = Repl.readShipsFromHuman()
    let randomComputerShips = Repl.createRandomShips()

    let game = Domain.initnewGame (5, humanShips, randomComputerShips)

    Domain.drawBoards(game)

    Repl.loop game
    0 // return an integer exit code
