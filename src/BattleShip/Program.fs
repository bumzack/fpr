[<EntryPoint>]
let main argv =
    printfn "Welcome to BattleShip!"
    printfn "Please enter your commands to interact with the system."
    printfn "Press CTRL+C to stop the program."
    printf "> "

    let humanShips = Repl.readShipsFromHuman()
    let randomComputerShips = Repl.createRandomShips()

    let game = Domain.initnewGame (5, humanShips, randomComputerShips)

    Domain.drawBoards(game)

    Repl.loop game
    0 // return an integer exit code
