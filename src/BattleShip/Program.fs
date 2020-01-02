[<EntryPoint>]
let main argv =
    printfn "Welcome to BattleShip!"
    printfn "Please enter your commands to interact with the system."
    printfn "Press CTRL+C to stop the program."
    printf "> "

    let HumanShips = Repl.readShipsFromHuman()
    let RandomComputerShips = Repl.createRandomShips()

    let game = Domain.initnewGame (5, HumanShips, RandomComputerShips)

    Domain.drawBoards(game)

    Repl.loop game
    0 // return an integer exit code
