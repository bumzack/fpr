[<EntryPoint>]
let main argv =

    printfn ""
    printfn "   ================================="
    printfn "   Welcome to BattleShip!"
    printfn "   Press CTRL+C to stop the program."
    printfn "   ================================="
    printfn ""

    let game = DomainFunctions.initNewGame 5

    printfn ""
    printfn "   Please choose where you want to put your ships"
    printfn "   (e.g.: Set A1)"
    printfn ""
    ConsoleHelper.drawFieldStatus game.HumanBoard
    printfn ""

    Repl.loop game
    0 // return an integer exit code
