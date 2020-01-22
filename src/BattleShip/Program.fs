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
    printfn "   Please choose where you want to put your ships using the 'Set position length direction' command"
    printfn "   (e.g.: Set C3 2 W)"
    printfn ""
    ConsoleHelper.drawHumanBoard game
    printfn ""

    let res = Repl.loop game
    0 // return an integer exit code
