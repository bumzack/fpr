module Repl

open System
open Parser

type ReplMessage =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string


let read (input: string) =
    match input with
    | TryAt v -> Domain.TryAt v |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed -> NotParsable input

open Domain
open Microsoft.FSharp.Reflection

let createHelpText(): string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")




let evaluate (update : Domain.Message -> Game -> Game) (game : Game) (msg : ReplMessage) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg game
        // TODO: better msg - or remove - or ?!?
        let message = "next try "
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (game, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (game, message)

let print (game: Game, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    printf "> "

    game

let rec loop (game : Game) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update game
    |> print
    |> loop



