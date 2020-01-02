module Domain

type State = int


// Message is a command entered by the user
type Message =
    | Increment
    | Decrement
    | IncrementBy of int
    | DecrementBy of int

let init () : State =
    0

let update (msg : Message) (model : State) : State =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1
    | IncrementBy x -> model + x
    | DecrementBy x -> model - x
