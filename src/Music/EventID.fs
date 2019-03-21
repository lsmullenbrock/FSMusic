module EventID

/// Tracks identity of an Event
/// @HACK: come up with more robust implementation
type EventID = EventID of int
    
let defaultEventID = EventID 0

let inline private (+.) (EventID a) (EventID b) = EventID(a + b)

/// Singleton to generate IDs for Events
type EventIDManager private () =
        
    let mutable currentID = defaultEventID
    static let instance = EventIDManager()

    static member Instance = instance

    /// Operator definition

    /// Simply increments IDs for now
    member private __.advanceID () =
        currentID <- currentID +. (EventID 1)

    member this.generateID () =
        let cur = currentID
        this.advanceID ()
        /// return ID
        cur

    member __.getCurrentID () = currentID