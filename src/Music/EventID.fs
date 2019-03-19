module EventID

/// @HACK: come up with better implementation
type EventID = int

/// Singleton to generate IDs for Events
type EventIDManager private () =
    let mutable currentID : EventID = 0
    static let instance = EventIDManager()

    static member Instance = instance

    member __.generateID() =
        let cur = currentID
        currentID <- currentID + 1
        cur

    member __.getCurrentID () = currentID