﻿module EventID

/// Tracks identity of an Event
/// Eventually come up with more robust implementation
type EventID = EventID of int
    
/// Singleton to generate IDs for Events
type EventIDManager private () =
    /// Localize operations on EventIDs to this class.
    let (+.) (EventID a) (EventID b) = EventID(a + b)
    static let incrID = EventID 1
    
    /// Default value of 0
    let defaultEventID = EventID 0

    /// Initialized to 0
    let mutable currentID = defaultEventID

    /// Singleton instance
    static let instance = EventIDManager()
    static member Instance = instance

    /// Simply increments IDs for now
    member private __.advanceID () =
        currentID <- currentID +. incrID

    /// Return ID and advance to next ID
    member this.generateID () =
        let cur = currentID
        this.advanceID ()
        /// return ID
        cur

    /// Getter
    member __.getCurrentID () = 
        currentID