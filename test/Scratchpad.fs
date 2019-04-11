#if COMPILED
module Scratch
#endif

// How do I create compiletime constants (constexpr equivalent in F#)?
//type Test private () =
//    let one = 
//        printfn "initializing One"
//        1
//    static let instance = Test()
//    static member Instance = instance
//    member __.getOne () = one

//Test.Instance.getOne ()

// Solution:
//module Test2 =
//    let one = 
//        printfn "Test2: one initializing"
//        let x = 1
//        printfn "x = 1 hit, returning x"
//        x
//    let two =
//        printfn "Test2: two initializing"
//        2
//        printfn "2 hit, returning 2"
//        2

//Test2.one
//Test2.two