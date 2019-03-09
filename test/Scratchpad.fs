#if COMPILED
module Scratch
#endif


type Foo = {x:int;y:int}

type Bar = {foo:Foo;z:int}

type Qux = {bar:Bar;foo:Foo;w:int}

let f1 = {x=1;y=2}

let b1 = {foo=f1;z=3}

let b2 = {b1 with z=4}

let q1 = {bar=b1;foo=f1;w=f1.x}

