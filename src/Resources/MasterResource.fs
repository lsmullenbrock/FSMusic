/// Must figure out how to make these usable to draw to Canvas in a relatively easy manner.
[<AutoOpen>]
module MasterResource

open FSharp.Configuration

type GlyphResources = ResXProvider<"src/Resources/GlyphResources.resx">
type FontResources = ResXProvider<"src/Resources/FontResources.resx">
type TextResources = ResXProvider<"src/Resources/TextResources.resx">