[<AutoOpen>]
module MasterResource

open FSharp.Configuration

type GlyphsResource = ResXProvider<"src/Resources/GlyphResources.resx">
type FontResources = ResXProvider<"src/Resources/FontResources.resx">
type TextResources = ResXProvider<"src/Resources/TextResources.resx">