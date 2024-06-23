namespace PrattParser

[<RequireQualifiedAccess>]
module internal Map =
    // For compat reasons, we target a very low FSharp.Core.
    let change k f m =
        match f (Map.tryFind k m) with
        | None -> Map.remove k m
        | Some v -> Map.add k v m
