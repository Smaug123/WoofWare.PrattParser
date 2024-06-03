namespace PrattParser.Test

open NUnit.Framework
open PrattParser
open ApiSurface

[<TestFixture>]
module TestSurface =
    let assembly = typedefof<Parser<_, obj, obj>>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () = ApiSurface.assertIdentical assembly

    [<Test>]
    [<Explicit "Not yet published">]
    // https://github.com/nunit/nunit3-vs-adapter/issues/876
    let CheckVersionAgainstRemote () =
        MonotonicVersion.validate assembly "WoofWare.PrattParser"

    [<Test ; Explicit>]
    let ``Update API surface`` () =
        ApiSurface.writeAssemblyBaseline assembly

    [<Test>]
    let ``Ensure public API is fully documented`` () =
        DocCoverage.assertFullyDocumented assembly
