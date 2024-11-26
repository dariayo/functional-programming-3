module Tests

open NUnit.Framework
open Program

[<Test>]
let ``Linear interpolation computes correct values`` () =
    let p1, p2 = (0.0, 0.0), (2.0, 2.0)
    let xValues = [ 0.0; 1.0; 2.0 ]
    let expected = [ 0.0; 1.0; 2.0 ]

    let results = xValues |> List.map (linearInterpolation p1 p2)
    Assert.AreEqual(expected, results)

[<Test>]
let ``Linear interpolation extrapolates correctly`` () =
    let p1, p2 = (1.0, 1.0), (3.0, 3.0)
    let xValues = [ 0.0; 4.0 ]
    let expected = [ 0.0; 4.0 ]

    let results = xValues |> List.map (linearInterpolation p1 p2)
    Assert.AreEqual(expected, results)

[<Test>]
let ``Chebyshev nodes are computed correctly`` () =
    let a, b, n = -1.0, 1.0, 5
    let nodes = chebyshevNodes a b n
    Assert.AreEqual(n, List.length nodes)

[<Test>]
let ``Divided differences compute coefficients correctly`` () =
    let points = [ (0.0, 1.0); (1.0, 2.0); (2.0, 0.0) ]
    let coeffs = dividedDifferences points
    Assert.AreEqual(points.Length, List.length coeffs)

[<Test>]
let ``Newton interpolation computes correct values`` () =
    let points = [ (0.0, 1.0); (1.0, 2.0); (2.0, 0.0) ]
    let coeffs = dividedDifferences points
    let xValues = [ 0.0; 1.0; 2.0 ]
    let expected = [ 1.0; 2.0; 0.0 ]

    let results =
        xValues
        |> List.map (newtonInterpolation points coeffs)

    Assert.AreEqual(expected, results)

[<Test>]
let ``Newton interpolation with many points`` () =
    let points =
        [ (0.0, 1.0)
          (1.0, 2.0)
          (2.0, 3.0)
          (3.0, 4.0)
          (4.0, 5.0) ]

    let coeffs = dividedDifferences points
    let xValues = [ 0.0; 1.5; 2.5; 4.0 ]
    let expected = [ 1.0; 2.5; 3.5; 5.0 ]

    let results =
        xValues
        |> List.map (newtonInterpolation points coeffs)

    Assert.AreEqual(expected, results)

[<Test>]
let ``Newton interpolation extrapolates correctly`` () =
    let points = [ (0.0, 0.0); (1.0, 2.0); (2.0, 4.0) ]
    let coeffs = dividedDifferences points
    let xValues = [ -1.0; 3.0 ]
    let expected = [ -2.0; 6.0 ]

    let results =
        xValues
        |> List.map (newtonInterpolation points coeffs)

    Assert.AreEqual(expected, results)
