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
let ``Newton interpolation computes correct values for given points`` () =
    let points = [ (0.0, 0.00); (1.571, 1.0) ]
    let xValues = [ 0.0; 1.571 ]
    let expected = [ 0.00; 1.0 ]
    let coeffs = dividedDifferences points

    let results =
        xValues
        |> List.map (fun x -> newtonInterpolation points coeffs x)

    Assert.AreEqual(expected, results)
