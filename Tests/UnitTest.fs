module Tests

open NUnit.Framework
open Program


[<Test>]
let ``Test Linear Function`` () =
    let points = [ (0.0, 0.0); (1.57, 1.0) ]
    let step = 1.0

    let expected =
        [ (0.00, 0.00)
          (1.00, 0.64)
          (2.00, 1.27) ]

    let result = linear points step
    Assert.That(result, Is.EqualTo(expected).Within(0.1))

[<Test>]
let ``Test Chebyshev Interpolation`` () =
    let points = [ (0.0, 0.0); (1.57, 1.0) ]
    let step = 1.0


    let expected =
        [ (0.00, -0.21)
          (1.00, 0.69)
          (2.00, 1.59) ]

    let result = chebyshev points step
    Assert.That(result, Is.EqualTo(expected).Within(0.1))


[<Test>]
let ``Test Linear Function Two`` () =
    let points =
        [ (0.0, 0.0)
          (2.48, 4.56)
          (5.23, 6.12) ]

    let step = 1.0

    let expected =
        [ (0.00, 0.00)
          (1.00, 1.84)
          (2.00, 3.68)
          (3.0, 5.51) ]

    let result = linear points step
    Assert.That(result, Is.EqualTo(expected).Within(0.1))

[<Test>]
let ``Test Chebyshev Interpolation Two`` () =
    let points =
        [ (0.0, 0.0)
          (2.48, 4.56)
          (5.23, 6.12) ]

    let step = 1.0


    let expected =
        [ (0.00, 0.41)
          (1.00, -0.39)
          (2.00, 2.76)
          (3.0, 9.86) ]

    let result = chebyshev points step
    Assert.That(result, Is.EqualTo(expected).Within(0.1))
