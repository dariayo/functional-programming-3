module Program

open System

let parseInputLine (line: string) =
    let parts = line.Split([| ' '; '\t'; ';' |], StringSplitOptions.RemoveEmptyEntries)
    (float parts.[0], float parts.[1])

let generateIntermediatePoints (startX: float) (endX: float) (step: float) = seq { startX..step..endX }

let linearInterpolation (p1: float * float) (p2: float * float) (x: float) =
    let (x1, y1), (x2, y2) = p1, p2
    y1 + (y2 - y1) * (x - x1) / (x2 - x1)

let dividedDifferences (points: (float * float) list) =
    let n = points.Length
    let table = Array.init n (fun i -> Array.create n 0.0)

    for i in 0 .. n - 1 do
        table.[i].[0] <- snd points.[i]

    for j in 1 .. n - 1 do
        for i in 0 .. n - j - 1 do
            table.[i].[j] <-
                (table.[i + 1].[j - 1] - table.[i].[j - 1])
                / (fst points.[i + j] - fst points.[i])

    [ for i in 0 .. n - 1 -> table.[0].[i] ]

let newtonInterpolation (points: (float * float) list) (coeffs: float list) (x: float) =
    let n = points.Length

    let rec computeResult i term acc =
        if i = n then
            acc
        else
            let newTerm = term * (x - fst points.[i - 1])
            let newAcc = acc + coeffs.[i] * newTerm
            computeResult (i + 1) newTerm newAcc

    computeResult 1 1.0 coeffs.[0]

let chebyshevNodes (a: float) (b: float) (n: int) =
    [ for i in 0 .. n - 1 ->
          0.5
          * ((b - a)
             * cos ((2.0 * float i + 1.0) / (2.0 * float n) * Math.PI)
             + (b + a)) ]

let linear (points: (float * float) list) (step: float) =
    if List.length points >= 2 then
        let (x1, y1), (x2, y2) = List.head points, List.tail points |> List.head
        let rangeStart = x1
        let rangeEnd = x2 + step

        let results =
            generateIntermediatePoints rangeStart rangeEnd step
            |> Seq.map (fun x -> (x, linearInterpolation (x1, y1) (x2, y2) x))
            |> Seq.toList

        results
    else
        []

let chebyshev (points: (float * float) list) (step: float) =
    if List.length points >= 2 then
        let (x1, y1), (x2, y2) = List.head points, List.tail points |> List.head
        let rangeStart = x1
        let rangeEnd = x2 + step

        let n = List.length points
        let nodes = chebyshevNodes x1 x2 n

        let chebyshevPoints =
            nodes
            |> List.map (fun x -> (x, snd (List.minBy (fun (x0, _) -> abs (x0 - x)) points)))

        let coeffs = dividedDifferences chebyshevPoints

        let results =
            generateIntermediatePoints rangeStart rangeEnd step
            |> Seq.map (fun x -> (x, newtonInterpolation chebyshevPoints coeffs x))
            |> Seq.toList

        results
    else
        []

let rec processInput (step: int) (window: (float * float) list) (interpolationType: string) (samplingRate: float) =
    match Console.ReadLine() with
    | null -> ()
    | line ->
        let point = parseInputLine line
        printfn "Ввод %d-й точки (формат X Y): %A" step point

        let newWindow =
            (window @ [ point ])
            |> List.skip (max 0 (List.length window - 1))

        if List.length newWindow >= 2 then
            let (x1, y1), (x2, y2) = List.head newWindow, List.tail newWindow |> List.head
            let rangeStart = x1
            let rangeEnd = x2 + samplingRate

            if interpolationType = "linear"
               || interpolationType = "both" then
                printfn "Линейная интерполяция:"
                let result = linear newWindow samplingRate

                result
                |> List.iter (fun (x, y) -> printf "%.2f\t%.2f\n" x y)

            if interpolationType = "chebyshev"
               || interpolationType = "both" then
                printfn "Интерполяция методом Ньютона с использованием полиномов Чебышева:"
                let results = chebyshev newWindow samplingRate

                results
                |> List.iter (fun (x, y) -> printf "%.2f\t%.2f\n" x y)


        processInput (step + 1) newWindow interpolationType samplingRate

let main (args: string []) =
    let interpolationTypes = [ "linear"; "chebyshev"; "both" ]

    if
        args.Length < 2
        || not (List.exists (fun x -> x = args.[1]) interpolationTypes)
    then
        printfn "Ошибка: Укажите тип интерполяции ('linear', 'chebyshev' или 'both')."
        Environment.Exit(1)

    let interpolationType = args.[1]

    let samplingRate =
        if args.Length > 2 then
            match System.Double.TryParse(args.[2]) with
            | true, value -> value
            | false, _ ->
                printfn "Ошибка: '%s' не является корректным шагом" args.[2]
                Environment.Exit(1)
                0.0
        else
            1.0

    printfn "Ввод первых двух точек (X Y через пробел):"
    processInput 1 [] interpolationType samplingRate

main (Environment.GetCommandLineArgs())
