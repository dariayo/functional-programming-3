module Program

open System

type State = { Points: (float * float) list }

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
    match List.length points with
    | n when n >= 2 ->
        let (x1, y1), (x2, y2) = List.head points, List.tail points |> List.head
        let rangeStart = x1
        let rangeEnd = x2 + step

        let results =
            generateIntermediatePoints rangeStart rangeEnd step
            |> Seq.map (fun x -> (x, linearInterpolation (x1, y1) (x2, y2) x))
            |> Seq.toList

        (List.tail points, results)
    | _ -> (points, [])

let chebyshev (points: (float * float) list) (step: float) =
    match List.length points with
    | n when n >= 2 ->
        let (x1, _), (x2, _) = List.head points, List.tail points |> List.head
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

        (List.tail points, results)
    | _ -> (points, [])

let rec processInput (step: float) (states: (string * State) list) =
    match Console.ReadLine() with
    | null -> ()
    | line ->
        let point = parseInputLine line
        printfn "Ввод точки (X Y): %A" point

        let updatedStates =
            states
            |> List.map (fun (interpolationType, state) ->
                let newPoints = state.Points @ [ point ]

                let updatedPoints, results =
                    match interpolationType with
                    | "linear" -> linear newPoints step
                    | "newton" -> chebyshev newPoints step
                    | _ -> (newPoints, [])

                printfn "%A" interpolationType

                results
                |> List.iter (fun (x, y) -> printf "%.2f\t%.2f\t\n" x y)

                (interpolationType, { state with Points = updatedPoints }))

        processInput step updatedStates


let main (args: string []) =
    let interpolationArg = args.[1]
    let interpolationTypes = interpolationArg.Split(',')

    let step = Double.Parse(args.[2])

    let initialStates =
        interpolationTypes
        |> Array.map (fun t -> (t, { Points = [] }))
        |> List.ofArray

    printfn "Введите точки (X Y через пробел):"
    processInput step initialStates


main (Environment.GetCommandLineArgs())
