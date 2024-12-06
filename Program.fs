module Program

open System

type State =
    { InterpolationType: string
      Points: (float * float) list }

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

        generateIntermediatePoints rangeStart rangeEnd step
        |> Seq.map (fun x -> (x, linearInterpolation (x1, y1) (x2, y2) x))
        |> Seq.toList
    | _ -> []

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

        generateIntermediatePoints rangeStart rangeEnd step
        |> Seq.map (fun x -> (x, newtonInterpolation chebyshevPoints coeffs x))
        |> Seq.toList
    | _ -> []

let rec processInput (step: float) (states: State list) =
    match Console.ReadLine() with
    | null -> ()
    | line ->
        let point = parseInputLine line
        printfn "Ввод точки (X Y): %A" point

        let updatedStates =
            states
            |> List.map (fun state ->
                let newPoints =
                    (state.Points @ [ point ])
                    |> List.skip (max 0 (List.length state.Points - 1))

                { state with Points = newPoints })

        updatedStates
        |> List.iter (fun state ->
            let interpolationFunctions =
                match state.InterpolationType with
                | "linear" -> [ ("Линейная интерполяция", linear) ]
                | "newton" -> [ ("Интерполяция методом Ньютона с использованием полиномов Чебышева", chebyshev) ]
                | "both" ->
                    [ ("Линейная интерполяция", linear)
                      ("Интерполяция методом Ньютона с использованием полиномов Чебышева", chebyshev) ]
                | _ -> []

            interpolationFunctions
            |> List.iter (fun (name, interpFunc) ->
                printfn "%s:" name
                let results = interpFunc state.Points step

                results
                |> List.iter (fun (x, y) -> printf "%.2f\t%.2f\n" x y)))

        processInput step updatedStates


let main (args: string []) =
    match args |> Array.tryItem 1 with
    | None ->
        printfn "Ошибка: Укажите типы интерполяции ('linear', 'newton', 'both')"
        Environment.Exit(1)
    | Some interpolationArg ->
        let interpolationTypes = interpolationArg.Split(',')

        let step =
            args
            |> Array.tryItem 2
            |> Option.bind (fun s ->
                if Double.TryParse(s) |> fst then
                    Some(Double.Parse(s))
                else
                    None)
            |> Option.defaultWith (fun () ->
                printfn "Ошибка: Укажите корректный шаг"
                Environment.Exit(1)
                0.0)

        let initialStates =
            interpolationTypes
            |> Array.map (fun t -> { InterpolationType = t; Points = [] })
            |> List.ofArray

        printfn "Введите точки (X Y через пробел):"
        processInput step initialStates


main (Environment.GetCommandLineArgs())
