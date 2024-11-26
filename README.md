# Лабораторная работа №3

`Шевченко Дарья Павловна  369053`

---

## Требования

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

  - какие алгоритмы использовать (в том числе два сразу);
  - частота дискретизации результирующих данных;

- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод

### Описание алгоритма - метод Ньютона с использованием полиномов Чебышева

1) Генерация узлов Чебышёва: На заданном интервале [a,b] вычисляются n узлов Чебышёва, которые равномерно распределяются, минимизируя ошибку интерполяции, особенно на краях интервала
2) Для каждой пары точек вычисляются разделённые разности, которые представляют собой коэффициенты для полинома Ньютона.
3) На основе вычисленных разделённых разностей строится интерполяционный полином Ньютона, который проходит через все данные точки.

---

## Ключевые элементы реализации

### Узлы Чебышева

```fsharp
let chebyshevNodes (a: float) (b: float) (n: int) =
    [ for i in 0 .. n - 1 ->
          0.5 * ((b - a) * cos ((2.0 * float i + 1.0) / (2.0 * float n) * Math.PI) + (b + a)) ]

```

### Интерполяция методом Ньютона

```fsharp
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

```

### Линейная интерполяция

```fsharp
let linearInterpolation (p1: float * float) (p2: float * float) (x: float) =
    let (x1, y1), (x2, y2) = p1, p2
    y1 + (y2 - y1) * (x - x1) / (x2 - x1)


if interpolationType = "linear"
               || interpolationType = "both" then
                printfn "Линейная интерполяция:"

                let results =
                    generateIntermediatePoints rangeStart rangeEnd samplingRate
                    |> Seq.map (fun x -> (x, linearInterpolation (x1, y1) (x2, y2) x))
                    |> Seq.toList

                results
                |> List.iter (fun (x, y) -> printf "%.2f\t%.2f\n" x y)
```

## Ввод / вывод программы

```zsh
cat data.csv | dotnet run both 1.0

Ввод первых двух точек (X Y через пробел):
Ввод 1-й точки (формат X Y): (0.0, 0.0)
Ввод 2-й точки (формат X Y): (1.571, 1.0)
Линейная интерполяция:
0.00    0.00
1.00    0.64
2.00    1.27
Интерполяция методом Ньютона с использованием полиномов Чебышева:
0.00    -0.21
1.00    0.69
2.00    1.59
Ввод 3-й точки (формат X Y): (3.142, 0.0)
Линейная интерполяция:
1.57    1.00
2.57    0.36
3.57    -0.27
Интерполяция методом Ньютона с использованием полиномов Чебышева:
1.57    1.21
2.57    0.31
3.57    -0.59
```

```zsh
dotnet run both 1.0 

Ввод первых двух точек (X Y через пробел):
0 0.00
Ввод 1-й точки (формат X Y): (0.0, 0.0)
1.571 1
Ввод 2-й точки (формат X Y): (1.571, 1.0)
Линейная интерполяция:
0.00    0.00
1.00    0.64
2.00    1.27
Интерполяция методом Ньютона с использованием полиномов Чебышева:
0.00    -0.21
1.00    0.69
2.00    1.59
3.142 0
Ввод 3-й точки (формат X Y): (3.142, 0.0)
Линейная интерполяция:
1.57    1.00
2.57    0.36
3.57    -0.27
Интерполяция методом Ньютона с использованием полиномов Чебышева:
1.57    1.21
2.57    0.31
3.57    -0.59
```

## Выводы

Я изучила такие приёмы, как работа с вводом и выводом через стандартные модули System.Console, что позволяет считывать данные с помощью Console.ReadLine() и выводить их с форматированием через printfn. Также я познакомилась с потоковой обработкой данных с использованием последовательностей (seq), что позволяет обрабатывать данные по мере их поступления, эффективно манипулируя ими с помощью таких функций, как Seq.map и List.iter, не перегружая память.
