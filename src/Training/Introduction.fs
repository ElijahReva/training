module Introduction
    open System
    
    
    let readM castFunc item = 
            match item with
            | null -> None
            | any -> Some (castFunc any)

    let read = Console.ReadLine >> readM int

    let limit() = 
        let limit = read()
        Seq.initInfinite (fun _ -> read())
            |> Seq.takeWhile (fun x -> x.IsSome)
            |> Seq.filter (fun x -> x < limit)
            |> Seq.choose id
            |> Seq.iter (printfn "%d")

    let position() =
        let mutable pos = 0
        Seq.initInfinite (fun _ -> pos <- pos + 1; read())
            |> Seq.takeWhile (fun x -> x.IsSome)
            |> Seq.choose id            
            |> Seq.filter (fun x -> pos % 2 = 0 )            
            |> Seq.iter (printfn "%d")
    
    let array() =
        let f n = Array.create n 1
        let input = Console.ReadLine()
        let n = int input
        printfn "%A" (f n)

    let reverse() =
        Seq.initInfinite (fun _ -> System.Console.ReadLine())
        |> Seq.takeWhile (fun x -> not (x = null))
        |> Seq.rev            
        |> Seq.iter (printfn "%s")

    let oddsum() =
        Seq.initInfinite (fun _ -> System.Console.ReadLine())
        |> Seq.takeWhile (fun x -> x <> null)
        |> Seq.map int
        |> Seq.filter (fun x -> x % 2 = 0 )        
        |> Seq.fold (fun acc x -> acc + x) 0         
        |> printfn "%d"

    let length() =
         Seq.initInfinite (fun _ -> System.Console.ReadLine())
            |> Seq.takeWhile (fun x -> x <> null)
            |> Seq.length        
            |> printfn "%d"

    let abs() =
         Seq.initInfinite (fun _ -> System.Console.ReadLine())
            |> Seq.takeWhile (fun x -> x <> null)
            |> Seq.map (int >> abs)        
            |> Seq.iter (printfn "%d")

    let ``e^x``() = 
        let fc n =
            let rec loop i acc =
                match i with
                | 0 | 1 -> acc
                | _ -> loop (i-1) (acc * i)
            loop n 1

        let calc item =
            [0..9] 
            |> Seq.map (fun x -> System.Math.Pow (item, (float x)) / (float (fc x)))
            |> Seq.reduce (+)

        System.Console.ReadLine() |> ignore
        Seq.initInfinite (fun _ -> System.Console.ReadLine())
            |> Seq.takeWhile (fun x -> (isNull >> not) x )            
            |> Seq.map (float >> calc)
            |> Seq.iter (printfn "%.4f")



