#r "./packages/Fuchu/lib/Fuchu.dll"

module Distance =
    open System

    type Direction =
    | North = 0
    | East = 1
    | South = 2
    | West = 3

    let turn heading leftRight =
        let offset =
            match leftRight with
            | 'R' -> 1
            | 'L' -> 3
            | _ -> failwith "can only turn left or right"
        (int heading |> (+) offset) % 4 |> enum<Direction> 

    let rec calc heading (x,y) (dirs: string list) =
        seq {
            match dirs with
            | head::tail ->
                let newHeading = head.[0] |> turn heading
                let steps = head.Substring(1) |> Int32.Parse
                let makeNewCoords =
                    match newHeading with
                        | Direction.North -> fun i -> (x,y+i)
                        | Direction.East -> fun i -> (x+i,y)
                        | Direction.South -> fun i -> (x,y-i)
                        | Direction.West -> fun i -> (x-i,y)
                        | _ -> failwith "bad direction"
                let newCoords = [ for i in 1..steps -> makeNewCoords i ]
                yield! newCoords
                yield! calc newHeading (Seq.last newCoords) tail
            | [] -> ()
        }

    let getCoord (i:string) = 
        let coords = i.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) 
                    |> Array.toList
                    |> calc Direction.North (0,0)
                    |> Seq.cache
        let grouped = coords |> Seq.groupBy id |> Map.ofSeq
        let isDuplicate c = Map.find c grouped |> Seq.length  > 1
        match coords |> Seq.tryFind isDuplicate with
        | Some xy -> xy
        | None -> Seq.last coords

    let calculate instructions = 
        let x,y = getCoord instructions
        abs x + abs y

open Fuchu

[
    testCase "get coordinates from instructions" (fun _ -> Assert.Equal("", (2,3), (Distance.getCoord "R2, L3")))
    [
        "R2, L3",5
        "R2, R2, R2",2
        "R5, L5, R5, R3",12    
    ]
    |> Seq.map (fun (instructions,distance) -> 
        testCase instructions (fun _ -> Assert.Equal("d", distance, Distance.calculate instructions)))
    |> testList "get distance"
    testCase "get distance with repeated locations" (fun _ -> Assert.Equal("", 4, (Distance.calculate "R8, R4, R4, R8")))
]
|> testList "day 1 tests"
|> run 
|> ignore

let instructions = System.IO.File.ReadAllText "day1-input.txt"
Distance.calculate instructions |> printfn "Easter Bunny HQ is %d blocks away"
