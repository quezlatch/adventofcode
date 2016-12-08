#r "./packages/Fuchu/lib/Fuchu.dll"

module Distance =
    open System

    type Direction =
    | North
    | East
    | South
    | West

    //lookup would be better. or cycle through a proper enum...
    let turn heading leftRight =
        match leftRight with
        | 'R' ->
            match heading with
            | North -> East
            | East -> South
            | South -> West
            | West -> North
        | 'L' ->
            match heading with
            | North -> West
            | East -> North
            | South -> East
            | West -> South
        | _ -> failwith "can only turn left or right"

    let rec calc heading (x,y) (dirs: string list) =
        match dirs with
        | head::tail ->
            let newHeading = head.[0] |> turn heading
            let steps = head.Substring(1) |> Int32.Parse
            let newCoord =
                match newHeading with
                    | North -> (x,y+steps)
                    | East -> (x+steps,y)
                    | South -> (x,y-steps)
                    | West -> (x-steps,y)
            calc newHeading newCoord tail
        | [] -> (x,y)

    let getCoord (i:string) = 
        let dirs = i.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList       
        calc North (0,0) dirs

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
]
|> testList "day 1 tests"
|> run 
|> ignore

let instructions = System.IO.File.ReadAllText "day1-input.txt"
Distance.calculate instructions |> printfn "Easter Bunny HQ is %d blocks away"