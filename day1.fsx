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

    let calc (heading,x,y) (dir: string) =
        let newHeading = dir.[0] |> turn heading
        let steps = dir.Substring(1) |> Int32.Parse
        let nx, ny =
            match newHeading with
                | North -> (x,y+steps)
                | East -> (x+steps,y)
                | South -> (x,y-steps)
                | West -> (x-steps,y)
        (newHeading, nx, ny)

    let getCoord (i:string) = 
        let _,x,y = i.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) 
                    |> Seq.scan calc (North,0,0) |> Seq.last
        (x,y)

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