#r "./packages/Fuchu/lib/Fuchu.dll"

module BathroomKeypad =
    open System

    let private keypad = array2D [ ['1';'2';'3'] ; ['4';'5';'6'] ; ['7';'8';'9']]

    let private centrePoint = (1,1)
    let private toCharList (s:string) = [for c in s.Trim() -> c]
    let rec private findDigit (x,y) line = 
        match line with
        | c::tail -> 
            let newXY =
                match c with
                | 'U' -> if y = 0 then (x,y) else (x,y-1)
                | 'D' -> if y = 2 then (x,y) else (x,y+1)
                | 'L' -> if x = 0 then (x,y) else (x-1,y)
                | 'R' -> if x = 2 then (x,y) else (x+1,y)
                | _ -> failwith "Up, down, left, right"    
            findDigit newXY tail
        | [] -> (x,y)

    let getAccessCode input =
        input    
        |> Seq.map toCharList
        |> Seq.filter (not << List.isEmpty)
        |> Seq.scan findDigit centrePoint
        |> Seq.tail
        |> Seq.map (fun (x,y) -> keypad.[y,x])
        |> Seq.toArray
        |> String

open Fuchu

let input =
    [
        "ULL  "
        "RRDDD"
        "LURDL"
        "UUUUD"
        ""
    ]

testCase "bathroom keypad" (fun _ -> Assert.Equal("", "1985", BathroomKeypad.getAccessCode input))
|> run 
|> ignore

System.IO.File.ReadAllLines("day2-input.txt")
|> BathroomKeypad.getAccessCode
|> printfn "The bathroom code is %s"