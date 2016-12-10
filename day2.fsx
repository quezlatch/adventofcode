#r "./packages/Fuchu/lib/Fuchu.dll"

module BathroomKeypad =
    open System

    let private keypad = 
        [
            "  1  "
            " 234 "
            "56789"
            " ABC "
            "  D  "
        ] |> array2D

    let private centrePoint = (0,2)
    let private move c (x,y) =
        match c with
            | 'U' -> (x,y-1)
            | 'D' -> (x,y+1)
            | 'L' -> (x-1,y)
            | 'R' -> (x+1,y)
            | _ -> failwith "Up, down, left, right"
    let private getKey (x,y) = keypad.[y,x]
    let private isValid = function
        | _,y when y < 0 || y >= 5 ->
            false
        | x,_ when x < 0 || x >= 5 ->
            false
        | xy -> getKey xy <> ' '

    let rec private findDigit xy line = 
        match line with
        | c::tail -> 
            let nxy = move c xy
            findDigit (if isValid nxy then nxy else xy) tail
        | [] -> xy

    let private toCharList (s:string) = [for c in s.Trim() -> c]

    let getAccessCode input =
        input    
        |> Seq.map toCharList
        |> Seq.filter (not << List.isEmpty)
        |> Seq.scan findDigit centrePoint
        |> Seq.tail
        |> Seq.map getKey
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

testCase "bathroom keypad" (fun _ -> Assert.Equal("", "5DB3", BathroomKeypad.getAccessCode input))
|> run 
|> ignore

System.IO.File.ReadAllLines("day2-input.txt")
|> BathroomKeypad.getAccessCode
|> printfn "The bathroom code is %s"