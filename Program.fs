open System
open Grokk

type T<'a, 'b> = Q of 'a * 'b

[<EntryPoint>]
let main argv =
  let runWith input parser =
    Parser.run input parser      
    |> printfn "Output: %A"
  
  runWith
  <| Input.from "Hi, mom"
  <| Parser.Chars.any

  runWith
  <| Input.from "Hi, mom"
  <| Parser.Chars.letter 'H'

  runWith
  <| Input.from "Hi, mom"
  <| Parser.Chars.letter 'X'

  runWith
  <| Input.from "aaab"
  <| Parser.repeatedly (Parser.Chars.letter 'a')

  let aaa = Parser.repeatedly (Parser.Chars.letter 'a')
  let b = Parser.Chars.letter 'b'
  let aaab = Parser.zip aaa b
  let p = aaab |> Parser.map (fun (prefix, suffix) -> suffix, prefix)

  runWith
  <| Input.from "aaab"
  <| p

  let q = p |> Parser.map Q

  runWith
  <| Input.from "aaab"
  <| q

  let text = Parser.Chars.text "Hi, mom"
  runWith
  <| Input.from "Hi, mom"
  <| text

  let z =
    Parser.zip
      <| Parser.Chars.text "Hello, "
      <| Parser.Chars.text "world"

  runWith
  <| Input.from "Hello, world"
  <| z

  let pq = 
    Parser.alternative
    <| Parser.Chars.text "Hi, mom"
    <| Parser.Chars.text "Hello, world"

  runWith
  <| Input.from "Hello, world"
  <| pq

  runWith
  <| Input.from "Hi, mom"
  <| pq

  runWith
  <| Input.from "Come at me bro."
  <| pq

  0