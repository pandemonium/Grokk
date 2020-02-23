open System
open Grokk
open Grokk.Parser.Operators

type T<'a, 'b> = Q of 'a * 'b

type Textual = 
  | MakeT of string

[<EntryPoint>]
let main argv =
  let runWith input parser =
    Parser.run input parser      
    |> printfn "Output: %A"
  
  runWith
  <| Input.from "Hi, mom"
  <| Parser.Chars.anyChar

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

  let x = Parser.Chars.text "Hello, "
  let y = Parser.Chars.text "world"

  let z = Parser.zip x y

  runWith
  <| Input.from "Hello, world"
  <| z

  let hiMom = Parser.Chars.text "Hi, mom"
  let helloWorld = Parser.Chars.text "Hello, world"

  let pq = hiMom <|> helloWorld

  runWith
  <| Input.from "Hello, world"
  <| pq

  runWith
  <| Input.from "Hi, mom"
  <| pq

  runWith
  <| Input.from "Come at me bro."
  <| pq

  runWith
  <| Input.from "Hello, world"
  <| (x .>> y)

  runWith
  <| Input.from "Hello, world"
  <| (x >>. y)

  runWith
  <| Input.from "Hello, world"
  <| (x .>> y |>> MakeT)

  0