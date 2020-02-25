open System
open Grokk
open Grokk.Parsers.Operators

type T<'a, 'b> = Q of 'a * 'b

type Textual = 
  | MakeT of string

[<EntryPoint>]
let main argv =
  let runWith input parser =
    Parsers.run input parser      
    |> printfn "Output: %A"
  
  runWith
  <| Input.from "Hi, mom"
  <| Parsers.Chars.anyChar

  runWith
  <| Input.from "Hi, mom"
  <| Parsers.Chars.charLiteral 'H'

  runWith
  <| Input.from "Hi, mom"
  <| Parsers.Chars.charLiteral 'X'

  runWith
  <| Input.from "aaab"
  <| Parsers.many (Parsers.Chars.charLiteral 'a')

  let aaa = Parsers.many (Parsers.Chars.charLiteral 'a')
  let b = Parsers.Chars.charLiteral 'b'
  let aaab = Parsers.zip aaa b
  let p = aaab |> Parsers.map (fun (prefix, suffix) -> suffix, prefix)

  runWith
  <| Input.from "aaab"
  <| p

  let q = p |> Parsers.map Q

  runWith
  <| Input.from "aaab"
  <| q

  let text = Parsers.Chars.text "Hi, mom"
  runWith
  <| Input.from "Hi, mom"
  <| text

  let x = Parsers.Chars.text "Hello, "
  let y = Parsers.Chars.text "world"

  let z = Parsers.zip x y

  runWith
  <| Input.from "Hello, world"
  <| z

  let hiMom = Parsers.Chars.text "Hi, mom"
  let helloWorld = Parsers.Chars.text "Hello, world"

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

  runWith
  <| Input.from "Hello, world"
  <| Parsers.maybe hiMom

  runWith
  <| Input.from "Hi, mom"
  <| Parsers.maybe hiMom

  runWith
  <| Input.from "\"Hi, mom\""
  <| Parsers.Chars.delimitedText "\""
  
  let someJson = 
    """
      { "hi": 
        [ "all",
          "of",
          "my",
          "moms"
        ]
      }
    """

  0