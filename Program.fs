open System
open Grokk
open Grokk.Parsers.Operators
open Grokk.Sample
open Grokk.Json.Decoders

type T<'a, 'b> = Q of 'a * 'b

type Textual = 
  | MakeT of string

type Person =
  { name:       string
    id:         Guid
    age:        int
    employment: string
    male:       bool
  }

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

  runWith
  <| Input.from "1,2,3,4,5"
  <| Parsers.manySep Parsers.Chars.digit (Parsers.Chars.charLiteral ',')

  let someJson = 
    """
      [{
        "Hi": null,
        "Hello": [ "world", 42]
      },
      {
        "Hi": false,
        "Hello": [ "world", 42.7]
      }]
    """

  runWith
  <| Input.from (someJson.Trim ())
  <| Json.Parser.root

  runWith
  <| Input.from "d3:cow3:moo4:spaml4:infoi32eee"  
  <| Bencode.Parse.value

  let someOtherJson =
    """
      [
        { "name": "Plucke Berrén",
          "id": "8beda949-c11c-4f1a-a46e-01bdb8c47447",
          "age": 47,
          "employment": "Nej!",
          "male": true
        },
        { "name": "Berra Pluckelius",
          "id": "552d53ba-9f3a-4ad2-a22e-dcc75901bd82",
          "age": 42,
          "employment": "Rojne Blom AB",
          "male": false
        }
      ]
    """

  runWith
  <| Input.from (someOtherJson.Trim ())
  <| Json.Parser.root
  
  let decodeWith (input: string) decoder =
    Decode.run (input.Trim ()) decoder
    |> printfn "Output: %A"

  let person : Person Decoder = decoder {
    let! name       = Decode.text
    let! id         = Decode.guid
    let! age        = Decode.number |> Decode.map int
    let! employment = Decode.text
    let! male       = Decode.boolean

    return
      { name       = name
        id         = id
        age        = age
        employment = employment
        male       = male
      }
  }

  let decodeListing : Person list Decoder = 
    Decode.array person

  decodeWith someOtherJson decodeListing

  0