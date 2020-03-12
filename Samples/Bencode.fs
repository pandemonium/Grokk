namespace Grokk.Sample

  open System

  module Bencode =

    type Value =
      | Integer    of int
      | ByteString of char array
      | List       of Value list
      | Dictionary of (string * Value) list

    module Parse =

      open Grokk
      open Grokk.Parsers
      open Grokk.Parsers.Operators


      let token = Chars.charLiteral
      
      let separator = token ':'

      let beginInt = token 'i'

      let beginList = token 'l'

      let beginDict = token 'd'

      let endItAll = token 'e'

      let value, valueRef = bootstrap ()

      let integer =
        Numbers.integer
        |> within beginInt endItAll
        |>> Integer

      let byteStringContents =
        Numbers.integer .>> separator
        >>= Chars.anyText

      let byteString =
        byteStringContents |>> ByteString

      let list =
        many value
        |> within beginList endItAll
        |>> List

      let dictionary =
        let entry =
          byteStringContents |>> String .>>. value

        many entry
        |> within beginDict endItAll
        |>> Dictionary

      valueRef := 
        integer <|> byteString <|> list <|> dictionary