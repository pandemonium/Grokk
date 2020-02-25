namespace Grokk.Tests

  open Grokk


  module Json =

    type Value =
      | MakeNumber  of decimal
      | MakeString  of string
      | MakeLiteral of Literal
      | MakeRoot    of Root

    and Root =
      | MakeObject  of (string * Value) list
      | MakeArray   of Value list

    and Literal =
      | True
      | False
      | Null

    module Parser =

      open Parsers
      open Parsers.Operators

      let quote       = Chars.charLiteral '"'

      let stringChar  = Chars.noneOf "\""

      let equals      = Chars.charLiteral '='

      let beginObject = Chars.charLiteral '{'

      let endObject   = Chars.charLiteral '}'

      let beginArray  = Chars.charLiteral '['

      let endArray    = Chars.charLiteral ']'

      let jstring     = Chars.delimitedText "\""

      let jnumber     = Numbers.floatingPoint

      let jnull       = Chars.text "null"

      let jtrue       = Chars.text "true"

      let jfalse      = Chars.text "false"

      let literal =
        (zipB jnull  <| yes Null) <|>
        (zipB jtrue  <| yes True) <|>
        (zipB jfalse <| yes False)
        |> map MakeLiteral

      let value, valueRef = bootstrap ()

      let field = jstring .>>. value

      let object = 
        beginObject >>. (many field) .>> endObject
        |> map MakeObject

      let array =
        beginArray >>. (many value) .>> endArray
        |> map MakeArray

      let root =
        object <|> array
        |> map MakeRoot

      let numberValue = jnumber |> map MakeNumber

      let stringValue = jstring |> map MakeString

      valueRef := 
        literal <|> root <|> numberValue <|> stringValue
