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

      let token (p: 'a Parser) : 'a Parser = 
        p .>> Chars.whiteSpace

      let quote       = Chars.charLiteral '"'

      let stringChar  = Chars.noneOf "\""

      let equals      = token <| Chars.charLiteral '='

      let beginObject = token <| Chars.charLiteral '{'

      let endObject   = token <| Chars.charLiteral '}'

      let beginArray  = token <| Chars.charLiteral '['

      let endArray    = token <| Chars.charLiteral ']'

      let jstring     = token <| Chars.delimitedText "\""

      let jnumber     = token <| Numbers.floatingPoint

      let jnull       = token <| Chars.text "null"

      let jtrue       = token <| Chars.text "true"

      let jfalse      = token <| Chars.text "false"

      let literal =
        produce jnull  Null <|>
        produce jtrue  True <|>
        produce jfalse False
        |> token
        |> map MakeLiteral

      let value, valueRef = bootstrap ()

      let field : (string * Value) Parser = 
        (jstring .>> equals) .>>. value

      let listOf item =
        manySep item
        <| Chars.charLiteral ','

      let array =
        listOf value
        |> within beginArray endArray
        |> map MakeArray

      let object = 
        listOf field
        |> within beginObject endObject
        |> map MakeObject

      let root =
        object <|> array
        |> map MakeRoot

      let numberValue = 
        jnumber 
        |> token
        |> map MakeNumber

      let stringValue = 
        jstring 
        |> token
        |> map MakeString

      valueRef := 
        literal <|> root <|> numberValue <|> stringValue
