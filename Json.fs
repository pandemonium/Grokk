namespace Grokk.Sample

  open Grokk


  module Json =

    type Value =
      | Number         of decimal
      | Text           of string
      | SpecialLiteral of Literal
      | Aggregate      of Aggregate

    and Aggregate =
      | Object         of (string * Value) list
      | Array          of Value list

    and Literal =
      | True
      | False
      | Null

    module Parser =

      open Parsers
      open Parsers.Operators

      let token (p: 'a Parser) : 'a Parser = 
        p .>> (many Chars.whiteSpace)

      let quote       = Chars.charLiteral '"'

      let stringChar  = Chars.noneOf "\""

      let charToken   = Chars.charLiteral >> token
      
      let nameValSep  = charToken ':'

      let beginObject = charToken '{'

      let endObject   = charToken '}'

      let beginArray  = charToken '['

      let endArray    = charToken ']'

      let listSep     = charToken ','

      let jstring     = token <| Chars.delimitedText "\""

      let jnumber     = token Numbers.floatingPoint

      let textToken   = Chars.text >> token

      let jnull       = textToken "null"

      let jtrue       = textToken "true"

      let jfalse      = textToken "false"

      let specialLiteral =
        produce jnull  Null <|>
        produce jtrue  True <|>
        produce jfalse False
        |> map SpecialLiteral

      let value, valueRef = bootstrap ()

      let field : (string * Value) Parser = 
        (jstring .>> nameValSep) .>>. value

      let listOf item =
        manySep item listSep

      let array =
        listOf value
        |> within beginArray endArray
        |> map Array

      let object = 
        listOf field
        |> within beginObject endObject
        |> map Object

      let toplevel =
        object <|> array
        |> map Aggregate

      let numberValue = 
        jnumber 
        |> map Number

      let stringValue = 
        jstring 
        |> map Text

      valueRef :=
        token <| (specialLiteral <|> toplevel <|> numberValue <|> stringValue)

      let root = toplevel