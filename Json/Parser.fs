namespace Grokk.Sample

  open Grokk


  module Json =

    type Value =
      | Scalar         of Scalar
      | Aggregate      of Aggregate

    and Aggregate =
      | Object         of (string * Value) list
      | Array          of Value list

    and Scalar =
      | Number         of decimal
      | Text           of string
      | Special        of SpecialLiteral

    and SpecialLiteral =
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
        |>> Special

      let numberValue = 
        jnumber |>> Number

      let stringValue = 
        jstring |>> Text

      let scalar =
        specialLiteral  <|> numberValue <|> stringValue
        |>> Scalar

      let value, valueRef = bootstrap ()

      let field : (string * Value) Parser = 
        (jstring .>> nameValSep) .>>. value

      let listOf item =
        manySep item listSep

      let array =
        listOf value
        |> within beginArray endArray
        |>> Array

      let object = 
        listOf field
        |> within beginObject endObject
        |>> Object

      let aggregate =
        object <|> array
        |>> Aggregate

      valueRef :=
        aggregate <|> scalar
        |> token

      let root = aggregate