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

      let literal =
        produce jnull  Null <|>
        produce jtrue  True <|>
        produce jfalse False
        |> map MakeLiteral

      let value, valueRef = bootstrap ()

      let field : (string * Value) Parser = 
        (jstring .>> nameValSep) .>>. value

      let listOf item =
        manySep item listSep

      let array =
        listOf value
        |> within beginArray endArray
        |> map MakeArray

      let object = 
        listOf field
        |> within beginObject endObject
        |> map MakeObject

      let toplevel =
        object <|> array
        |> map MakeRoot

      let numberValue = 
        jnumber 
        |> map MakeNumber

      let stringValue = 
        jstring 
        |> map MakeString

      valueRef :=
        token <| (literal <|> toplevel <|> numberValue <|> stringValue)

      let root = toplevel