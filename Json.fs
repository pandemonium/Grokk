namespace Grokk.Tests

  open Grokk


  module Json =

    type Value =
      | MakeNumber  of decimal
      | MakeString  of string
      | MakeLiteral of Literal
      | MakeRoot    of RootValue

    and RootValue =
      | MakeObject  of (string * Value) list
      | MakeArray   of Value list

    and Literal =
      | True
      | False
      | Null

    module Parser =

      open Parsers

      let whiteSpace = Chars.anyOf " \t\r\n"

      let quote = Chars.letter '"'

      let stringChar = Chars.noneOf "\""

      let equals = Chars.letter '='

      let beginObject = Chars.letter '{'
      
      let endObject = Chars.letter '}'
      
      let beginArray = Chars.letter '['
      
      let endArray = Chars.letter ']'

      let jnull = Chars.text "null"
      
      let jtrue = Chars.text "true"
      
      let jfalse = Chars.text "false"

      let x = 0