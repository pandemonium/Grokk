namespace Grokk.Json

  open Grokk

  module Encoders =
    
    type Encoder<'a> = 
      'a -> Output
    and
      Output = Value

    module Encoder =

      let contramap (f:'b -> 'a) (e: 'a Encoder) : 'b Encoder =
        e << f

      let rec asString = function
        | Scalar scalar -> 
          scalarAsString scalar
        | Aggregate (Object fields)  -> 
          fields
          |> List.map (tuple field)
          |> String.concat ","
          |> sprintf "{%s}" 
        | Aggregate (Array elements) -> 
          elements
          |> List.map asString
          |> String.concat ","
          |> sprintf "[%s]" 

      and field name value =
        sprintf "%s: %s" 
        <| quotedText name
        <| asString value

      and scalarAsString = function
        | Number n      -> sprintf "%f" n
        | Text t        -> quotedText t
        | Special True  -> "true"
        | Special False -> "false"
        | Special Null  -> "null"

      and quotedText = sprintf "\"%s\""

      let run x (encoder: 'a Encoder) : string = 
        encoder x
        |> asString


    module Encode =
      let number =
        Scalar << Number

      let text =
        Scalar << Text

      let boolean = function
        | true  -> Scalar <| Special True
        | false -> Scalar <| Special False

      let array e =
        List.map e
        >> Array
        >> Aggregate

      let object =
        Object >> Aggregate

      let optional (e: 'a Encoder) =
        Option.map e
        >> Option.defaultWith (fun () -> Scalar <| Special Null)

        
