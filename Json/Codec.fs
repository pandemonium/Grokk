namespace Grokk.Json

  open System
  open Grokk


  module Decoders =

    type Decoder<'a> = 
      Input -> 'a Decoded
    and Input =
      Value
    and Decoded<'a> = 
      Result<'a, Reason>
    and Reason = 
      | Expected     of Thing
      | ParserFailed of Because * Grokk.Input
    and Thing =
      | Text
      | Number
      | Boolean
      | Array
      | Object
      | Field of string
      | Guid


    module Decoded =

      let fold whenOk whenError = function
        | Ok ok       -> whenOk ok
        | Error error -> whenError error

      let bind = Result.bind

      let map = Result.map

      let failed = Error

      let parserFailed reason = Error <| ParserFailed reason

      let expected thing = failed <| Expected thing

      let point (x: 'a) : 'a Decoded = Ok x

      let apply fa a =
        fa |> bind (fun f -> a |> map f)

      let (<*>) = apply

      let map2 (f: 'a -> 'b -> 'c) (p: 'a Decoded) (q: 'b Decoded) : 'c Decoded =
        point f <*> p <*> q

      let traverse (f: 'a -> 'b Decoded) (xs: 'a list) : 'b list Decoded =
        let inject = 
          map2 (fun acc x -> x::acc)

        xs
        |> List.map f
        |> List.fold inject (Ok [])


    module Thing =

      let rec show = function
        | Scalar literal      -> showLiteral literal
        | Aggregate aggregate -> showAggregate aggregate

      and showLiteral = function
        | Json.Number x  -> x.ToString ()
        | Json.Text x    -> x
        | Json.Special x -> x.ToString ()

      and showAggregate = function
        | Json.Object stuff -> 
          stuff 
          |> List.map (fun (name, value) -> sprintf "%s: %s" name <| show value)
          |> String.Concat
        | Json.Array stuff -> 
          stuff 
          |> List.map show
          |> String.Concat


    module Decode =
      let expect thing extract : 'a Decoder =
        extract >> function
        | Some thing -> Decoded.point thing
        | None       -> Error <| Expected thing

      let yes (x: 'a) : 'a Decoder =
        konst <| Decoded.point x
        
      let expected thing : 'a Decoder =
        konst <| Decoded.expected thing

      let bind (f: 'a -> 'b Decoder) (decoder: 'a Decoder) : 'b Decoder = fun input ->
        match decoder input with
        | Ok thing -> f thing input
        | Error e  -> Error e

      let map (f: 'a -> 'b) (decoder: 'a Decoder) : 'b Decoder =
        decoder >> Decoded.map f

      let apply (fa: ('a -> 'b) Decoder) (a: 'a Decoder) : 'b Decoder =
        bind (fun f -> map f a) fa

      let (<*>) = apply

      let map2 (f: 'a -> 'b -> 'c) (d0: 'a Decoder) (d1: 'b Decoder) : 'c Decoder =
        yes f <*> d0 <*> d1

      let text : string Decoder =
        expect Text (function
          | Scalar (Json.Text t) -> 
            Some t
          | _ -> 
            None
        )

      let number : decimal Decoder =
        expect Number (function
          | Scalar (Json.Number n) -> 
            Some n
          | _ -> 
            None
        )

      let boolean : bool Decoder =
        expect Boolean (function
          | Scalar (Special (True as x))
          | Scalar (Special (False as x)) -> 
            Some <| (x = True)
          | _ -> 
            None
        )

      let guid : Guid Decoder =
        text
        |> bind (Guid.TryParse >> function
            | true, guid -> yes guid
            | false, _   -> expected Guid
          )

      let optional (decoder: 'a Decoder) : 'a option Decoder =
        decoder 
        >> Decoded.fold (Some >> Ok) (fun _ -> Ok None)

      let array (decoder: 'a Decoder) : 'a list Decoder =
        expect Array (function
          | Aggregate (Json.Array xs) ->
            Some xs
          | _ ->
            None
        )
        >> Decoded.bind (Decoded.traverse decoder)

      let field name (decoder : 'a Decoder) : 'a Decoder =
        expect Object (function
          | Aggregate (Json.Object xs) ->
            Some xs
          | _ ->
            None
        )
        >> Decoded.bind (
            List.tryFind (fun (name', _) -> name = name')
            >> function
               | Some (_, thing) -> decoder thing
               | None            -> Decoded.expected <| Field name
        )

      let run input (decoder: 'a Decoder) =
        let input' = Input.from input
        let parsed = Parsers.run input' Parser.root

        let runDecoder (root, remainingInput) =
          decoder root

        parsed
        |> Output.fold Decoded.parserFailed runDecoder

    type DecoderBuilder () =
      member __.Bind(d, f)  = Decode.bind f d
      member __.Return(x) = Decode.yes x
        
    let decoder = DecoderBuilder ()

    module Encoders =
      let x = 1