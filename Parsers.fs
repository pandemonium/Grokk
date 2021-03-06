namespace Grokk

  open System


  [<AutoOpen>]
  module Function =
    
    let konst a        = fun _ -> a
    
    let untuple f a b  = f (a, b)
    
    let tuple f (a, b) = f a b

    let cons x xs = x::xs


  type Parser<'a> = 
    Input -> Output<'a>

  and Input =
    { data   : char array
      start  : int
      finish : int
      at     : int
    }

  and Output<'a> =
    | Yes of 'a * Input
    | No  of Because * Input

  and Because =
    | Expected of string
    | NotThatThing
    | EndOfInput

  
  module Input =
  
    let from (text: string) =
      { data   = text.ToCharArray ()
        start  = 0
        at     = 0
        finish = text.Length
      }

    let available input = 
      input.at < input.finish

    let advanceWith delta input =
      { input with at = input.at + delta }

    let advance =
      advanceWith 1

    let consumeWhen predicate input =
      if available input then 
        let c = input.data.[input.start + input.at]

        if predicate c 
          then Yes (c, advance input)
          else No (NotThatThing, input)
      else 
        No (EndOfInput, input)

    let consume =
      consumeWhen <| konst true

    let consumes length input =
      let start = input.start + input.at
      if start + length <= input.finish
        then 
          let slice = input.data.[start .. (start + length - 1)]
          Yes (slice, advanceWith length input) 
        else        
          No (EndOfInput, input)

    let position input = 
      Yes (input.at, input)

  
  module Output =
  
    let fold no yes = function
      | Yes (a, b) -> yes (a, b)
      | No  (a, b) -> no  (a, b)

    let map f = 
      fold No (fun (a, b) -> Yes <| (f a, b))

    let bind f =
      fold No f 

  
  module Parsers =

    let yes thing = fun input -> 
      Yes (thing, input)

    let no aThing = fun input ->
      No (Expected aThing, input)

    let expectedAThing = fun input ->
      No (NotThatThing, input)

    let map f parser = 
      parser >> Output.map f

    let bind f parser =
      parser >> Output.fold No (tuple f)

    let apply fa a =
      fa |> bind (fun f -> map f a)

    let zipWith f p q =
      bind (fun p' ->
        map (fun q' -> f p' q') q
      ) p

    let zip p q = 
      zipWith (fun p' q' -> p', q') p q

    let zipA a b =
      zipWith (fun a' _ -> a') a b

    let zipB a b =
      zipWith (fun _ b' -> b') a b

    let produce p x =
      zipB p <| yes x

    let skip p =
      produce p ()

    let position : int Parser =
      Input.position


    let suchThat qualifies (parser: 'a Parser) = fun input ->
      bind (fun theThing ->
        if qualifies theThing 
          then yes theThing
          else konst <| expectedAThing input
      ) parser input

    let alternative (p: 'a Parser) (q: 'a Parser) : 'a Parser =
      p >> Output.fold (fun (_, p') -> q p') Yes

    let maybe (p: 'a Parser) : 'a option Parser =
      alternative
        <| map Some p
        <| yes None  

    let rec many (p: 'a Parser) : 'a list Parser =
      p
      |> bind (fun theThing ->
           many p
           |> map (fun stuff -> theThing::stuff)
         )
      >> Output.fold (fun (_, b) -> Yes ([], b)) Yes

    let manySep (p: 'a Parser) (sep: 'b Parser) : 'a list Parser =
      maybe p 
      |> bind (function
        | Some p' -> 
          zipB sep p
          |> many
          |> map (fun stuff -> p' :: stuff)
        | None    -> 
          yes []
      )

    let within popen pclose (pbody: 'a Parser) : 'a Parser  =
      zipA
        <| zipB popen pbody
        <| pclose

    let bootstrap () : 'a Parser * 'a Parser ref =
      let hcp = fun input -> Yes (failwith "must initialize ref", input)
      let future = ref hcp

      (fun input -> !future input), future

    let run input (p: 'a Parser) = p input


    module Operators =

      let (|>>) fa f = map f fa

      let (>>=) fa f = bind f fa

      let (<*>) = apply

      let (.>>) = zipA

      let (>>.) = zipB

      let (.>>.) = zip

      let (<|>) = alternative


    module Chars =

      let anyChar : Parser<char> =
        Input.consume

      let satisfies p =
        anyChar |> suchThat p

      let anyOf selection =
        let set = Set.ofSeq selection
        satisfies set.Contains

      let noneOf selection =
        let set = Set.ofSeq selection
        satisfies (not << set.Contains)

      let charLiteral c =
        satisfies ((=) c)

      let digit = 
        anyOf <| seq { '0' .. '9' }

      let anyText length =
        Input.consumes length

      let text (t: string) =
        anyText t.Length
        |> map String
        |> suchThat ((=) t)

      let whiteSpace = 
        anyOf " \t\r\n"

      let delimitedText (delimiter: string) =
        let quoted =
          many (noneOf delimiter)
          |> map String.Concat

        within
        <| text delimiter 
        <| text delimiter 
        <| quoted


    module Numbers =
      
      open Operators

      let parseInt (text: string) =
        match Int32.TryParse text with
          | true, number -> yes number
          | false, _     -> no <| sprintf "%s is not a number" text

      let integer =
        Chars.digit
        |> many
        |> map String.Concat
        |> bind parseInt

      let parseDecimal (text: string) =
        match Decimal.TryParse text with
          | true, number -> yes number
          | false, _     -> no <| sprintf "%s is not a number" text

      let floatingPoint =
        Chars.digit <|> Chars.charLiteral '.'
        |> many
        |> map String.Concat
        |> bind parseDecimal
        