namespace Grokk

  open System


  [<AutoOpen>]
  module Function =
    
    let konst a        = fun _ -> a
    
    let untuple f a b  = f (a, b)
    
    let tuple f (a, b) = f a b


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

    let zipWith f p q =
      bind (fun p' ->
        map (fun q' -> f p' q') q
      ) p

    let zip p q = 
      zipWith (fun p' q' -> p', q') p q

    let zipA p q =
      zipWith (fun p' _ -> p') p q

    let zipB p q =
      zipWith (fun _ q' -> q') p q

    let suchThat qualifies (parser: 'a Parser) = fun input ->
      bind (fun theThing ->
        if qualifies theThing 
          then yes theThing
          else konst <| expectedAThing input
      ) parser input

    let rec many (p: 'a Parser) : 'a list Parser =
      p
      |> bind (fun theThing ->
           many p
           |> map (fun stuff -> theThing::stuff)
         )
      >> Output.fold (fun (_, b) -> Yes ([], b)) Yes

    let alternative (p: 'a Parser) (q: 'a Parser) : 'a Parser =
      p >> Output.fold (fun (_, p') -> q p') Yes

    let maybe (p: 'a Parser) : 'a option Parser =
      alternative
        <| map Some p
        <| yes None

    let enclosed popen pclose (pbody: 'a Parser) : 'a Parser  =
      zipA
        <| zipB popen pbody
        <| pclose

    let bootstrap () : 'a Parser * 'a Parser ref =
      let hcp _  = failwith "must initialize ref"
      let future = ref hcp

      !future, future

    let run input (p: 'a Parser) = p input


    module Operators =

      let (|>>) fa f = map f fa

      let (>>=) fa f = bind f fa

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
        |> map String

      let text (t: string) =
        anyText t.Length
        |> suchThat ((=) t)

      let whiteSpace = 
        anyOf " \t\r\n"

      let delimitedText (delimiter: string) =
        let quoted =
          many (noneOf delimiter)
          |> map String.Concat

        enclosed
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
        