module Grokk

  open System


  let konst a        = fun _ -> a
  let untuple f a b  = f (a, b)
  let tuple f (a, b) = f a b

  type Parser<'a> = Input -> Output<'a>

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

  
  module Parser =

    let yes thing = fun input -> 
      Yes (thing, input)

    let expected aThing = fun input ->
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

    let rec repeatedly (read: 'a Parser) : 'a list Parser =
      read
      |> bind (fun theThing ->
           repeatedly read
           |> map (fun things -> things @ [ theThing ])
         )
      >> Output.fold (fun (_, b) -> Yes ([], b)) Yes

    let suchThat qualifies (parser: 'a Parser) = fun input ->
      bind (fun theThing ->
        if qualifies theThing 
          then yes theThing
          else konst <| expectedAThing input
      ) parser input

    let alternative (p: 'a Parser) (q: 'a Parser) : 'a Parser =
      p >> Output.fold (fun (_, p') -> q p') Yes
    
    let run input (p: 'a Parser) = p input


    module Chars =

      let any : Parser<char> =
        Input.consume

      let letter c =
        any |> suchThat ((=) c)

      let anyText length =
        Input.consumes length
        |> map String

      let text (t: string) =
        anyText t.Length
        |> suchThat ((=) t)
