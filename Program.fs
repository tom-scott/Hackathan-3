open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open FSharp.Data

type H = FSharp.Data.Http
type J = FSharp.Data.JsonValue

[<EntryPoint>]
let main argv = 

(*
  choose
    [ GET >=> choose
        [ path "/hello" >=> OK "Hello GET"
          path "/goodbye" >=> OK "Good bye GET" ]
      POST >=> choose
        [ path "/hello" >=> OK "Hello POST"
          path "/goodbye" >=> OK "Good bye POST" ] ]


*)

// /commerce/bag/v3/bags/6c49a40f-9483-475b-970c-6aef41878544/product?expand=total,summary,address,spendLevelDiscount,deliveryoptions&language=en-GB

//pathScan "/commerce/bag/v3/bags/%s/product?expand=total,summary,address,spendLevelDiscount,deliveryoptions&language=en-GB" (fun bagId -> OK (sprintf "THis is bag %s" bag))

  let bagApiUrl = "https://api.asos.com/commerce/bag/v3/bags/"

  let headersNotToPassOn =
    [|
      "content-length"
      "host"
      "connection"
    |] |> Set.ofArray

  let (|RecordContainingKey|_|) (key:string) (json:J) =
    match json with
    | J.Record _ as record ->
      record.TryGetProperty key
    | _ -> None

  let addToBag =
    POST
      >=> pathScan "/commerce/bag/v3/bags/%s/product" (fun bagId -> (fun ctx ->
        async {
        
        let realUri = sprintf "%s%s/product" bagApiUrl bagId
        
        let headers =
          ctx.request.headers
          |> Seq.filter (fun (k,_) -> headersNotToPassOn.Contains (k.ToLower()) = false)
          |> Seq.toList
          
        let body = ctx.request

        let requestJson = System.Text.Encoding.UTF8.GetString(ctx.request.rawForm)

        let json = J.Parse requestJson

        let variantId =
          match json with
          | RecordContainingKey "variantId" (J.Number variantId) -> int variantId
          | _ -> failwith "Unexpected input"

        let query =
          ctx.request.query
          |> Seq.choose (fun (k,v) ->
            match v with
            | Some x -> Some (k, x)
            | None -> None)
          |> Seq.toList

        let! result = H.AsyncRequestString(realUri, query=query, headers=headers, httpMethod="POST", body = HttpRequestBody.BinaryUpload ctx.request.rawForm)

        let resultJson = J.Parse result

        let item =
          match resultJson with
          | RecordContainingKey "bag" (RecordContainingKey "items" (J.Array items)) ->
            items
            |> Seq.tryFind (fun x ->
            match x with
            | RecordContainingKey "variantId" (J.Number x) when int x = variantId -> true
            | _ -> false)
          | _ -> failwith "Unexpected response from ASOS API"
          |> Option.get
        
        return! OK result ctx
        }))


  let app =
    choose [
      addToBag
      GET >=> path "/hello" >=> Successful.OK "Hello World!"
      ]
    
  startWebServer defaultConfig app

  0 // return an integer exit code
