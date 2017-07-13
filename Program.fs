open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open FSharp.Data

type H = FSharp.Data.Http
type J = FSharp.Data.JsonValue

(*
Known issues:
Only supporting one user at the moment
Multiple quantities (what happens if bag expires but user then adds same item again... we only have 1 item!)

*)

module DataStore =
  
  let mutable private data = Map.empty<int,J>

  let update (variantId:int) (json:J) =
    data <- data.Add(variantId, json)

[<EntryPoint>]
let main argv = 

  let withStatusCode s status: WebPart =
    fun ctx -> { ctx with response = { ctx.response with status = status; content = Bytes s }} |> succeed

  let bagApiUrl = "https://api.asos.com/commerce/bag/v3/bags/"

  let headersNotToPassOn =
    [|
      "content-length"
      "host"
      "connection"
    |] |> Set.ofArray

  let getHeaders (ctx:HttpContext) = 
    ctx.request.headers
    |> Seq.filter (fun (k,_) -> headersNotToPassOn.Contains (k.ToLower()) = false)
    |> Seq.toList

  let getQueryString (ctx:HttpContext) =
    ctx.request.query
    |> Seq.choose (fun (k,v) ->
    match v with
    | Some x -> Some (k, x)
    | None -> None)
    |> Seq.toList

  let (|RecordContainingKey|_|) (key:string) (json:J) =
    match json with
    | J.Record _ as record ->
      record.TryGetProperty key
    | _ -> None

  //let replaceItems (j:J) =
    

  let addToBag =
    POST
      >=> pathScan "/commerce/bag/v3/bags/%s/product" (fun bagId -> (fun ctx ->
        async {
        
        let realUri = sprintf "%s%s/product" bagApiUrl bagId
        
        let headers = getHeaders ctx
        let query = getQueryString ctx

        let requestJson = System.Text.Encoding.UTF8.GetString(ctx.request.rawForm)
        let json = J.Parse requestJson

        let variantId =
          match json with
          | RecordContainingKey "variantId" (J.Number variantId) -> int variantId
          | _ -> failwith "Unexpected input"


        let! result = H.AsyncRequestString(realUri, query=query, headers=headers, httpMethod="POST", body = HttpRequestBody.BinaryUpload ctx.request.rawForm, silentHttpErrors=true)

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

        DataStore.update variantId item
        
        return! withStatusCode (UTF8.bytes result) ctx.response.status ctx
        }))

  let passThrough =
    fun (ctx:HttpContext) ->
      async {
      let headers = getHeaders ctx
      let query = getQueryString ctx
      let realUri = "https://api.asos.com" + ctx.request.path
      let method = ctx.request.method.ToString()

      let! result = H.AsyncRequestString(realUri, query=query, headers=headers, httpMethod=method, body = HttpRequestBody.BinaryUpload ctx.request.rawForm, silentHttpErrors=true)
      return! withStatusCode (UTF8.bytes result) ctx.response.status ctx
      }

  let app =
    choose [
      //addToBag
      GET >=> path "/hello" >=> Successful.OK "Hello World!"
      passThrough
      ]
    
  startWebServer defaultConfig app

  0 // return an integer exit code
