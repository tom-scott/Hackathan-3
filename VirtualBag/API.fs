﻿namespace VirtualBag

module API =
  open Suave
  open Suave.Filters
  open Suave.Operators
  open Suave.Successful
  open FSharp.Data

  
  [<Literal>]
  let sampleFile = __SOURCE_DIRECTORY__ + "/sampleStockResponse.json"
  type StockResponse = FSharp.Data.JsonProvider<sampleFile>

  let withStatusCode s status: WebPart =
    fun ctx -> { ctx with response = { ctx.response with status = status; content = Bytes s }} |> succeed

  let bagApiUrl = "https://api.asos.com/commerce/bag/v3/bags/"

  let (-) (a:Map<'a,'b>) (b:Map<'a,'c>) : Map<'a,'b> = Map (seq {
      for KeyValue(k, va) in a do
          match Map.tryFind k b with
          | Some vb -> ()
          | None    -> yield k, (va) })

  let headersNotToPassOn =
    [|
      "content-length"
      "host"
      "connection"
      "proxy-connection"
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

  let transformRecordWithKey (key:string) (f: Option<J> -> J) (record:J) =
    match record with
    | J.Record xs ->
      let existing = xs |> Map.ofSeq
      let item = existing.TryFind key
      let newItem = f item
      let updatedList = existing.Add(key, newItem)
      J.Record (updatedList |> Map.toArray)
    | _ -> failwith "Was expecting a record"

  let replaceItems (j:J) =
    let newItems = DataStore.getItems ()
    j |> transformRecordWithKey "bag" (fun bag -> transformRecordWithKey "items" (fun _ -> newItems) bag.Value)  

  let getBagUri bagId = sprintf "https://api.asos.com/commerce/bag/v3/bags/%s" bagId

  let getRealBagContents bagId query headers =
    let uri = getBagUri bagId
    async {
    let! realBagContentsStr =
      H.AsyncRequestString(uri, query=query, headers=headers, httpMethod = "GET", silentHttpErrors=true)

    let realBagContentsJson = J.Parse realBagContentsStr

    let realItems =
      match realBagContentsJson with
      | RecordContainingKey "bag" (RecordContainingKey "items" (J.Array items)) -> items
      | _ -> [||]
      |> Seq.choose (fun json ->
        match json with
        | RecordContainingKey "id" (J.String bagItemId) -> Some(bagItemId, json)
        | _ -> None)
      |> Map.ofSeq

    return realItems
    }


  let getStockForVariants (variantIds:seq<int>) =
    if variantIds |> Seq.isEmpty
      then async.Return Map.empty
      else
        let variantIdsStr = variantIds |> Seq.map (sprintf "%d") |> String.concat ","
        let url = sprintf "http://asos-prod-eun-pdt-catalogue-api.cloudapp.net/product/catalogue/V2/variants?variantIds=%s&Store=com" variantIdsStr
    
        let headers =
          [
          ]

        async {
        let! response = Http.AsyncRequestString (url, headers=headers)
        let variantData = StockResponse.Parse response
        return
          variantData
          |> Seq.map (fun variant -> variant.VariantId, variant.IsInStock)
          |> Map.ofSeq
        }


  let enrichWithStock (localItems:Map<string,JsonValue>) (realItems:Map<string,JsonValue>) (j:J) =
    let possiblyOutOfStock = localItems - realItems

    let variantIds =
      possiblyOutOfStock
      |> Map.toSeq
      |> Seq.map (fun (k,v) ->
        match v with
        | RecordContainingKey "variantId" (J.Number variantId) -> int variantId
        | _ -> failwith "Invalid response, could not find variant id")

    async {
    let! stockLevels = getStockForVariants variantIds

    let unknownItems =
      possiblyOutOfStock
      |> Map.toSeq
      |> Seq.map (fun (k,v) ->
        let variantId = 
          match v with
          | RecordContainingKey "variantId" (J.Number variantId) -> int variantId
          | _ -> failwith "Invalid response, could not find variant id"
        let enriched = v |> transformRecordWithKey "isInStock" (fun _ -> J.Boolean stockLevels.[variantId])
        k, enriched)
      |> Map.ofSeq

    let updatedRealItems =
      realItems
      |> Map.toSeq
      |> Seq.map (fun (k,v) ->
        let enriched = v |> transformRecordWithKey "isInStock" (fun _ -> J.Boolean true)
        k, enriched)
      |> Map.ofSeq  
      
    let combined =
      let a = unknownItems |> Map.toSeq |> Seq.map snd
      let b = updatedRealItems |> Map.toSeq |> Seq.map snd
      Seq.append a b |> Seq.toArray

    return 
      j |> transformRecordWithKey "bag" (fun bag -> transformRecordWithKey "items" (fun _ -> J.Array combined) bag.Value)  
    }


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

        let! response = H.AsyncRequest(realUri, query=query, headers=headers, httpMethod=ctx.request.method.ToString(), body = HttpRequestBody.BinaryUpload ctx.request.rawForm, silentHttpErrors=true)

        match response.StatusCode, response.Body with
        | 201, HttpResponseBody.Text result ->
          let resultJson = J.Parse result
        
          let item =
            match resultJson with
            | RecordContainingKey "bag" (RecordContainingKey "items" (J.Array items)) ->
              items
              |> Seq.tryFind (fun x ->
                match x with
                | RecordContainingKey "variantId" (J.Number x) when int x = variantId -> true
                | _ -> false)
              |> Option.get
            | _ -> failwith "Unexpected response from ASOS API"

          let bagItemId =
            match item with
            | RecordContainingKey "id" (J.String key) -> key
            | _ -> failwith "Could not locate bag item id in json"
        
          DataStore.update bagItemId item

          let! realItems = getRealBagContents bagId query headers
          let localItems = DataStore.data
        
          let! responseWithVirtualBag = resultJson |> replaceItems |> enrichWithStock localItems realItems
          
          return! OK (responseWithVirtualBag.ToString()) ctx
        | _ -> return None
        }))

  let addMultipleToBag =
    POST
      >=> pathScan "/commerce/bag/v3/bags/%s/products" (fun bagId -> (fun ctx ->
        async {
        
        let realUri = sprintf "%s%s/products" bagApiUrl bagId
        
        let headers = getHeaders ctx
        let query = getQueryString ctx

        let requestJson = System.Text.Encoding.UTF8.GetString(ctx.request.rawForm)
        let json = J.Parse requestJson

        let variantIds =
          match json with
          | J.Array xs ->
            xs |> Seq.map (fun x ->
              match x with
              | RecordContainingKey "variantId" (J.Number variantId) -> int variantId
              | _ -> failwith "Could not extract variantid")
          | _ -> failwith "Unexpected input"
          |> Set.ofSeq

        let! response = H.AsyncRequest(realUri, query=query, headers=headers, httpMethod=ctx.request.method.ToString(), body = HttpRequestBody.BinaryUpload ctx.request.rawForm, silentHttpErrors=true)

        match response.StatusCode, response.Body with
        | 201, HttpResponseBody.Text result ->
          let resultJson = J.Parse result
        
          let items =
            match resultJson with
            | RecordContainingKey "bag" (RecordContainingKey "items" (J.Array items)) ->
              items
              |> Seq.filter (fun x ->
                match x with
                | RecordContainingKey "variantId" (J.Number x) when variantIds.Contains (int x) -> true
                | _ -> false)
            
            | _ -> failwith "Unexpected response from ASOS API"

          for item in items do
            let bagItemId =
              match item with
              | RecordContainingKey "id" (J.String key) -> key
              | _ -> failwith "Could not locate bag item id in json"
        
            DataStore.update bagItemId item

          let! realItems = getRealBagContents bagId query headers
          let localItems = DataStore.data
        
          let! responseWithVirtualBag = resultJson |> replaceItems |> enrichWithStock localItems realItems
          
          return! OK (responseWithVirtualBag.ToString()) ctx
        | _ -> return None
        }))


  let getBag =
    GET
      >=> pathScan "/commerce/bag/v3/bags/%s" (fun bagId -> (fun ctx ->
        async {
        let realUri = "https://api.asos.com" + ctx.request.path        
        let headers = getHeaders ctx
        let query = getQueryString ctx

        let! response = H.AsyncRequest(realUri, query=query, headers=headers, httpMethod=ctx.request.method.ToString(), silentHttpErrors=true)

        let! realItems = getRealBagContents bagId query headers
        let localItems = DataStore.data

        match response.StatusCode, response.Body with
        | 200, HttpResponseBody.Text result ->
          let resultJson = J.Parse result                       
          let! responseWithVirtualBag = resultJson |> replaceItems |> enrichWithStock localItems realItems
          
          return! OK (responseWithVirtualBag.ToString()) ctx
        | _ -> return None
        }))

  let deleteFromBag =
    DELETE
      >=> pathScan "/commerce/bag/v3/bags/%s/product/%s" (fun (bagId, bagItemId) -> (fun ctx ->
        async {
        let realUri = "https://api.asos.com" + ctx.request.path        
        let headers = getHeaders ctx
        let query = getQueryString ctx
        
        let! response = H.AsyncRequest(realUri, query=query, headers=headers, httpMethod=ctx.request.method.ToString(), silentHttpErrors=true)

        DataStore.removeBagItem bagItemId

        let! realItems = getRealBagContents bagId query headers
        let localItems = DataStore.data

        match response.StatusCode, response.Body with
        | 200, HttpResponseBody.Text result ->
          let resultJson = J.Parse result                       
          let! responseWithVirtualBag = resultJson |> replaceItems |> enrichWithStock localItems realItems
          
          return! OK (responseWithVirtualBag.ToString()) ctx
        | _ -> return None
        }))

  let checkout =
    PUT
      >=> pathScan "/commerce/bag/v3/bags/%s/checkout" (fun bagId -> (fun ctx ->
        async {
        let realUri = "https://api.asos.com" + ctx.request.path        
        let headers = getHeaders ctx
        let query = getQueryString ctx

        let requestJson = System.Text.Encoding.UTF8.GetString(ctx.request.rawForm)
        let json = J.Parse requestJson


        // Get expired items by diffing our bag with the real one
        
        let! realItems = getRealBagContents bagId headers query
                    
        let expiredItems = DataStore.data - realItems
        
        do!
          expiredItems
          |> Map.toArray
          |> Array.map (fun (bagId, item) ->
            async {
            let variantId =
              match item with
              | RecordContainingKey "variantId" variantId ->  variantId
              | _ -> failwith "Could not extract variant ID from response"
            
            let addToBagJson =
              J.Record 
                [|
                  "bagId", J.String bagId
                  "variantId", variantId
                |]
           
            let! response = H.AsyncRequest(getBagUri bagId, query=query, headers=headers, httpMethod="POST", body = HttpRequestBody.TextRequest (addToBagJson.ToString()), silentHttpErrors=true)
            return ()
            }
            )
         |> Async.Parallel
         |> Async.Ignore
         

        // Now call the checkout api

        let! response = H.AsyncRequest(realUri, query=query, headers=headers, httpMethod=ctx.request.method.ToString(), body = HttpRequestBody.BinaryUpload ctx.request.rawForm, silentHttpErrors=true)

        match response.StatusCode, response.Body with
        | n, HttpResponseBody.Text result ->
          let status : HttpStatus = { code = n; reason = "" }
          return! withStatusCode (UTF8.bytes result) status ctx
        | _ -> return None
        }))
      
  let passThrough =
    fun (ctx:HttpContext) ->
      async {
      let headers = getHeaders ctx
      let query = getQueryString ctx
      let realUri = "https://api.asos.com" + ctx.request.path
      let method = ctx.request.method.ToString()

      
      let! response =
        if method <> "GET"
          then H.AsyncRequest(realUri, query=query, headers=headers, httpMethod=method, body = HttpRequestBody.BinaryUpload ctx.request.rawForm, silentHttpErrors=true)
          else H.AsyncRequest(realUri, query=query, headers=headers, httpMethod=method, silentHttpErrors=true)

      match response.StatusCode, response.Body with
      | n, HttpResponseBody.Text result ->
        let status : HttpStatus = { code = n; reason = "" }
        return! withStatusCode (UTF8.bytes result) status ctx
      | _ -> return failwith "Unexpected response (binary)"
      
      }


