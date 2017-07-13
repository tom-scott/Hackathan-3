open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open FSharp.Data

type H = FSharp.Data.Http

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
        let foo = headers |> Seq.toArray

        let query =
          ctx.request.query
          |> Seq.choose (fun (k,v) ->
            match v with
            | Some x -> Some (k, x)
            | None -> None)
          |> Seq.toList


        let! result = H.AsyncRequestString(realUri, query=query, headers=headers, httpMethod="POST", body = HttpRequestBody.BinaryUpload ctx.request.rawForm)

        return! OK result ctx
        }))
      //>=> (fun ctx ->
      //  1
      //  )
        
      //  let realUri = sprintf "%s%s/product" bagApiUrl bagId



        
      //  OK (sprintf "THis is bag %s" bagId)
      //  )

  let app =
    choose [
      addToBag
      GET >=> path "/hello" >=> Successful.OK "Hello World!"
      ]
    
  startWebServer defaultConfig app

  0 // return an integer exit code
