open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

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

  let addToBag =
    POST
      >=> pathScan "/commerce/bag/v3/bags/%s/product" (fun bagId ->
        OK (sprintf "THis is bag %s" bagId)
        )

  let app =
    choose [
      addToBag
      GET >=> path "/hello" >=> Successful.OK "Hello World!"
      ]
    
  startWebServer defaultConfig app

  0 // return an integer exit code
