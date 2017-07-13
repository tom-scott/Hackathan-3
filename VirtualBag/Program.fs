namespace VirtualBag
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

(*
Known issues:
Only supporting one user at the moment
Multiple quantities (what happens if bag expires but user then adds same item again... we only have 1 item!)

*)

module Server =

    [<EntryPoint>]
    let main argv = 

      let app =
        choose [
          API.addToBag
          API.getBag
          API.deleteFromBag
          GET >=> path "/hello" >=> Successful.OK "Hello World!"
          API.passThrough
          ]

      let bindings =
        [ HttpBinding.createSimple Protocol.HTTP "0.0.0.0" 8080 ]

      let webServerConfig =
        { defaultConfig with bindings = bindings}

      startWebServer webServerConfig app

      0 // return an integer exit code
