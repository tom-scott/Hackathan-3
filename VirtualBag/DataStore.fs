namespace VirtualBag
module DataStore =
  
  let mutable data = Map.empty<string,J>

  let update (bagItemId:string) (json:J) =
    data <- data.Add(bagItemId, json)

  let removeBagItem (bagItemId:string) =
    data <- data.Remove bagItemId

  let getItems () =
    data
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.toArray
    |> J.Array