
module fsphys.broadphase

open geom


type trp = { body:body.t; shape:shape.t; bb:aabb }

let query bodies =
  let rec sweep trps =
    match trps with
    | [] -> []
    | a :: xs ->
        let section b = (fst b.bb).x < (snd a.bb).x
        let intersect b = aabb_intersect a.bb b.bb
        
        [ for b in 
            xs |> Seq.takeWhile section |> Seq.filter intersect ->
            a.body,a.shape, b.body,b.shape
          
          yield! sweep xs ]
  
  seq {
    for b:body.t in bodies do
      for s in b.shapes do
        yield { body = b; shape = s; bb = shape.aabb s }
    }
  |> Seq.sortBy (fun x -> (fst x.bb).x)
  |> Seq.toList 
  |> sweep
