
module fsphys.body

open geom


type [<ReferenceEquality>] t =
  { mutable pos:vec
    mutable vel:vec
    mutable ang:scalar
    mutable rot:scalar
    
    mutable snap:vec
    mutable asnap:scalar
    
    imass:scalar
    iinertia:scalar
    
    shapes:shape.t array
    }


let create density shapes pos ang =
  
  for s in shapes do shape.update pos ang s
  
  let area,inertia =
    if density<>infinity then
      shapes
      |> Seq.map shape.props
      |> Seq.reduce (fun (a,i) (a',i') -> a+a',i+i')
    else 1.,1.
  
  { pos  = pos;   ang   = ang
    vel  = vec(); rot   = 0.
    snap = vec(); asnap = 0.
    
    shapes = Array.ofSeq shapes
    
    imass    = inv (density*area)
    iinertia = inv (density*inertia*area)
    }


let update dt g b =
  let damp = 0.995
  
  b.pos <- b.pos +| dt.*b.vel +| b.snap
  b.ang <- b.ang +  dt *b.rot +  b.asnap
  b.vel <- damp.*b.vel
  b.rot <- damp *b.rot
  
  b.snap <- vec()
  b.asnap <- 0.
  
  if b.imass <> 0.
    then b.vel <- b.vel +| dt.*g
  
  for s in b.shapes do
    shape.update b.pos b.ang s
