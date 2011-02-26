
module fsphys.shape

open geom


let sides (xs:_ array) =
  seq
    { for i in 0 .. xs.Length-1 ->
        xs.[i], xs.[(i+1)%xs.Length]
      }

let sides3 (xs:_ array) =
  seq
    { for i in 0 .. xs.Length-1 ->
        xs.[i], xs.[(i+1)%xs.Length], xs.[(i+2)%xs.Length]
      }


type [<ReferenceEquality>] circle =
  { lcenter:vec
    radius:scalar
    mutable center:vec
    }

type [<ReferenceEquality>] poly =
  { lverts:vec array
    laxes:axis array
    mutable verts:vec array
    mutable axes:axis array
    }

type [<ReferenceEquality>] t =
  Circle of circle | Poly of poly


let circle r c =
  Circle { radius = r; lcenter = c; center = vec() }

let poly vs = 
  Poly
    { lverts = vs
      laxes =
        [| for v1,v2 in sides vs ->
             let n = v2-|v1 |> vunit |> vperp
             axis(n, dot n v1) |]
      verts = null
      axes = null
      }


let update pos ang =
  let dir = vpolar ang
  function
  | Circle c -> c.center <- pos +| vrotate dir c.lcenter
  | Poly p ->
      p.verts <-
        [| for v in p.lverts -> pos +| vrotate dir v |]
      p.axes <-
        [| for a in p.laxes ->
             let n = vrotate dir a.n
             axis(n, dot n pos + a.d) |]


let props =
  function
  | Circle c -> sq c.radius * System.Math.PI, sq c.radius / 2.
  | Poly p ->
      let s =
        seq
          { for v1,v2,v3 in sides3 p.lverts ->
              v2.x * (v1.y-v3.y)
            }
        |> Seq.reduce (+)
      s/2. ,
      
      let s1,s2 =
        seq
          { for v1,v2 in sides p.lverts ->
              let a = cross v2 v1
              let b = vsq v1 + vsq v2 + dot v1 v2
              a*b,a
            }
        |> Seq.reduce (fun (a,b) (a',b') -> a+a', b+b')
      s1/(6.*s2)


let aabb =
  function
  | Circle c ->
      let r = vec(c.radius,c.radius)
      c.center -| r, c.center +| r
  | Poly p ->
      p.verts |>
      Array.fold (fun (tl,br) v ->
        vec(min tl.x v.x, min tl.y v.y),
        vec(max br.x v.x, max br.y v.y) )
        (p.verts.[0],p.verts.[0])
