
module fsphys.collision

open geom
open shape


let (+>) v f = Option.bind f v
let enum a = Seq.zip (Seq.initInfinite id) a

type t = { pts:(vec*int) array; n:vec; dist:scalar }

let flipcol c = { c with n = -c.n }


let sepaxispp p1 p2 =
  let axes =
    [| for a in p1.axes ->
         axis(a.n,
           Array.min [|for v in p2.verts -> dot v a.n|] - a.d)
       |]
  
  let a = axes |> Array.maxBy (fun a -> a.d)
  if a.d > 0. then None else Some a

let containsv poly v =
  poly.axes |> Array.forall (fun a -> dot a.n v < a.d) 

let containsvn poly v n =
  poly.axes |> Array.forall (fun a -> dot a.n v < a.d || dot a.n n>0.)

let findvs p1 p2 (a:axis) =
  { n = a.n; dist = a.d
    pts = 
      [| for i,v in enum p1.verts do
           if containsv p2 v       then yield v, i
         for i,v in enum p2.verts do
           if containsvn p1 v -a.n then yield v, 100+i
         |]
    }

let poly_poly p1 p2 =
  sepaxispp p1 p2 +> fun a1 ->
  sepaxispp p2 p1 +> fun a2 ->
  Some <|
    if a1.d>a2.d
      then findvs p1 p2 a1 
      else findvs p2 p1 a2 |> flipcol


let circle_circle c1 c2 =
  let r = c2.center -| c1.center
  let min = c1.radius + c2.radius
  
  if vsq r > sq min
    then None
    else
      let p = c1.center +| (0.5 + (c1.radius-min/2.)/vlen r) .* r
      Some 
        { pts = [| p, 0 |]
          n = vunit r; dist = vlen r - min
          }


let sepaxispc p c =
  let axes =
    [| for a in p.axes ->
         dot a.n c.center - a.d - c.radius
       |]
  
  let i,a = enum axes |> Seq.maxBy snd
  if a > 0. then None else Some (i,a)

let poly_circle p c =
  sepaxispc p c +> fun (i,max) ->
  
  let v1,v2 = sides p.verts |> Seq.nth i
  let a = p.axes.[i]
  
  let d = cross a.n c.center
  
  let corner v = circle_circle {lcenter=vec(); center=v; radius=0.} c
  
  if   d > cross a.n v1 then corner v1
  elif d < cross a.n v2 then corner v2
  else
    let p = c.center -| (c.radius+max/2.) .* a.n
    Some
      { pts = [| p, 0 |]
        n = a.n; dist = max
        }


let check a b =
  match (a,b) with
  | (Poly a, Poly b)     -> poly_poly a b
  | (Circle a, Circle b) -> circle_circle a b
  | (Poly a, Circle b)   -> poly_circle a b
  | (Circle a, Poly b)   -> poly_circle b a |> Option.map flipcol
