
module fsphys.phys

open System.Collections.Generic

open geom
open space


module def =
  type shape = Poly of vec array | Circle of vec*scalar
  
  let poly vs    = Poly (Array.ofSeq vs)
  let circle r c = Circle (c,r)
  
  type body = 
    { shapes:shape seq
      density:scalar
      }
  
  let create (pos,ang) b =
    body.create b.density
      [| for s in b.shapes ->
           match s with
           | Poly vs      -> shape.poly vs
           | Circle (c,r) -> shape.circle r c
         |]
      pos ang
  
  let body d s      = { shapes = s; density = d }
  let body_static s = { shapes = s; density = infinity }


type placement = vec*scalar

type action =
  | AddBody of placement*def.body

type contact_config = contact.config
type collision_jdg = space.collision_jdg


type tag = int
let tag = ref 0

type main(cfg) =
  
  let s = space.init ()
  
  let actions = ResizeArray()
  let action = actions.Add
  
  member p.dump = s
  
  member p.update() =
    
    for tag,a in actions do
      match a with
      | AddBody (pl,b) ->
          s.bodies.Add (def.create pl b)
    
    actions.Clear()
    
    space.update cfg s
  
  
  member p.add_body b =
    incr tag; action (!tag, AddBody b); !tag


let init cfg = main(cfg)

let default_cfg fps =
  { iters = 5
    dt = 1./fps
    gravity = vec(0.,300.)
    collision_ask = fun _ -> DefCollision
    contact_config =
      { bounce = 0.7
        friction = 0.6
        tangentvel = 0.
        }
    }
