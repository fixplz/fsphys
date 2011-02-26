
module fsphys.space

open System.Collections.Generic

open geom
open contact
open collision


type ctid = int

type state =
  { bodies:body.t ResizeArray
    contacts:Dictionary<ctid,contact.t>
    mutable stamp:int
    }


type config =
  { iters:int
    dt:scalar
    gravity:vec
    contact_config:contact.config
    collision_ask : body.t*shape.t*body.t*shape.t -> collision_jdg
    }

and collision_jdg =
  | NoCollision
  | DefCollision
  | CustCollision of contact.config


let init () =
  { bodies = ResizeArray()
    contacts = Dictionary()
    stamp = 0
    }


let contacts cfg s =
  
  let regcol (a,s1,b,s2) col ctcfg =
    
    let ctid = hash s1 + hash s2 // commutative
    seq {
      for p,id in col.pts ->
        let id = hash (ctid,id)
        
        let ct =
          try s.contacts.[id]
          with _ ->
            let ct = contact.make a b
            s.contacts.[id] <- ct
            ct
        
        ct.p <- p; ct.stamp <- s.stamp
        contact.worker a b col.dist col.n p ct ctcfg
      }
  
  [| for a,s1,b,s2 as x in broadphase.query s.bodies do
       if a<>b then
         match collision.check s1 s2 with
         | None -> ()
         | Some col -> 
         
         match cfg.collision_ask x, cfg.contact_config with
         | NoCollision, _ -> ()
         | DefCollision, ctcfg | CustCollision ctcfg, _ ->
         
         yield! regcol x col ctcfg
    |]


let update cfg s =
  for b in s.bodies do
    body.update cfg.dt cfg.gravity b
  
  let cts = contacts cfg s
  
  [ for kv in s.contacts do
      if kv.Value.stamp < s.stamp-3 then
        yield kv.Key
    ]
  |> Seq.iter (s.contacts.Remove >> ignore)
  
  for ct in cts do
    ct.app_acc ()
  
  for _ in 1 .. cfg.iters do
    for ct in cts do
      ct.app_corr ()
  
  for _ in 1 .. cfg.iters do
    for ct in cts do
      ct.app_elastic ()
  
  s.stamp <- s.stamp+1

