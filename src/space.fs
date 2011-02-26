
module fsphys.space

open System.Collections.Generic

open geom
open contact


type ctid = int

type state =
  { bodies:body.t ResizeArray
    forces:Dictionary<ctid,contact.t>
    }


type config =
  { iters:int
    dt:scalar
    gravity:vec
    collision_ask:body.t*shape.t*body.t*shape.t -> collision_jdg
    contact_config:contact.config
    }

and collision_jdg =
  | NoCollision
  | DefCollision
  | CustCollision of contact.config


let init () =
  { bodies = ResizeArray()
    forces = Dictionary()
    }


let contacts cfg s =
  let getct id =
    try s.forces.[id]
    with _ ->
      let ct = contact.get ()
      s.forces.[id] <- ct
      ct
  
  [| for a,s1,b,s2 as x in broadphase.query s.bodies do
       if a<>b then
         match collision.check s1 s2 with
         | None -> ()
         | Some col ->
         
         match cfg.collision_ask x, cfg.contact_config with
         | NoCollision, _ -> ()
         | DefCollision, ctcfg | CustCollision ctcfg, _ ->
         
         let ctid = hash s1 + hash s2 // commutative
         for p,id in col.pts ->
           let id = hash (ctid,id)
           contact.worker a b col.dist col.n p (getct id) ctcfg
    |]

let update cfg s =
  for b in s.bodies do
    body.update cfg.dt cfg.gravity b
  
  let contacts = contacts cfg s
  
  for ct in contacts do
    ct.app_acc ()
  
  for _ in 1 .. cfg.iters do
    for ct in contacts do
      ct.app 0.
  
  for _ in 1 .. cfg.iters do
    for ct in contacts do
      ct.app_elastic ()

