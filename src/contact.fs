
module fsphys.contact

open geom
open body


let (|?>) (v:_ byref) f =
  let o = v in v <- f v; v-o

let clamp x = min x >> max -x

let apps b j r =
  b.snap  <- b.snap  +| b.imass .* j
  b.asnap <- b.asnap +  b.iinertia * dot j (vperp r)

let appj b j r =
  b.vel <- b.vel +| b.imass .* j
  b.rot <- b.rot +  b.iinertia * dot j (vperp r)


type config =
  { bounce:scalar
    friction:scalar
    tangentvel:scalar
    }


type t =
  { a:body.t; b:body.t
    mutable p:vec
    mutable stamp:int
    mutable jnacc:scalar
    mutable jtacc:scalar
    }

let make a b =
  { a=a; b=b; p=vec()
    stamp=0
    jnacc=0.; jtacc=0.
    }

let force ct = vec(ct.jnacc,ct.jtacc)


type worker =
  { app_acc : unit->unit
    app_corr : unit->unit
    app_elastic : unit->unit
    }


let worker a b dist n p ct cfg =
  
  let r1 = p-|a.pos
  let r2 = p-|b.pos
  
  let app f j = f a -j r1; f b j r2
  
  let kin n =
    inv <| a.imass + b.imass
      + a.iinertia * sq (cross r1 n)
      + b.iinertia * sq (cross r2 n)
  let mn = kin n
  let mt = kin (vperp n)
  
  let rel bv br av ar = 
    bv +| br .* vperp r2 -| av -| ar .* vperp r1
  let relv () = rel b.vel  b.rot   a.vel  a.rot
  let rels () = rel b.snap b.asnap a.snap a.asnap
  
  let snapt = 0.2 * -min 0. (dist + 0.1)
  let bouncet =
    max 0. <|
      cfg.bounce * (-dot (rel b.vel b.rot a.vel a.rot) n - ct.jnacc)
  
  { app_acc = fun () -> app appj (vrotate n (force ct))
    
    app_elastic = fun () ->
      let v = relv ()
      
      let jn = mn * (bouncet - dot v n)
      let jt = mt * (-dot v (vperp n) + cfg.tangentvel)
      
      let jn = &ct.jnacc |?> ((+) jn >> max 0.)
      let jt = &ct.jtacc |?> ((+) jt >> clamp (ct.jnacc*cfg.friction))
      
      app appj (vrotate n (vec(jn,jt)))
    
    app_corr = fun () ->
      let s = rels ()
      let v = relv ()
      
      let sn = mn * (snapt - dot s n)
      
      let jn = mn * -dot v n
      let jn = &ct.jnacc |?> ((+) jn >> max 0.)
      
      if sn>0. then
        app apps (sn .* n)
      
      app appj (jn .* n)
    }
