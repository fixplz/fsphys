
module test

open System.Drawing
open System.Windows.Forms

open fsphys
open fsphys.geom


let ri,rd =
  let r = System.Random()
  r.Next, r.NextDouble


let box w h =
  let w = w/2.
  let h = h/2.
  phys.def.poly [| vec(w,h); vec(w,-h); vec(-w,-h); vec(-w,h) |]

let randshape () =
  let n = float (4 + ri 4)
  phys.def.poly
    [| for i in 1. .. n ->
         (30.+rd()*20.) .* vpolar(-i/n*6.28)
       |]

let circle () =
  phys.def.circle 20.


let fps = 40.

let init () =
  let p = phys.init <| phys.default_cfg fps
  
  List.iter (p.add_body>>ignore)
    [ for _ in 0 .. 30 do
        let pos = vec(rd()*700., rd()*200.)
        yield (pos,0.), phys.def.body 1. [randshape ()]
      
      yield
        (vec(350., 450.),0.), phys.def.body_static [box 700. 50.]
      ]
  
  p

let p = ref (init ())

let update () = (!p).update()


let draw (g:Graphics) =
  
  let s = (!p).data
  
  let line = new Pen(Brushes.Black)
  let green = Brushes.Green
  
  let pointf (v:vec) = PointF(float32 v.x, float32 v.y)
  
  g.Clear(Color.White)
  
  for b in (!p).data.bodies do
  for s in b.shapes do
    match s with
    | shape.Poly p ->
        g.DrawPolygon(line, 
          [| for v in p.verts -> pointf v |] )
    | shape.Circle c ->
        let r = vec(c.radius,c.radius)
        g.DrawEllipse(line,
          RectangleF(pointf (c.center -| r), SizeF(pointf (2. .* r))) )

  (*for ct in s.contacts do
    let s = 5. in let r = vec(s,s)
    g.FillEllipse(green,
      RectangleF(pointf (ct.p-|0.5.*r), SizeF(pointf r)) )*)


type Main =
  inherit Form
  
  new() as w = {} then
    w.Width <- 700
    w.Height <- 500
    w.DoubleBuffered <- true
    
    let l = new Label()
    l.Text <- "Click to reset"
    w.Controls.Add l
    
    let t = new Timer()
    t.Interval <- 1000/int fps
    t.Start()
    w.Closed.Add (fun _ -> t.Stop())
    
    t.Tick.Add (fun _ -> update(); w.Invalidate())
    w.Paint.Add (fun e -> try draw e.Graphics with _ -> ())
    
    w.Click.Add (fun _ -> p := init ())


do (new Main()).ShowDialog() |> ignore
