
module fsphys.geom


type scalar = float

let sq x : scalar = x*x
let inv x = 1. / x


type [<Struct>] vec =
  val x:scalar
  val y:scalar
  new(x,y) = {x=x;y=y}
  static member (~-) (a:vec) = vec(-a.x,-a.y)

let (+|) (a:vec) (b:vec) = vec(a.x+b.x, a.y+b.y)
let (-|) (a:vec) (b:vec) = vec(a.x-b.x, a.y-b.y)

let (.*) (a:scalar) (b:vec) = vec(a*b.x,a*b.y)

let vperp (a:vec) = vec(-a.y,a.x)

let dot (a:vec) (b:vec) = a.x*b.x+a.y*b.y
let cross a b = dot a -(vperp b)

let vsq v = dot v v
let vlen v = sqrt (vsq v)
let vunit (a:vec) = let l = inv (vlen a) in vec(a.x*l, a.y*l)

let vrotate (a:vec) (b:vec) =
  vec(a.x*b.x - a.y*b.y, a.y*b.x + a.x*b.y)

let vpolar a = vec(cos a,sin a)


type [<Struct>] axis =
  val n:vec
  val d:scalar
  new(n,d) = {n=n;d=d}
  static member (~-) (a:axis) = axis(-a.n,a.d)


type aabb = vec*vec

let aabb_intersect ((a,b) : aabb) ((a',b') : aabb) =
     a.x < b'.x && a'.x < b.x
  && a.y < b'.y && a'.y < b.y
