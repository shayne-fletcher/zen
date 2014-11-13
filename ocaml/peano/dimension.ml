module type S = sig

  type +'a s = 'a * 'a
  type (+'a,+'b,+'c,+'d,+'e,+'f,+'g,+'h,+'i,+'j,+'k,+'l,+'m,+'n) t

  (*base dimensions*)
  val mass : float -> ('a,'a s,'b,'b,'c,'c,'d,'d,'e,'e,'f,'f,'g,'g) t
  val length : float -> ('a,'a,'b,'b s,'c,'c,'d,'d,'e,'e,'f,'f,'g,'g) t
  val time : float -> ('a,'a,'b,'b,'c,'c s,'d,'d,'e,'e,'f,'f,'g,'g) t
  val charge : float -> ('a,'a,'b,'b,'c,'c,'d,'d s,'e,'e,'f,'f,'g,'g) t
  val temperature : float -> ('a,'a,'b,'b,'c,'c,'d,'d,'e,'e s,'f,'f,'g,'g) t
  val intensity : float -> ('a,'a,'b,'b,'c,'c,'d,'d,'e,'e,'f,'f s,'g,'g) t
  val angle : float -> ('a,'a,'b,'b,'c,'c,'d,'d,'e,'e,'f,'f,'g,'g s) t

  (*composite dimensions*)
  val velocity : float -> ('a,'a,'b,'b s,'c s,'c,'d,'d,'e,'e,'f,'f,'g,'g) t
  val acceleration : float -> ('a,'a,'b,'b s,'c s s,'c,'d,'d,'e,'e,'f,'f,'g,'g) t
  val momentum : float -> ('a,'a s,'b,'b s,'c s,'c,'d,'d,'e,'e,'f,'f,'g,'g) t
  val force : float -> ('a,'a s,'b,'b s,'c s s,'c,'d,'d,'e,'e,'f,'f,'g,'g) t

  (*arithmetic*)
  val ( + ) : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n) t -> 
                 ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n) t -> 
                      ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n) t
  val ( - ) : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n) t -> 
                  ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n) t -> 
                      ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n) t
   val ( * ) : ('a0,'b0,'c0,'d0,'e0,'f0,'g0,'h0,'i0,'j0,'k0,'l0,'m0,'n0) t -> 
                 ('b0,'b1,'d0,'d1,'f0,'f1,'g0,'h1,'i0,'j1,'k0,'l1,'m0,'n1) t -> 
                      ('a0,'b1,'c0,'d1,'e0,'f1,'g0,'h1,'i0,'j1,'k0,'l1,'m0,'n1) t
   val ( / ) : ('a0,'b0,'c0,'d0,'e0,'f0,'g0,'h0,'i0,'j0,'k0,'l0,'m0,'n0) t -> 
               ('a1,'b0,'c1,'d0,'e1,'f0,'g1,'h0,'i1,'j0,'k1,'l0,'m1,'n0) t -> 
                      ('a0,'a1,'c0,'c1,'e0,'e1,'g0,'g1,'i0,'i1,'k0,'k1,'m0,'m1) t

  (*conversion to float*)
  val value : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n) t -> float
end

module Dim : S = struct

  type +'a s = 'a * 'a
  type (+'a,+'b,+'c,+'d,+'e,+'f,+'g,+'h,+'i,+'j,+'k,+'l,+'m,+'n) t = float

  let mass x = x
  let length x = x
  let time x = x
  let charge x = x
  let temperature x = x
  let intensity x = x
  let angle x = x

  let velocity x = x
  let acceleration x = x
  let momentum x = x
  let force x = x

  let ( + ) = (+.)
  let ( - ) = ( -. )
  let ( * ) = ( *. )
  let ( / ) = ( /. )

  let value x = x

end
