package popl.js.util

abstract class State[S, R]:
  def apply(s: S): (S, R)

  def map[P](f: R => P): State[S,P] = 
    s =>
      val (sp, r) = this(s) // = this.apply(s)
      (sp, f(r))
    
  def flatMap[P](f: R => State[S,P]): State[S,P] = 
    s =>
      val (sp, r) = this(s)
      f(r)(sp)
  
object State:
  def apply[S, R](f: S => (S, R)): State[S, R] =
    s => f(s)
  
  def init[S]: State[S, S] = 
    s => (s, s)
  
  def insert[S, R](r: R): State[S, R] = 
    s => (s, r)
    // init map (_ => r)
  
  def read[S, R](f: S => R): State[S, R] =
    s => (s, f(s))
    // init[S] map (s => f(s))
    
  def write[S](f: S => S): State[S, Unit] = 
    s => (f(s), ())
    // init[S] flatMap (_ => s => (f(s), ()))
