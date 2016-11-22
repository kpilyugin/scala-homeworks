// Church numerals

sealed trait CInt

case object CZero extends CInt
type CZero = CZero.type
case class CSucc[P <: CInt](prev: P) extends CInt

// Heterogeneous list
// HList = HNil | Cons(T, HList)

sealed trait HList {
  def ::[H](h: H): HCons[H, this.type] = HCons(h, this)
}

object HList {
  type ::[+H, +T <: HList] = HCons[H, T]
  type HNil = HNil.type

  trait Splittable[S <: HList, N <: CInt, L <: HList, R <: HList] {
    def apply(s: S, n: N): (L, R)
  }

  object Splittable {
    //base: S splitAt 0 == (HNil, S)
    implicit def base[S <: HList]: Splittable[S, CZero, HNil, S] = new Splittable[S, CZero, HNil, S] {
      override def apply(s: S, n: CZero): (HNil, S) = (HNil, s)
    }

    // step: (H :: S) splitAt n == (H :: L, R) <==> (L, R) == S splitAt n-1
    implicit def step[H, S <: HList, N <: CInt, L <: HList, R <: HList]
    (implicit splittable: Splittable[S, N, L, R]): Splittable[H :: S, CSucc[N], H :: L, R] = {
      new Splittable[H :: S, CSucc[N], H :: L, R] {
        override def apply(s: H :: S, n: CSucc[N]): (H :: L, R) = {
          val split: (L, R) = splittable(s.tail, n.prev)
          (s.head :: split._1, split._2)
        }
      }
    }
  }

  def splitAt[S <: HList, N <: CInt, L <: HList, R <: HList](s: S, n: N)(implicit splittable: Splittable[S, N, L, R]): (L, R) = {
    splittable(s, n)
  }
}

case object HNil extends HList
case class HCons[+H, +T <: HList](head: H, tail: T) extends HList
import HList._

val t: String :: Int :: HNil = "text" :: 1 :: HNil

val one = CSucc(CZero)
val two = CSucc(one)
val three = CSucc(two)

splitAt(t, CZero)
splitAt(t, one)
splitAt(t, two)
//splitAt(t, three) // should not compile
