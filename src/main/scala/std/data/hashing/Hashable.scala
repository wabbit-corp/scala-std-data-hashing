package std.data.hashing

import scala.{specialized => sp}
import std.algebra.Order

/**
 * A type class used to represent a hashing scheme for objects of a given type.
 */
trait Hashable[@sp A] extends Any with Serializable {
  def hash[Z](value: A)(implicit Z: Hash[Z]): Z =
    hashWithSeed[Z](Z.empty, value)

  def hashWithSeed[Z](seed: Z, value: A)(implicit hash: Hash[Z]): Z
}
object Hashable {
  /**
   * Summon a `Hash` instance given the specific type.
   */
  @inline final def apply[A](implicit A: Hashable[A]): Hashable[A] = A

  def hash[Z: Hash, A: Hashable](z: Z, a: A): Z =
    Hashable[A].hashWithSeed[Z](z, a)

  def hash[Z: Hash, A: Hashable, B: Hashable](z0: Z, a: A, b: B): Z = {
    val z1 = Hashable[A].hashWithSeed(z0, a)
    val z2 = Hashable[B].hashWithSeed(z1, b)
    z2
  }

  def hash[Z: Hash, A: Hashable, B: Hashable, C: Hashable](z0: Z, a: A, b: B, c: C): Z = {
    val z1 = Hashable[A].hashWithSeed(z0, a)
    val z2 = Hashable[B].hashWithSeed(z1, b)
    val z3 = Hashable[C].hashWithSeed(z2, c)
    z3
  }

  implicit val int: Hashable[Int] = new Hashable[Int] {
    def hashWithSeed[Z](z: Z, a: Int)(implicit Z: Hash[Z]): Z =
      Z.int(z, a)
  }

  implicit val bigint: Hashable[BigInt] = new Hashable[BigInt] {
    def hashWithSeed[Z](z: Z, a: BigInt)(implicit Z: Hash[Z]): Z =
      Z.bytes(z, a.toByteArray)
  }

  implicit def list[A](implicit A: Hashable[A]): Hashable[List[A]] =
    new Hashable[List[A]] {
      def hashWithSeed[Z](z: Z, list: List[A])(implicit Z: Hash[Z]): Z =
        list.foldRight(Z.long(z, 0x8c6b486a6a044063L))((a, z) => A.hashWithSeed(z, a))
    }

  implicit def set[A](implicit A: Hashable[A], ord: Order[A]): Hashable[Set[A]] =
    new Hashable[Set[A]] {
      def hashWithSeed[Z](z: Z, set: Set[A])(implicit Z: Hash[Z]): Z = {
        val list = set.toList.sortWith(ord.lt)
        list.foldRight(Z.long(z, 0x9fcda00fb8f98458L))((a, z) => A.hashWithSeed(z, a))
      }
    }
}
