package std.data.hashing

trait Hash[Z] extends Any with Serializable {
  def empty: Z

  def bytes(z: Z, bytes: Array[Byte]): Z

  def byte(z: Z, x: Byte): Z =
    bytes(z, Array(x))

  def short(z: Z, x: Short): Z =
    bytes(z, Array((x >>> 8).toByte, (x & 0xFF).toByte))

  def int(z: Z, x: Int): Z =
    bytes(z, Array(
      (x >>> 24).toByte,
      ((x >>> 16) & 0xFF).toByte,
      ((x >>> 8) & 0xFF).toByte,
      (x & 0xFF).toByte))

  def long(z: Z, x: Long): Z = bytes(z, Array(
    ((x >>> 56) & 0xFF).toByte,
    ((x >>> 48) & 0xFF).toByte,
    ((x >>> 40) & 0xFF).toByte,
    ((x >>> 32) & 0xFF).toByte,
    ((x >>> 24) & 0xFF).toByte,
    ((x >>> 16) & 0xFF).toByte,
    ((x >>> 8) & 0xFF).toByte,
    (x & 0xFF).toByte))

  def string(z: Z, x: String): Z =
    bytes(z, x.getBytes("UTF-8"))
}

object Hash {
  /**
   * Summon a `Hash` instance given the specific type.
   */
  @inline final def apply[A](implicit A: Hash[A]): Hash[A] = A
}
