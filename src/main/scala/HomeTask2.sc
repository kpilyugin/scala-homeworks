//Реалзуйте IntArrayBuffer с интерфейсом IntTraversable

trait IntTraversable {
  def isEmpty: Boolean

  def size: Int

  def contains(element: Int): Boolean

  def head: Int

  def tail: IntTraversable

  def ++(traversable: IntTraversable): IntTraversable

  def filter(predicate: Int => Boolean): IntTraversable

  def map(function: Int => Int): IntTraversable

  def flatMap(function: Int => IntTraversable): IntTraversable

  def foreach(function: Int => Unit): Unit
}

class IntArrayBuffer extends IntTraversable {
  private var data = new Array[Int](0)
  private var mySize = 0

  def this(array: Array[Int]) = {
    this()
    data = array
    mySize = array.length
  }

  def apply(index: Int): Int = data(index)

  def update(index: Int, element: Int): Unit = data.update(index, element)

  def clear(): Unit = mySize = 0

  def +=(element: Int): IntArrayBuffer = {
    ensureSize(size + 1)
    data.update(size, element)
    mySize += 1
    this
  }

  def ++=(elements: IntTraversable): IntArrayBuffer = {
    ensureSize(size + elements.size)
    elements.foreach(+=(_))
    this
  }

  def remove(index: Int): Int = {
    val removed = data(index)
    Array.copy(data, index + 1, data, index, size - index - 1)
    mySize -= 1
    removed
  }

  override def isEmpty: Boolean = size == 0

  override def size: Int = mySize

  override def contains(element: Int): Boolean = data.take(size).contains(element)

  override def head: Int = data(0)

  override def tail: IntArrayBuffer = new IntArrayBuffer(data.slice(1, size))

  override def ++(traversable: IntTraversable): IntArrayBuffer = {
    new IntArrayBuffer(data.take(size)) ++= traversable
  }

  protected def ensureSize(size: Int): Unit = {
    var arraySize = math.max(1, data.length)
    while (arraySize < size) {
      arraySize *= 2
    }
    val newArray = new Array[Int](arraySize)
    Array.copy(data, 0, newArray, 0, mySize)
    data = newArray
  }

  override def filter(predicate: (Int) => Boolean): IntArrayBuffer = {
    data = data.take(size).filter(predicate)
    mySize = data.length
    this
  }

  override def map(function: (Int) => Int): IntArrayBuffer = {
    data = data.take(size).map(function)
    mySize = data.length
    this
  }

  override def flatMap(function: (Int) => IntTraversable): IntArrayBuffer = {
    val flat = new IntArrayBuffer()
    foreach(flat ++= function(_))
    flat
  }

  override def foreach(function: (Int) => Unit): Unit = data.take(size).foreach(function)
}

object IntArrayBuffer {
  def empty: IntArrayBuffer = new IntArrayBuffer()

  def apply(elements: Int*): IntArrayBuffer = new IntArrayBuffer(elements.toArray)

  def unapplySeq(buffer: IntArrayBuffer): Option[IntArrayBuffer] =
    if (buffer == null || buffer.isEmpty) {
      None
    } else {
      Some(buffer)
    }
}