abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  override def union(other:IntSet): IntSet =
    ((left union right) union other) incl elem
  override def toString = "{"+ left + "" + elem + "" + right +"}"
}

class Empty extends IntSet {
  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def contains(x: Int): Boolean = false
  override def union(other: IntSet): IntSet = other
  override def toString = "."
}

val t1 = new NonEmpty(3, new Empty, new Empty)
val t2 = t1 incl 4

val t3 = new NonEmpty(5, new Empty, new Empty)
val t4 = t3 incl 6

t2 union t4

val a: Array[IntSet] = Array(new NonEmpty(1,new Empty, new Empty))
val b: Array[IntSet] = a
