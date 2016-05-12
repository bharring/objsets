
object intsets {
  val t1 = new NonEmpty(1, Empty, Empty)
  val t2 = new NonEmpty(2, Empty, Empty)
  val t3 = new NonEmpty(3, Empty, Empty)
  val t4 = new NonEmpty(4, Empty, Empty)
  t1 union t2 union t3
  t1 union t3 union t2
  t2 union t3 union t1
  t3 union t4 union t2 union t1
  t2 union t1 union t3
  ((t2 union t1) union t3) union t4
  t3 union t2 union t1
  t3 union t1 union t2

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus", "Google")
  val google2 = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val s = "Why Apple really ditched Google Maps http://t.co/evVBDYCu"
  google.exists(x => s.contains(x))
  google2.exists(x => s.contains(x))


  trait IntSet {
    def contains(x:Int): Boolean
    def incl(x:Int):IntSet
    def union(other:IntSet): IntSet
  }
  class NonEmpty(elem:Int, left:IntSet, right:IntSet) extends IntSet {
    def contains(x:Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x:Int):IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    override def toString:String = "{" + left + elem + right + "}"
    def union(other:IntSet): IntSet =
      ((left union right) union other) incl elem
  }
  object Empty extends IntSet {
    def contains(x:Int): Boolean = false
    def incl(x:Int):IntSet = new NonEmpty(x, Empty, Empty)
    override def toString:String = "."
    def union(other:IntSet): IntSet = other
  }
  trait List[T] {
    def isEmpty : Boolean
    def head: T
    def tail: List[T]
  }
  class Cons[T](val head:T, val tail: List[T]) extends List[T] {
    def isEmpty : Boolean = false
  }
  class Nil[T] extends List[T] {
    def isEmpty : Boolean = true
    def head:Nothing = throw new NoSuchElementException("Nil.head")
    def tail:Nothing = throw new NoSuchElementException("Nil.head")
  }

}
