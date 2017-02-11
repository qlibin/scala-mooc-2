trait Generator[+T] {
  self => // an alias for ”this”.
  def generate: T
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }
}
val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

integers.generate

var naturals: Generator[Int] = for (i <- integers) yield if (i >= 0) i else -i

val booleans: Generator[Boolean] = for (i <- integers) yield i > 0

booleans.generate

val pairs = new Generator[(Int, Int)] {
  def generate = (integers.generate, integers.generate)
}

pairs.generate

def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
  x => u map { y => (x, y) } }

var pairs2 = pairs(integers, booleans)

pairs2.generate


//---

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}
def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- naturals) yield lo + x % (hi - lo)
def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)


oneOf(1,2,3).generate


def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list
def emptyLists = single(Nil)
def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

lists.generate