// infinite stream
def from(n: Int): Stream[Int] = n #:: from(n + 1)

val nats = from(0)

nats.take(10).toList

val m4s = nats map (_ * 4)

m4s.take(10).toList

// The Sieve of Eratosthenes (prime numbers generator)
def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

sieve(from(2)).take(10).toList

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(4).take(10).toList

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0001

sqrtStream(4) filter (isGoodEnough(_, 4))


