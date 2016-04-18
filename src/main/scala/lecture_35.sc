def sum(f: Int => Int, a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  loop(a, 0)
}

sum(x => x * x * x, 3, 5)


def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f,(x,y) => x*y,1)(a,b)

product(x => x * x)(3, 4)

def fact(n: Int) = product(x => x)(1, n)
fact(5)

import math.abs

val tolerance = 0.0001
def isCloseEnought(x:Double, y:Double) =
  abs((x - y)/x) / x < tolerance
def fixedPoint(f:Double => Double)(firstGuess:Double) = {
  def iterate(guess:Double):Double = {
    println("guess = "+guess)
    val next = f(guess)
    if(isCloseEnought(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint(x => 1 +x/2)(1)

def sqrt(x:Double) = fixedPoint(y => (y+x/y)/2)(1)
sqrt(2)



