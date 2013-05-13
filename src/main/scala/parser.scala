package com.daniel.parsers

trait Parser[+A] { self =>
  def apply(input: Stream[Char]): Option[(A, Stream[Char])]
  
  def parse(input: Stream[Char]): Option[A] = {
    this(input) flatMap {
      case (result, tail) =>
        if (tail.isEmpty) Some(result) else None
    }
  }
  
  def ~[B](that: Parser[B]): Parser[A ~ B] = new ConjParser(this, that)
  
  def ~(that: String): Parser[A ~ String] = new ConjParser(this, new TokenParser(that))
  
  def ^^[B](f: A => B) = this map f
  
  def map[B](f: A => B): Parser[B] = new Parser[B] {
    def apply(input: Stream[Char]): Option[(B, Stream[Char])] = {
      self(input) map {
        case (a, tail) => (f(a), tail)
      }
    }
  }
  
  def flatMap[B](f: A => Parser[B]): Parser[B] = new Parser[B] {
    def apply(input: Stream[Char]): Option[(B, Stream[Char])] = {
      self(input) flatMap {
        case (a, tail) => f(a)(tail)
      }
    }
  }
}

object Parser {
  implicit def keyword(str: String): Parser[String] = new TokenParser(str)
  
  implicit class DisjConv[A](left: => Parser[A]) {
    def |[B >: A](right: => Parser[B]): Parser[B] = new DisjParser(left, right)
  }
  
  implicit class DisjConvStr(left: => String) {
    def |[B <% Parser[String]](right: => B): Parser[String] =
      new DisjParser(new TokenParser(left), right)
  }
}

class ConjParser[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[A ~ B] {
  def apply(input: Stream[Char]): Option[(A ~ B, Stream[Char])] = {
    for {
      (a, tail) <- left(input)
      (b, tail2) <- right(tail)
    } yield (new ~(a, b), tail2)
  }
}

case class ~[+A, +B](a: A, b: B)

class DisjParser[+A](_left: => Parser[A], _right: => Parser[A]) extends Parser[A] {
  lazy val left = _left
  lazy val right = _right
  
  def apply(input: Stream[Char]): Option[(A, Stream[Char])] = {
    lazy val leftResult = left(input)
    lazy val rightResult = right(input)
    leftResult orElse rightResult
  }
}

class TokenParser(str: String) extends Parser[String] {
  def apply(input: Stream[Char]): Option[(String, Stream[Char])] = {
    val matches = str zip input forall { case (a, b) => a == b }
    
    if (matches && input.lengthCompare(str.length) >= 0)
      Some((str, input drop str.length))
    else
      None
  }
}

