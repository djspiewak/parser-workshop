package com.daniel.parsers

import org.specs2.mutable._

object ParserSpecs extends Specification {
  import Parser._
  
  "parser" should {
    "consume the entire stream on parse" in {
      "foo".parse("foo".toStream) must beSome("foo")
    }
    
    "validate the paren grammar" in {
      lazy val p: Parser[Any] = "(" ~ p ~ ")" | ("()": Parser[Any])
      
      p.parse("(())".toStream) must beLike {
        case Some(_) => ok
      }
    }
    
    "evaluate a arithmetic grammar" in {
      lazy val expr: Parser[Int] = (
          n ~ "+" ~ expr ^^ {
            case n1 ~ _ ~ n2 => n1 + n2
          }
        | n ~ "-" ~ expr ^^ {
            case n1 ~ _ ~ n2 => n1 - n2
          }
        | "(" ~ expr ~ ")" ^^ {
            case _ ~ n ~ _ => n
          }
        | n
      )
      
      lazy val n: Parser[Int] =
        ("0" | "1" | "2" | "3" | "4") ^^ { _.toInt }
      
      expr.parse("1+2-3".toStream) must beSome(0)
    }
  }
  
  "conj parser" should {
    "parse two tokens in sequence" in {
      val p = "foo" ~ "bar"
      p.parse("foobar".toStream) must beSome(new ~("foo", "bar"))
    }
  }
  
  "disj parser" should {
    "parse two tokens in alternation" in {
      val p = "foo" | "bar"
      p.parse("foo".toStream) must beSome("foo")
    }
    
    "not parse commutatively" in {
      val p = "abc" ^^ { _ => 123 } | "abc" ^^ { _ => 345 }
      
      p.parse("abc") must beSome(123)
    }
  }
  
  "token parser" should {
    "successfully parse a prefix of a stream" in {
      val parser = new TokenParser("foo")
      parser("foobarbaz".toStream) must beSome(("foo", "barbaz".toStream))
    }
    
    "successfully parse the exact stream" in {
      val parser = new TokenParser("foo")
      parser("foo".toStream) must beSome(("foo", Stream()))
    }
    
    "fail to parse substring of token" in {
      val parser = new TokenParser("foo")
      parser("fo".toStream) must beNone
    }
  }
}
