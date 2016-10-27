package com.memoizr.scalacheck

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class GeneratorsEx extends FlatSpec with Checkers {

  import org.scalacheck.Gen
  import org.scalacheck.Prop.forAll

  it should "support custom generators" in {
    val myGen = for {
      n <- Gen.choose(10, 20)
      m <- Gen.choose(2 * n, 500)
    } yield (n, m)

    check {
      forAll(myGen) {
        case (n, m) => m >= 2 * n
      }
    }
  }

  it should "allow to select one of a range" in {
    val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'U')

    val validChars: Seq[Char] = Seq('A', 'E', 'I', 'O', 'U')

    check {
      forAll(vowel) { v =>
        validChars.contains(v)
      }
    }
  }

  it should "allow for a thing called a 'frequency combinator'" in {
    val vowels = Gen.frequency(
      (1, 'A'),
      (2, 'E'),
      (1, 'I'),
      (6, 'O'),
      (1, 'U')
    )
  }

  import org.scalacheck.Gen.{alphaChar, posNum, listOfN}
  import org.scalacheck.Prop.forAll

  it should "allow for some other cool methods" in {
    check {
      forAll(alphaChar)(_.isDigit == false)
    }

    check {
      forAll(posNum[Int])(n => n > 0)
    }

    check {
      forAll(listOfN(10, posNum[Int])) { list =>
        !list.exists(_ < 0) && list.length == 10
      }
    }
  }

  it should "have conditional generators" in {
    val smallEvenInteger = Gen.choose(0, 200) suchThat (_ % 2 == 0)

    check {
      forAll(smallEvenInteger) {
        _ % 2 == 0
      }
    }
  }


  it should "be able to generate random case classes" in {
    case class Foo(intValue: Int, charValue: Char)

    val fooGen = for {
      intValue <- Gen.posNum[Int]
      charValue <- Gen.alphaChar
    } yield Foo(intValue, charValue)

    check {
      forAll(fooGen) {
        foo => foo.intValue > 0 && !foo.charValue.isDigit
      }
    }
  }

  it should "support sized generators" in {
    val myGen = Gen.sized { size =>
      val positiveNumbers = size / 3
      val negativeNumbers = size * 2 / 3

      for {
        posNumList <- Gen.listOfN(positiveNumbers, Gen.posNum[Int])
        negNumList <- Gen.listOfN(negativeNumbers, Gen.posNum[Int] map (n => -n))
      } yield (size, posNumList, negNumList)
    }
    check {
      forAll(myGen) {
        case (genSize, posN, negN) =>
          posN.length == genSize / 3 && negN.length == (genSize * 2 / 3)
      }
    }
  }

  it should "be able to generate containers" in {
    val genIntList = Gen.containerOf[List, Int](Gen.oneOf(1,3,5))

    val genStringStream = Gen.containerOf[Stream, String](Gen.alphaStr)

    val genBoolArray = Gen.containerOf[Array, Boolean](true)

    val validNumbers = List(1,2,3,4,5,6)

    check {
      forAll(genIntList) (_ forall (elem => validNumbers.contains(elem)))
    }
  }
}
