package com.mchange.sc.v2.yinyang

import org.scalacheck.{Arbitrary,Prop,Properties,Gen};
import Arbitrary.arbitrary;
import Prop._;

// modified from scalacheck test for scala.util.Either
object YinYangProperties extends Properties("YinYang") {
  implicit def arbitraryYinYang[X, Y](implicit xa: Arbitrary[X], ya: Arbitrary[Y]): Arbitrary[YinYang[X, Y]] =
    Arbitrary[YinYang[X, Y]](Gen.oneOf(arbitrary[X].map(Yin(_)), arbitrary[Y].map(Yang(_))))

  object CheckYinBiased {
    import YinYang.YinBias._

    val prop_value = forAll((n: Int) => Yin(n).get == n)

    val prop_getOrElse = forAll((e: YinYang[Int, Int], or: Int) => e.getOrElse(or) == (e match {
      case Yin(a) => a
      case Yang(_) => or
    }))

    val prop_forall = forAll((e: YinYang[Int, Int]) =>
      e.forall(_ % 2 == 0) == (e.isYang || e.get % 2 == 0))

    val prop_exists = forAll((e: YinYang[Int, Int]) =>
      e.exists(_ % 2 == 0) == (e.isYin && e.get % 2 == 0))

    val prop_flatMapYinIdentity = forAll((e: YinYang[Int, Int], n: Int, s: String) => {
      def f(x: Int) = if(x % 2 == 0) Yin(s) else Yang(s)
      Yin(n).flatMap(f(_)) == f(n)})

    val prop_flatMapYangIdentity = forAll((e: YinYang[Int, Int]) => e.flatMap(Yin(_)) == e)

    val prop_flatMapComposition = forAll((e: YinYang[Int, Int]) => {
      def f(x: Int) = if(x % 2 == 0) Yin(x) else Yang(x)
      def g(x: Int) = if(x % 7 == 0) Yang(x) else Yin(x)
      e.flatMap(f(_)).flatMap(g(_)) == e.flatMap(f(_).flatMap(g(_)))})

    val prop_mapIdentity = forAll((e: YinYang[Int, Int]) => e.map(x => x) == e)

    val prop_mapComposition = forAll((e: YinYang[String, Int]) => {
      def f(s: String) = s.toLowerCase
      def g(s: String) = s.reverse
      e.map(x => f(g(x))) == e.map(x => g(x)).map(f(_))})

    val prop_seq = forAll((e: YinYang[Int, Int]) => e.toSeq == (e match {
      case Yin(a) => Seq(a)
      case Yang(_) => Seq.empty
    }))

    val prop_option = forAll((e: YinYang[Int, Int]) => e.toOption == (e match {
      case Yin(a) => Some(a)
      case Yang(_) => None
    }))

    val prop_withFilter = forAll((e: YinYang[Int, Int] ) => {
      if ( e.isYin ) {
        if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
        else {
          try { e.withFilter( _ % 2 == 0 ); false }
          catch { case _ : NoSuchElementException => true }
        }
      } else {
        e.withFilter(_ % 2 == 0) == e // right should be unchanged
      }
    })

    val prop_extractTuple = forAll((e: YinYang[(Int,Int,Int),Int]) => {
      if ( e.isYin ) {
      e.get._1 == (for ( ( a, b, c ) <- e ) yield a).get
      } else {
        e == (for ( ( a, b, c ) <- e ) yield a) // right should be unchanged
      }
    })

    val prop_assignVariable = forAll((e: YinYang[(Int,Int,Int),Int]) => {
      if ( e.isYin ) {
        e.get._2 == (for ( tup <- e; b = tup._2 ) yield b).get
      } else {
        e == (for ( tup <- e; b = tup._2 ) yield b) // right should be unchanged
      }
    })

    val prop_filterInFor = forAll((e: YinYang[Int,Int], mul : Int, passThru: Boolean) => {
      if ( e.isYin && passThru) {
        e.map(_ * mul) == (for ( x <- e if passThru ) yield (mul * x))
      } else if ( e.isYin && !passThru ) {
        try { for ( x <- e if passThru ) yield x; false }
        catch { case nse : NoSuchElementException => true; }
      } else {
        e == (for ( x <- e ) yield x) // right should be unchanged
      }
    })
  }

  object CheckYinBiasedWithEmptyToken {
    val Bias = YinYang.YinBias.withEmptyToken(-1);
    import Bias._;

    val prop_withFilter = forAll((e: YinYang[Int, Int] ) => {
      if ( e.isYin ) {
        if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
        else e.withFilter( _ % 2 == 0 ) == Yang[Int,Int](-1)
      } else {
        e.withFilter(_ % 2 == 0) == e // right should be unchanged
      }
    })

    val prop_filterInFor = forAll((e: YinYang[Int,Int], mul : Int, passThru: Boolean) => {
      if ( e.isYin && passThru) {
        e.map(_ * mul) == (for ( x <- e if passThru ) yield (mul * x))
      } else if ( e.isYin && !passThru ) {
        (for ( x <- e if passThru ) yield x) == Yang[Int,Int](-1)
      } else {
        e == (for ( x <- e ) yield x) // right should be unchanged
      }
    })
  }

  object CheckYangBiased {
    import YinYang.YangBias._

    val prop_value = forAll((n: Int) => Yang(n).get == n)

    val prop_getOrElse = forAll((e: YinYang[Int, Int], or: Int) => e.getOrElse(or) == (e match {
      case Yin(_) => or
      case Yang(b) => b
    }))

    val prop_forall = forAll((e: YinYang[Int, Int]) =>
      e.forall(_ % 2 == 0) == (e.isYin || e.get % 2 == 0))

    val prop_exists = forAll((e: YinYang[Int, Int]) =>
      e.exists(_ % 2 == 0) == (e.isYang && e.get % 2 == 0))

    val prop_flatMapYinIdentity = forAll((e: YinYang[Int, Int], n: Int, s: String) => {
      def f(x: Int) = if(x % 2 == 0) Yin(s) else Yang(s)
      Yang(n).flatMap(f(_)) == f(n)})

    val prop_flatMapYangIdentity = forAll((e: YinYang[Int, Int]) => e.flatMap(Yang(_)) == e)

    val prop_flatMapComposition = forAll((e: YinYang[Int, Int]) => {
      def f(x: Int) = if(x % 2 == 0) Yin(x) else Yang(x)
      def g(x: Int) = if(x % 7 == 0) Yang(x) else Yin(x)
      e.flatMap(f(_)).flatMap(g(_)) == e.flatMap(f(_).flatMap(g(_)))})

    val prop_mapIdentity = forAll((e: YinYang[Int, Int]) => e.map(x => x) == e)

    val prop_mapComposition = forAll((e: YinYang[Int, String]) => {
      def f(s: String) = s.toLowerCase
      def g(s: String) = s.reverse
      e.map(x => f(g(x))) == e.map(x => g(x)).map(f(_))})

    val prop_seq = forAll((e: YinYang[Int, Int]) => e.toSeq == (e match {
      case Yin(_) => Seq.empty
      case Yang(b) => Seq(b)
    }))

    val prop_option = forAll((e: YinYang[Int, Int]) => e.toOption == (e match {
      case Yin(_) => None
      case Yang(b) => Some(b)
    }))

    val prop_withFilter = forAll((e: YinYang[Int, Int] ) => {
      if ( e.isYang ) {
        if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
        else {
          try { e.withFilter( _ % 2 == 0 ); false }
          catch { case _ : NoSuchElementException => true }
        }
      } else {
        e.withFilter(_ % 2 == 0) == e // left should be unchanged
      }
    })

    val prop_extractTuple = forAll((e: YinYang[Int,(Int,Int,Int)]) => {
      if ( e.isYang ) {
        e.get._1 == (for ( ( a, b, c ) <- e ) yield a).get
      } else {
        e == (for ( ( a, b, c ) <- e ) yield a) // left should be unchanged
      }
    })

    val prop_assignVariable = forAll((e: YinYang[Int,(Int,Int,Int)]) => {
      if ( e.isYang ) {
        e.get._2 == (for ( tup <- e; b = tup._2 ) yield b).get
      } else {
        e == (for ( tup <- e; b = tup._2 ) yield b) // left should be unchanged
      }
    })

    val prop_filterInFor = forAll((e: YinYang[Int,Int], mul : Int, passThru: Boolean) => {
      if ( e.isYang && passThru) {
        e.map(_ * mul) == (for ( x <- e if passThru ) yield (mul * x))
      } else if ( e.isYang && !passThru ) {
        try { for ( x <- e if passThru ) yield x; false }
        catch { case nse : NoSuchElementException => true; }
      } else {
        e == (for ( x <- e ) yield x) // left should be unchanged
      }
    })
  }

  object CheckYangBiasedWithEmptyToken {
    val Bias = YinYang.YangBias.withEmptyToken(-1);
    import Bias._;

    val prop_withFilter = forAll((e: YinYang[Int, Int] ) => {
      if ( e.isYang ) {
        if (e.get % 2 == 0) e.withFilter( _ % 2 == 0 ) == e;
        else e.withFilter( _ % 2 == 0 ) == Yin[Int,Int](-1)
      } else {
        e.withFilter(_ % 2 == 0) == e // left should be unchanged
      }
    })

    val prop_filterInFor = forAll((e: YinYang[Int,Int], mul : Int, passThru: Boolean) => {
      if ( e.isYang && passThru) {
        e.map(_ * mul) == (for ( x <- e if passThru ) yield (mul * x))
      } else if ( e.isYang && !passThru ) {
        (for ( x <- e if passThru ) yield x) == Yin[Int,Int](-1)
      } else {
        e == (for ( x <- e ) yield x) // left should be unchanged
      }
    })
  }

  val tests = List(
      ("YinBiased.prop_value", CheckYinBiased.prop_value),
      ("YinBiased.prop_getOrElse", CheckYinBiased.prop_getOrElse),
      ("YinBiased.prop_forall", CheckYinBiased.prop_forall),
      ("YinBiased.prop_exists", CheckYinBiased.prop_exists),
      ("YinBiased.prop_flatMapYinIdentity", CheckYinBiased.prop_flatMapYinIdentity),
      ("YinBiased.prop_flatMapYangIdentity", CheckYinBiased.prop_flatMapYangIdentity),
      ("YinBiased.prop_flatMapComposition", CheckYinBiased.prop_flatMapComposition),
      ("YinBiased.prop_mapIdentity", CheckYinBiased.prop_mapIdentity),
      ("YinBiased.prop_mapComposition", CheckYinBiased.prop_mapComposition),
      ("YinBiased.prop_seq", CheckYinBiased.prop_seq),
      ("YinBiased.prop_option", CheckYinBiased.prop_option),
      ("YinBiased.prop_withFilter", CheckYinBiased.prop_withFilter),
      ("YinBiased.prop_extractTuple", CheckYinBiased.prop_extractTuple),
      ("YinBiased.prop_assignVariable", CheckYinBiased.prop_assignVariable),
      ("YinBiased.prop_filterInFor", CheckYinBiased.prop_filterInFor),

      ("YinBiasedWithEmptyToken.prop_withFilter", CheckYinBiasedWithEmptyToken.prop_withFilter),
      ("YinBiasedWithEmptyToken.prop_filterInFor", CheckYinBiasedWithEmptyToken.prop_filterInFor),

      ("YangBiased.prop_value", CheckYangBiased.prop_value),
      ("YangBiased.prop_getOrElse", CheckYangBiased.prop_getOrElse),
      ("YangBiased.prop_forall", CheckYangBiased.prop_forall),
      ("YangBiased.prop_exists", CheckYangBiased.prop_exists),
      ("YangBiased.prop_flatMapYinIdentity", CheckYangBiased.prop_flatMapYinIdentity),
      ("YangBiased.prop_flatMapYangIdentity", CheckYangBiased.prop_flatMapYangIdentity),
      ("YangBiased.prop_flatMapComposition", CheckYangBiased.prop_flatMapComposition),
      ("YangBiased.prop_mapIdentity", CheckYangBiased.prop_mapIdentity),
      ("YangBiased.prop_mapComposition", CheckYangBiased.prop_mapComposition),
      ("YangBiased.prop_seq", CheckYangBiased.prop_seq),
      ("YangBiased.prop_option", CheckYangBiased.prop_option),
      ("YangBiased.prop_withFilter", CheckYangBiased.prop_withFilter),
      ("YangBiased.prop_extractTuple", CheckYangBiased.prop_extractTuple),
      ("YangBiased.prop_assignVariable", CheckYangBiased.prop_assignVariable),
      ("YangBiased.prop_filterInFor", CheckYangBiased.prop_filterInFor),

      ("YangBiasedWithEmptyToken.prop_withFilter", CheckYangBiasedWithEmptyToken.prop_withFilter),
      ("YangBiasedWithEmptyToken.prop_filterInFor", CheckYangBiasedWithEmptyToken.prop_filterInFor)
    )

  for ((label, prop) <- tests) {
    property(label) = prop
  }
}
