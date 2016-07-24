package com.mchange.sc.v2.yinyang

import scala.language.implicitConversions;

object YinYang {
  trait Bias[+E] {
    def empty : E;

    def isYinBias  : Boolean;
    def isYangBias : Boolean = !isYinBias;
    def conformsToBias[A,B]( target : YinYang[A,B] ) : Boolean = {
      target match {
        case Yin( _ )  => isYinBias;
        case Yang( _ ) => isYangBias;
      }
    }
  }
  final object YangBias {

    private[YinYang] final val DefaultThrowingOps = withEmptyToken.Throwing( throw new NoSuchElementException( noSuchElementMessage( true ) ) );

    implicit class Ops[A,B]( val target : YinYang[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[AA >: A, Z]( f : B => YinYang[AA,Z] ) : YinYang[AA,Z] = DefaultThrowingOps.flatMap[A,AA,B,Z]( target )( f );
      def map[Z]( f : B => Z )                          : YinYang[A,Z]  = DefaultThrowingOps.map( target )( f );
      def withFilter( p : B => Boolean )                : YinYang[A,B]  = DefaultThrowingOps.withFilter( target )( p );

      // extra ops
      def exists( f : B => Boolean )                  : Boolean           = DefaultThrowingOps.exists( target )( f );
      def forall( f : B => Boolean )                  : Boolean           = DefaultThrowingOps.forall( target )( f );
      def foreach[U]( f : B => U )                    : Any               = DefaultThrowingOps.foreach( target )( f );
      def get                                         : B                 = DefaultThrowingOps.get( target );
      def getOrElse[ BB >: B ]( or : =>BB )           : BB                = DefaultThrowingOps.getOrElse[A,B,BB]( target )( or );
      def toOption                                    : Option[B]         = DefaultThrowingOps.toOption( target );
      def toSeq                                       : collection.Seq[B] = DefaultThrowingOps.toSeq( target );
      def xget                                        : A                 = DefaultThrowingOps.xget( target );
      def xgetOrElse[AA>:A]( or : =>AA )              : AA                = DefaultThrowingOps.xgetOrElse[A,AA,B]( target )( or );
      def xmap[Z]( f : A => Z )                       : YinYang[Z,B]       = DefaultThrowingOps.xmap( target )( f );
      def replaceIfEmpty[AA>:A]( replacement : =>AA ) : YinYang[AA,B]      = DefaultThrowingOps.replaceIfEmpty[A,AA,B]( target )( replacement );
      def isEmpty                                     : Boolean           = DefaultThrowingOps.isEmpty( target );
      def isYinBiased                                 : Boolean           = DefaultThrowingOps.isYinBias;
      def isYangBiased                                : Boolean           = DefaultThrowingOps.isYangBias;
      def conformsToBias                              : Boolean           = DefaultThrowingOps.conformsToBias( target );
    }

    object withEmptyToken {
      abstract class AbstractOps[A,B]( target : YinYang[A,B] )( opsTypeClass : YinYang.YangBias.withEmptyToken.Generic[A] ) {

        // monad ops
        def flatMap[AA >: A, Z]( f : B => YinYang[AA,Z] ) : YinYang[AA,Z] = opsTypeClass.flatMap[A,AA,B,Z]( target )( f );
        def map[Z]( f : B => Z )                          : YinYang[A,Z]  = opsTypeClass.map( target )( f );
        def withFilter( p : B => Boolean )                : YinYang[A,B]  = opsTypeClass.withFilter( target )( p );

        // extra ops
        def exists( f : B => Boolean )                  : Boolean           = opsTypeClass.exists( target )( f );
        def forall( f : B => Boolean )                  : Boolean           = opsTypeClass.forall( target )( f );
        def foreach[U]( f : B => U )                    : Any               = opsTypeClass.foreach( target )( f );
        def get                                         : B                 = opsTypeClass.get( target );
        def getOrElse[BB>:B]( or : =>BB )               : BB                = opsTypeClass.getOrElse[A,B,BB]( target )( or );
        def toOption                                    : Option[B]         = opsTypeClass.toOption( target );
        def toSeq                                       : collection.Seq[B] = opsTypeClass.toSeq( target );
        def isEmpty                                     : Boolean           = opsTypeClass.isEmpty( target );
        def xget                                        : A                 = opsTypeClass.xget( target );
        def xgetOrElse[AA>:A]( or : =>AA )              : AA                = opsTypeClass.xgetOrElse[A,AA,B]( target )( or );
        def xmap[Z]( f : A => Z )                       : YinYang[Z,B]       = opsTypeClass.xmap( target )( f );
        def replaceIfEmpty[AA>:A]( replacement : =>AA ) : YinYang[AA,B]      = opsTypeClass.replaceIfEmpty[A,AA,B]( target )( replacement );
        def isYinBiased                                 : Boolean           = opsTypeClass.isYinBias;
        def isYangBiased                                : Boolean           = opsTypeClass.isYangBias;
        def conformsToBias                              : Boolean           = opsTypeClass.conformsToBias( target );
      }

      implicit final class Ops[A,B]( target : YinYang[A,B] )( implicit opsTypeClass : YinYang.YangBias.withEmptyToken.Generic[A] ) extends AbstractOps( target )( opsTypeClass )

      trait Generic[+E] extends YinYang.Bias[E] {
        /*
         * In order to meet the contract of withFilter(...) [from which this method is called],
         * no object allocation should occur on each non-Exception-raising call of this method.
         * Raising an exception is "fine" (in that it represents a hackish violation of the contract
         * anyway), as is overriding this with a val. But no new Yin should be created on each
         * invocation.
         */ 
        protected def yinEmpty : Yin[E,Nothing];

        def isEmpty[A>:E,B]( target : YinYang[A,B] ) : Boolean;

        // monad ops
        def flatMap[A>:E,AA>:A,B,Z]( target : YinYang[A,B] )( f : B => YinYang[AA,Z] ) : YinYang[AA,Z] = {
          target match {
            case Yin( _ )  => target.asInstanceOf[Yin[A,Z]]
            case Yang( b ) => f( b )
          }
        }
        def map[A>:E,B,Z]( target : YinYang[A,B] )( f : B => Z ) : YinYang[A,Z] = {
          target match {
            case Yin( _ )  => target.asInstanceOf[Yin[A,Z]]
            case Yang( b ) => Yang( f( b ) )
          }
        }
        def withFilter[A>:E,B]( target : YinYang[A,B] )( p : B => Boolean ) : YinYang[A,B] = {
          target match {
            case      Yin( _ ) => target;
            case r @ Yang( b ) => if ( p(b) ) r else yinEmpty;
          }
        }

        // extra ops
        def exists[A>:E,B]( target : YinYang[A,B] )( f : B => Boolean ) : Boolean = {
          target match {
            case Yin( _ )  => false;
            case Yang( b ) => f( b );
          }
        }
        def forall[A>:E,B]( target : YinYang[A,B] )( f : B => Boolean ) : Boolean = {
          target match {
            case Yin( _ )  => true;
            case Yang( b ) => f( b );
          }
        }
        def foreach[A>:E,B,U]( target : YinYang[A,B] )( f : B => U ) : Any = {
          target match {
            case Yin( _ )  => ();
            case Yang( b ) => f( b );
          }
        }
        def get[A>:E,B]( target : YinYang[A,B] ) : B = {
          target match {
            case Yin( _ )  => throw new NoSuchElementException( NoSuchYangMessage );
            case Yang( b ) => b;
          }
        }
        def getOrElse[A>:E,B,BB>:B]( target : YinYang[A,B] )( or : =>BB ) : BB = {
          target match {
            case Yin( _ )  => or;
            case Yang( b ) => b;
          }
        }
        def toOption[A>:E,B]( target : YinYang[A,B] ) : Option[B] = {
          target match {
            case Yin( _ )  => None;
            case Yang( b ) => Some( b );
          }
        }
        def toSeq[A>:E,B]( target : YinYang[A,B] ) : collection.Seq[B] = {
          target match {
            case Yin( _ )  => collection.Seq.empty[B];
            case Yang( b ) => collection.Seq( b );
          }
        }
        def xget[A>:E,B]( target : YinYang[A,B] ) : A = {
          target match {
            case Yin( a )  => a;
            case Yang( _ ) => throw new NoSuchElementException( NoSuchXYinMessage );
          }
        }
        def xgetOrElse[A>:E,AA>:A,B]( target : YinYang[A,B] )( or : =>AA ) : AA = {
          target match {
            case Yin( a )  => a;
            case Yang( _ ) => or;
          }
        }
        def xmap[A>:E,B,Z]( target : YinYang[A,B] )( f : A => Z ) : YinYang[Z,B] = {
          target match {
            case Yin( a )  => Yin( f( a ) )
            case Yang( _ ) => target.asInstanceOf[Yang[Z,B]]
          }
        }
        def replaceIfEmpty[A>:E,AA>:A,B]( target : YinYang[A,B] )( replacement : =>AA ) : YinYang[AA,B] = {
          if (isEmpty( target )) Yin( replacement ) else target;
        }
        def isYinBias  : Boolean = false;

        implicit def toOps[A>:E,B]( target : YinYang[A,B] ) : YangBias.withEmptyToken.Ops[A,B] = new YangBias.withEmptyToken.Ops[A,B]( target )( this )
      }
      def apply[E]( token : E ) : withEmptyToken[E] = new withEmptyToken( token );

      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends withEmptyToken.Generic[Nothing] {
        override protected def yinEmpty : Nothing = empty;

        override def empty : Nothing = throw throwableBuilder;

        override def isEmpty[A,B]( target : YinYang[A,B] ) : Boolean = false; // no state represents empty, empties cannot be formed as an Exception is thrown when it is tried
      }
    }

    final class withEmptyToken[+E] private( override val empty : E ) extends withEmptyToken.Generic[E] {
      override protected val yinEmpty : Yin[E,Nothing] = Yin(empty);

      override def isEmpty[A>:E,B]( target : YinYang[A,B] ) : Boolean = (target == yinEmpty);
    }
    abstract class Base[A]( emptyToken : A ) extends YangBias[A] {
      override val EmptyTokenDefinition = YangBias.withEmptyToken[A]( emptyToken )
    }
  }
  trait YangBias[A] {
    val EmptyTokenDefinition : YinYang.YangBias.withEmptyToken.Generic[A] = YangBias.DefaultThrowingOps;

    implicit def toYangBiasEtherOps[B]( target : YinYang[A,B] ) : YangBias.withEmptyToken.AbstractOps[A,B] = new YangBias.withEmptyToken.Ops[A,B]( target )( EmptyTokenDefinition );
  }

  final object YinBias {

    private[YinYang] final val DefaultThrowingOps = withEmptyToken.Throwing( throw new NoSuchElementException( noSuchElementMessage( false ) ) );

    implicit class Ops[A,B]( val target : YinYang[A,B] ) extends AnyVal { // alas, we don't define a trait, but write all these ops twice, so we can avoid boxing here

      // monad ops
      def flatMap[BB >: B, Z]( f : A => YinYang[Z,BB] ) : YinYang[Z,BB] = DefaultThrowingOps.flatMap[A,B,BB,Z]( target )( f );
      def map[Z]( f : A => Z )                          : YinYang[Z,B]  = DefaultThrowingOps.map( target )( f );
      def withFilter( p : A => Boolean )                : YinYang[A,B]  = DefaultThrowingOps.withFilter( target )( p );

      // extra ops
      def exists( f : A => Boolean )                  : Boolean           = DefaultThrowingOps.exists( target )( f );
      def forall( f : A => Boolean )                  : Boolean           = DefaultThrowingOps.forall( target )( f );
      def foreach[U]( f : A => U )                    : Any               = DefaultThrowingOps.foreach( target )( f );
      def get                                         : A                 = DefaultThrowingOps.get( target );
      def getOrElse[AA >: A ]( or : =>AA )            : AA                = DefaultThrowingOps.getOrElse[A,AA,B]( target )( or );
      def toOption                                    : Option[A]         = DefaultThrowingOps.toOption( target );
      def toSeq                                       : collection.Seq[A] = DefaultThrowingOps.toSeq( target );
      def isEmpty                                     : Boolean           = DefaultThrowingOps.isEmpty( target );
      def xget                                        : B                 = DefaultThrowingOps.xget( target );
      def xgetOrElse[BB>:B]( or : =>BB )              : BB                = DefaultThrowingOps.xgetOrElse[A,B,BB]( target )( or );
      def xmap[Z]( f : B => Z )                       : YinYang[A,Z]       = DefaultThrowingOps.xmap( target )( f );
      def replaceIfEmpty[BB>:B]( replacement : =>BB ) : YinYang[A,BB]      = DefaultThrowingOps.replaceIfEmpty[A,B,BB]( target )( replacement )
      def isYinBiased                                 : Boolean           = DefaultThrowingOps.isYinBias;
      def isYangBiased                                : Boolean           = DefaultThrowingOps.isYangBias;
      def conformsToBias                              : Boolean           = DefaultThrowingOps.conformsToBias( target );
    }

    object withEmptyToken {
      abstract class AbstractOps[A,B]( target : YinYang[A,B] )( opsTypeClass : YinYang.YinBias.withEmptyToken.Generic[B] ) {

        // monad ops
        def flatMap[BB >: B, Z]( f : A => YinYang[Z,BB] ) : YinYang[Z,BB] = opsTypeClass.flatMap[A,B,BB,Z]( target )( f );
        def map[Z]( f : A => Z ) : YinYang[Z,B] = opsTypeClass.map( target )( f );
        def withFilter( p : A => Boolean ) : YinYang[A,B] = opsTypeClass.withFilter( target )( p );

        // extra ops
        def exists( f : A => Boolean ) : Boolean = opsTypeClass.exists( target )( f );
        def forall( f : A => Boolean ) : Boolean = opsTypeClass.forall( target )( f );
        def foreach[U]( f : A => U ) : Any = opsTypeClass.foreach( target )( f );
        def get : A = opsTypeClass.get( target );
        def getOrElse[AA >: A ]( or : =>AA ) : AA = opsTypeClass.getOrElse[A,AA,B]( target )( or );
        def toOption : Option[A] = opsTypeClass.toOption( target );
        def toSeq : collection.Seq[A] = opsTypeClass.toSeq( target );
        def isEmpty : Boolean = opsTypeClass.isEmpty( target );
        def xget : B = opsTypeClass.xget( target );
        def xgetOrElse[BB>:B]( or : =>BB ) : BB = opsTypeClass.xgetOrElse[A,B,BB]( target )( or );
        def xmap[Z]( f : B => Z ) : YinYang[A,Z] = opsTypeClass.xmap( target )( f );
        def replaceIfEmpty[BB>:B]( replacement : =>BB ) : YinYang[A,BB] = opsTypeClass.replaceIfEmpty[A,B,BB]( target )( replacement )
        def isYinBiased : Boolean = opsTypeClass.isYinBias;
        def isYangBiased : Boolean = opsTypeClass.isYangBias;
        def conformsToBias : Boolean = opsTypeClass.conformsToBias( target );
      }

      implicit final class Ops[A,B]( target : YinYang[A,B] )( implicit opsTypeClass : YinYang.YinBias.withEmptyToken.Generic[B] ) extends AbstractOps( target )( opsTypeClass );

      trait Generic[+E] extends YinYang.Bias[E] {
        /*
         * In order to meet the contract of withFilter(...) [from which this method is called],
         * no object allocation should occur on each non-Exception-raising call of this method.
         * Raising an exception is "fine" (in that it represents a hackish violation of the contract
         * anyway), as is overriding this with a val. But no new Yang should be created on each
         * invocation.
         */ 
        protected def yangEmpty : Yang[Nothing,E]; 

        def isEmpty[A>:E,B]( target : YinYang[A,B] ) : Boolean;

        // monad ops
        def flatMap[A, B>:E, BB>:B ,Z]( target : YinYang[A,B] )( f : A => YinYang[Z,BB] ) : YinYang[Z,BB] = {
          target match {
            case Yin( a )  => f( a )
            case Yang( _ ) => target.asInstanceOf[Yang[Z,B]]
          }
        }
        def map[A, B>:E, Z]( target : YinYang[A,B] )( f : A => Z ) : YinYang[Z,B] = {
          target match {
            case Yin( a )  => Yin( f( a ) )
            case Yang( _ ) => target.asInstanceOf[Yang[Z,B]]
          }
        }
        def withFilter[A,B>:E]( target : YinYang[A,B] )( p : A => Boolean ) : YinYang[A,B] = {
          target match {
            case l @  Yin( a ) => if ( p(a) ) l else yangEmpty;
            case     Yang( _ ) => target;
          }
        }

        // extra ops
        def exists[A,B>:E]( target : YinYang[A,B] )( f : A => Boolean ) : Boolean = {
          target match {
            case Yin( a )  => f(a);
            case Yang( _ ) => false;
          }
        }
        def forall[A,B>:E]( target : YinYang[A,B] )( f : A => Boolean ) : Boolean = {
          target match {
            case Yin( a )  => f(a)
            case Yang( _ ) => true;
          }
        }
        def foreach[A,B>:E,U]( target : YinYang[A,B] )( f : A => U ) : Any = {
          target match {
            case Yin( a )  => f(a);
            case Yang( _ ) => ();
          }
        }
        def get[A,B>:E]( target : YinYang[A,B] ) : A = {
          target match {
            case Yin( a )  => a;
            case Yang( _ ) => throw new NoSuchElementException( NoSuchYinMessage );
          }
        }
        def getOrElse[A, AA>:A, B>:E]( target : YinYang[A,B] )( or : =>AA ) : AA = {
          target match {
            case Yin( a )  => a;
            case Yang( _ ) => or;
          }
        }
        def toOption[A,B>:E]( target : YinYang[A,B] ) : Option[A] = {
          target match {
            case Yin( a )  => Some( a );
            case Yang( _ ) => None; 
          }
        }
        def toSeq[A,B>:E]( target : YinYang[A,B] ) : collection.Seq[A] = {
          target match {
            case Yin( a )  => collection.Seq( a );
            case Yang( _ ) => collection.Seq.empty[A];
          }
        }
        def xget[A,B>:E]( target : YinYang[A,B] ) : B = {
          target match {
            case Yin( _ )  => throw new NoSuchElementException( NoSuchXYangMessage );
            case Yang( b ) => b;
          }
        }
        def xgetOrElse[A,B>:E,BB>:B]( target : YinYang[A,B] )( or : =>BB ) : BB = {
          target match {
            case Yin( _ )  => or;
            case Yang( b ) => b;
          }
        }
        def xmap[A,B>:E,Z]( target : YinYang[A,B] )( f : B => Z ) : YinYang[A,Z] = {
          target match {
            case Yin( _ )  => target.asInstanceOf[Yin[A,Z]]
            case Yang( b ) => Yang( f(b) )
          }
        }
        def replaceIfEmpty[A,B>:E,BB>:B]( target : YinYang[A,B] )( replacement : =>BB ) : YinYang[A,BB] = {
          if (isEmpty( target )) Yang( replacement ) else target;
        }
        def isYinBias  : Boolean = true;

        implicit def toOps[A,B>:E]( target : YinYang[A,B] ) : YinBias.withEmptyToken.Ops[A,B] = new YinBias.withEmptyToken.Ops[A,B]( target )( this )
      }
      def apply[E]( token : E ) : withEmptyToken[E] = new withEmptyToken( token );

      object Throwing {
        def apply( throwableBuilder : =>java.lang.Throwable ) : Throwing = new Throwing( throwableBuilder );
      }
      final class Throwing private( throwableBuilder : =>java.lang.Throwable ) extends withEmptyToken.Generic[Nothing] {
        override protected def yangEmpty : Nothing = empty;

        override def empty : Nothing = throw throwableBuilder;

        override def isEmpty[A,B]( target : YinYang[A,B] ) : Boolean = false; // no state represents empty, empties cannot be formed as an Exception is thrown when it is tried
      }
    }
    final class withEmptyToken[+E] private( override val empty : E ) extends withEmptyToken.Generic[E] {
      override protected val yangEmpty : Yang[Nothing,E] = Yang(empty);

      override def isEmpty[A>:E,B]( target : YinYang[A,B] ) : Boolean = (target == yangEmpty);
    }
    abstract class Base[B]( emptyToken : B ) extends YinBias[B] {
      override val EmptyTokenDefinition = YinBias.withEmptyToken[B]( emptyToken )
    }
  }
  trait YinBias[B] {
    val EmptyTokenDefinition : YinYang.YinBias.withEmptyToken.Generic[B] = YinBias.DefaultThrowingOps;

    implicit def toYinBiasEtherOps[A]( target : YinYang[A,B] ) : YinBias.withEmptyToken.AbstractOps[A,B] = new YinBias.withEmptyToken.Ops[A,B]( target )( EmptyTokenDefinition );
  }

  private def noSuchElementMessage[A,B]( yangBias : Boolean, mbYinYang : Option[YinYang[A,B]] = None ) = {
    val bias = if ( yangBias ) "Yang-biased" else "Yin-biased";
    val withToken = if ( yangBias ) "YangBias.withEmptyToken" else "YinBias.withEmptyToken";
    val eitherRep = mbYinYang.fold(" ")( either => s" '${either}' " );
    s"${bias} YinYang${eitherRep}filtered to empty or failed to match a pattern. Consider using ${withToken}"
  }

  private val NoSuchYinMessage = "Can't get a value from a yin-biased YinYang which is in fact a Yang.";
  private val NoSuchYangMessage = "Can't get a value from a yang-biased YinYang which is in fact a Yin.";
  private val NoSuchXYinMessage = "This yang-biased either is in fact a Yang. xget requires a value against its bias."
  private val NoSuchXYangMessage = "This yin-biased either is in fact a Yin. xget requires a value against its bias."
}
sealed trait YinYang[+A,+B] {
  def isYin  : Boolean
  def isYang : Boolean = !isYin

  def fold[X]( fa : A => X, fb : B => X ) : X

  def swap : YinYang[B,A]
}

case class Yin[+A,+B]( value : A ) extends YinYang[A,B] {
  def isYin : Boolean = true

  def fold[X]( fa : A => X, fb : B => X ) : X = fa( value )

  def swap : YinYang[B,A] = Yang[B,A]( value )
}

case class Yang[+A,+B]( value : B ) extends YinYang[A,B] {
  def isYin : Boolean = false

  def fold[X]( fa : A => X, fb : B => X ) : X = fb( value )

  def swap : YinYang[B,A] = Yin[B,A]( value )
}

