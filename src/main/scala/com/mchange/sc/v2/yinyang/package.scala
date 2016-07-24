package com.mchange.sc.v2

package object yinyang {
  implicit class RetrofitYinYang[+A,+B]( val inner : YinYang[A,B] ) extends AnyVal {
    def toEither : Either[A,B] = {
      inner match {
        case Yin( a )  => Left(a)
        case Yang( b ) => Right(b)
      }
    }
  }
  implicit class RetrofitYin[+A,+B]( val inner : Yin[A,B] ) extends AnyVal {
    def toLeft : Left[A,B] = Left( inner.value )
  }
  implicit class RetrofitYang[+A,+B]( val inner : Yang[A,B] ) extends AnyVal {
    def toRight : Right[A,B] = Right( inner.value )
  }
}
