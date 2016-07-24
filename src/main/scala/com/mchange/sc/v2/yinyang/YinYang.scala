package com.mchange.sc.v2.yinyang

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

