package com.cs.until

class Complex(real:Double, imag: Double) {
  private var re = real
  private var im = imag
  
  override def toString(): String = {
    if (im == 0){
      re + ""
    } else if (re == 0) {
      im + "i"
    } else if (im < 0) {
      re + " - " + (-im) + "i"
    } else {
      re + " + " + im + "i"
    }
  }
  
  def abs(): Double = {
    Math.hypot(re, im)
  }
  
  def phase(): Double = {
    Math.atan2(im, re)
  }
  
  def plus(b: Complex): Complex = {
    val a = this
    val real = a.re + b.re
    val imag = a.im + b.im
    new Complex(real, imag)
  }
  
  def minus(b: Complex): Complex = {
    var a = this
    val real = a.re - b.re
    val imag = a.im - b.im
    new Complex(real, imag)
  }
  
  def multiply(b: Complex): Complex = {
    val a = this
    val real = a.re * b.re - a.im * b.im
    val imag = a.re * b.im + a.im * b.re
    new Complex(real, imag)
  }
  
  def divideDouble(b: Double): Complex = {
    val a = this
    val real = a.re / b
    val imag = a.im / b
    new Complex(real, imag)
  }
  
  def getRe(): Double = { this.re }
  
  def getIm(): Double = { this.im }
  
  def setRe(x: Double) = { this.re = x }
  
  def setIm(x: Double) = { this.im = x }
}