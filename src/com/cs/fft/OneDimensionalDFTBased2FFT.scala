package com.cs.fft

import com.cs.until.Complex

/**
 * 一维DFT的基2FFT算法-时域抽取
 */
object OneDimensionalDFTBased2FFT {
  def fft(x: Array[Complex]): Array[Complex] = {
    val N = x.size
    if (N == 1) {
      x
    } else if (!isPowerOfTwo(N)) {
      dft(x)
    } else {
      //偶数抽取
      val even = new Array[Complex](N/2)
      for (i <- 0 until N/2) {
        even(i) = x(2 * i)
      }
      val evenValue = fft(even)
      //奇数抽取
      val odd = new Array[Complex](N/2)
      for (i <- 0 until N/2) {
        odd(i) = x(2 * i + 1)
      }
      val oddValue = fft(odd)
      val result = new Array[Complex](N)
      for (i <- 0 until N/2) {
        //欧拉公式 e^(-i*2pi*k/N) = cos(-2pi*k/N) + i*sin(-2pi*k/N)
        val kth = -2 * i * Math.PI / N
        val m = new Complex(Math.cos(kth), Math.sin(kth))
        //蝶形计算
        result(i) = evenValue(i).plus(m.multiply(oddValue(i)))
        result(i + N/2) = evenValue(i).minus(m.multiply(oddValue(i)))
      }
      result
    }
  }
  
  /**
   * 计算双面频谱y,然后根据y和偶数信号长度N计算单面频谱y1，接着定义频域f，以数组形式返回对应的单面振幅频谱图
   */
  def doSth(x: Array[Complex]): Array[Array[Double]] = {
    val size = x.size
    val Fs = x.size
    val y = new Array[Double](size)
    for (i <- 0 until size) {
      y(i) = x(i).divideDouble(Fs).abs
    }
    val y1 = new Array[Double](size / 2 + 1)
    for (i <- 0 until size / 2 + 1) {
      y1(i) = y(i)
    }
    for (i <- 1 until y1.size / 2 + 1) {
      y1(i) = y1(i) * 2
    }
    val f = new Array[Double](size/2 + 1)
    for (i <- 0 until size/2 + 1) {
      f(i) = Fs * i / size 
    }
    Array(f, y1)
  }
  
  def dft(x: Array[Complex]): Array[Complex] = {
    val N = x.size
    if (N == 1) {
      x
    } else {
      val result = new Array[Complex](N)
      for (i <- 0 until N) {
        result(i) = new Complex(0, 0)
        for (k <- 0 until N) {
          val kth = -2 * k * i * Math.PI / N
          val m = new Complex(Math.cos(kth), Math.sin(kth))
          result(i) = result(i).plus(x(k).multiply(m))
        }
      }
      result
    }
  }
  
  def show(x: Array[Array[Double]]) = {
    println("-------------------")
    for(i <- 0 until x(0).size) {
      println(x(0)(i) + " ----- " + x(1)(i))
    }
    println("-------------------")
  }
  
  def isPowerOfTwo(x: Int): Boolean = {
//    x > 0 & (x & (x - 1)) == 0 //检测是否是2的幂次方
    x % 2 == 0
  }
  
}