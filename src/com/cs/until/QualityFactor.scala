package com.cs.until

import scala.collection.mutable.ArrayBuffer

object QualityFactor {
  def primeFactor(x: Integer): Array[Integer] = {
    var num = x
    val flag = false
    var result = new ArrayBuffer[Integer]
    if (num <= 2) {
      result.append(num)
    } else {
      var primeNumber = 2  
      while (primeNumber <= num) {
        if (num % primeNumber == 0){
          result.append(primeNumber)
          num = num / primeNumber
        } else {
          primeNumber = primeNumber + 1
        }
      }
    }
    println(result.toArray.mkString(";"))
    result.toArray
  }
}