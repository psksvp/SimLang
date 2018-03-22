object Iterative
{
  def div(n:Int, d:Int) : (Int, Int) =
  {
    if(n < d) 
      (0, n)
    else  
    {
      val (m, r) = div(n - d, d)
      (1 + m, r)
    }
  }
  
  def rdiv(n:Int, d:Int) : (Int, Int) =
  {
    var m = 1
    while(d * m < n)
    {
      m = m + 1
    }
    
    if(d * m == n)
      (m, 0)
    else  
      (m - 1, n - d * (m - 1))
    
  }
  
  def main(args:Array[String]) : Unit = 
  {
    println(div(5, 2))
    println(div(10, 2))
    println(div(10, 3))
    println(div(20, 3))
    println("--------")
    println(rdiv(5, 2))
    println(rdiv(10, 2))
    println(rdiv(10, 3))
    println(rdiv(20, 3))
  }
}