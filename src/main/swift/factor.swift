//psksvp@gmail.com
//where 

func divisibleQ(_ n:UInt64, _ d:UInt64) -> Bool
{
  return 0 == n % d
}

func evenQ(_ n:UInt64) -> Bool
{
  return 0 == n % 2
}


//AKS 
func primeQ(_ n:UInt64) -> Bool
{
  if(n < 2) { return false }
  if(n <= 3) {return true }
  if(divisibleQ(n, 2) || divisibleQ(n, 3)) {return false}
  
  var i:UInt64 = 5
  while(i * i <= n)
  {
    if(divisibleQ(n, i) || divisibleQ(n, i + 2))
    {
      return false;
    }
    i = i + 6
  }
  
  return true;
}

func nextPrime(_ p:UInt64) -> UInt64
{
  if(primeQ(p + 1))
  {
    return p + 1
  }
  else
  {
    return nextPrime(p + 1)
  }
}

func smallestPrimeDivisorOf(_ n:UInt64, _ p:UInt64 = 2) -> UInt64
{
  if(divisibleQ(n, p))
  {
    return p
  }
  else
  {
    return smallestPrimeDivisorOf(n, nextPrime(p))
  }
}

func factor(_ n:UInt64) -> [UInt64]
{
  if(primeQ(n))
  {
    return [n, 1]
  }
  else
  {
    let s = smallestPrimeDivisorOf(n)
    return [s] + factor(n / s)
  }
}

func mul(_ a:[UInt64], _ s:Int = 0) -> UInt64
{
  if(a.count - 1 == s)
  {
    return a[s]
  }
  else
  {
    return a[s] * mul(a, s + 1)
  }
}

let m:UInt64 = 777777777777777777
let r = factor(m)
print(r)
print(m == mul(r))
/*
var i = 0
var p:UInt64 = 2;
while(i < 1000)
{
  print("\(p) --> \(primeQ(p))")
  p = nextPrime(p)
  i = i + 1
} */
