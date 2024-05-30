//BEGIN Method negate on Int should negate the integer (1pt)
fun Int.negate() -> Int { -self }
let main = print((42).negate())

//OUT
-42
//END

//BEGIN Method negate on negative Int should negate the integer (1pt)
fun Int.negate() -> Int { -self }
let main = print((-42).negate())

//OUT
42
//END


//BEGIN Method double on Float should return double the value (1pt)
fun Float.doubleur() -> Float { self * 2 }
let main = print((21.5).doubleur())

//OUT
43.0
//END

//BEGIN Method square on Int should return the square of the integer (1pt)
fun Int.square() -> Int { self * self }
let main = print((7).square())

//OUT
49
//END

//BEGIN Chaining methods on Int should work correctly (2pts)
fun Int.negate() -> Int { -self }
fun Int.square() -> Int { self * self }
let main = print((5).negate().square())

//OUT
25
//END

//BEGIN Method call inside an if expression should work (2pts)
fun Int.isPositive() -> Bool { self > 0 }
let main = if ((10).isPositive()) then print("Positive") else print("Negative")

//OUT
Positive
//END