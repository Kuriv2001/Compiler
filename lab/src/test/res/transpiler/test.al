//BEGIN Narrowing unconditionally should work with valid cast (2pts)
let x = ((1 @ Any) @! Int)
let main = print(x)
