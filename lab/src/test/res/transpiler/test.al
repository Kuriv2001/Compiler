//BEGIN Narrowing conditionally should return a #some if a subtype (2pts)
let main = match ((1 @ Any) @? Int) {
    case #some(let x) then print(x)
    case #none then print("none")
}