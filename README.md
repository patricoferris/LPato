# LPato 🦆
-----------

A simple programming language that currently only supports `lambda` functions and named (`func`) functions as well as the four operators (`+ - * /`) on integers. Each program can only return a single value and that will be printed to the console. Here is an example: 

```
func add(x, y) => x + y {
  add(4, 5)
}	
```

The add function is available within the scope of the curly brackets that follow it (and any nested function that follows. Lambda functions can be written as: 

```
lambda (x) => lambda (y) => x + y end end 4 5 
```

### What's next?
----------------

- Add booleans along with if-then-else statements
- Have a global scope so functions are available across multiple lines
- Allow functions to be recursive

