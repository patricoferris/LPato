# LPato 🦆
-----------

A simple programming language that currently only supports `lambda` functions and named (`func`) functions as well as the four operators (`+ - * /`) on integers. Each program can only return a single value and that will be printed to the console. Here is an example: 

```
func add(x, y) => x + y {
  add(4, 5)
}	
```

The add function is available within the scope of the curly brackets that follow it (and any nested function that follows). Lambda functions can be written as: 

```
lambda (x) => lambda (y) => x + y end end (4) (5) 
```

Using sequences you can define functions and then use them later and with the `let` construct you can also assign variable names to values. Here is an example:

```
(func add(x, y) => x + y end);
(let z = 4);
(add(4, z));;
```

LPato also supports two numerical comparators `==` and `<` which should be enough. With these it is possible to define much more powerful examples using recursion and catching the base case with the `if-then-else` in conjunction with a comparator. Below is the simple fibonacci function:

```
(func fib(x) => 
  if (x == 0) || (x == 1) then 1
  else fib(x - 1) + fib(x - 2) end);
(fib(10));;
```

## What's next?
----------------

- Adding some sort of print construct - because it looks like a duck `(o)<`
- How to remove the need for brackets surrounding sequences (probably a shift-reduce problem)
