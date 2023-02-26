# topaz
compiled programming langauge created with C++ and LLVM, inspired by [smalltalk](https://en.wikipedia.org/wiki/Smalltalk) and [ocaml](https://en.wikipedia.org/wiki/OCaml)
```
// hello world
main|| : () -> int
  printf("Hello world!\n")
  end
```

currently has many bugs :( but cool way to learn about compilers and llvm!

### some features

#### compiler
topaz's compiler is called `tpzc` and you can compile by typing `tpzc [filename1] [filename2] ...`

#### external reference
because topaz is compiled with LLVM, you can reference C functions :)
```
extern free : (void ptr) -> void
extern malloc : (int) -> void ptr
extern memcpy : (char ptr, char ptr, int) -> char ptr
extern strlen : (char ptr) -> int
extern strcat : (char ptr, char ptr) -> char ptr
extern strcmp : (char ptr, char ptr) -> int
```

#### pointer syntax
```
x = 10
y ==> x // y is pointing to x
y <== 4 // effectively setting x = 4
ref x   // get address of x. should be equal to value of y
```

#### type declarations
topaz type declarations are very much inspired by ocaml's type syntax.
```
// type declaration method #1
func foobar : (char ptr) -> void

// type declaration method #2
foobar|x| : (char ptr) -> void
  printf("%s", x)
  end
```

#### object oriented programming
```
// Vector example

func main : () -> int

class Vector :
  x : int
  y : int
  func constructor : (int, int) -> void
  func add : (Vector) -> Vector
  func str : () -> char ptr
  end

Vector|x, y|
  this.x = x
  this.y = y

  add|vec|
    z = new Vector(this.x + vec.x, this.y + vec.y)
    return z
    end

  str||
    s = malloc(32)
    sprintf(s, "<%d, %d>", this.x, this.y)
    return s
    end
  end

main||
  x = new Vector(3, 5)
  y = new Vector(8, 10)
  
  printf("%s\n", x.add(y).str()) // Prints out <11, 15>
  end
```


