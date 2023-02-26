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
topaz's compiler is called `tpzc` and you can compile by typing `./tpzc [filename1] [filename2] ...`. it is complete with syntax and type checking and will output (50% of the time) helpful error messages!

`tpzc` is actually more of a linker as all it really does is link the `.o` files together. it calls a different program called `tpz2obj` which actually compiles your source code to `.o` files through LLVM.

#### "header" files
`.tdc` files are for all of the type declarations, similar to a C header file. `.tpz` files are for definitions.

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
it's recommended to declare class types first, like this:
```
class Vector :
  x : int
  y : int
  func constructor : (int, int) -> void
  func add : (Vector) -> Vector
  end
```

then define your class.
```
Vector|x, y|
  this.x = x
  this.y = y

  add|vec|
    z = new Vector(this.x + vec.x, this.y + vec.y)
    return z
    end
```
#### variants
similar to C++ `templates` and ocaml functors. lets you create the same function or class but for specified types.
```
variant Tuple|type1, type2| :
  first            : type1
  second           : type2
  func constructor : (type1, type2) -> void
	end
```
you can instantiate variants like so:
```
tup = new Tuple<int, int>(3, 5)
```
#### custom types
can use `type` keyword to give an alias to a certain type
```
type String : char ptr
type Stack  : List<int>
```

there's also a `template` keyword that's basically functions but with types (you input a type and it returns a type). not particularly useful but somewhat saves time if types become too big and ugly and has too many nested <> brackets...
```
template dict_val|dict_val_t|:
	LinkedList<KeyValPair<char ptr, typeof: dict_val_t>>
  ...
  
buckets : List<dict_val<dict_type>>
```

