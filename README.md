# Wallaby Compiler

> By [Marco Marasco](https://github.com/marcom48/), [Austen McClernon](https://github.com/kvoli/) & [Karl Flores](https://github.com/karlflores)

This project contains a compiler for a procedural (C-like) language called Roo, implemented in Haskell. The compiler translates Roo source programs to the assembly language of a target machine named Oz. These programs are then be run via an Oz emulator.


## Compilation Steps
```bash
make
cd Oz
make
```


## Create Abstract Syntax Tree (AST)
```bash
./Roo -a prime.roo
```

## Pretty Print Roo File
```bash
./Roo -p prime.roo
```

## Generate Intermediate Code
```bash
./Roo prime.roo > prime.oz
```

## Execute Oz Code
> Note: Must first generate Intermediate Code as above.
```bash
./Oz/oz prime.oz
```
