# CS2006-Calculator-H1

## running the program
```
ghc --make Main.hs -o calc 
./calc
```

### running the calculator 
```
ghci Main.hs 
main
```

### reading commands from a file 
```
ghci Main.hs 
runFile "filename.txt"
```

## calculator commands 
- `:q` = quit
- `:1` = go back to command 1 
- `:c` = clear command history
- `#`  = comment

## file commands 
- `:print` = prints out a `Set` or `Eval` command (other commands lead to errors)
- `:q`, `:c`, and `:1` are not valid file commands
