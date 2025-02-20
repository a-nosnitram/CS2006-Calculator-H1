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
runFile "test-files/filename.txt"
```

## calculator commands 
- `:q` = quit
- `:n` = go back to command number `n`
- `:c` = clear command history
- `#`  = comment
- `:print` = prints out one or more expressions (concatenated using `++`)
- `:loop n [ command1 ; command2 ]` = loops commands (excluding `:c`, `:q`, and `:n`) `n` times

## file commands 
- `:q`, `:c`, and `:1` are not valid file commands
