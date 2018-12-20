# LaTeX Truthtable Generator
Small beginner Haskell Project, creates Truthtable in LaTeX format.
Example:
```
Please enter Formula
A&(B|C)
    A	B	C	A&(B|C)
    False & False & False & False\\
    False & False & True & False\\
    False & True & False & False\\
    False & True & True & False\\
    True & False & False & False\\
    True & False & True & True\\
    True & True & False & True\\
    True & True & True & True\\
```
Supported Operators:        
**& = And    
| = Or     
\- = Not     
< = <- Implies        
\> = -> Implies    
\# = <-\> Biconditional    
Single Letter Variable names     
Brackets ( )**        
