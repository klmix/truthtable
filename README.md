# LaTeX Truthtable Generator
Small beginner Haskell Project, creates Truthtable in LaTeX format.
Example:
```
Please enter Formula
A>(-A&B)
A   B   A>(-A&B)
0 & 0 & 1\\
0 & 1 & 1\\
1 & 0 & 0\\
1 & 1 & 0\\
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
