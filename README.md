# LaTeX Wahrheitstabellen Generator
Kleines anfänger Projekt zur generierung von Wahrheitstabellen aus Aussagenlogischen Formeln im LaTeX Format.

Bsp.:
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
Unterstützte Operatoren:        
**& = And    
| = Or     
\- = Not**        
Sehr fehlerhaft (gerade bei nutzung von - )
