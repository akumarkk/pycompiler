# pycompiler
 Python 3.4 compiler includes the following components :
 1. Lexical analyzer [ Pylex ]
 2. Parser [ Pyparse ]
 3. High-level translator [ Pydesugar1 ]
 4. Normalizer & CPS converter [ Pydesugar2 ]
 5. Low-level translator
 6. Register allocator & Assembly code emitter
 
Pylex is implemented as per the specification https://docs.python.org/3/reference/lexical_analysis.html
Pyparse is implemented for all the grammars defined in Python 3.4 https://docs.python.org/3.4/reference/grammar.html
Pydesugar1 includes desugaring all the Python language elements.


