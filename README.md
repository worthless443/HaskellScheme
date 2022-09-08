The parser 
---

A scheme/lisp like syntax parsing, namely for what works 

```
"(test a)"
"(a (test a))"
```
produces error if:
```
"(test a)"
"(a (test a)"
	    ^
	    missing paren
```
p

