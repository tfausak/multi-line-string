# multi-line-string

This pre-processor lets you write multi-line strings in Haskell using `"""..."""` syntax.
It's a simple pre-processor and works as you'd expect, but I wouldn't recommend using it.
It will break pretty much any other tool that tries to deal with Haskell files.
I would recommend using quasi-quotes instead.
