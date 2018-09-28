# DESTRUCTURING-BIND*

The standard `DESTRUCTURING-BIND` signals an error of type `ERROR` when the
destructuring fails. That's not too specific of a condition if you want to catch
specifically destructuring errors, eh?

The macro `DESTRUCTURING-BIND*` available from this system instead signals
a custom error of type `DESTRUCTURING-ERROR` which contains readers for the
expression that was attempted to be destructured, the lambda-expression that
was destructured against, and the original error that was signaled during
destructuring.

## Exports

  * **Macro `DESTRUCTURING-BIND*`**
  * **Condition Type `DESTRUCTURING-ERROR`**
    * **Reader `DESTRUCTURING-ERROR-LAMBDA-LIST`**
    * **Reader `DESTRUCTURING-ERROR-EXPRESSION`**
    * **Reader `DESTRUCTURING-ERROR-REASON`**

## Usage

Either use the `DESTRUCTURING-BIND-STAR` package and call the
`DESTRUCTURING-BIND*` macro, or shadow the `DESTRUCTURING-BIND` symbol and:

```common-lisp
(setf (macro-function 'destructuring-bind)
      (macro-function 'destructuring-bind*))
```

## MIT License

Copyright © 2018 Michał "phoe" Herda.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the “Software”), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
