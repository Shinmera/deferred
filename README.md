About Deferred
--------------
Sometimes I would like to include code in my project that is only available if certain libraries are loaded. Since I don't want to require these libraries as dependencies, but can't refer to their symbols either without them being loaded, things get a bit messy. This library attempts to provide a much more convenient way.

How To
------
Deferred uses two components: a dispatch reader macro on `#^` and a macro, `WITH-DEFERRED-LIBRARY`. `#^` is used to circumvent the reader problem of being unable to refer to symbols in packages that don't exist at read-time. The macro is used to then acquire access to the symbol's environment to figure out the proper transformation for the symbol into an execution-time determined value.

```
(with-deferred-library (:drakma)
  (#^http-request "http://google.com"))
```

Deferred symbols can be used in function calls, with `FUNCTION` and `QUOTE` and as values (f.e. special variables). Currently unsupported are deferred `SETF` functions as it's impossible to properly defer a `SETF`-expander until execution-time.

If the default dispatch reader macro has been overwritten in your readtable for one reason or another, you can use `(named-readtables:in-readtable :deferred)`.
