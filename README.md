# HTTPure Extras

Various extras to be used with [HTTPure].

- Path prefix based router
- REST endpoints
- HTTP Basic authentication
- Static file serving

**Note**: The HTTP Basic authentication support
(`HTTPure.Contrib.Auth`) requires the `npm` package `text-encoding` to
be installed.

[HTTPure]: https://github.com/cprussin/purescript-httpure

## Examples

Examples are in the `examples/` diretory. Run them like this,
substituting `Todo` with the name of an example:

```
pulp run -I examples -m Todo
```

Here's the list of examples:

[Todo.purs](https://github.com/cprussin/purescript-httpure-rest-router/tree/master/examples/Todo.purs)

A simple, stateless, incomplete TODO backend app.
