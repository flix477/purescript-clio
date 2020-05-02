# clio
Command-line arguments parser for PureScript

![CI](https://github.com/flix477/purescript-clio/workflows/PureScript%20CI/badge.svg)

## Features
- Fully-featured configuration with multiple types of options and arguments
- Automatic help dialog generation and error messages
- Backend agnostic
- Arguments builder that enforces correctness at compile-time

## Example
To parse a simplified version of `ls`:
```purescript
config' "ls"
  [ description "list directory contents"
  , options
    [ switch (Short 'a')
      $ O.description "Include entries whose names begin with a dot."
    , switch (Short 'l')
      $ O.description "List in long format."
    ]
  , arguments
    $ (variadic "dirs"
      $ A.description "Display the names of files contained per directory.")
  ]
```

This will generate a help dialog that looks exactly like this:
```
ls: list directory contents
Usage: ls [options] [...dirs]
Options:
  -a         Include entries whose names begin with a dot.
  -l         List in long format.
Arguments:
  [...dirs]  Display the names of files contained per directory.
```

## Note
This was done as a learning project for me to get better understanding of PureScript/FP in general.
If you want a great alternative you should look up https://github.com/f-o-a-m/purescript-optparse
