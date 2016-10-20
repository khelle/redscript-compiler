# RedScript JISON Compiler

[![Redscript Compatible](https://img.shields.io/badge/redscript-compatible-cb0012.svg)][1]

## Description

[RedScript][2] was created to provide a first class functional experience in the browser. This repository contains a compiler for it written in [JISON][2], which is essentially a clone of the parser generator Bison (thus Yacc,) but in JavaScript.

## Compiling

```sh
jison redscript.jison
```

## Usage

#### As Command

```sh
node redscript.js input.js > output.js
```

#### As Module

```js
var parser = require("./redscript").parser;

function exec (input) {
    return parser.parse(input);
}

// input is String variable with RedScript code, output is translated code to JavaScript
var output = exec(input);
```

#### As Website Widget

```html
<script src="./redscript.js"></script>
<script>
parser.parse(input);
</script>
```

## Bibliography

- [JavaScript Spec][4]
- [RedScript Spec][5]
- [Flex Manual][6]
- [Bison Manual][7]
- [Jison Documentation][8]

## License

This repository is open-sourced software licensed under the [MIT license][3].

[1]: https://github.com/AdamBrodzinski/RedScript
[2]: http://zaach.github.io/jison
[3]: http://opensource.org/licenses/MIT
[4]: https://github.com/khelle/redscript-compiler/blob/master/spec/js-spec.pdf
[5]: https://github.com/khelle/redscript-compiler/blob/master/spec/rds-spec.html
[6]: http://dinosaur.compilertools.net/flex/index.html
[7]: http://dinosaur.compilertools.net/bison/index.html
[8]: http://zaach.github.io/jison/docs
