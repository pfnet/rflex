# Tutorial

## rflex command line interface

There is no options in rflex.

```
$ rflex target.l
```

That command generates `target.rs` file in the same directory.

## Basic

Syntax of rflex is very similar to flex.
The first '%%' means the beginning of the rules.
Second one means the end of the rules.

```
%%
%class Lexer
%result_type i32
abc      println!("match abc rule"); return Ok(0i32);
[a-z]+   println!("'{}'", self.yytext());
         return Ok(10i32); /* action can be defined in multiple lines that starts with white space */
" "      /* Skip white space. This comment cannot be omitted. */
%%
```

The rule contains `pattern` and `action` in the lines.
`Pattern` is a regular-expression sequences.
`Action` is a Rust code block to execute when the pattern accepted.
In the above example, `abc` is pattern, `println!("match abc rule"); return Ok(0i32);` is action.

`%class` and `%result_type` is special directive to replace generated default struct name and return type.

### Precedence

`abc` and `[a-z]+` patterns can both accept `abc`.
In the rflex, it takes priority that pattern appears first when ambiguous patterns defined.

## rflex functions

Scanner code can be called some functions from action or user program.
For example, we can get length of accepted string by `yylength` function.

* `pub fn yylex(&mut self) -> Result<i32, Error>`
  * Return next token in `i32`. `i32` can be replaced with `%result_type` directive.
* `pub fn is_eof(&self) -> bool`
  * Return is the scanner reached EOF.
* `pub fn yybegin(&mut self, new_state: usize)`
  * Use `new_state` lexer state in the next scan.
* `pub fn yystate(&self) -> usize`
  * Return current lexer state.
* `pub fn yylength(&self) -> usize`
  * Return the length of accepted string.
* `pub fn yytext(&self) -> String`
  * Return the accepted string.
* `pub fn yycharat(&self, pos: usize) -> Option<char>`
  * Return the character at the relative position (0-origin) in the accepted string.
* `pub fn yypushback(&mut self, num: usize)`
  * Back to `num` count for next scanning. The number must not be greater than `yylength()`.

## enum Error

`yylex` function returns `Result<any_type, Error>`.
`Error` enum type is defined as follows.
When reached end of file, `yylex` returns `Err(Error::EOF)`.
It returns `Err(Error::Unmatch)` if the input wasn't accepted.

```rust
#[derive(Debug)]
pub enum Error {
    EOF,
    Unmatch,
}
```

## Embed Your own Rust code in DSL

See codes of example1 and example2, too.


```
// Write your own Rust code here.
// This code will be inserted into the header of generated lexer file.
use std::io; // example

%%
%class Lexer
%result_type i32
abc      println!("match abc rule"); return Ok(0i32);
[a-z]+   println!("'{}'", self.yytext()); return Ok(10i32);
%%

    // Write your own Rust code that will be inserted into
    // `Lexer` impl.
    // So this code can be executed like `lexer.remain();`.
    pub fn remain(&self) -> usize {
        self.current.clone().count()
    }
```

### Put any fields in lexer struct

See example1 code.

```
%field SpaceCounter space_counter
```

rflex has a `%field` directive to append any fields to lexer struct.
That makes Lexer struct have `space_counter` field and generated impl is below:

```rust
pub fn new(input: String, space_counter: SpaceCounter) -> Lexer { /* omission */ }
pub fn get_space_counter(&mut self) -> &mut SpaceCounter { &mut self.space_counter } 

```

Then we can specify in `new` and access SpaceCounter struct via `get_space_counter`.
`%field` directive can be specified multiple times not only one.
