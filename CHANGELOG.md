## 0.8.0 (2021-02-05)

* Add yybytepos function to lexer ([#13](https://github.com/pfnet/rflex/issues/13))
* Remove yypushback function due to incorrect implementation

## 0.7.0 (2020-11-29)

* Add yytextpos function to lexer ([#13](https://github.com/pfnet/rflex/issues/13)
* Make enum Error derive PartialEq in lexer for convenience

## 0.6.0 (2020-02-17)

* Fix to reduce huge memory allocation in lexer ([#11](https://github.com/pfnet/rflex/issues/11)

## 0.5.0 (2019-07-05)

* Change process arguments by dalance ([#9](https://github.com/pfnet/rflex/pull/9))
  * This enables `process` to specify output directory and improves type of Path
* Use failure ([#6](https://github.com/pfnet/rflex/issues/6), [#10](https://github.com/pfnet/rflex/pull/10))
  * This introduces `failure::Error` to `process` function

## 0.4.0 (2019-06-19)

* Remove unsafe `std::mem::transmute` call by @au-phiware ([#5](https://github.com/pfnet/rflex/pull/5))
  * This changes Lexer interface that replaced `String` with `&str`


## 0.3.0 (2019-05-24)

* Support multi-line action code block


## 0.2.0 (2019-04-11)

* Public `rflex::process` function


## 0.1.0 (2019-04-08)

* Initial release
