# YinYang

## What is this?

YinYang is a clean, simple binary disjunction, yet another replacement for `scala.util.Either`.

YinYang has no intrinsic bias, but users can import either a `YinBias` or `YangBias`. In a scope
with a bias, YinYang implements all the conventional methods of a Scala monad, and is a full
citizen of for comprehensions, including (unlike `scala.util.Either`) support for pattern matching,
guards, and assignment within for comprehensions. Biased YinYangs behave as monads similar to `Option`,
except rather than yielding `None` if something goes wrong, a counterbias `YinYang` can contain state
describing the error or problem. Users should nominate a counterbias token that describes or represents "empty" --
the result of a pattern match or guard that fails.

YinYang began as a proposed improvement to `scala.util.Either`, see [leftright](https://github.com/swaldman/leftright/blob/master/README.md),
[pull request](https://github.com/scala/scala/pull/4547), and [SLIP proposal](https://github.com/swaldman/slip/blob/enrich-bias-either/text/0000-enrich-bias-either.md).
`scala.util.Either` [went another way](https://github.com/scala/scala/pull/5135#issuecomment-234378292), and so has this library. Despite changes,
`scala.util.Either` still does not fully support Scala for comprehensions.

To use, you will first need to include a dependency, currently

    "com.mchange" %% "yinyang" % "0.0.1"

and

```scala
import com.mchange.sc.v2.yinyang._
```

## Example

Here is a simple example:

```scala
import com.mchange.sc.v2.yinyang._

val YangBias = YinYang.YangBias.withEmptyToken[String]("EMPTY")
import YangBias._

val a : YinYang[String,Int] = Yang(1)
val b : YinYang[String,Int] = Yang(99)

for( v <- a; w <- b ) yield v+w          // Yang(100)
for( v <- a; w <- b if v > 10) yield v+w // Yin(EMPTY)
```

