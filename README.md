# Scala関数型デザイン&プログラミング を読んで

## 答え、解説
[fpinscala](https://github.com/fpinscala/fpinscala)

## テスト実行方法
### 全てのテストを実行
```
scala > test
```

### 指定のファイルのテストを実行
```
scala > testOnly fpinscala.gettingstarted.MyModuleSpec
```

## メモ
### 用語
#### 純粋関数
純粋関数とは、副作用を持たない関数のこと。

参照透過な引数により呼び出しが参照透過になる関数のこと。

#### 参照透過性
どのようなプログラムにおいても、プログラムの意味を変えることなく、式をその結果に置き換えることができること。

>`2 + 3`という式(関数)を`5`に置き換えても結果は変わらない。

#### 高階関数
関数を引数として受け取り、出力として関数を返す関数。

#### 単相関数
一つの型のデータだけを操作する関数。

#### 多相関数
複数の型のデータを操作する関数。

#### 部分関数(partial function)
一部の入力に対して定義されない関数のこと。
```
// この関数は空でないSeqの入力は定義されているが、
// 空のSeqに対する入力が定義されていない。
def mean(xs: Seq[Double]): Double = {
  if (xs.isEmpty)
    throw new ArithmeticException("mean of empty list!")
  else xs.sum / xs.length
}
```


### Tips
#### :paste
REPLで`:paste`と入力すると、複数行を入力できるモードになる。

#### 可変長引数
##### 可変長引数を受け取る関数の定義
```
def foo(ints: Int*)
```
この場合、`ints`は`Seq[Int]`となる。
##### 可変長引数の受け取る関数の呼び出し方
```
foo(1, 2, 3)
```
または、
```
val ints: Seq[Int] = Seq(1, 2, 3)
foo(ints: _*)
```
#### foldRight
リストを最後まで走査してから、畳込みを開始する。
