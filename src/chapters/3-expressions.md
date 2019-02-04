[WIP]

# 式

この章では、私たちはHaskellの式の構文と非形式的な意味論を説明する。また必要であらばHaskellカーネルへの変換についても説明する。`let`式の場合を除いて、これらの変換は静的、動的な意味論の両方を保存する。これらの変換を使った束縛されていない変数とコンストラクタは常に`Prelude`によって定義された実体を参照する。例えば、リスト内包表記の変換(セクション[3.11])で使われる`"concatMap"`は`Prelude`によって定義された`concatMap`を意味する。これは識別子`"concatMap"`がリスト内包表記で使われているスコープ内にあるかないかは関係なく、また、(もしスコープ内にあったとしても)束縛されていても関係はない。

|||||
|--|--|--|--|
|<em>exp</em>|→|<em>infixexp</em> :: [context =>] type|(expression type signature)|
|       |&#124;|<em>infixexp</em>|(expression type signature)|
| | | | |
|<em>infixexp</em>|→|<em>lexp</em> qop <em>infixexp</em>|(infix operator application)|
|            |&#124;|- <em>infixexp</em>|(prefix negation)|
|            |&#124;|<em>lexp</em>| |
| | | | |
|<em>lexp</em>|→|\ <em>apat<sub>1</sub></em> … <em>apat<sub>n</sub></em> -> <em>exp</em>|(lambda abstraction, <em>n</em> ≥ 1)|
|        |&#124;|<tt>let</tt> <em>decls</em> <tt>in</tt> <em>exp</em>|(let expression)|
|        |&#124;|<tt>if</tt> <em>exp</em> [;] <tt>then</tt> <em>exp</em> [;] <tt>else</tt> <em>exp</em>|(conditional)|
|	       |&#124;|<tt>case</tt> <em>exp</em> of { <em>alts</em> }|(case expression)|
|        |&#124;|<tt>do</tt> { <em>stmts</em> }|(do expression)|
|        |&#124;|<em>fexp</em>| |
| | | | |
|<em>fexp</em>|→|[<em>fexp</em>] <em>aexp</em>|(function application)|
| | | | |
|<em>aexp</em>|→|<em>qvar</em>|(variable)|
|        |&#124;|<em>gcon</em>|(general constructor)|
|        |&#124;|<em>literal</em>| |
|        |&#124;|( <em>exp</em> )|(parenthesized expression)|
|        |&#124;|( <em>exp<sub>1</sub></em> , … , <em>exp<sub>k</sub></em> )|(tuple, <em>k</em> ≥ 2)|
|        |&#124;|[ <em>exp<sub>1</sub></em> , … , <em>exp<sub>k</sub></em> ]|(list, <em>k</em> ≥ 1)|
|        |&#124;|[ <em>exp<sub>1</sub></em> [, <em>exp<sub>2</sub></em>] .. [<em>exp<sub>3</sub></em>] ]|(arithmetic sequence)|
|        |&#124;|[ <em>exp</em> &#124; <em>qual<sub>1</sub></em> , … , <em>qual<sub>n</sub></em> ]|(list comprehension, <em>n</em> ≥ 1)|
|        |&#124;|( <em>infixexp</em> <em>qop</em> )|(left section)|
|        |&#124;|( <em>qop<sub>⟨-⟩</sub></em> <em>infixexp</em> )|(right section)|
|        |&#124;|<em>qcon</em> { <em>fbind<sub>1</sub></em> , … , <em>fbind<sub>n</sub></em> }|(labeled construction, <em>n</em> ≥ 0)|
|        |&#124;|<em>aexp<sub>⟨qcon⟩</sub></em> { <em>fbind<sub>1</sub></em> , … , <em>fbind<sub>n</sub></em> }|(labeled update, <em>n</em>  ≥  1)|

中置演算子を含む式は演算子の結合性によって曖昧さを排除されている(セクション[4.4.2](./4-declarations-and-bindings.md)参照)。同じ優先度をもつ連続した括弧を持たない演算子は構文エラーを避けるためにどちらも左または右のどちらかに結合しなければならない。括弧を持たない式<em>"x qop<sup>(a,i)</sup> y qop<sup>(b,j)</sup> z"</em> ( <em>qop<sup>(a,i)</sup></em>は`a`と優先順位`i`に関連付いた演算子を意味する)が与えられた場合、括弧は`i = j`でかつ`a = b = l`か`a = b = r`でない時は、<em>"x qop<sup>(a,i)</sup> y"</em>か<em>"y qop<sup>(b,i)</sup> z"</em>のどちらかを囲むよう追加されなければいけない。

中置演算子を含む式の解決するためのアルゴリズムの例はセクション[10.6](./10-syntax-reference.md)にある。

符号反転演算子はHaskellにおいて唯一の接頭語になる。中置と同じ優先順位を持ち、演算子はPreludeの中に定義されている(セクション[4.4.2](./4-declarations-and-bindings.md), 図[4.1](./4-declarations-and-bindings.md))。

この文法は条件式、let式、ラムダ抽象の拡張については曖昧だ。その曖昧さは各構成ができるだけ右へ拡張されるメタ規則により解決される。

構文解析の例を以下に示す。

|これが| このように解析される|
|--|--|
| f x + g y | (f x) + (g y) |
| - f x + y | (- (f x)) + y |
| let { ... } in x + y | let { ... } in (x + y) |
| z + let { ... } in x + y | z + (let { ... } in (x + y)) |
| f x y :: Int | (f x y) :: Int |
| \\ x -> a+b :: Int | \\ x -> ((a+b) :: Int) |

わかりやすくするため、以後このセクションでは中置演算子を含む式が演算子の結合性に従って解決されているということにする。

## エラー
式の評価中のエラーは、⊥("bottom")と表記されるが、停止しないことからHaskellプログラムには区別できない。Haskellは非正格評価の言語なことから、全てのHaskellの型は⊥を含む。つまり、いかなる型の値も、ユーザーが望めばエラーを返す計算になる可能性がある。評価されたとき、エラーは直ちにプログラムを停止させ、ユーザーが捕捉されることはできない。Preludeは直接そのようなエラーを引き起こす二つの関数を提供している。

``` hs
error     :: String -> a
undefined :: a
```

`error`の呼び出しはプログラムの実行を終了させ、OSに適切なエラー表示を返す。そのエラー表示にはシステム依存の方法で文字列を画面に表示するべきである。`undefined`が使われたとき、そのエラーメッセージはコンパイラーによって作成される。

Haskellの式の変換は実行時エラーが発生したことを明示的に表示するため`error`と`undefined`を使用する。エラーが発生した際の実際のプログラムの振舞は実装次第である。そのメッセージはこれらの変換のみ提案するため`error`関数へ渡される。エラー発生時、詳しい情報または乏しい情報を表示することを実装側は選択するかもしれない。

## 変数、コンストラクタ、演算子、リテラル

|||||
|--|--|--|--|
|<em>aexp</em>|→|<em>qvar</em>|(variable)|
|        |&#124;|<em>gcon</em>|(general constructor)|
|        |&#124;|<em>literal</em>| |
| | | | |
|<em>gcon</em>|→|()| |
|	       |&#124;|[]| |
|	       |&#124;|(,{,})| |
|	       |&#124;|<em>qcon</em>| |
| | | | |
|    <em>var</em>|→|<em>varid</em> &#124; ( <em>varsym</em> )|(variable)|
|   <em>qvar</em>|→|<em>qvarid</em> &#124; ( <em>qvarsym</em> )|(qualified variable)|
|    <em>con</em>|→|<em>conid</em> &#124; ( <em>consym</em> )|(constructor)|
|   <em>qcon</em>|→|<em>qconid</em> &#124; ( <em>gconsym</em> )|(qualified constructor)|
|  <em>varop</em>|→|<em>varsym</em> &#124; \`  <em>varid</em> \`|(variable operator)|
| <em>qvarop</em>|→|<em>qvarsym</em> &#124; \`  <em>qvarid</em> \`|(qualified variable operator)|
|  <em>conop</em>|→|<em>consym</em> &#124; \`  <em>conid</em> \`|(constructor operator)|
| <em>qconop</em>|→|<em>gconsym</em> &#124; \`  <em>qconid</em> \`|(qualified constructor operator)|
|     <em>op</em>|→|<em>varop</em> &#124; <em>conop</em>|(operator)|
|    <em>qop</em>|→|<em>qvarop</em> &#124; <em>qconop</em>|(qualified operator)|
|<em>gconsym</em>|→|: &#124; <em>qconsym</em>

Haskellは中置記法に対応するため特別な構文を提供している。**演算子**は中置構文を用いて適用が可能である(セクション[3.4]("#3.4"))か、**セクション**(セクション[3.5]("#3.5"))を用いて部分的に適用が可能な関数のことである。

**演算子**は、`+`や`$$`といった**演算子シンボル**か、`op`のようにグレイブ・アクセント(バッククォート)で囲まれた通常の識別子かのいずれかである。例えば、`op x y`という前置適用を書く代わりに、<code>x \`op\` y</code>という中置適用を書くことができる。もし、<code>\` op \`</code>に対して結合性が宣言されていない場合には、優先順位は最高で左結合をデフォルトとする。(セクション[4.4.2]("./4-declarations-andbindings.md")参照)。

対照的に、演算子シンボルは括弧で閉じられた普通の識別子へ変換可能である。例として、`(+) x y`は`x + y`に等しく、`foldr (⋆) 1 xs`は`foldr (\x y -> x⋆y) 1 xs`に等しくなる。

一部の組み込み型のコンストラクタの名前をつけるのに特別な構文がつかわれているものがあり、実際に`qcon`や`literal`で見ることができる。これらについてはセクション[6.1]("./6-predefined-types-and-classes.md")で説明される。

整数リテラルは`fromInteger`関数を`Integer`型の適した値への適用を表す。同様に、浮動小数点リテラルは`Rational`型(つまり、`Ratio Integer`)の値に`fromRational`を適用することを表す。

<div class="column">

**変換:** 整数リテラル`i`は`fromInteger i`に等しく、`fromInteger`は`Num`クラスのメソッドである。(セクション[6.4.1]("./6-predefined-types-and-classes,md"))

浮動小数点リテラル`f`は`fromRational (n Ratio.% d)`に等しく、`fromRational`は`Fractional`クラスのメソッドで、`Ratio.%`は`Ratio`ライブラリで定義されており、2つの整数から有理数を構築する。整数`n`と`d`は`n/d = f`を満たすものとして選ばれる。

</div>

## カリー化された適用とラムダ抽象

|||||
|--|--|--|--|
|<em>fexp</em>|→|[<em>fexp</em>] <em>aexp</em>|(function application)|
|<em>lexp</em>|→|\ <em>apat<sub>1</sub></em> … <em>apat<sub>n</sub></em> -> <em>exp</em>|(lambda abstraction, <em>n</em> ≥ 1)|

関数適用は<code><em>e<sub>1</sub></em> <em>e<sub>2</sub></em></code>と書く。適用は左結合性をもつので、`(f x) y`の括弧は省略することができる。`e1`はデータ構成子である可能性もあるため、データ構成子の部分的な適用は許されている。

ラムダ抽象は<code>\ <em>p<sub>1</sub></em> … <em>p<sub>n</sub></em> -> e</code>と書き、<code><em>p<sub>i</sub></em></code>はパターンである。`\x:xs->x`のような式は構文的に正しくない。`\(x:xs)->x`と書くのが正しい。

パターンの集合は線形でなければならない。つまり、変数は集合の中で2回以上出現してはいけない。

<div class="column">

**変換:** 以下の等式が成り立つ。

<code><em>p<sub>1</sub></em> … <em>p<sub>n</sub></em> -> e = \ X<sub>1</sub> … X<sub>n</sub> -> case (X<sub>1</sub>, …, X<sub>n</sub>) of (<em>p<sub>1</sub></em>, …, <em>p<sub>n</sub></em>) -> e</code>

<code>X<sub>i</sub></code>は新しい識別子である。

</div>

この変換がセクション[3.17.3]("#3.17.3")で説明するcase式とパターンマッチの意味論と組み合わさって与えられたとき、もしもパターンマッチに失敗すれば結果は⊥となる。

## 演算子適用

|||||
|--|--|--|--|
|<em>infixexp</em>|→|<em>lexp</em> <em>qop</em> <em>infixexp</em>| |
|            |&#124;|- <em>infixexp</em>|(prefix negation)|
|            |&#124;|<em>lexp</em>| |
|<em>qop</em>|→|<em>qvarop</em> &#124; <em>qconop</em>|(qualified operator)|

<code><em>e<sub>1</sub></em> qop <em>e<sub>2</sub></em></code>という形式は二項演算子`qop`の式<code><em>e<sub>1</sub></em></code>と<code><em>e<sub>2</sub></em></code>への中置適用である。

特殊な形式`-e`は前置の符号反転演算子を表す。この演算子はHaskellにおける唯一の前置演算子であり、`negate (e)`という意味の構文である。二項演算子`-`はPrelude内の`-`の定義への参照を必要とせず、モジュールシステムによって再束縛されるかもしれない。しかしながら、単項演算子`-`はPrelude内で定義された`negate`関数を常に参照する。`-`演算子の局所的な意味と単項の符号反転演算との間には何の関連もない。

前置の符号反転演算子はPrelude内(表[4.1](./4-declarations-and-bindings.md)を参照)で定義された中置演算子`-`と同じ優先順位を持つ。`e1-e2`は二項演算子`-`の中置表現解析されるため、前置の符号反転演算子を使うには構文解析に代わって`e1(-e2)`と書かなければいけない。同様に、`(-)`は中置演算子と同様に`(\ x y -> x-y)`のための構文であるが、`(\ x -> -x)`を表せず、そのためには`negate`を使う必要がある。

<div class="column">

**変換：** 以下の等式が成り立つ。
<pre>
<em>e<sub>1</sub></em> op <em>e<sub>2</sub></em>  =	(op) <em>e<sub>1</sub></em> <em>e<sub>2</sub></em>
-e        =	<em>negate</em> (e)
</pre>

</div>

## セクション

<pre>
<em>aexp</em>	→	( <em>infixexp</em> <em>qop</em> )	    (left section)
        |	( <em>qop<sub>⟨-⟩</sub></em> <em>infixexp</em> )	    (right section)
</pre>

**セクション**は<code>( <em>op</em> <em>e</em> )</code>や<code>( <em>e</em> <em>op</em> )</code>のように書かれる。このときの`op`は二項演算子で`e`は式である。セクションは二項演算子を部分的に適用する便利な構文である。

シンタックスの先行ルールは次のとおりのセクションへ適用する。(op e) is legal if and only if (x op e) parses in the same way as (x op (e)); and similarly for (e op).例えば、`(⋆a+b)`は構文的に不当であるが、`(+a⋆b)`と`(⋆(a+b))`は有効である。なぜなら`(+)`は左結合であり、`(a+b+)`は構文的に正しいが、`(+a+b)`はそうではない。後者は`(+(a+b))`のように書かれるのが正当である。他の例として、次の式は

``` hs
(let n = 10 in n +)
```

セクション[3]("./3-expressions.md")にあるように、letとラムダに関するメタルールにより誤りである。次の式は

``` hs
(let n = 10 in n + x)
```

以下のように解析され

``` hs
(let n = 10 in (n + x))
```

次のようにはならない

``` hs
((let n = 10 in n) + x)
```

なぜなら、`-`は文法内で特別に扱われるからだ。前のセクションで説明したように、`(- exp)`はセクションではなく、前置の符号反転演算子の適用である。しかしながら、Prelude名で定義された`subtract`関数があり、それによって<code>(<em>subtract</em> exp)</code>が不正なセクション(**訳注**: (- exp)のこと)と同じ意味となる。式`(+ (- exp))`は同じ用途で役立つことができる。

<div class="column">

**変換:** 以下の等式が成り立つ。

```hs
(op e)  =       \ x -> x op e
(e op)  =       \ x -> e op x
```

`op`は二項演算子で、`e`は式であり、`x`は`e`の中で自由出現ではない変数である。
</div>

## 条件文

|||||
|--|--|--|--|
|<em>lexp</em>|→|<tt>if</tt> <em>exp</em> [;] <tt>then</tt> <em>exp</em> [;] <tt>else</tt> <em>exp</em>| |

条件式は<code><tt>if</tt> <em>e<sub>1</sub></em> <tt>then</tt> <em>e<sub>2</sub></em> <tt>else</tt> <em>e<sub>3</sub></em></code>の形式をとり、もし<code><em>e<sub>1</sub></em></code>が真なら、<code><em>e<sub>2</sub></em></code>を返し、<code><em>e<sub>1</sub></em></code>が偽なら<code><em>e<sub>3</sub></em></code>を返し、それ以外なら`⊥`を返す。

<div class="column">

**変換:** 以下の等式が成り立つ。

<pre><code><tt>if</tt> <em>e<sub>1</sub></em> <tt>then</tt> <em>e<sub>2</sub></em> <tt>else</tt> <em>e<sub>3</sub></em>(op e) = <tt>case</tt> <em>e<sub>1</sub></em> <tt>of</tt> { <tt>True</tt> -> <em>e<sub>2</sub></em> <tt>;</tt> <tt>False</tt> -> <em>e<sub>3</sub></em> }
</code></pre>

<tt>True</tt>と<tt>False</tt>はPrelude内で定義されている<tt>Bool</tt>型の2つの引数のないコンストラクタである。<code><em>e<sub>1</sub></em></code>は<tt>Bool型</tt>でなければならず、<code><em>e<sub>2</sub></em></code>と<code><em>e<sub>3</sub></em></code>も同じ型でなければならない。条件式全体の型も同様である。
</div>

## リスト

|||||
|--|--|--|--|
|<em>infixexp</em>|→|<em>exp<sub>1</sub></em> <em>qop</em> <em>exp<sub>2</sub></em>| |
|    <em>aexp</em>|→|[ <em>exp<sub>1</sub></em> , … , <em>exp<sub>k</sub></em> ]|(k ≥ 1)|
|            |&#124;|<em>gcon</em>| |
|    <em>gcon</em>|→|<tt>[]</tt>| |
|            |&#124;|<em>qcon</em>| |
|    <em>qcon</em>|→|( <em>gconsym</em> )| |
|     <em>qop</em>|→|<em>qconop</em>| |
|  <em>qconop</em>|→|<em>gconsym</em>| |
| <em>gconsym</em>|→|<tt>:</tt>| |

**List**は`k ≥ 1`として、<code>[<em>e<sub>1</sub></em>, …, <em>e<sub>k</sub></em>]</code>のように書く。リストコンストラクタは `:`であり、空リストは`[]`で表記される。リストの標準操作はPrelude内で与えられる(セクション[6.1.3]("./6-predefined-types-and-classes.md")と[9章]("./9-standard-prelude.md")の特にセクション[9.1]("./9-standard-prelude.md")を参照)。

<div class="column">

**変換:** 以下の等式が成り立つ。

<code>[<em>e<sub>1</sub></em>, …, <em>e<sub>k</sub></em>]</code> = <code><em>e<sub>1</sub></em> : (<em>e<sub>2</sub></em> : ( … (<em>e<sub>k</sub></em> : [])))</code>

`:`と`[]`はPredule内(セクション[6.1.3]("./6-predefined-types-and-classes.md"))で定義されたリストのコンストラクタである。<code><em>e<sub>1</sub></em></code>から<code><em>e<sub>k</sub></em></code>までの型は同じでなければならない(それを`t`と呼ぶ)。式全体の型は`[t]`になる(セクション[4.1.2]("./4-declarations-and-bindings.md"))。
</div>

コンストラクタ`:`はリストコンストラクタとしてのみ予約されており、言語構文の一部と見做されている。また、それは隠すことも再定義することもできない。`:`は優先順位レベル5の右結合演算子である(セクション[4.4.2]("./4-declarations-and-bindings.md"))。

## タプル

|||||
|--|--|--|--|
|<em>aexp</em>|→|( <em>exp<sub>1</sub></em> , … , <em>exp<sub>k</sub></em> )|(k ≥ 2)|
|        |&#124;|<em>qcon</em>| |
|<em>qcon</em>|→|(,{,})| |

**タプル**は`k ≥ 2`以上の<code>(<em>e<sub>1</sub></em>, …, <em>e<sub>k</sub></em></code>のように書く。`n-tuple`のコンストラクタは`(,…,)`と表記され、`n-1`のコンマがある。従って、`(a,b,c)`と`(,,) a b c`は同じ値を表す。タプルの標準操作はPrelude内で定義されている(セクション[6.1.4]("./6-predefined-types-and-classes.md")と[9章]("./9-standard-prelude.md"))。

<div class="column">

**変換:** `k ≥ 2`のときの<code>(<em>e<sub>1</sub></em>, …, <em>e<sub>k</sub></em></code>はPrelude内で定義された`k-tuple`のインスタンスになり、変換は要求されない。もし、<code>t<sub>1</sub></code>から<code>t<sub>k</sub></code>はそれぞれ<code><em>e<sub>1</sub></em></code>から<code><em>e<sub>k</sub></em></code>の型があり、最終的なタプルの型は<code>t<sub>1</sub>,…,t<sub>k</sub></code>になる(セクション[4.1.2]("./4-declarations-and-bindings.md"))。

</div>

## 単位式と括弧付き式

|||||
|--|--|--|--|
|<em>aexp</em>|→|<em>gcon</em>| |
|        |&#124;|( <em>exp</em> )| |
|<em>gcon</em>|→|()| |

`(e)`の形式はシンプルに**括弧付き式**であり、`e`と等しい。**ユニット(unit)**式`()`は`()`型を持つ(セクション[4.1.2]("./4-declarations-and-bindings.md")を参照)。それは⊥以外の型のメンバのみで、"引数のないタプル"のように考えられる(セクション[6.1.5]("./6-predefined-types-and-classes.md")を参照)。

<div class="column">

**変換:** `(e)`は`e`と等しい。
</div>

## 数列

|||||
|--|--|--|--|
|<em>aexp</em>|→|[ <em>exp<sub>1</sub></em> [, <em>exp<sub>2</sub></em>] .. [<em>exp<sub>3</sub></em>] ]| |

**数列**[<em>e<sub>1</sub></em>,<em>e<sub>2</sub></em> .. <em>e<sub>3</sub></em>]は型`t`の値のリストを表し、各<code><em>e<sub>i</sub></em></code>は型`t`を持ち、`t`は`Enum`クラスのインスタンスである。

<div class="column">

**変換:**　数列はこれらの等式を満たす。

<pre>
[ <em><em>e<sub>1</sub></em></em>.. ]	=	<tt>enumFrom</tt> <em><em>e<sub>1</sub></em></em>
[ <em><em>e<sub>1</sub></em></em>,<em><em>e<sub>2</sub></em></em>.. ]	=	<tt>enumFromThen</tt> <em><em>e<sub>1</sub></em></em> <em><em>e<sub>2</sub></em></em>
[ <em><em>e<sub>1</sub></em></em>..<em><em>e<sub>3</sub></em></em> ]	=	<tt>enumFromTo</tt> <em><em>e<sub>1</sub></em></em> <em><em>e<sub>3</sub></em></em>
[ <em><em>e<sub>1</sub></em></em>,<em><em>e<sub>2</sub></em></em>..<em><em>e<sub>3</sub></em></em> ]	=	<tt>enumFromThenTo</tt> <em><em>e<sub>1</sub></em></em> <em><em>e<sub>2</sub></em></em> <em><em>e<sub>3</sub></em></em>
</pre>

`enumForm`、`enumFormThen`、`enumFormTo`、`enumFormThenTo`はPrelude内で定義されている`Enum`クラスのクラスメソッドになる。
</div>

故に数列の意味論は型`t`のインスタンス宣言に完全に依存している。どの`Prelude`型が`Enum`型にあるか、そしてそれらの意味論についてのより詳しいことについてはセクション[6.3.4]("./6-predefined-types-and-classes.md")を参照すること。

## リスト内包表記

|||||
|--|--|--|--|
|<em>aexp</em>|→|[ <em>exp</em> | <em>qual<sub>1</sub></em> , … , <em>qual<sub>n</sub></em> ]|(list comprehension, <em>n</em> ≥ 1)|
|<em>qual</em>|→|<em>pat</em> <- <em>exp</em>|(generator)|
|        |&#124;|<tt>let</tt> <em>decls</em>|(local declaration)|
|        |&#124;|<em>exp</em>|(boolean guard)|

**リスト内包表記**は<code>[ <em>e</em> | q<sub>1</sub>, …, q<sub>n</sub> ]</code>、`n ≥ 1`形式を持ち、<code>q<sub>i</sub></code>修飾子は次のいずれかである。

- 形式`<em>p</em> <- e`の**ジェネレータ**。`p`は型`t`のパターン(セクション[3.17](#3.17))であり、`e`は型`[t]`の式である。
- 生成された式eで、あるいは後方のブーリアンガードとジェネレータで使われる新しい定義を提供するローカル束縛。
- **ブーリアンガード**。Bool型の任意の式を表すことができる。

このようなリスト内包表記は修飾子リスト内のジェネレータのネストされた深さ優先探索の評価によって作成された連続した環境で`e`を評価することによって生成された要素のリストを返す。変数の束縛は通常のパターンマッチングルール(セクション[3.17]("#3.17"))に従って発生し、もし一致に失敗したら、その時はそのリストの要素は単純にスキップされる。従って、

```hs
[ x |  xs   <- [ [(1,2),(3,4)], [(5,4),(3,2)] ],  
      (3,x) <- xs ]
```

リスト[4,2]を返す。もし修飾子がboolean guardなら、成功した前のパターンマッチのために真と評価しなけれないけない。通常通り、リスト内法表記における束縛は外部スコープの束縛をシャドーイングできる。例えば以下のようになる。

```hs
[ x | x <- x, x <- x ] = [ z | y <- x, z <- y]
```

<div class="column">

**変換:** リスト内包表記はこれらの等式を満たし、これらの等式はカーネルへの変換として使われる可能性がある。

||||
|--|--|--|
|[  <em>e</em> &#124; <tt>True</tt> ]	         |=|[<em>e</em>]|
|[  <em>e</em> &#124; <em>q</em> ]	             |=|[  <em>e</em> &#124; <em>q</em>, <tt>True</tt> ]|
|[  <em>e</em> &#124; <em>b</em>,  <em>Q</em>  ]|=|<tt>if</tt> <em>b</em> <tt>then</tt> [  <em>e</em> &#124; <em>Q</em> ] <tt>else</tt> []|
|[  <em>e</em> &#124; <em>p</em> <- <em>l</em>,  <em>Q</em> ]|=|<tt>let</tt> <tt>ok</tt> <em>p</em> = [  <em>e</em> &#124; <em>Q</em> ]|
|                                       | |<tt>ok</tt> _ = []|
|                              | |<tt>in</tt> <tt>concatMap</tt> <tt>ok</tt>  <em>l</em>|
|[  <em>e</em> &#124; <tt>let</tt> <em>decls</em>,  Q ]|=|<tt>let</tt> <em>decls</em> <tt>in</tt> [  <em>e</em> &#124; <em>Q</em> ]|

<em>e</em>は式にわたる範囲で、`p`はパターンにわたり、`l`はリスト値式にわたり、`b`はブーリアン式にわたり、**decls**は宣言リストにわたり、`q`は修飾子にわたり、`Q`は修飾子の列にわたる範囲をもつ。`ok`は新しい変数である。関数`concatMap`とブーリアン値`True`はPrelude内で定義されている。

</div>

リスト内法表記の変換で示した通り、letによって束縛された変数は最大限多相的な型を持つ一方で<-によって束縛されたものはラムダ束縛であり、よって単相的になる。 (セクション[4.5.4]("./4-declarations-and-bindings.md")を参照).

## Let式

|||||
|--|--|--|--|
|<em>lexp</em>|→|<tt>let</tt> <em>decls</em> <tt>in</tt> <em>exp</em>| |

**let**式は一般的な形式<code><tt>let</tt> { <em>d<sub>1</sub></em> ; … ; <em>d<sub>n</sub></em> } <tt>in</tt> <em>e</em></code>を持ち、ネストされたレキシカルスコープをもつ相互再帰的な宣言のリストを導入する(<tt>let</tt>は他の言語で<tt>letrc</tt>としばしば呼ばれる)。宣言の範囲は式`e`と宣言の右側である。宣言は[4章]("./4-declarations-and-bindings.md")で説明される。パターン束縛のマッチは遅延され、暗黙的な`~`がこれらのパターンを反駁不可にする。 例えば、

```hs
<tt>let</tt> (x,y) = <tt>undefined</tt> <tt>in</tt> e
```

は`x`または`y`が評価されるまでランタイムエラーをもたらさない。

<div class="column">

**変換:** 式<code><tt>let</tt> { <em>d<sub>1</sub></em> ; … ; <em>d<sub>n</sub></em>} in <em>e<sub>0</sub></em></code>の動的な意味論は次の変換によって捕捉される。全ての型シグネチャを取り除いた後、それぞれの宣言<em>d<sub>i</sub></em>は<code><em>p<sub>i</sub></em> = <em>e<sub>i</sub></em></code>の形の等式へと変換される。<code>p<sub>i</sub></code>と<code><em>e<sub>i</sub></em></code>はセクション[4.4.3]("./4-declarations-and-bindings.md")での変換を使用する、各々のパターンと式である。一度この変換が終われば、次のような等式が成り立つ。この等式はカーネルへの変換として使われる場合がある。

||||
|--|--|--|
|<tt>let</tt> {<em>p<sub>1</sub></em> = <em>e<sub>1</sub></em>;  ... ; <em>p<sub>n</sub></em> = <em>e<sub>n</sub></em>} <tt>in</tt> <em>e<sub>0</sub></em>|=|<tt>let</tt> (<tt>~</tt><em>p<sub>1</sub></em>, ... ,<tt>~</tt><em>p<sub>n</sub></em>) = (<em>e<sub>1</sub></em>, ... ,<em>e<sub>n</sub></em>) in <em>e<sub>0</sub></em>|
|<tt>let</tt> <em>p</em> = <em>e<sub>1</sub></em>  <tt>in</tt>  e<sub>0</sub>|=|<tt>case</tt> <em>e<sub>1</sub></em> of <tt>~</tt><em>p</em> -> <em>e<sub>0</sub></em>|
|                                               | |where no variable in <em>p</em> appears free in <em>e<sub>1</sub></em>|
|<tt>let</tt> <em>p</em> = <em>e<sub>1</sub></em>  <tt>in</tt>  <em>e<sub>0</sub></em>|=|<tt>let</tt> <em>p</em> = <tt>fix</tt> ( \ <tt>~</tt><em>p</em> -> <em>e<sub>1</sub></em>) in <em>e<sub>0</sub></em>|

`fix`は最小不動点演算子である。反駁不可パターン`~p`の使用は注意すべきだ。この変換は静的な意味論を保存しない。なぜなら、caseを使用すると束縛変数が完全な多相型へ型付けされなくなるからである。`let`式で束縛された静的な意味論はセクション[4.4.3]("./4-declarations-and-bindings.md")で説明される。
</div>

## Case式

|||||
|--|--|--|--|
|<em>lexp</em>|→|<tt>case</tt> <em>exp</em> <tt>of</tt> { <em>alts</em> }| |
|<em>alts</em>|→|<em>alt<sub>1</sub></em> ; … ; <em>alt<sub>n</sub></em>|(n ≥ 1)|
| <em>alt</em>|→|<em>pat</em> -> <em>exp</em> [<tt>where</tt> <em>decls</em>]| |
|        |&#124;|	<em>pat</em> <em>gdpat</em> [<tt>where</tt> <em>decls</em>]
|        |&#124;|                |(empty alternative)|
|||||
| <em>gdpat</em>|→|<em>guards</em> -> <em>exp</em> [ <em>gdpat</em> ]| |
|<em>guards</em>|→| <tt>&#124;</tt> <em>guard<sub>1</sub></em>, …, <em>guard<sub>n</sub></em>|(n ≥ 1)|
| <em>guard</em>|→|<em>pat</em> <- <em>infixexp</em>|(pattern guard)|
|          |&#124;|	<tt>let</tt> <em>decls</em>	           |(local declaration)|
|          |&#124;|	<em>infixexp</em>	            |(boolean guard)|

**case**式は一般的な形式<code><tt>case</tt> <em>e</em> <tt>of</tt> { <em>p<sub>1</sub></em> <em>match<sub>1</sub></em> ; … ; <em>p<sub>n</sub></em> <em>match<sub>n</sub></em> }</code>を持つ。各<code><em>match</em><sub>i</sub></code>は一般的な形式

<pre>
| <em>gs<sub>i1</sub></em>    -> <em>e<sub>i1</sub></em>
…
| <em>gs<sub>imi</sub></em>   -> <em>e<sub>imi</sub></em>
<tt>where</tt> <em>decls</em><sub>i</sub>
</pre>

(**ガード**の構文ルールについて注目して欲しい。`|`は区切りを表す構文的なメタシンボルではなく終端記号である。)各選択子<code><em><em>p<sub>i</sub></em> match<sub>i</sub></em></code>はパターン<em>p<sub>i</sub></em>から成り、<code>match<sub>i</sub></code>と一致する。各マッチは順繰りにガード<code>gs<sub>ij</sub></code>と本体<code><em>e<sub>ij</sub></em></code>のペアの列から成り、代替となる全てのガードと式上の範囲での付加的な束縛(<code>decls<sub>i</sub></code>)に従う。

**ガード**は次の形式をの一つを持つ。

- **パターンガード**は形式`<em>p</em> <- e`で、`p`は型`t`のパターンで、`e`は式の種類`t`である。もし、式`e`がパターン`p`に一致するなら成功し、パターンの束縛をその環境にもたらす。
- **局地的束縛**は形式<code><tt>let</tt> <em>decls</em></code>である。それらは常に成功し、その環境に`decls`と定義した名前をもたらす。
- **ブーリアンガード**は`Bool`型の数式である。もし、式が`True`と評価するなら成功し、その環境に新しい名前をもたらさない。ブーリアンガード`g`はパターンガード`True <- g`に意味的に等しい。

形式`pat -> exp where decls`の代わりの以下の簡略記法が扱われる。

<pre>
<em>pat</em> | <tt>True</tt> -> <em>exp</em>
<tt>where</tt> <em>decls</em>
</pre>

ケース式は少なくとも1つの選択句を持たなければならず、各選択句は一つの実体を持たないといけない。各実体は同じ型を持たなければならず、式全体の型はその型になる。

ケース式は式`e`が個々の選択句に反するパターンマッチングによって評価される。その選択子は上から下へ連続的に試される。もし、`e`が選択句のパターンと一致したら、そのとき選択句のガード式は始めにパターンの一致の間に生成された束縛によって展開されたケース式の環境内で上から下へ連続的に試される。その時、`where`句内の<code>decls<sub>i</sub></code>のよって、その選択句は関連付けられる。

各ガード式のためにコンマ区切りのガードは左から右へ連続的に試される。もし、そのすべてに成功したなら、そのときは対応する式はガードによって生成された束縛で展開された環境で評価される。すなわち、(let句かパターンガードのいずれかを使った)ガードによって生成された束縛は続くガードと対応する式のスコープ内にある。もし、あらゆるガードが失敗したら、その時はこのガード式は失敗し、次のガード式を試す。

もし選択句に与えられたガード式の`none`が成功したら、その時マッチングは次の選択句を続行する。もし、どの選択句も成功しなければ、そのときの結果は`⊥`となる。パターンマッチングはセクション[3.17]("#3.17")で説明され、ケース式の正式な意味論はセクション[3.17.3]("#3.17.3")で説明される。

**パースについての注意点**。以下の式は

```hs
case x of { (a,_) | let b = not a in b :: Bool -> a }
```

これを正しく構文解析するには用心しなければならない。ただ一つの曖昧さのない構文解析は、すなわち次のようにすることである。

```hs
case x of { (a,_) | (let b = not a in b :: Bool) -> a }
```

しかしながら、`Bool -> a`というフレーズは型として構文的に正当であり、先読みが制限されているパーサーは、この選択に誤ってコミットする可能性があり、それゆえプログラムは拒否する。故に、プログラマーは型シグネチャで終わるガードを避けるように勧められる。これは実際にガードが`exp`ではなく`infixexp`を含んでいる理由になる。

## Do式

|||||
|--|--|--|--|
| <em>lexp</em>|→|<tt>do</tt> { <em>stmts</em> }|(do expression)|
|<em>stmts</em>|→|<em>stmt<sub>1</sub></em> … <em>stmt<sub>n</sub></em> <em>exp</em> [<tt>;</tt>]|(n ≥ 0)|
| <em>stmt</em>|→|<em>exp</em> ;| |
|	        |&#124;|<em>pat</em> <- <em>exp</em> ;| |
|         |&#124;|<tt>let</tt> <em>decls</em> ;| |
|         |&#124;|<tt>;</tt> 	                   |(empty statement)|

do式はモナドのプログラミングのためのより従来的な構文を提供する。それは以下のような式を許す。

```hs
putStr "x: "    >>  
getLine         >>= \l ->  
return (words l)
```

より、旧来の方法による書き方は次のものになる。

```hs
do putStr "x: "  
   l <- getLine  
   return (words l)
```

<div class="column">

**変換：** Do式はこれらの等式を満たし、排除した空の`stmts`の後にカーネルの中への変換のように使われるかもしれない。

||||
|--|--|--|
|<tt>do</tt> {<em>e</em>} |=| 	<em>e</em>|
|<tt>do</tt> {<em>e</em>;<em>stmts</em>} |=| 	<em>e</em> >> <tt>do</tt> {<em>stmts</em>}|
|<tt>do</tt> {<em>p</em> <- e; <em>stmts</em>}|=|<tt>let</tt> <tt>ok</tt> <em>p</em> = <tt>do</tt> {<em>stmts</em>}|
|		  　　　　　　　　| |&ensp;&emsp;<tt>ok</tt> _ = <tt>fail</tt> "..."|
|		               | |&ensp;<tt>in</tt> <em>e<em> >>= <tt>ok</tt>|
|<tt>do</tt> {<tt>let</tt> <em>decls</em>; <em>stmts</em>}|=|<tt>let</tt> <em>decls</em> <tt>in</tt> <tt>do</tt> {<em>stmts</em>}|

コンパイラが生成したエラーメッセージを表す省略記号`"..."`の部分は`fail`へ渡され、そして可能であればパターンマッチに失敗した場所を表示する。関数`>>`,`>>=`と`fail`はPreludeで定義されたクラス`Monad`の操作であり、`ok`は新しい識別子である。

</div>

`do`の変換でも示したように、`let`に束縛された変数は完全に多相的な型をもつ一方で`<-`によって定義された変数はラムダ束縛であり、ゆえに単相的である。

## フィールドラベル付きのデータ型

データ型の宣言はフィールドラベルを必要に応じて定義してもよい。(セクション[4.2.1]("./4-declarations-and-bindings.md")を参照)これらのフィールドラベルは構築、形式の選択、データ型全体の構造に依存した方法でのフィールドの更新することに使用される。

異なるデータ型は同じスコープの共通のフィールドラベルを共有することはできない。フィールドラベルはコンストラクタ内で高々一度だけ、使用することができる。しかしながら、データ型の中で、あるフィールドがすべてのコンストラクタ内で同じ型を持つときに限り1つのフィールドを複数のコンストラクタで使用することができる。最後の点については次が良い例である:

```hs
data S = S1 { x :: Int } | S2 { x :: Int }   -- OK  
data T = T1 { y :: Int } | T2 { y :: Bool }  -- BAD
```

ここでの`s`は正当であるが`T`はそうではない。また`y`は後者では矛盾する型付けが与えられている。

### フィールドセレクション

||||
|--|--|--|
|aexp|→|qvar|

フィールドラベルはセレクタ関数のように使用される。変数のように使われる際は、フィールドラベルはオブジェクトからフィールドを抽出する関数のように振る舞う。セレクタはトップレベルの束縛であり、よってローカル変数によってシャドーイングされる場合があるが、しかし他のトップレベルの束縛で同じ名前のものと衝突してはならない。この覆いはセレクタ関数にのみ影響を及ぼし、レコード作成(セクション[3.15.2]("#3.15.2"))及びに更新(セクション[3.15.3]("#3.15.3"))、フィールドラベルは通常の変数と混合されることはない。

<div class="column">

**変換:** フィールドラベル`f`は次のようなセレクタ関数を生成する。

||||
|--|--|--|
|f x|=|<tt>case</tt> x <tt>of</tt> { <em>C<sub>1</sub></em> <em>p<sub>11</sub></em> … <em>p<sub>1k</sub></em>  ->  <em>e<sub>1</sub></em> ;… ; <em>C<sub>n</sub> p<sub>n1</sub></em> … <em>p<sub>nk</sub></em>  ->  <em>e<sub>n</sub></em> }|

<code>C<sub>1</sub> ... C<sub>n</sub></code>は全て<code>f</code>とラベルされたフィールドを含むデータ型のコンストラクタで、<code>p<sub>ij</sub></code>は`f`が<code>C<sub>i</sub></code>の要素の`j`番目、または`_`をラベルした時の`y`であり、<code><em>e<code>i</code></em></code>は<code><em>C<sub>i</sub></em></code>のフィールドが`f`または`undefined`のラベルを持つ時の`y`である。

</div>

### フィールドラベルを用いた生成

|||||
|--|--|--|--|
| <em>aexp</em>|→|<em>qcon</em> { <em>fbind<sub>1</sub></em> , … , <em>fbind<sub>n</sub></em> }|(labeled construction, <em>n</em> ≥ 0)|
|<em>fbind</em>|→|<em>qvar</em> = <em>exp</em>| |

ラベル付けされたフィールドを使うコンストラクタが値の生成に使われる場合があるが、その時には各コンポーネントは位置ではなく名前によって指定する。宣言リストの中で使われる中括弧とは異なりレイアウトの対象にならない。`{`と`}`の文字は明示しなければならない。(これはフィールドの更新、フィールドパターンにおいても正しい。)フィールドラベルを使用する構築は次の制約に応じる。

- 指定されたコンストラクタで宣言されたフィールドラベルのみ言及してよい。
- フィールドラベルは複数回言及してはならない。
- 言及されないフィールドは`⊥`で初期化される。
- 正格なフィールド(宣言された型のフィールドの接頭語に`!`が付けられている)が生成の際に省略された時はコンパイルエラーが発生する。厳格なフィールドはセクション[4.2.1]("./4-declarations-and-bindings.md")で説明される。

式`F {}`は、`F`はデータコンストラクタであり、<strong><code>F</code>がレコード構文により宣言されたかどうかに関わらず</strong>、正当である(ただし`F`が正格フィールドを持たない時に限る。上の4番目の箇条書きを参照)。それは<code>F ⊥<sub>1</sub> … ⊥<sub>n</sub></code>を表し、`n`は`F`の引数の数である。

<div class="column">

**変換：** `f = v`の束縛で、フィールド`f`は`v`でラベルする。

||||
|--|--|--|
|<em>C</em> { <em>bs</em> }|=|C (<em>pick<sub>1</sub><sup>C</sup></em> <em>bs</em> <em><sub>undefined</sub></em>) … (<em>pick<sub>k</sub><sup>C</sup></em> <em>bs</em> <em><sub>undefined</sub></em>)|

`k`は`C`の引数の数である。

補助関数<code>pick<sub>i</sub><sup>C</sup> bs d</code>は次にように定義される。
<p>
もし、コンストラクタ`C`の`i`番目の要素がフィールドラベル`f`を持ち、`if f=v`は束縛された`bs`に表示されるなら、その時は<code>pick<sub>i</sub><sup>C</sup> bs d</code>は`v`である。言い換えると<code>pick<sub>i</sub><sup>C</sup> bs d</code>はデフォルト値`d`である。
</p>
</div>

### フィールドラベルを使用した更新

|||||
|--|--|--|--|
|<em>aexp</em>|→|<em>aexp<sub>⟨qcon⟩</sub></em> { <em>fbind<sub>1</sub></em> , … , <em>fbind<sub>n</sub></em> }|(labeled update, <em>n</em> ≥ 1)|

フィールドラベルを使ったデータ型に所属する値は非破壊的に更新されるかもしれない。これは元々存在していた値を指定されたフィールドの値で書き換えた新しい値を生成する。更新は次の方法に制限される。

- 全てのラベルは同じデータ型から取られなければいけない。
- 少なくともあるコンストラクタは更新の中で全ての言及されたラベルを定義しなければいけない。
- 2回以上言及されるラベルがあってはならない。
- 実行エラーは更新された値が全ての明記されたラベルを含まない時に発生する。

<div class="column">

**変換:** 以下は以前の`pick`の定義を使用する。

||||
|--|--|--|
|e { bs }|=|<tt>case</tt> <em>e</em> <tt>of</tt>|
|		     | |&emsp;&emsp;&emsp;&emsp;<em>C<sub>1</sub></em> <em>v<sub>1</sub></em> … <em>v<sub>k1</sub></em> -> <em>C<sub>1</sub></em> (<em>pick<sub>1</sub><sup>C1</sup></em> <em>bs</em> <em>v<sub>1</sub></em>) … (<em>pick<sub>k 1</sub><sup>C1</sup></em> <em>bs</em> <em>v <sub>k1</sub></em>)
|		     | |&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;...|
|		     | |&emsp;&emsp;&emsp;&emsp;<em>C<sub>j</sub></em> <em>v<sub>1</sub></em> … <em>v<sub>kj</sub></em> -> <em>C<sub>j</sub></em> (<em>pick<sub>1</sub><sup>Cj</sup></em> <em>bs</em> <em>v<sub>1</sub></em>) … (<em>pick<sub>k j</sub><sup>Cj</sup></em> <em>bs</em> <em>v <sub>k<sub>j</sub></sub></em>)|
|		     | |&emsp;&emsp;&emsp;&emsp;_ -> error "Update error"|

<code>{ <em>C<sub>1</sub></em>,...,<em>C<sub>j</sub></em>}</code>は`bs`内の全てのラベルを含むコンストラクタの集合で、ｋ<sub>i</sub>は<em>C<sub>i</sub></em>の引数の数である。

</div>

これはラベル付けされたフィールドを使用している例である。

```hs
data T    = C1 {f1,f2 :: Int}  
          | C2 {f1 :: Int,  
                f3,f4 :: Char}
```

|式|変換|
|--|--|
| C1 {f1 = 3} | C1 3 <tt>undefined</tt> |
|  C2 {f1 = 1, f4 = 'A', f3 = 'B'} | C2 1 'B' 'A' |
| x {f1 = 1} | <pre><tt>case</tt> x <tt>of</tt> C1 _ f2    -> C1 1 f2<br>          C2 _ f3 f4 -> C2 1 f3 f4</pre> |

フィールド`f1`は両方の`T`のコンストラクタに共通である。この例では、フィールドラベル表記でコンストラクタを使った式をフィールドラベルを使わない同じコンストラクタを使った同値な式へと変換している。もし、`x {f2 = 1, f3 = 'x'}`のように、どのコンストラクタも、更新で使われたフィールドラベルの集合を定義していないのであれば、コンパイル時エラーという結果になるだろう。

## 式の型シグネチャ

||||
|--|--|--|
|<em>exp</em>|→|<em>exp</em> <tt>::</tt> [<em>context</em> =>] <em>type</em>|

式の型シグネチャは形式`e :: t`を持つ。`e`は式で、`t`は型(セクション[4.1.2]("./4-declarations-and-bindings.md"))であり、それらは明示的に式を分類することに使用され、オーバーロード(セクション[4.1.2](”./4-declarations-and-bindings.md”)を参照)するために曖昧な型付けを解決することに使われるかもしれない。式の値は`exp`の値である。通常の型シグネチャと同様に(セクション[4.4.1](”./4-declarations-and-bindings.md”)を参照)、宣言された型は`exp`から導出可能な主要な型より具体的になるかもしれないが、主要な型より一般的なまたは同程度な型を与えることはエラーである。

<div class="column">

**変換：**

<pre><em>e</em> :: <em>t</em> 	= 	<tt>let</tt> { <em>v</em> :: <em>t</em>;  <em>v</em> = <em>e</em> } <tt>in</tt> <em>v</em></pre>

</div>

## パターンマッチング

**パターン**はラムダ抽象や関数定義、パターン束縛、リスト内包表記、do式、case式内で現れる。しかしながら、はじめの5つは最終的にcase式に変換されるので、パターンマッチの意味論はcase式のときのみ定めれば十分である。

### パターン

パターンはこの構文を持つ。

|||||
|--|--|--|--|
|<em>pat</em>|→|<em>lpat</em> <em>qconop</em> <em>pat</em>|(infix constructor)|
|	      |&#124;|<em>lpat</em>| |
|||||
|<em>lpat</em>|→|<em>apat</em>| |
|	       |&#124;|<tt>-</tt> (<em>integer</em> &#124; <em>float</em>)|(negative literal)|
|	       |&#124;|<em>gcon</em> <em>apat<sub>1</sub></em> … <em>apat<sub>k</sub></em>|(arity gcon  =  <em>k</em>, <em>k</em> ≥ 1)|
|||||
|<em>apat</em>|→|<em>var</em> [ <tt>@</tt> <em>apat</em>]|(as pattern)|
|	       |&#124;|<em>gcon</em>|(arity gcon  =  0)|
|	       |&#124;|<em>qcon</em> { <em>fpat1</em> , … , <em>fpatk</em> }|(labeled pattern, <em>k</em> ≥ 0)|
|	       |&#124;|<em>literal</em>| |
|	       |&#124;|<tt>_</tt> 	                    |(wildcard)|
|	       |&#124;|( <em>pat</em> )                     |(parenthesized pattern)|
|	       |&#124;|( <em>pat<sub>1</sub></em> , … , <em>pat<sub>k</sub></em> )|(tuple pattern, <em>k</em> ≥ 2)|
|	       |&#124;|[ <em>pat<sub>1</sub></em> , … , <em>pat<sub>k</sub></em> ]|(list pattern, <em>k</em> ≥ 1)|
|	       |&#124;|<tt>~</tt> <em>apat</em>|(irrefutable pattern)|
|||||
|<em>fpat</em>|→|<em>qvar</em> <tt>=</tt> <em>pat</em>| |

コンストラクタの引数の数はそれに関係するサブパターンの数と一致しなければいけない。部分的に適用されるコンストラクタに反して一致することはできない。

全てのパターンは**線形**でなければならない。変数は2回以上現れないかもしれない。例として、この定義は不正である。

```hs
f (x,x) = x     -- ILLEGAL; x used twice in pattern
```

形式`var@pat`のパターンは`as-patterns`と呼ばれ、`var`を`pat`によってマッチされた値に付ける名前として使うことができる。例えば以下のものは、

```hs
case e of { xs@(x:rest) -> if x==0 then rest else xs }
```

は次のものと等しい。

```hs
let { xs = e } in  
  case xs of { (x:rest) -> if x==0 then rest else xs }
```

形式`_`のパターンは`ワイルドカード`であり、パターンのいくつかの部分が右手側で参照されない時に便利である。それは他の場所で使われない識別子がその場所に置かれているかのようである。例えば、以下は、

```hs
case e of { [x,_,_]  ->  if x==0 then True else False }
```

は次のものと等しい。

```hs
case e of { [x,y,z]  ->  if x==0 then True else False }
```

### パターンマッチングの非形式的の意味論

パターンは値に対してマッチが行われる。パターンマッチを行おうとした場合、次の3つのいずれかの結果を得る。**失敗**かもしれない、**成功**かもしれず、その時はパターン内の各変数に束縛を返す、**分岐する**かもしれない(例:`⊥`を返す)。パターンマッチングは次のルールによって外から内へ、左から右へ進行する。

1. 値`v`に対してマッチするパターン`var`のマッチングは常に成功し、`var`を`v`に束縛する。
1. 値`v`に対してマッチするパターン`~apat`のマッチングは常に成功する。もし`v`に対してマッチする`apat`のマッチングが別の方法で成功するならば、`apat`内の束縛されていない変数は適切な値に束縛される。`v`に対してマッチする`apat`のマッチングが失敗または分岐するなら`⊥ `に束縛される(束縛は評価をほのめかさない)。<br><br>運用上、これはある`apat`内の変数が使われるまで、パターン`~apat`が何とも一致しないことを意味する。その時点でパターン全体はその値に対してマッチし、もし一致が失敗または分岐するなら、全体の計算を行う。
1. あらゆる値に対してマッチするワイルドパターン`_`のマッチングは常に成功し、束縛は行われない。
1. 値に対してマッチするパターン`con pat`のマッチングは、`con`は`newtype`によって定義されたコンストラクタである、以下の項目でその値に依存する。
     - もし値が形式`con v`であるなら、その時`pat`は`v`に対してマッチされる。
     - もし値が`⊥`なら、その時`pat`は`⊥`に対してマッチする。

    すなわち`newtype`と関連するコンストラクタが値の型を変更することのみに務める。
1. 値に対しての<code>con pat<sub>1</sub> ... pat<sub>n</sub></code>のマッチングは、`con`は`data`によって定義されるコンストラクタである、依存するその値に依存する。
     - もし値が形式<code>con pat<sub>1</sub> ... pat<sub>n</sub></code>であるなら、サブパターンはそのデータ値の要素に対して左から右へ一致される。もし、全てのマッチングが成功したなら、マッチング全体は成功し、はじめの失敗または分岐はマッチング全体を各々、失敗または分岐へともたらす。
     - もし値が形式<code>con' <em>v<sub>1</sub></em> ... <em>v<sub>m</sub></em></code>であるなら、`con`は`con'`への異なるコンストラクタである、そのマッチングは失敗する。
     - もし値が`⊥`なら、そのマッチングは分岐する。
1. ラベル付きフィールドを使ったコンストラクタに対してのマッチングはそのフィールドがフィールドリスト内で指定された順序で照合されることを除いて、通常のコンストラクタパターンのマッチングと同じである。全てのリストされたフィールドはコンストラクタによって宣言されなければならず、フィールドは2回以上指定されないかもしれない。パターンによって指定されたフィールドは無視される(`_`に対して一致する)。
1. 値`v`対する数値、文字、文字列リテラルパターン`k`のマッチングはもし、`v == k`なら成功する。<code><tt>==</tt></code>はパターンの型を元にオーバロードされる。マッチングはもしこのテストが分岐するなら分岐する。<br><br>数値リテラルの解釈はまさにセクション[3.2]("#3.2")で記載のとおりである。即ち、オーバロードされた関数`fromInteger`または`fromRational`は(それぞれ)適切な型へ変換することによって`Integer`または`Rational`リテラルに適用される。


静的型の制約(例えば、文字とbooleanを一致させる静的なエラー)は別として、次の静的クラスの制約は保持する。

- 整数リテラルパターンはクラス`Num`の値とのみ照合できる。
- 浮動小数点リテラルパターンはクラス`Factional`の値とのみ照合できる。

2種類のパターンの区別することはしばしば有用である。`反駁できない`パターンの照合は厳密ではなく、そのパターンはもし、照合された値が`⊥`なら一致する。`反駁できる`パターンは厳密であり、その一致される値が`⊥`なら分岐する。反駁できないパターンは次のものである。変数やワイルドカード、`N`が`newtype`と`apat`によって定義されたコンストラクタ`N apat`は反駁できず(セクション[4.2.3]("./4-declarations-and-bindings.md"))、`var@apat`の`apat`は反駁できない、または形式`~apat`(`apat`が反駁できないかどうか)である。他の全てのパターンは反駁できる。

ここにいくつかの例をだす。

1. もし、パターン`['a','b']`が`['x',⊥]`と一致されるなら、その時、`'a'`は`x`との一致に**失敗し**、その結果は失敗と一致する。しかし、もし`['a','b']`が`[⊥,'x']`と一致されるなら、その時、'a'と`⊥`を一致するよう試みることは**分岐**と一致することをもたらす。
1. これらの例は反駁できるものとできないもののマッチングの実演である。

    ||||
    |--|--|--|
    |(\ ~(x,y) -> 0) ⊥|⇒|0|
    |(\  (x,y) -> 0) ⊥|⇒|⊥|

    ||||
    |--|--|--|
    |(\ ~[x] -> 0) []|⇒|0|
    |(\ ~[x] -> x) []|⇒|⊥|

    ||||
    |--|--|--|
    |(\ ~[x,~(a,b)] -> x) [(0,1),⊥]|⇒|(0,1)|
    |(\ ~[x, (a,b)] -> x) [(0,1),⊥]|⇒|⊥|

    ||||
    |--|--|--|
    |(\  (x:xs) -> x:x:xs) ⊥|⇒|⊥|
    |(\ ~(x:xs) -> x:x:xs) ⊥|⇒|⊥:⊥:⊥|
1. 次の宣言を考えてほしい。
    ```hs
    newtype N = N Bool  
    data    D = D !Bool
    ```
    これらの例は`data`と`newtype`によって定義された型においてのパターンマッチングの違いを説明する。

    ||||
    |--|--|--|
    |(\  (N True) -> True) ⊥|⇒|⊥|
    |(\  (D True) -> True) ⊥|⇒|⊥|
    |(\ ~(D True) -> True) ⊥|⇒|True|

    追加の例はセクション[4.2.3]("./4-declarations-and-bindings.md")で見つかるかもしれない。

関数内のcase式内の最上位パターンと最上位パターンの集合またはパターン束縛は0以上の`ガード`に関係する持つかもしれない。ガードの構文と意味論についてはセクション[3.13]("#3.13")を参照してもらいたい。

ガード意味論は関数またはcase式の厳密な特徴への影響を持つ。特に、他の反駁できないパターンがガードのために評価されるかもしれない。例えば、次の

```hs
f :: (Int,Int,Int) -> [Int] -> Int  
f ~(x,y,z) [a] | (a == y) = 1
```

`a`と`y`の両方はガードの`==`によって評価される。

### パターンマッチングの正式な意味論

case式を除くすべてのパターンマッチの意味論は、パターンマッチの構成と`case`式との間を関連付ける等式を与えることで定められる(**訳注**: パターンマッチの意味論は一旦case式を使って定義し、そのあとcase式の意味論に従って処理を行う)。<tt>case</tt>式の意味論自体は図[3.1]("#figure-3.1")、[3.3](#figure-3.3)の、一連の識別子のように順番に与えられる。どんな実装でもこれらの識別子を保持するために振る舞わなければならず、かなり非効率的なコードを生成することから、それはそれらを直接使用することは期待されない。

<a name="figure-3.1"></a>
<table class="fbox">
<tbody>
 <tr>
  <td class="code-number">(a)</td>
  <td><pre>
<tt>case</tt> <em>e</em> <tt>of</tt> { alts } = (\v <tt>-></tt> <tt>case</tt> <em>v</em> <tt>of</tt> { alts }) <em>e</em>
<tt>where</tt> <em>v</em> <tt>is</tt> a <tt>new</tt> <tt>variable</tt> </pre></td>
 </tr>
 <tr>
  <td class="code-number">(b)</td>
  <td><pre>
<tt>case</tt>  <em>v</em> <tt>of</tt> {  <em>p</em> <sub>1</sub>  match<sub>1</sub>;  … ; <em>p<sub>n</sub></em>  match<sub>n</sub> }
=  <tt>case</tt> <em>v</em> <tt>of</tt> { <em>p<sub>1</sub></em>  match<sub>1</sub> ;
               _  -> … <tt>case</tt> <em>v</em> <tt>of</tt> {
                         <em>p<sub>n</sub></em>  match<sub>n</sub> ;
                         _  -> <tt>error</tt> "No match" }…}
<tt>where</tt> <tt>each</tt> match<sub>i</sub> <tt>has the form:</tt>
 | <em>gs<sub>i,1</sub></em>  -> <em>e<sub>i,1</sub></em> ; … ; | <em>gs<sub>i,m<sub>i</sub></sub></em> -> <em>e<sub>i,m<sub>i</sub></sub></em> <tt>where</tt> { <em>decls</em><sub>i</sub> } </pre></td>
 </tr>
 <tr>
  <td class="code-number">(c)</td>
  <td><pre>
<tt>case</tt> <em>v</em> <tt>of</tt> { <em>p</em> | <em>gs<sub>1</sub></em> -> <em>e<sub>1</sub></em> ; …
             | <em>gs<sub>n</sub></em> -> <em>e<sub>n</sub></em> <tt>where</tt> { <em>decls</em> }
            _     -> <em>e′</em> }
= <tt>case</tt> <em>e′</em> <tt>of</tt> { <em>y</em> ->
   <tt>case</tt> <em>v</em> <tt>of</tt> {
     <em>p</em> -> <tt>let</tt> { <em>decls</em> } <tt>in</tt>
          <tt>case</tt> <tt>()</tt> <tt>of</tt> {
            <tt>()</tt> | <em>gs<sub>1</sub></em> -> <em>e<sub>1</sub></em>;
            _ -> … <tt>case</tt> <tt>()</tt> <tt>of</tt> {
                       <tt>()</tt> | <em>gs<sub>n</sub><em> -> <em>e<sub>n</sub></em>;
                       _  -> <em>y</em> } … }
     _ -> <em>y</em> }}
<tt>where</tt> <em>y</em> <tt>is a new variable</tt></pre></td>
 </tr>
 <tr>
 <td class="code-number">(d)</td>
 <td><pre>
<tt>case</tt> <em>v</em> <tt>of</tt> { ~p -> <em>e</em>; _ -> <em>e′</em> }
= (\x<sub>1</sub> … x<sub>n</sub> -> <em>e</em> ) (<tt>case</tt> <em>v</em> <tt>of</tt> { p-> x<sub>1</sub> })… (<tt>case</tt> <em>v</em> <tt>of</tt> { <em>p</em> -> x<sub>n</sub>})
<tt>where</tt> x<sub>1</sub>,…,x<sub>n</sub> <tt>are all the variables in</tt> p</pre></td>
 </tr>
 <tr>
 <td class="code-number">(<em>e</em>)</td>
 <td><pre>
<tt>case</tt> <em>v</em> <tt>of</tt> { x<tt>@</tt><em>p</em> -> <em>e</em>; _ -> <em>e′</em> }
=  <tt>case</tt> <em>v</em> <tt>of</tt> { <em>p</em> -> ( \ x -> <em>e</em> ) <em>v</em> ; _ -> <em>e′</em> }</pre></td>
 </tr>
 <tr>
 <td class="code-number">(f)</td>
 <td><pre>
<tt>case</tt> <em>v</em> <tt>of</tt> { _ -> <em>e</em>; _ -> <em>e′</em> } = <em>e</em> </pre></td>
 </tr>
</tobdy>
</table>

**図 3.1:** case式の意味論、パート1
<div class="separator"></div>

<div class="separator"></div>

<a name="figure-3.2"></a>
<table class="fbox">
<tbody>
 <tr>
  <td class="code-number">(g)</td>
  <td><pre><tt>case</tt> <em>v</em> <tt>of</tt> { <em>K</em> p<sub>1</sub>…p<sub>n</sub> -> <em>e</em>; _ -> <em>e′</em> }
	= <tt>case</tt> <em>v</em> <tt>of</tt> {
	     <em>K</em> x<sub>1</sub>…x<sub>n</sub> -> <tt>case</tt> x<sub>1</sub> <tt>of</tt> {
	                    <em>p<sub>1</sub></em> -> … <tt>case</tt> xn <tt>of</tt> { <em>p<sub>n</sub></em> -> <em>e</em> ; _ -> <em>e′</em> } …
	                    _  -> <em>e′</em> }
	     _ -> <em>e′</em> }
	<tt>at least one of</tt> <em>p<sub>1</sub></em>,…,<em>p<sub>n</sub></em> <tt>is not a variable;</tt> x<sub>1</sub>,…,x<sub>n</sub> <tt>are new variables</tt> </pre></td>
 </tr>
 <tr>
  <td class="code-number">(h)</td>
  <td><pre><tt>case</tt> <em>v</em> <tt>of</tt> { <em>k</em> -> <em>e</em>; _ -> <em>e′</em> } = <tt>if</tt> (v==k) <tt>then</tt> <em>e</em> <tt>else</tt> <em>e′</em>
	<tt>where</tt> <em>k</em> <tt>is a numeric, character, or string literal</tt></pre></td>
 </tr>
 <tr>
  <td class="code-number">(i)</td>
  <td><pre><tt>case</tt> <em>v</em> <tt>of</tt> { x -> <em>e</em>; _ -> <em>e′</em> } = <tt>case</tt> <em>v</em> <tt>of</tt> { x -> <em>e</em> }</pre></td>
 </tr>
 <tr>
 <td class="code-number">(j)</td>
 <td><pre><tt>case</tt> <em>v</em> <tt>of</tt> { x -> <em>e</em> } = ( \ x -> <em>e</em> ) <em>v</em> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(k)</td>
 <td><pre><tt>case</tt> <em>N</em> <em>v</em> <tt>of</tt> { <em>N</em> <em>p</em> -> <em>e</em>; _ -> <em>e′</em> }
	= <tt>case</tt> <em>v</em> <tt>of</tt> { <em>p</em> -> <em>e</em>; _ -> <em>e′</em> }
	<tt>where</tt> <em>N</em> <tt>is a newtype constructor</tt> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(l)</td>
 <td><pre><tt>case</tt> ⊥ <tt>of</tt> { <em>N</em> <em>p</em> -> <em>e</em>; _ -> <em>e′</em> } = <tt>case</tt> ⊥ <tt>of</tt> { <em>p</em> -> <em>e</em> }
	<tt>where</tt> <em>N</em> <tt>is a newtype constructor</tt> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(m)</td>
 <td><pre><tt>case<tt>  <em>v</em>  <tt>of<tt> {  <em>K</em>  { <em>f<sub>1</sub></em>  =  <em>p<sub>1</sub></em>  ,  <em>f<sub>2</sub></em>  =  <em>p<sub>2</sub></em>  , … } ->  <em>e</em> ; _ ->  <em>e′</em> }
	=  <tt>case</tt> <em>e′</em> <tt>of</tt> {
	    <em>y</em> ->
	    <tt>case</tt>  <em>v</em>  <tt>of</tt> {
	      <em>K</em>  {  <em>f<sub>1</sub></em>  =  <em>p<sub>1</sub></em>  } ->
	            <tt>case</tt>  <em>v</em>  <tt>of</tt> { <em>K</em>  { <em>f<sub>2</sub></em>  =  <em>p<sub>2</sub></em>  , …  } ->  <em>e</em> ; _ ->  <em>y</em>  };
	            _ ->  <em>y</em>  }}
	<tt>where</tt> <em>f<sub>1</sub></em>, <em>f<sub>2</sub></em>, … <tt>are fields of constructor</tt> <em>K</em>; <em>y</em> <tt>is a new variable</tt> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(n)</td>
 <td><pre><tt>case</tt>  <em>v</em>  <tt>of</tt> {  <em>K</em>  { f  =  <em>p</em> } ->  <em>e</em> ; _ ->  <em>e′</em> }
  = <tt>case</tt>  <em>v</em>  <tt>of</tt> {
       <em>K</em> <em>p<sub>1</sub></em> … <em>p<sub>n</sub></em>  ->  <em>e</em> ; _ ->  <em>e′</em> }
  <tt>where</tt> <em>p<sub>i</sub></em> <tt>is</tt> <em>p</em> <tt>if f labels the ith component of</tt> <em>K</em>, _ <tt>otherwise</tt> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(o)</td>
 <td><pre><tt>case</tt>  <em>v</em>  <tt>of</tt> {  <em>K</em>  {} ->  <em>e</em> ; _ ->  <em>e′</em> }
  = <tt>case</tt>  <em>v</em>  <tt>of</tt> {
       <em>K</em> _ … _ ->  <em>e</em> ; _ ->  <em>e′</em> }</pre></td>
 </tr>
 <tr>
 <td class="code-number">(p)</td>
 <td><pre><tt>case</tt> (K′ <em>e<sub>1</sub></em> … <em>e<sub>m</sub></em>) <tt>of</tt> { <em>K</em> x<sub>1</sub> … x<sub>n</sub> -> <em>e</em>; _ -> <em>e′</em> } = <em>e′</em>
  <tt>where</tt> <em>K</em> <tt>and</tt> <em>K′</em> <tt>are distinct data constructors of arity</tt> <em>n</em> <tt>and</tt> m<tt>, respectively</tt></pre></td>
 </tr>
 <tr>
 <td class="code-number">(q)</td>
 <td><pre><tt>case</tt> (K <em>e<sub>1</sub></em> … <em>e<sub>n</sub></em>) <tt>of</tt> { <em>K</em> x<sub>1</sub> … x<sub>n</sub> -> <em>e</em>; _ -> <em>e′</em> }
  = (\x<sub>1</sub> … x<sub>n</sub> -> <em>e</em>) <em>e<sub>1</sub></em> … <em>e<sub>n</sub></em>
  <tt>where</tt> <em>K</em> <tt>is a data constructor of arity</tt> <em>n</em> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(r)</td>
 <td><pre><tt>case</tt> ⊥ <tt>of</tt> { <em>K</em> x<sub>1</sub> … x<sub>n</sub> -> <em>e</em>; _ -> <em>e′</em> } =  ⊥
  <tt>where</tt> <em>K</em> <tt>is a data constructor of arity</tt> <em>n</em></pre></td>
 </tr>
</tobdy>
</table>

**図 3.2:** case式の意味論、パート2
<div class="separator"></div>

<div class="separator"></div>

<a name="figure-3.3"></a>
<table class="fbox">
<tbody>
<tr>
 <td class="code-number">(s)</td>
 <td><pre><tt>case</tt> <tt>()</tt> <tt>of</tt> { <tt>()</tt> | <em>g<sub>1</sub></em>, …, <em>g<sub>n</sub></em> -> <em>e</em>; _ -> <em>e′</em> }
= <tt>case</tt> <tt>()</tt> <tt>of</tt> {
     <tt>()</tt> | <em>g<sub>1</sub></em> -> … <tt>case</tt> <tt>()</tt> <tt>of</tt> {
                    <tt>()</tt> | <em>g<sub>n</sub></em> -> <em>e</em>;
                    _ -> <em>e′</em> } …
     _ -> <em>e′</em> }
 where <em>y</em> is a new variable </pre></td>
</tr>
 <td class="code-number">(t)</td>
 <td><pre>case <tt>()</tt> <tt>of</tt> { <tt>()</tt> | <em>p</em> <- <em>e<sub>0</sub></em> -> <em>e</em>; _ -> <em>e′</em> }
= <tt>case</tt> <em>e</em><sub>0</sub> <tt>of</tt> { <em>p</em> -> <em>e</em>; _ -> <em>e′</em> }</pre></td>
</tr>
<tr>
<td class="code-number">(u)</td>
<td><pre><tt>case</tt> <tt>()</tt> <tt>of</tt> { <tt>()</tt> | <tt>let</tt> <em>decls</em> -> <em>e</em>; _ -> <em>e′</em> }
= <tt>let</tt> <em>decls</em> in <em>e</em> </pre></td>
</tr>
<tr>
 <td class="code-number">(v)</td>
 <td><pre><tt>case</tt> <tt>()</tt> <tt>of</tt> { <tt>()</tt> | <em>e<sub>0</sub></em> -> <em>e</em>; _ -> <em>e′</em> }
  = if <em>e<sub>0</sub></em> then <em>e</em> else <em>e′</em> </pre></td>
</tr>
</tobdy>
</table>

**図 3.3:** case式の意味論、パート3
<div class="separator"></div>

図[3.1]("#figure-3.1")-[3.3]("#figure-3.3")の<code>e, e'と<em>e<sub>i</sub></em></code>は式で、<code>g<sub>i</sub>とgs<sub>i</sub></code>はガードと各々のガードの並びであり、<code>pと<em>p<sub>i</sub></em></code>はパターン、<code>v, x, x<sub>i</sub></code>は変数、`K,K'`は代数的データ型`(data)`コンストラクタ(タプルコンストラクタを含む)で、`N`は`newtype`コンストラクタである。

ルール(b)は実際にガードを含むかどうかにはかかわらず、一般的な表層ソース言語の`case`式に適合するものである。もしガードが書かれていなければ、その時、`True`が形式<code>match<sub>i</sub></code>内のガード<code>gs<sub>i,j</sub></code>に代用される。各々の識別子はもっと簡単な形式へと`case`式の結果を操作する。

図[3.2]("#figure-3.2")のルール(h)はオーバロードされた`==`演算子を起動し、パターンマッチングの意味をオーバーロードされた定数に対して定義するというルールである。

これらの識別子は静的な意味論を全て保存する。ルール(d)、(e)、(j)、(q)は`let`ではなくラムダを使っていて、これは`case`によって束縛された変数が単相型ということを示す(セクション[4.1.4]("./4-declarations-and-bindings.md")を参照)。
