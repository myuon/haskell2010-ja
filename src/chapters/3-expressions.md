# 式

この章では、私たちはHaskellの式の構文と非形式的な意味論を説明する。また必要であらばHaskellカーネルへの変換についても説明する。<code>let</code>式の場合を除いて、これらの変換は静的、動的な意味論の両方を保存する。これらの変換を使った束縛されていない変数とコンストラクタは常に<code>Prelude</code>によって定義された実体を参照する。例えば、リスト内包表記の変換(セクション[3.11])で使われる<code>"concatMap"</code>は<code>Prelude</code>によって定義された<code>concatMap</code>を意味する。これは識別子<code>"concatMap"</code>がリスト内包表記で使われているスコープ内にあるかないかは関係なく、また、(もしスコープ内にあったとしても)束縛されていても関係はない。

|||||
|--|--|--|--|
|<em>exp</em>|→|<em>infixexp</em> <code>::</code> [<em>context</em> <code>=></code>] <em>type</em>|(expression type signature)|
|       |&#124;|<em>infixexp</em>| |
| | | | |
|<em>infixexp</em>|→|<em>lexp</em> <em>qop</em> <em>infixexp</em>|(infix operator application)|
|            |&#124;|<code>-</code> <em>infixexp</em>|(prefix negation)|
|            |&#124;|<em>lexp</em>| |
| | | | |
|<em>lexp</em>|→|<code>\\</code> <em>apat<sub>1</sub></em> … <em>apat<sub>n</sub></em> <code>-></code> <em>exp</em>|(lambda abstraction, <em>n</em> ≥ 1)|
|        |&#124;|<code>let</code> <em>decls</em> <code>in</code> <em>exp</em>|(let expression)|
|        |&#124;|<code>if</code> <em>exp</em> [<code>;</code>] <code>then</code> <em>exp</em> [<code>;</code>] <code>else</code> <em>exp</em>|(conditional)|
|	       |&#124;|<code>case</code> <em>exp</em> <code>of</code> <code>{</code> <em>alts</em> <code>}</code>|(case expression)|
|        |&#124;|<code>do</code> <code>{</code> <em>stmts</em> <code>}</code>|(do expression)|
|        |&#124;|<em>fexp</em>| |
| | | | |
|<em>fexp</em>|→|[<em>fexp</em>] <em>aexp</em>|(function application)|
| | | | |
|<em>aexp</em>|→|<em>qvar</em>|(variable)|
|        |&#124;|<em>gcon</em>|(general constructor)|
|        |&#124;|<em>literal</em>| |
|        |&#124;|<code>(</code> <em>exp</em> <code>)</code>|(parenthesized expression)|
|        |&#124;|<code>(</code> <em>exp<sub>1</sub></em> <code>,</code> … <code>,</code> <em>exp<sub>k</sub></em> <code>)</code>|(tuple, <em>k</em> ≥ 2)|
|        |&#124;|<code>[</code> <em>exp<sub>1</sub></em> <code>,</code> … <code>,</code> <em>exp<sub>k</sub></em> <code>]</code>|(list, <em>k</em> ≥ 1)|
|        |&#124;|<code>[</code> <em>exp<sub>1</sub></em> [<code>,</code> <em>exp<sub>2</sub></em>] … [<em>exp<sub>3</sub></em>] <code>]</code>|(arithmetic sequence)|
|        |&#124;|<code>[</code> <em>exp</em> <code>&#124;</code> <em>qual<sub>1</sub></em> <code>,</code> … <code>,</code> <em>qual<sub>n</sub></em> <code>]</code>|(list comprehension, <em>n</em> ≥ 1)|
|        |&#124;|<code>(</code> <em>infixexp</em> <em>qop</em> <code>)</code>|(left section)|
|        |&#124;|<code>(</code> <em>qop<sub>⟨<code>-</code>⟩</sub></em> <em>infixexp</em> <code>)</code>|(right section)|
|        |&#124;|<em>qcon</em> <code>{</code> <em>fbind<sub>1</sub></em> <code>,</code> … <code>,</code> <em>fbind<sub>n</sub></em> <code>}</code>|(labeled construction, <em>n</em> ≥ 0)|
|        |&#124;|<em>aexp<sub>⟨qcon⟩</sub></em> <code>{</code> <em>fbind<sub>1</sub></em> <code>,</code> … <code>,</code> <em>fbind<sub>n</sub></em> <code>}</code>|(labeled update, <em>n</em>  ≥  1)|

中置演算子を含む式は演算子の結合性によって曖昧さを排除されている(セクション[4.4.2](./4-declarations-and-bindings.md)参照)。同じ優先度をもつ連続した括弧を持たない演算子は構文エラーを避けるためにどちらも左または右のどちらかに結合しなければならない。括弧を持たない式<em>"x qop<sup>(a,i)</sup> y qop<sup>(b,j)</sup> z"</em> ( <em>qop<sup>(a,i)</sup></em>は結合性が<em>a</em>で優先順位が<em>i</em>の演算子を意味する)が与えられた場合、<em>i = j</em>でかつ<em>a = b = l</em>か<em>a = b = r</em>でない時は、括弧は<em>"x qop<sup>(a,i)</sup> y"</em>か<em>"y qop<sup>(b,i)</sup> z"</em>のどちらかを囲むよう追加されなければいけない。

中置演算子を含む式の解決するためのアルゴリズムの例はセクション[10.6](./10-syntax-reference.md)にある。

符号反転演算子はHaskellにおいて唯一の接頭語になる。中置と同じ優先順位を持ち、演算子はPreludeの中に定義されている(セクション[4.4.2](./4-declarations-and-bindings.md), 図[4.1](./4-declarations-and-bindings.md))。

ラムダ抽象、let式、条件分岐の有効範囲について、本文法は曖昧である。この曖昧性は、各構文はできるだけ右へ拡張される、というメタ規則により解決される。

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
`⊥` ("bottom") と表記される式の評価中のエラーと、停止しないこととを、Haskellプログラムは区別することができない。Haskellは非正格評価の言語なことから、全てのHaskellの型は`⊥`を含む。つまり、いかなる型の値もユーザーが望めばエラーを返す計算になる可能性がある。評価されるとき、エラーは直ちにプログラムを停止させ、ユーザーによって捕捉されることはできない。Preludeは直接そのようなエラーを引き起こす二つの関数を提供している。

``` hs
error     :: String -> a
undefined :: a
```

<code>error</code>の呼び出しはプログラムの実行を終了させ、OSに適切なエラー表示を返す。そのエラー表示にはシステム依存の方法で文字列を画面に表示するべきである。<code>undefined</code>が使われたとき、そのエラーメッセージはコンパイラーによって作成される。

Haskellの式の変換は実行時エラーが発生したことを明示的に表示するため<code>error</code>と<code>undefined</code>を使用する。エラーが発生した際の実際のプログラムの振舞は実装次第である。そのメッセージはこれらの変換のみ提案するため<code>error</code>関数へ渡される。エラー発生時、実装側は多少の情報を表示することを選択してもよい。

## 変数、コンストラクタ、演算子、リテラル

|||||
|--|--|--|--|
|<em>aexp</em>|→|<em>qvar</em>|(variable)|
|        |&#124;|<em>gcon</em>|(general constructor)|
|        |&#124;|<em>literal</em>| |
| | | | |
|<em>gcon</em>|→|<code>()</code>| |
|	       |&#124;|<code>[]</code>| |
|	       |&#124;|<code>(,</code>{<code>,</code>}<code>)</code>| |
|	       |&#124;|<em>qcon</em>| |
| | | | |
|    <em>var</em>|→|<em>varid</em> &#124; <code>(</code> <em>varsym</em> <code>)</code>|(variable)|
|   <em>qvar</em>|→|<em>qvarid</em> &#124; <code>(</code> <em>qvarsym</em> <code>)</code>|(qualified variable)|
|    <em>con</em>|→|<em>conid</em> &#124; <code>(</code> <em>consym</em> <code>)</code>|(constructor)|
|   <em>qcon</em>|→|<em>qconid</em> &#124; <code>(</code> <em>gconsym</em> <code>)</code>|(qualified constructor)|
|  <em>varop</em>|→|<em>varsym</em> &#124; <code>\`</code>  <em>varid</em> <code>\`</code>|(variable operator)|
| <em>qvarop</em>|→|<em>qvarsym</em> &#124; <code>\`</code>  <em>qvarid</em> <code>\`</code>|(qualified variable operator)|
|  <em>conop</em>|→|<em>consym</em> &#124; <code>\`</code>  <em>conid</em> <code>\`</code>|(constructor operator)|
| <em>qconop</em>|→|<em>gconsym</em> &#124; <code>\`</code>  <em>qconid</em> <code>\`</code>|(qualified constructor operator)|
|     <em>op</em>|→|<em>varop</em> &#124; <em>conop</em>|(operator)|
|    <em>qop</em>|→|<em>qvarop</em> &#124; <em>qconop</em>|(qualified operator)|
|<em>gconsym</em>|→|<code>:</code> &#124; <em>qconsym</em>

Haskellは中置記法に対応するため特別な構文を提供している。 **演算子**  は中置構文を用いて適用が可能である(セクション[3.4]("#3.4"))か、 **セクション**  (セクション[3.5]("#3.5"))を用いて部分的に適用が可能な関数のことである。

 **演算子**  は、`+`や`$$`といった  **演算子シンボル**  か、<code>\` op \`</code>のようにグレイブ・アクセント(バッククォート)で囲まれた通常の識別子かのいずれかである。例えば、<code>op x y</code>という前置適用を書く代わりに、<code>x \`op\` y</code>という中置適用を書くことができる。もし、<code>\` op \`</code>に対して結合性が宣言されていない場合には、優先順位は最高で左結合をデフォルトとする。(セクション[4.4.2](./4-declarations-andbindings.md)参照)。

対照的に、演算子シンボルは括弧で閉じられた普通の識別子へ変換可能である。例として、<code>(+) x y</code>は<code>x + y</code>に等しく、<code>foldr (⋆) 1 xs</code>は<code>foldr (\x y -> x⋆y) 1 xs</code>に等しくなる。

一部の組み込み型のコンストラクタの名前をつけるのに特別な構文がつかわれているものがあり、実際に<em>gcon</em>や<em>literal</em>で見ることができる。これらについてはセクション[6.1](./6-predefined-types-and-classes.md)で説明される。

整数リテラルは<code>fromInteger</code>関数を<code>Integer</code>型の適した値への適用を表す。同様に、浮動小数点リテラルは<code>Rational</code>型(つまり、<code>Ratio Integer</code>)の値に<code>fromRational</code>を適用することを表す。

<div class="column">

**変換:** 整数リテラル<em>i</em>は<code>fromInteger *i*</code>に等しく、<code>fromInteger</code>は<code>Num</code>クラスのメソッドである。(セクション[6.4.1](./6-predefined-types-and-classes,md))

浮動小数点リテラル<em>f</em>は<code>fromRational (<em>n</em> Ratio.% <em>d</em>)</code>に等しく、<code>fromRational</code>は<code>Fractional</code>クラスのメソッドで、<code>Ratio.%</code>は<code>Ratio</code>ライブラリで定義されており、2つの整数から有理数を構築する。整数<em>n</em>と<em>d</em>は<em>n/d = f</em>を満たすものとして選ばれる。

</div>

## カリー化された適用とラムダ抽象

|||||
|--|--|--|--|
|<em>fexp</em>|→|[<em>fexp</em>] <em>aexp</em>|(function application)|
|<em>lexp</em>|→|<code>\\</code> <em>apat<sub>1</sub></em> … <em>apat<sub>n</sub></em> <code>-></code> <em>exp</em>|(lambda abstraction, <em>n</em> ≥ 1)|

関数適用は<em>e<sub>1</sub></em> <em>e<sub>2</sub></em>と書く。適用は左結合性をもつので、<code>(f x) y</code>の括弧は省略することができる。<em>e<sub>1</sub></em>はデータ構成子である可能性もあるため、データ構成子の部分的な適用は許されている。

 **ラムダ抽象** は<code>\ <em>p<sub>1</sub></em> … <em>p<sub>n</sub></em> -> e</code>と書き、<code><em>p<sub>i</sub></em></code>はパターンである。<code>\x:xs->x</code>のような式は構文的に正しくない。<code>\\(x:xs)->x</code>と書くのが正しい。

パターンの集合は **線形** でなければならない。つまり、変数は集合の中で2回以上出現してはいけない。

<div class="column">

**変換:** 以下の等式が成り立つ。

<pre><code>\<em>p<sub>1</sub></em> … <em>p<sub>n</sub></em> -> e = \ X<sub>1</sub> … X<sub>n</sub> -> case (X<sub>1</sub>, …, X<sub>n</sub>) of (<em>p<sub>1</sub></em>, …, <em>p<sub>n</sub></em>) -> e</code></pre>

<em>X<sub>i</sub></em>は新しい識別子である。

</div>

この変換と、セクション[3.17.3]("#3.17.3")で説明するcase式とパターンマッチの意味論に基づけば、もしもパターンマッチに失敗すれば結果は`⊥`となる。

## 演算子適用

|||||
|--|--|--|--|
|<em>infixexp</em>|→|<em>lexp</em> <em>qop</em> <em>infixexp</em>| |
|            |&#124;|<code>-</code> <em>infixexp</em>|(prefix negation)|
|            |&#124;|<em>lexp</em>| |
|<em>qop</em>|→|<em>qvarop</em> &#124; <em>qconop</em>|(qualified operator)|

<em>e<sub>1</sub> qop e<sub>2</sub></em>という形式は二項演算子<em>qop</em>の式<em>e<sub>1</sub></em>と<em>e<sub>2</sub></em>への中置適用である。

特殊な形式<em>-e</em>は前置の符号反転演算子を表す。この演算子はHaskellにおける唯一の前置演算子であり、<code>negate (*e*)</code>という意味の構文である。二項演算子<code>-</code>はPrelude内の<code>-</code>の定義への参照を必要とせず、モジュールシステムによって再束縛されるかもしれない。しかしながら、単項演算子<code>-</code>はPrelude内で定義された<code>negate</code>関数を常に参照する。<code>-</code>演算子の局所的な意味と単項の符号反転演算との間には何の関連もない。

前置の符号反転演算子はPrelude内(表[4.1](./4-declarations-and-bindings.md)を参照)で定義された中置演算子<code>-</code>と同じ優先順位を持つ。<code>e1-e2</code>は二項演算子<code>-</code>の中置表現解析されるため、前置の符号反転演算子を使うには構文解析に代わって<code>e1(-e2)</code>と書かなければいけない。同様に、<code>(-)</code>は他の中置演算子と同様に<code>(\ x y -> x-y)</code>のための構文であるが、<code>(\ x -> -x)</code>を表せず、そのためには<code>negate</code>を使う必要がある。

<div class="column">

**変換：** 以下の等式が成り立つ。
<pre>
<code><em>e<sub>1</sub></em> op <em>e<sub>2</sub></em>  =	(op) <em>e<sub>1</sub></em> <em>e<sub>2</sub></em>
-e        =	<em>negate</em> (e)
</code></pre>

</div>

## セクション

|||||
|--|--|--|--|
|<em>aexp</em>|→| <code>(</code> <em>infixexp</em> <em>qop</em> <code>)</code>                           |(left section)|
|        |&#124;| <code>(</code> <em>qop<sub>⟨<code>-</code>⟩</sub></em> <em>infixexp</em> <code>)</code>|(right section)|


**セクション** は( <em>op</em> <em>e</em> )や( <em>e</em> <em>op</em> )のように書かれる。このときの<em>op</em>は二項演算子で<em>e</em>は式である。セクションは二項演算子を部分的に適用する便利な構文である。

構文上の優先順位のルールは、セクションには次のように適用される。<em>(op e)</em>は<em>(x op e)</em>が<em>(x op (e))</em>と同じように解釈される場合に限り正当であり、<em>(e op)</em>も同様である。例えば、<code>(⋆a+b)</code>は構文的に不当であるが、<code>(+a⋆b)</code>と<code>(⋆(a+b))</code>は有効である。なぜなら<code>(+)</code>は左結合であり、<code>(a+b+)</code>は構文的に正しいが、<code>(+a+b)</code>はそうではない。後者は<code>(+(a+b))</code>のように書かれるのが正当である。他の例として、次の式

``` hs
(let n = 10 in n +)
```

は不当である。これはセクション[3](./3-expressions.md)にある、let/ラムダに関するメタ規則による。次の式は

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

なぜなら、<code>-</code>は文法内で特別に扱われるからだ。前のセクションで説明したように、(- <em>exp</em>)はセクションではなく、前置の符号反転演算子の適用である。しかしながら、Prelude名で定義された<code>subtract</code>関数があり、それによって<code>(subtract <em>exp</em>)</code>が不正なセクション(**訳注**: (- <em>exp</em>)のこと)と同じ意味となる。式(+ (- <em>exp</em>))も同じ目的に適う。

<div class="column">

**変換:** 以下の等式が成り立つ。

```hs
(op e)  =       \ x -> x op e
(e op)  =       \ X -> e op x
```

<em>op</em>は二項演算子で、<em>e</em>は式であり、<em>x</em>は<em>e</em>の中で自由出現ではない変数である。

</div>

## 条件文

|||||
|--|--|--|--|
|<em>lexp</em>|→|<code>if</code> <em>exp</em> [<code>;</code>] <code>then</code> <em>exp</em> [<code>;</code>] <code>else</code> <em>exp</em>| |

条件式は<code>if <em>e<sub>1</sub></em> then <em>e<sub>2</sub></em> else <em>e<sub>3</sub></em></code>の形式をとり、もし<em>e<sub>1</sub></em>が`True`なら、<em>e<sub>2</sub></em>を返し、<em>e<sub>1</sub></em>が`False`なら<em>e<sub>3</sub></em>を返し、それ以外なら`⊥`を返す。

<div class="column">

**変換:** 以下の等式が成り立つ。

<pre><code>if <em>e<sub>1</sub></em> then <em>e<sub>2</sub></em> else <em>e<sub>3</sub></em>(op e) = case <em>e<sub>1</sub></em> of { True -> <em>e<sub>2</sub></em> ; False -> <em>e<sub>3</sub></em> }
</code></pre>

<code>True</code>と<code>False</code>はPrelude内で定義されている<code>Bool</code>型の2つの引数のないコンストラクタである。<em>e<sub>1</sub></em>は<code>Bool</code>型でなければならず、<em>e<sub>2</sub></em>と<em>e<sub>3</sub></em>も同じ型でなければならない。条件式全体の型も同様である。
</div>

## リスト

|||||
|--|--|--|--|
|<em>infixexp</em>|→|<em>exp<sub>1</sub></em> <em>qop</em> <em>exp<sub>2</sub></em>| |
|    <em>aexp</em>|→|<code>[</code> <em>exp<sub>1</sub></em> <code>,</code> … <code>,</code> <em>exp<sub>k</sub></em> <code>]</code>|(<em>k</em> ≥ 1)|
|            |&#124;|<em>gcon</em>| |
|    <em>gcon</em>|→|<code>[]</code>| |
|            |&#124;|<em>qcon</em>| |
|    <em>qcon</em>|→|<code>(</code> <em>gconsym</em> <code>)</code>| |
|     <em>qop</em>|→|<em>qconop</em>| |
|  <em>qconop</em>|→|<em>gconsym</em>| |
| <em>gconsym</em>|→|<code>:</code>| |

<em>List</em> は<em>k</em> ≥ 1として、[<em>e<sub>1</sub></em>, …, <em>e<sub>k</sub></em>]のように書く。リストコンストラクタは <code>:</code>であり、空リストは<code>[]</code>で表記される。リストの標準操作はPrelude内で与えられる(セクション[6.1.3](./6-predefined-types-and-classes.md)と[9章](./9-standard-prelude.md)の特にセクション[9.1](./9-standard-prelude.md)を参照)。

<div class="column">

**変換:** 以下の等式が成り立つ。

<pre>
<code>[<em>e<sub>1</sub></em>, …, <em>e<sub>k</sub></em>] = <em>e<sub>1</sub></em> : (<em>e<sub>2</sub></em> : ( … (<em>e<sub>k</sub></em> : [])))</code></pre>

<code>:</code>と<code>[]</code>はPredule内(セクション[6.1.3](./6-predefined-types-and-classes.md))で定義されたリストのコンストラクタである。<em>e<sub>1</sub></em>から<em>e<sub>k</sub></em>までの型は同じでなければならない(それを<em>t</em>と呼ぶ)。式全体の型は<em>[t]</em>になる(セクション[4.1.2](./4-declarations-and-bindings.md))。
</div>

コンストラクタ<code>":"</code>は`[]`のようにリストコンストラクタとしてのみ予約されており、言語構文の一部と見做されている。また、それは隠すことも再定義することもできない。<code>:</code>は優先順位レベル5の右結合演算子である(セクション[4.4.2](./4-declarations-and-bindings.md))。

## タプル

|||||
|--|--|--|--|
|<em>aexp</em>|→|<code>(</code> <em>exp<sub>1</sub></em> <code>,</code> … <code>,</code> <em>exp<sub>k</sub></em> <code>)</code>|(<em>k</em> ≥ 2)|
|        |&#124;|<em>qcon</em>| |
|<em>qcon</em>|→|<code>(,</code>{<code>,</code>}<code>)</code>| |

 **タプル**  は<em>k</em> ≥ 2以上の(<em>e<sub>1</sub></em>, …, <em>e<sub>k</sub></em>)のように書く。<em>n-tuple</em>のコンストラクタは`(,…,)`と表記され、<em>n</em> - 1のコンマがある。従って、<code>(a,b,c)</code>と<code>(,,) a b c</code>は同じ値を表す。タプルの標準操作はPrelude内で定義されている(セクション[6.1.4](./6-predefined-types-and-classes.md)と[9章](./9-standard-prelude.md))。

<div class="column">

**変換:** <em>k</em> ≥ 2のときの(<em>e<sub>1</sub></em>, …, <em>e<sub>k</sub></em>)はPrelude内で定義された<em>k</em>-tupleのインスタンスになり、変換は要求されない。もし、<em>t<sub>1</sub></em>から<em>t<sub>k</sub></em>はそれぞれ<em>e<sub>1</sub></em>から<em>e<sub>k</sub></em>の型があり、最終的なタプルの型は(<em>t<sub>1</sub></em>,…,<em>t<sub>k</sub></em>)になる(セクション[4.1.2](./4-declarations-and-bindings.md))。

</div>

## 単位式と括弧付き式

|||||
|--|--|--|--|
|<em>aexp</em>|→|<em>gcon</em>| |
|        |&#124;|`(` <em>exp</em> `)`| |
|<em>gcon</em>|→|`()`| |

<em>(e)</em>の形式はシンプルに **括弧付き式** であり、<em>e</em>と等しい。<em>ユニット(unit)</em>式`()`は`()`型を持つ(セクション[4.1.2](./4-declarations-and-bindings.md)を参照)。それは`⊥`以外の型のメンバのみで、"引数のないタプル"のように考えられる(セクション[6.1.5](./6-predefined-types-and-classes.md)を参照)。

<div class="column">

**変換:** <em>(e)</em>は<em>e</em>と等しい。
</div>

## 数列

|||||
|--|--|--|--|
|<em>aexp</em>|→|`[` <em>exp<sub>1</sub></em> `[`, <em>exp<sub>2</sub></em>`]` … `[`<em>exp<sub>3</sub></em>`]` `]`| |

 **数列** [<em>e<sub>1</sub></em>,<em>e<sub>2</sub></em> .. <em>e<sub>3</sub></em>]は型<em>t</em>の値のリストを表し、各<em>e<sub>i</sub></em>は型<em>t</em>を持ち、<em>t</em>は`Enum`クラスのインスタンスである。

<div class="column">

**変換:** 数列はこれらの等式を満たす。

||||
|--|--|--|
|[ <em>e<sub>1</sub></em>.. ]                                              |=| <code>enumFrom</code> <em>e<sub>1</sub></em>|
|[ <em>e<sub>1</sub></em>,<em>e<sub>2</sub></em>.. ]                       |=| <code>enumFromThen</code> <em>e<sub>1</sub></em> <em>e<sub>2</sub></em>|
|[ <em>e<sub>1</sub></em>..<em>e<sub>3</sub></em> ]                        |=|	<code>enumFromTo</code> <em>e<sub>1</sub></em> <em>e<sub>3</sub></em>|
|[ <em>e<sub>1</sub></em>,<em>e<sub>2</sub></em>..<em>e<sub>3</sub></em> ] |=| <code>enumFromThenTo</code> <em>e<sub>1</sub></em> <em>e<sub>2</sub></em> <em>e<sub>3</sub></em>|

<code>enumForm</code>、<code>enumFormThen</code>、<code>enumFormTo</code>、<code>enumFormThenTo</code>はPrelude内で定義されている<code>Enum</code>クラスのクラスメソッドになる。
</div>

故に数列の意味論は型<em>t</em>のインスタンス宣言に完全に依存している。どの<code>Prelude</code>型が<code>Enum</code>型にあるか、そしてそれらの意味論についてのより詳しいことについてはセクション[6.3.4](./6-predefined-types-and-classes.md)を参照すること。

## リスト内包表記

|||||
|--|--|--|--|
|<em>aexp</em>|→|<code>[</code> <em>exp</em> <code>&#124;</code> <em>qual<sub>1</sub></em> <code>,</code> … <code>,</code> <em>qual<sub>n</sub></em> <code>]</code>|(list comprehension, <em>n</em> ≥ 1)|
|<em>qual</em>|→|<em>pat</em> <code><-</code> <em>exp</em>|(generator)|
|        |&#124;|<code>let</code> <em>decls</em>|(local declaration)|
|        |&#124;|<em>exp</em>|(boolean guard)|

**リスト内包表記** は[ <em>e</em> | q<sub>1</sub>, …, q<sub>n</sub> ]、<em>n</em> ≥ 1形式を持ち、<em>q<sub>i</sub></em>修飾子は次のいずれかである。

- 形式<em>p <- e</em>の **ジェネレータ**  。<em>p</em>は型<em>t</em>のパターン(セクション[3.17](#3.17))であり、<em>e</em>は型<code>[t]</code>の式である。
- 生成された式eで、あるいは後方のブーリアンガードとジェネレータで使われる新しい定義を提供する **ローカル束縛** 。
-  **ブーリアンガード**  。`Bool`型の任意の式を表すことができる。

このようなリスト内包表記は修飾子リスト内のジェネレータのネストされた深さ優先探索の評価によって作成された連続した環境で<em>e</em>を評価することによって生成された要素のリストを返す。変数の束縛は通常のパターンマッチングルール(セクション[3.17]("#3.17"))に従って発生し、もし一致に失敗したら、その時はそのリストの要素は単純にスキップされる。従って、

```hs
[ x |  xs   <- [ [(1,2),(3,4)], [(5,4),(3,2)] ],  
      (3,x) <- xs ]
```

リスト`[4,2]`を返す。もし修飾子がブーリアンガードなら、成功した前のパターンマッチのために`True`と評価しなけれないけない。通常通り、リスト内法表記における束縛は外部スコープの束縛をシャドーイングできる。例えば以下のようになる。

```hs
[ x | x <- x, x <- x ] = [ z | y <- x, z <- y]
```

<div class="column">

**変換:** リスト内包表記はこれらの等式を満たし、これらの等式はカーネルへの変換として使われる可能性がある。

||||
|--|--|--|
|[  <em>e</em> &#124; <code>True</code> ]	                   |=|[<em>e</em>]|
|[  <em>e</em> &#124; <em>q</em> ]	                         |=|[  <em>e</em> &#124; <em>q</em>, <code>True</code> ]|
|[  <em>e</em> &#124; <em>b</em>,  <em>Q</em>  ]             |=|<code>if</code> <em>b</em> <code>then</code> [  <em>e</em> &#124; <em>Q</em> ] <code>else</code> []|
|[  <em>e</em> &#124; <em>p</em> <- <em>l</em>,  <em>Q</em> ]|=|<code>let</code> <code>ok</code> <em>p</em> = [  <em>e</em> &#124; <em>Q</em> ]|
|                                                            | |&ensp;&ensp;&ensp;&ensp;&ensp;<code>ok</code> _ = []|
|                                                            | |<code>in</code> <code>concatMap</code> <code>ok</code>  <em>l</em>|
|[  <em>e</em> &#124; <code>let</code> <em>decls</em>,  Q ]  |=|<code>let</code> <em>decls</em> <code>in</code> [  <em>e</em> &#124; <em>Q</em> ]|

<em>e</em>は式にわたる範囲で、<em>p</em>はパターンにわたり、<em>l</em>はリスト値式にわたり、<em>b</em>はブーリアン式にわたり、<em>decls</em> は宣言リストにわたり、<em>q</em>は修飾子にわたり、<em>Q</em>は修飾子の列にわたる範囲をもつ。<code>ok</code>は新しい変数である。関数<code>concatMap</code>とブーリアン値<code>True</code>はPrelude内で定義されている。

</div>

リスト内法表記の変換で示した通り、letによって束縛された変数は最大限多相的な型を持つ一方で<-によって束縛されたものはラムダ束縛であり、よって単相的になる。 (セクション[4.5.4](./4-declarations-and-bindings.md)を参照).

## Let式

|||||
|--|--|--|--|
|<em>lexp</em>|→|<code>let</code> <em>decls</em> <code>in</code> <em>exp</em>| |

**let** 式は一般的な形式<code><code>let</code> { <em>d<sub>1</sub></em> ; … ; <em>d<sub>n</sub></em> } <code>in</code> <em>e</em></code>を持ち、ネストされたレキシカルスコープをもつ相互再帰的な宣言のリストを導入する(<code>let</code>は他の言語で<code>letrc</code>としばしば呼ばれる)。宣言の範囲は式<em>e</em>と宣言の右側である。宣言は[4章](./4-declarations-and-bindings.md)で説明される。パターン束縛のマッチは遅延され、暗黙的な`~`がこれらのパターンを反駁不可にする。 例えば、

```hs
let (x,y) = undefined in e
```

は<code>x</code>または<code>y</code>が評価されるまでランタイムエラーをもたらさない。

<div class="column">

**変換:** 式<code><code>let</code> { <em>d<sub>1</sub></em> ; … ; <em>d<sub>n</sub></em>} in <em>e<sub>0</sub></em></code>の動的な意味論は次の変換によって捕捉される。全ての型シグネチャを取り除いた後、それぞれの宣言<em>d<sub>i</sub></em>は<code><em>p<sub>i</sub></em> = <em>e<sub>i</sub></em></code>の形の等式へと変換される。<code>p<sub>i</sub></code>と<code><em>e<sub>i</sub></em></code>はセクション[4.4.3](./4-declarations-and-bindings.md)での変換を使用する、各々のパターンと式である。一度この変換が終われば、次のような等式が成り立つ。この等式はカーネルへの変換として使われる場合がある。

||||
|--|--|--|
|<code>let</code> {<em>p<sub>1</sub></em> = <em>e<sub>1</sub></em>;  ... ; <em>p<sub>n</sub></em> = <em>e<sub>n</sub></em>} <code>in</code> <em>e<sub>0</sub></em> |=| <code>let</code> (<code>~</code><em>p<sub>1</sub></em>, ... ,<code>~</code><em>p<sub>n</sub></em>) = (<em>e<sub>1</sub></em>, ... ,<em>e<sub>n</sub></em>) in <em>e<sub>0</sub></em>|
|<code>let</code> <em>p</em> = <em>e<sub>1</sub></em>  <code>in</code>  e<sub>0</sub>                                                                              |=| <code>case</code> <em>e<sub>1</sub></em> of <code>~</code><em>p</em> -> <em>e<sub>0</sub></em>|
|                                                                                                                                                          | |where no variable in <em>p</em> appears free in <em>e<sub>1</sub></em>|
|<code>let</code> <em>p</em> = <em>e<sub>1</sub></em>  <code>in</code>  <em>e<sub>0</sub></em>                                                                     |=|<code>let</code> <em>p</em> = <code>fix</code> ( \ <code>~</code><em>p</em> -> <em>e<sub>1</sub></em>) in <em>e<sub>0</sub></em>|

<code>fix</code>は最小不動点演算子である。反駁不可パターン<em>~p</em>の使用は注意すべきだ。この変換は静的な意味論を保存しない。なぜなら、<code>case</code>を使用すると束縛変数が完全な多相型へ型付けされなくなるからである。<code>let</code>式で束縛された静的な意味論はセクション[4.4.3](./4-declarations-and-bindings.md)で説明される。
</div>

## Case式

|||||
|--|--|--|--|
|<em>lexp</em>|→|<code>case</code> <em>exp</em> <code>of</code> <code>{</code> <em>alts</em> <code>}</code>| |
|<em>alts</em>|→|<em>alt<sub>1</sub></em> <code>;</code> … <code>;</code> <em>alt<sub>n</sub></em>|(<em>n</em> ≥ 1)|
| <em>alt</em>|→|<em>pat</em> <code>-></code> <em>exp</em> [<code>where</code> <em>decls</em>]| |
|        |&#124;|	<em>pat</em> <em>gdpat</em> [<code>where</code> <em>decls</em>]
|        |&#124;|                |(empty alternative)|
|||||
| <em>gdpat</em>|→|<em>guards</em> <code>-></code> <em>exp</em> [ <em>gdpat</em> ]| |
|<em>guards</em>|→| <code>&#124;</code> <em>guard<sub>1</sub></em>, …, <em>guard<sub>n</sub></em>|(<em>n</em> ≥ 1)|
| <em>guard</em>|→|<em>pat</em> <code><-</code> <em>infixexp</em>|(pattern guard)|
|          |&#124;|	<code>let</code> <em>decls</em>	           |(local declaration)|
|          |&#124;|	<em>infixexp</em>	            |(boolean guard)|

<em>case</em> 式は一般的な形式<code>case <em>e</em> of { <em>p<sub>1</sub></em> <em>match<sub>1</sub></em> ; … ; <em>p<sub>n</sub></em> <em>match<sub>n</sub></em> }</code>を持つ。各<em>match<sub>i</sub></em>は一般的な形式

<pre>
<code>| <em>gs<sub>i1</sub></em>    -> <em>e<sub>i1</sub></em>
…
| <em>gs<sub>imi</sub></em>   -> <em>e<sub>imi</sub></em>
where <em>decls<sub>i</sub></em>
</code></pre>

( **ガード**  の構文ルールについて注目して欲しい。`|`は区切りを表す構文的なメタシンボルではなく終端記号である。)各選択子<em>p<sub>i</sub> match<sub>i</sub></em>はパターン<em>p<sub>i</sub></em>から成り、<em>match<sub>i</sub></em>と一致する。各マッチは順繰りにガード<em>gs<sub>ij</sub></em>と本体<em>e<sub>ij</sub></em>のペアの列から成り、代替となる全てのガードと式上の範囲での付加的な束縛(<em>decls<sub>i</sub></em>)に従う。

 **ガード**  は次の形式をの一つを持つ。

-  **パターンガード**  は形式<em>p</em> <- <em>e</em>で、<em>p</em>は型<em>t</em>のパターンで、<em>e</em>は式の種類<em>t</em>である。もし、式<em>e</em>がパターン<em>p</em>に一致するなら成功し、パターンの束縛をその環境にもたらす。
-  **局地的束縛**  は形式<code>let <em>decls</em></code>である。それらは常に成功し、その環境に<em>decls</em>と定義した名前をもたらす。
-  **ブーリアンガード**  は<code>Bool</code>型の数式である。もし、式が<code>True</code>と評価するなら成功し、その環境に新しい名前をもたらさない。ブーリアンガード<em>g</em>はパターンガード<code>True <- <em>g</em></code>に意味的に等しい。

形式<code><em>pat</em> -> <em>exp</em> where <em>decls</em></code>の代わりの以下の簡略記法が扱われる。

```hs
pat | True -> exp
where decls
```

ケース式は少なくとも1つの選択句を持たなければならず、各選択句は一つの実体を持たないといけない。各実体は同じ型を持たなければならず、式全体の型はその型になる。

ケース式は式<em>e</em>が個々の選択句に反するパターンマッチングによって評価される。その選択子は上から下へ連続的に試される。もし、<em>e</em>が選択句のパターンと一致したら、そのとき選択句のガード式は始めにパターンの一致の間に生成された束縛によって展開されたケース式の環境内で上から下へ連続的に試される。その時、<code>where</code>句内の<em>decls<sub>i</sub></em>によって、その選択句は関連付けられる。

各ガード式のためにコンマ区切りのガードは左から右へ連続的に試される。もしそのすべてに成功したなら、そのときは対応する式はガードによって生成された束縛で展開された環境で評価される。すなわち、(let句かパターンガードのいずれかを使った)ガードによって生成された束縛は続くガードと対応する式のスコープ内にある。もしあらゆるガードが失敗したら、その時はこのガード式は失敗し次のガード式を試す。

もし与えられた選択句のどのガード式も成功しなかったら、その時マッチングは次の選択句へ継続する。もしどの選択句も成功しなければ、そのときの結果は`⊥`となる。パターンマッチングはセクション[3.17]("#パターンマッチング")で説明され、ケース式の正式な意味論はセクション[3.17.3]("#パターンマッチングの正式な意味論")で説明される。

 **パースについての注意点**  。以下の式は

```hs
case x of { (a,_) | let b = not a in b :: Bool -> a }
```

これを正しく構文解析するには用心しなければならない。ただ一つの曖昧さのない構文解析は、すなわち次のようにすることである。

```hs
case x of { (a,_) | (let b = not a in b :: Bool) -> a }
```

しかしながら、<code>Bool -> a</code>というフレーズは型として構文的に正当であり、先読みが制限されているパーサーはこの選択に誤ってコミットする可能性があり、それゆえプログラムは拒否する。故に、プログラマーは型シグネチャで終わるガードを避けるように勧められる。これは実際に **ガード** が<em>exp</em>ではなく<em>infixexp</em>を含んでいる理由になる。

## Do式

|||||
|--|--|--|--|
| <em>lexp</em>|→|<code>do</code> <code>{</code> <em>stmts</em> <code>}</code>|(do expression)|
|<em>stmts</em>|→|<em>stmt<sub>1</sub></em> … <em>stmt<sub>n</sub></em> <em>exp</em> [<code>;</code>]|(<em>n</em> ≥ 0)|
| <em>stmt</em>|→|<em>exp</em> <code>;</code>| |
|	        |&#124;|<em>pat</em> <code><-</code> <em>exp</em> <code>;</code>| |
|         |&#124;|<code>let</code> <em>decls</em> <code>;</code>| |
|         |&#124;|<code>;</code> 	                   |(empty statement)|

<em>do式</em>はモナドのプログラミングのためのより従来的な構文を提供する。それは以下のような式を許す。

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

**変換：** Do式はこれらの等式を満たし、排除した空の<em>stmts</em>の後にカーネルの中への変換のように使われるかもしれない。

||||
|--|--|--|
|<code>do</code> {<em>e</em>}                                     |=|<em>e</em>|
|<code>do</code> {<em>e</em>;<em>stmts</em>}                      |=|<em>e</em> >> <code>do</code> {<em>stmts</em>}|
|<code>do</code> {<em>p</em> <- e; <em>stmts</em>}                |=|<code>let</code> <code>ok</code> <em>p</em> = <code>do</code> {<em>stmts</em>}|
|                                                                 | |&ensp;&emsp;<code>ok</code> _ = <code>fail</code> "..."|
|                                                                 | |&ensp;<code>in</code> <em>e<em> >>= <code>ok</code>|
|<code>do</code> {<code>let</code> <em>decls</em>; <em>stmts</em>}|=|<code>let</code> <em>decls</em> <code>in</code> <code>do</code> {<em>stmts</em>}|

コンパイラが生成したエラーメッセージを表す省略記号`"..."`の部分は<code>fail</code>へ渡され、そして可能であればパターンマッチに失敗した場所を表示する。関数<code>>></code>,<code>>>=</code>と<code>fail</code>はPreludeで定義されたクラス<code>Monad</code>の操作であり、<code>ok</code>は新しい識別子である。

</div>

<code>do</code>の変換でも示したように、<code>let</code>に束縛された変数は完全に多相的な型をもつ一方で<code><-</code>によって定義された変数はラムダ束縛であり、ゆえに単相的である。

## フィールドラベル付きのデータ型

データ型の宣言はフィールドラベルを必要に応じて定義してもよい。(セクション[4.2.1](./4-declarations-and-bindings.md)を参照)これらのフィールドラベルは構築、形式の選択、データ型全体の構造に依存した方法でのフィールドの更新することに使用される。

異なるデータ型は同じスコープの共通のフィールドラベルを共有することはできない。フィールドラベルはコンストラクタ内で高々一度だけ、使用することができる。しかしながら、データ型の中で、あるフィールドがすべてのコンストラクタ内で同じ型を持つときに限り1つのフィールドを複数のコンストラクタで使用することができる。最後の点については次が良い例である:

```hs
data S = S1 { x :: Int } | S2 { x :: Int }   -- OK  
data T = T1 { y :: Int } | T2 { y :: Bool }  -- BAD
```

ここでの<code>s</code>は正当であるが<code>T</code>はそうではない。また<code>y</code>は後者では矛盾する型付けが与えられている。

### フィールドセレクション

||||
|--|--|--|
|aexp|→|qvar|

フィールドラベルはセレクタ関数のように使用される。変数のように使われる際は、フィールドラベルはオブジェクトからフィールドを抽出する関数のように振る舞う。セレクタはトップレベルの束縛であり、よってローカル変数によってシャドーイングされる場合があるが、しかし他のトップレベルの束縛で同じ名前のものと衝突してはならない。この覆いはセレクタ関数にのみ影響を及ぼし、レコード作成(セクション[3.15.2]("#3.15.2"))及びに更新(セクション[3.15.3]("#3.15.3"))、フィールドラベルは通常の変数と混合されることはない。

<div class="column">

**変換:** フィールドラベル<em>f</em>は次のようなセレクタ関数を生成する。

||||
|--|--|--|
|f x|=|<code>case</code> x <code>of</code> { <em>C<sub>1</sub></em> <em>p<sub>11</sub></em> … <em>p<sub>1k</sub></em>  ->  <em>e<sub>1</sub></em> ;… ; <em>C<sub>n</sub> p<sub>n1</sub></em> … <em>p<sub>nk</sub></em>  ->  <em>e<sub>n</sub></em> }|

<em>C<sub>1</sub> ... C<sub>n</sub></em>は全て<em>f</em>とラベルされたフィールドを含むデータ型のコンストラクタで、<em>p<sub>ij</sub></em>は<em>f</em>が<em>C<sub>i</sub></em>の要素の<em>j</em>番目、または<code>\_</code>をラベルした時の<code>y</code>であり、<em>e<sub>i</sub></em>は<em>C<sub>i</sub></em>のフィールドが<em>f</em>または<code>undefined</code>のラベルを持つ時の<code>y</code>である。

</div>

### フィールドラベルを用いた生成

|||||
|--|--|--|--|
| <em>aexp</em>|→|<em>qcon</em> <code>{</code> <em>fbind<sub>1</sub></em> <code>,</code> … <code>,</code> <em>fbind<sub>n</sub></em> <code>}</code>|(labeled construction, <em>n</em> ≥ 0)|
|<em>fbind</em>|→|<em>qvar</em> <code>=</code> <em>exp</em>| |

ラベル付けされたフィールドを使うコンストラクタが値の生成に使われる場合があるが、その時には各コンポーネントは位置ではなく名前によって指定する。宣言リストの中で使われる中括弧とは異なりレイアウトの対象にならない。<code>{</code>と<code>}</code>の文字は明示しなければならない。(これはフィールドの更新、フィールドパターンにおいても正しい。)フィールドラベルを使用する構築は次の制約に応じる。

- 指定されたコンストラクタで宣言されたフィールドラベルのみ言及してよい。
- フィールドラベルは複数回言及してはならない。
- 言及されないフィールドは`⊥`で初期化される。
- 正格なフィールド(宣言された型のフィールドの接頭語に`!`が付けられている)が生成の際に省略された時はコンパイルエラーが発生する。厳格なフィールドはセクション[4.2.1](./4-declarations-and-bindings.md)で説明される。

式<code>F {}</code>は、<code>F</code>はデータコンストラクタであり、<em><code>F</code>がレコード構文により宣言されたかどうかに関わらず</em>、正当である(ただし<code>F</code>が正格フィールドを持たない時に限る。上の4番目の箇条書きを参照)。それは<code>F ⊥<sub>1</sub> … ⊥<sub>n</sub></code>を表し、<em>n</em>は<code>F</code>の引数の数である。

<div class="column">

**変換：** <em>f = v</em>の束縛で、フィールド<em>f</em>は<em>v</em>でラベルする。

||||
|--|--|--|
|<em>C</em> { <em>bs</em> }|=|<em>C</em> (<em>pick<sub>1</sub><sup>C</sup></em> <em>bs</em> <em><sub>undefined</sub></em>) … (<em>pick<sub>k</sub><sup>C</sup></em> <em>bs</em> <em>undefined</em>)|

<em>k</em>は<em>C</em>の引数の数である。

補助関数<em>pick<sub>i</sub><sup>C</sup> bs d</em>は次にように定義される。
<p>
もし、コンストラクタ<em>C</em>の<em>i</em>番目の要素がフィールドラベル<em>f</em>を持ち、<em>if f=v</em>は束縛された<em>bs</em>に表示されるなら、その時は<em>pick<sub>i</sub><sup>C</sup> bs d</em>は<em>v</em>である。言い換えると<em>pick<sub>i</sub><sup>C</sup> bs d</em>はデフォルト値<em>d</em>である。
</p>
</div>

### フィールドラベルを使用した更新

|||||
|--|--|--|--|
|<em>aexp</em>|→|<em>aexp<sub>⟨qcon⟩</sub></em> <code>{</code> <em>fbind<sub>1</sub></em> <code>,</code> … <code>,</code> <em>fbind<sub>n</sub></em> <code>}</code>|(labeled update, <em>n</em> ≥ 1)|

フィールドラベルを使ったデータ型に所属する値は非破壊的に更新されるかもしれない。これは元々存在していた値を指定されたフィールドの値で書き換えた新しい値を生成する。更新は次の方法に制限される。

- 全てのラベルは同じデータ型から取られなければいけない。
- 少なくともあるコンストラクタは更新の中で全ての言及されたラベルを定義しなければいけない。
- 2回以上言及されるラベルがあってはならない。
- 実行エラーは更新された値が全ての明記されたラベルを含まない時に発生する。

<div class="column">

**変換:** 以下は以前の<em>pick</em>の定義を使用する。

||||
|--|--|--|
|e { bs }|=|<code>case</code> <em>e</em> <code>of</code>|
|		     | |&emsp;&emsp;&emsp;&emsp;<em>C<sub>1</sub></em> <em>v<sub>1</sub></em> … <em>v<sub>k1</sub></em> -> <em>C<sub>1</sub></em> (<em>pick<sub>1</sub><sup>C1</sup></em> <em>bs</em> <em>v<sub>1</sub></em>) … (<em>pick<sub>k 1</sub><sup>C1</sup></em> <em>bs</em> <em>v <sub>k1</sub></em>)
|		     | |&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;...|
|		     | |&emsp;&emsp;&emsp;&emsp;<em>C<sub>j</sub></em> <em>v<sub>1</sub></em> … <em>v<sub>kj</sub></em> -> <em>C<sub>j</sub></em> (<em>pick<sub>1</sub><sup>Cj</sup></em> <em>bs</em> <em>v<sub>1</sub></em>) … (<em>pick<sub>k j</sub><sup>Cj</sup></em> <em>bs</em> <em>v <sub>k<sub>j</sub></sub></em>)|
|		     | |&emsp;&emsp;&emsp;&emsp;_ -> <code>error</code> "Update error"|

<em>{ C<sub>1</sub>,...,C<sub>j</sub>}</em>は<em>bs</em>内の全てのラベルを含むコンストラクタの集合で、<em>ｋ<sub>i</sub></em>は<em>C<sub>i</sub></em>の引数の数である。

</div>

これはラベル付けされたフィールドを使用している例である。

```hs
data T    = C1 {f1,f2 :: Int}  
          | C2 {f1 :: Int,  
                f3,f4 :: Char}
```

|式|変換|
|--|--|
| C1 {f1 = 3} | C1 3 <code>undefined</code> |
|  C2 {f1 = 1, f4 = 'A', f3 = 'B'} | C2 1 'B' 'A' |
| x {f1 = 1} | <code>case</code> x <code>of</code> C1 _ f2&ensp;&ensp;&ensp;-> C1 1 f2
|            |&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;C2 _ f3 f4 -> C2 1 f3 f4 |

フィールド<code>f1</code>は両方の<code>T</code>のコンストラクタに共通である。この例では、フィールドラベル表記でコンストラクタを使った式をフィールドラベルを使わない同じコンストラクタを使った同値な式へと変換している。もし、<code>x {f2 = 1, f3 = 'x'}</code>のように、どのコンストラクタも、更新で使われたフィールドラベルの集合を定義していないのであれば、コンパイル時エラーという結果になるだろう。

## 式の型シグネチャ

||||
|--|--|--|
|<em>exp</em>|→|<em>exp</em> <code>::</code> [<em>context</em> <code>=></code>] <em>type</em>|

 **式の型シグネチャ** は形式<em>e :: t</em>を持つ。<em>e</em>は式で、<em>t</em>は型(セクション[4.1.2](./4-declarations-and-bindings.md))であり、それらは明示的に式を分類することに使用され、オーバーロード(セクション[4.1.2](”./4-declarations-and-bindings.md”)を参照)するために曖昧な型付けを解決することに使われるかもしれない。式の値は<em>exp</em>の値である。通常の型シグネチャと同様に(セクション[4.4.1](”./4-declarations-and-bindings.md”)を参照)、宣言された型は<em>exp</em>から導出可能な主要な型より具体的になるかもしれないが、主要な型より一般的なまたは同程度な型を与えることはエラーである。

<div class="column">

**変換：**

```hs
e :: t = let { v :: t;  v = e } in v
```

</div>

## パターンマッチング

 **パターン**  はラムダ抽象や関数定義、パターン束縛、リスト内包表記、do式、case式内で現れる。しかしながら、はじめの5つは最終的にcase式に変換されるので、パターンマッチの意味論はcase式のときのみ定めれば十分である。

### パターン

パターンはこの構文を持つ。

|||||
|--|--|--|--|
|<em>pat</em>|→|<em>lpat</em> <em>qconop</em> <em>pat</em>|(infix constructor)|
|	      |&#124;|<em>lpat</em>| |
|||||
|<em>lpat</em>|→|<em>apat</em>| |
|	       |&#124;|<code>-</code> (<em>integer</em> &#124; <em>float</em>)|(negative literal)|
|	       |&#124;|<em>gcon</em> <em>apat<sub>1</sub></em> … <em>apat<sub>k</sub></em>|(arity gcon  =  <em>k</em>, <em>k</em> ≥ 1)|
|||||
|<em>apat</em>|→|<em>var</em> [ <code>@</code> <em>apat</em>]|(as pattern)|
|	       |&#124;|<em>gcon</em>|(arity gcon  =  0)|
|	       |&#124;|<em>qcon</em> <code>{</code> <em>fpat1</em> <code>,</code> … <code>,</code> <em>fpatk</em> <code>}</code>|(labeled pattern, <em>k</em> ≥ 0)|
|	       |&#124;|<em>literal</em>| |
|	       |&#124;|<code>_</code> 	                    |(wildcard)|
|	       |&#124;|<code>(</code> <em>pat</em> <code>)</code>                     |(parenthesized pattern)|
|	       |&#124;|<code>(</code> <em>pat<sub>1</sub></em> <code>,</code> … <code>,</code> <em>pat<sub>k</sub></em> <code>)</code>|(tuple pattern, <em>k</em> ≥ 2)|
|	       |&#124;|<code>[</code> <em>pat<sub>1</sub></em> <code>,</code> … <code>,</code> <em>pat<sub>k</sub></em> <code>]</code>|(list pattern, <em>k</em> ≥ 1)|
|	       |&#124;|<code>~</code> <em>apat</em>|(irrefutable pattern)|
|||||
|<em>fpat</em>|→|<em>qvar</em> <code>=</code> <em>pat</em>| |

コンストラクタの引数の数はそれに関係するサブパターンの数と一致しなければいけない。部分的に適用されるコンストラクタに反して一致することはできない。

全てのパターンは **線形** でなければならない。変数は2回以上現れないかもしれない。例として、この定義は不正である。

```hs
f (x,x) = x     -- ILLEGAL; x used twice in pattern
```

形式<em>var@pat</em>のパターンは<em>as-patterns</em>と呼ばれ、<em>var</em>を<em>pat</em>によってマッチされた値に付ける名前として使うことができる。例えば以下のものは、

```hs
case e of { xs@(x:rest) -> if x==0 then rest else xs }
```

は次のものと等しい。

```hs
let { xs = e } in  
  case xs of { (x:rest) -> if x==0 then rest else xs }
```

形式<code>\_</code>のパターンは **ワイルドカード** であり、パターンのいくつかの部分が右手側で参照されない時に便利である。それは他の場所で使われない識別子がその場所に置かれているかのようである。例えば、以下は、

```hs
case e of { [x,_,_]  ->  if x==0 then True else False }
```

は次のものと等しい。

```hs
case e of { [x,y,z]  ->  if x==0 then True else False }
```

### パターンマッチングの非形式的の意味論

パターンは値に対してマッチが行われる。パターンマッチを行おうとした場合、次の3つのいずれかの結果を得る。 **失敗** かもしれない、 **成功** かもしれず、その時はパターン内の各変数に束縛を返す、 **分岐する** かもしれない(例:`⊥`を返す)。パターンマッチングは次のルールによって外から内へ、左から右へ進行する。

1. 値<em>v</em>に対してマッチするパターン<em>var</em>のマッチングは常に成功し、<em>var</em>を<em>v</em>に束縛する。
1. 値<em>v</em>に対してマッチするパターン<em>~apat</em>のマッチングは常に成功する。もし<em>v</em>に対してマッチする<em>apat</em>のマッチングが別の方法で成功するならば、<em>apat</em>内の束縛されていない変数は適切な値に束縛される。<em>v</em>に対してマッチする<em>apat</em>のマッチングが失敗または分岐するなら<em>⊥ </em>に束縛される(束縛は評価を **ほのめかさない** )。
    <br><br>
    運用上、これはある<em>apat</em>内の変数が使われるまで、パターン<em>~apat</em>が何とも一致しないことを意味する。その時点でパターン全体はその値に対してマッチし、もし一致が失敗または分岐するなら、全体の計算を行う。
1. あらゆる値に対してマッチするワイルドパターン<code>\_</code>のマッチングは常に成功し、束縛は行われない。
1. 値に対してマッチするパターン<em>con pat</em>のマッチングは、<em>con</em>は<code>newtype</code>によって定義されたコンストラクタである、以下の項目でその値に依存する。
     - もし値が形式<em>con v</em>であるなら、その時<em>pat</em>は<em>v</em>に対してマッチされる。
     - もし値が`⊥`なら、その時<em>pat</em>は`⊥`に対してマッチする。

    すなわち<code>newtype</code>と関連するコンストラクタが値の型を変更することのみに務める。
1. 値に対しての<em>con pat<sub>1</sub> ... pat<sub>n</sub></em>のマッチングは、<em>con</em>は<code>data</code>によって定義されるコンストラクタである、依存するその値に依存する。
     - もし値が形式<em>con pat<sub>1</sub> ... pat<sub>n</sub></em>であるなら、サブパターンはそのデータ値の要素に対して左から右へ一致される。もし、全てのマッチングが成功したなら、マッチング全体は成功し、はじめの失敗または分岐はマッチング全体を各々、失敗または分岐へともたらす。
     - もし値が形式<em>con' v<sub>1</sub> ... v<sub>m</sub></em>であるなら、<em>con</em>は<em>con'</em>への異なるコンストラクタである、そのマッチングは失敗する。
     - もし値が`⊥`なら、そのマッチングは分岐する。
1. ラベル付きフィールドを使ったコンストラクタに対してのマッチングはそのフィールドがフィールドリスト内で指定された順序で照合されることを除いて、通常のコンストラクタパターンのマッチングと同じである。全てのリストされたフィールドはコンストラクタによって宣言されなければならず、フィールドは2回以上指定されないかもしれない。パターンによって指定されたフィールドは無視される(<code>\_</code>に対して一致する)。
1. 値<em>v</em>対する数値、文字、文字列リテラルパターン<em>k</em>のマッチングはもし、<em>v == k</em>なら成功する。<code>==</code>はパターンの型を元にオーバロードされる。マッチングはもしこのテストが分岐するなら分岐する。
    <br><br>
    数値リテラルの解釈はまさにセクション[3.2]("#3.2")で記載のとおりである。即ち、オーバロードされた関数<code>fromInteger</code>または<code>fromRational</code>は(それぞれ)適切な型へ変換することによって<code>Integer</code>または<code>Rational</code>リテラルに適用される。


静的型の制約(例えば、文字とbooleanを一致させる静的なエラー)は別として、次の静的クラスの制約は保持する。

- 整数リテラルパターンはクラス<code>Num</code>の値とのみ照合できる。
- 浮動小数点リテラルパターンはクラス<code>Factional</code>の値とのみ照合できる。

2種類のパターンの区別することはしばしば有用である。 **反駁できない** パターンの照合は厳密ではなく、そのパターンはもし、照合された値が`⊥`なら一致する。 **反駁できる** パターンは厳密であり、その一致される値が`⊥`なら分岐する。反駁できないパターンは次のものである。変数やワイルドカード、<em>N</em>が<code>newtype</code>と<em>apat</em>によって定義されたコンストラクタ<em>N apat</em>は反駁できず(セクション[4.2.3](./4-declarations-and-bindings.md))、<em>var@apat</em>の<em>apat</em>は反駁できない、または形式<em>~apat</em>(<em>apat</em>が反駁できないかどうか)である。他の全てのパターンは反駁できる。

ここにいくつかの例をだす。

1. もし、パターン<code>['a','b']</code>が<code>['x',⊥]</code>と一致されるなら、その時、<code>'a'</code>は<code>x</code>との一致に **失敗し**  、その結果は失敗と一致する。しかし、もし<code>['a','b']</code>が<code>[⊥,'x']</code>と一致されるなら、その時、<code>'a'</code>と`⊥`を一致するよう試みることは **分岐** と一致することをもたらす。
1. これらの例は反駁できるものとできないもののマッチングの実演である。

    ||||
    |--|--|--|
    |(\ ~(x,y) -> 0) ⊥|⇒|0|
    |(\  (x,y) -> 0) ⊥|⇒|⊥|
    | | | |
    |(\ ~[x] -> 0) []|⇒|0|
    |(\ ~[x] -> x) []|⇒|⊥|
    | | | |
    |(\ ~[x,~(a,b)] -> x) [(0,1),⊥]|⇒|(0,1)|
    |(\ ~[x, (a,b)] -> x) [(0,1),⊥]|⇒|⊥|
    | | | |
    |(\  (x:xs) -> x:x:xs) ⊥|⇒|⊥|
    |(\ ~(x:xs) -> x:x:xs) ⊥|⇒|⊥:⊥:⊥|
1. 次の宣言を考えてほしい。
    ```hs
    newtype N = N Bool  
    data    D = D !Bool
    ```
    これらの例は<code>data</code>と<code>newtype</code>によって定義された型においてのパターンマッチングの違いを説明する。

    ||||
    |--|--|--|
    |(\  (N True) -> True) ⊥|⇒|⊥|
    |(\  (D True) -> True) ⊥|⇒|⊥|
    |(\ ~(D True) -> True) ⊥|⇒|True|

    追加の例はセクション[4.2.3](./4-declarations-and-bindings.md)で見つかるだろう。

関数内のcase式内の最上位パターンと最上位パターンの集合またはパターン束縛は0以上の **ガード** に関係する持つかもしれない。ガードの構文と意味論についてはセクション[3.13]("#3.13")を参照してもらいたい。

ガード意味論は関数またはcase式の厳密な特徴への影響を持つ。特に、他の反駁できないパターンがガードのために評価されるかもしれない。例えば、次の

```hs
f :: (Int,Int,Int) -> [Int] -> Int  
f ~(x,y,z) [a] | (a == y) = 1
```

<code>a</code>と<code>y</code>の両方はガードの<code>==</code>によって評価される。

### パターンマッチングの正式な意味論

case式を除くすべてのパターンマッチの意味論は、パターンマッチの構成と<code>case</code>式との間を関連付ける等式を与えることで定められる( **訳注** : パターンマッチの意味論は一旦case式を使って定義し、そのあとcase式の意味論に従って処理を行う)。<code>case</code>式の意味論自体は図[3.1]("#figure-3.1")、[3.3](#figure-3.3)の、一連の識別子のように順番に与えられる。どんな実装でもこれらの識別子を保持するために振る舞わなければならず、かなり非効率的なコードを生成することから、それはそれらを直接使用することは期待されない。

<a name="figure-3.1"></a>
<table class="fbox">
<tbody>
 <tr>
  <td class="code-number">(a)</td>
  <td><pre>
<code>case <em>e</em> of { alts } = (\v -> case <em>v</em> of { alts }) <em>e</em>
where <em>v</em> is a new variable</code></pre>
 </td>
 </tr>
 <tr>
  <td class="code-number">(b)</td>
  <td><pre>
<code>case  <em>v</em> of {  <em>p</em> <sub>1</sub>  match<sub>1</sub>;  … ; <em>p<sub>n</sub></em>  match<sub>n</sub> }
=  case <em>v</em> of { <em>p<sub>1</sub></em>  match<sub>1</sub> ;
               _  -> … case <em>v</em> of {
                         <em>p<sub>n</sub></em>  match<sub>n</sub> ;
                         _  -> error "No match" }…}
where each match<sub>i</sub> has the form:
 | <em>gs<sub>i,1</sub></em>  -> <em>e<sub>i,1</sub></em> ; … ; | <em>gs<sub>i,m<sub>i</sub></sub></em> -> <em>e<sub>i,m<sub>i</sub></sub></em> where { <em>decls</em><sub>i</sub> } </code></pre>
 </td>
 </tr>
 <tr>
  <td class="code-number">(c)</td>
  <td><pre>
<code>case <em>v</em> of { <em>p</em> | <em>gs<sub>1</sub></em> -> <em>e<sub>1</sub></em> ; …
             | <em>gs<sub>n</sub></em> -> <em>e<sub>n</sub></em> where { <em>decls</em> }
            _     -> <em>e′</em> }
= case <em>e′</em> of { <em>y</em> ->
   case <em>v</em> of {
     <em>p</em> -> let { <em>decls</em> } in
          case () of {
            () | <em>gs<sub>1</sub></em> -> <em>e<sub>1</sub></em>;
            _ -> … case () of {
                       () | <em>gs<sub>n</sub><em> -> <em>e<sub>n</sub></em>;
                       _  -> <em>y</em> } … }
     _ -> <em>y</em> }}
where <em>y</em> is a new variable</code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(d)</td>
 <td><pre>
<code>case <em>v</em> of { ~p -> <em>e</em>; _ -> <em>e′</em> }
= (\x<sub>1</sub> … x<sub>n</sub> -> <em>e</em> ) (case <em>v</em> of { p-> x<sub>1</sub> })… (case <em>v</em> of { <em>p</em> -> x<sub>n</sub>})
where x<sub>1</sub>,…,x<sub>n</sub> are all the variables in p</code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(<em>e</em>)</td>
 <td><pre>
<code>case <em>v</em> of { x@<em>p</em> -> <em>e</em>; _ -> <em>e′</em> }
=  case <em>v</em> of { <em>p</em> -> ( \ x -> <em>e</em> ) <em>v</em> ; _ -> <em>e′</em> }</code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(f)</td>
 <td><pre>
<code>case <em>v</em> of { _ -> <em>e</em>; _ -> <em>e′</em> } = <em>e</em> </code></pre>
 </td>
 </tr>
</tbody>
</table>

**図 3.1:** case式の意味論、パート1
<div class="separator"></div>

<div class="separator"></div>

<a name="figure-3.2"></a>
<table class="fbox">
<tbody>
 <tr>
  <td class="code-number">(g)</td>
  <td><pre><code>case <em>v</em> of { <em>K</em> p<sub>1</sub>…p<sub>n</sub> -> <em>e</em>; _ -> <em>e′</em> }
	= case <em>v</em> of {
	     <em>K</em> x<sub>1</sub>…x<sub>n</sub> -> case x<sub>1</sub> of {
	                    <em>p<sub>1</sub></em> -> … case xn of { <em>p<sub>n</sub></em> -> <em>e</em> ; _ -> <em>e′</em> } …
	                    _  -> <em>e′</em> }
	     _ -> <em>e′</em> }
	at least one of <em>p<sub>1</sub></em>,…,<em>p<sub>n</sub></em> is not a variable; x<sub>1</sub>,…,x<sub>n</sub> are new variables</code></pre>
 </td>
 </tr>
 <tr>
  <td class="code-number">(h)</td>
  <td><pre><code>case <em>v</em> of { <em>k</em> -> <em>e</em>; _ -> <em>e′</em> } = if (v==k) then <em>e</em> else <em>e′</em>
	where <em>k</em> is a numeric, character, or string literal</code></pre>
 </td>
 </tr>
 <tr>
  <td class="code-number">(i)</td>
  <td><pre><code>case <em>v</em> of { x -> <em>e</em>; _ -> <em>e′</em> } = case <em>v</em> of { x -> <em>e</em> }</code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(j)</td>
 <td><pre><code>case <em>v</em> of { x -> <em>e</em> } = ( \ x -> <em>e</em> ) <em>v</em> </code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(k)</td>
 <td><pre><code>case <em>N</em> <em>v</em> of { <em>N</em> <em>p</em> -> <em>e</em>; _ -> <em>e′</em> }
	= case <em>v</em> of { <em>p</em> -> <em>e</em>; _ -> <em>e′</em> }
	where <em>N</em> is a newtype constructor </code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(l)</td>
 <td><pre><code>case ⊥ of { <em>N</em> <em>p</em> -> <em>e</em>; _ -> <em>e′</em> } = case ⊥ of { <em>p</em> -> <em>e</em> }
	where <em>N</em> is a newtype constructor </code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(m)</td>
 <td><pre><code>case  <em>v</em>  of {  <em>K</em>  { <em>f<sub>1</sub></em>  =  <em>p<sub>1</sub></em>  ,  <em>f<sub>2</sub></em>  =  <em>p<sub>2</sub></em>  , … } ->  <em>e</em> ; _ ->  <em>e′</em> }
	=  case <em>e′</em> of {
	    <em>y</em> ->
	    case  <em>v</em>  of {
	      <em>K</em>  {  <em>f<sub>1</sub></em>  =  <em>p<sub>1</sub></em>  } ->
	            case  <em>v</em>  of { <em>K</em>  { <em>f<sub>2</sub></em>  =  <em>p<sub>2</sub></em>  , …  } ->  <em>e</em> ; _ ->  <em>y</em>  };
	            _ ->  <em>y</em>  }}
	where <em>f<sub>1</sub></em>, <em>f<sub>2</sub></em>, … are fields of constructor <em>K</em>; <em>y</em> is a new variable </code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(n)</td>
 <td><pre><code>case  <em>v</em>  of {  <em>K</em>  { f  =  <em>p</em> } ->  <em>e</em> ; _ ->  <em>e′</em> }
  = case  <em>v</em>  of {
       <em>K</em> <em>p<sub>1</sub></em> … <em>p<sub>n</sub></em>  ->  <em>e</em> ; _ ->  <em>e′</em> }
  where <em>p<sub>i</sub></em> is <em>p</em> if f labels the ith component of <em>K</em>, _ otherwise </code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(o)</td>
 <td><pre><code>case  <em>v</em>  of {  <em>K</em>  {} ->  <em>e</em> ; _ ->  <em>e′</em> }
  = case  <em>v</em>  of {
       <em>K</em> _ … _ ->  <em>e</em> ; _ ->  <em>e′</em> }</code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(p)</td>
 <td><pre><code>case (K′ <em>e<sub>1</sub></em> … <em>e<sub>m</sub></em>) of { <em>K</em> x<sub>1</sub> … x<sub>n</sub> -> <em>e</em>; _ -> <em>e′</em> } = <em>e′</em>
  where <em>K</em> and <em>K′</em> are distinct data constructors of arity <em>n</em> and m, respectively</code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(q)</td>
 <td><pre><code>case (K <em>e<sub>1</sub></em> … <em>e<sub>n</sub></em>) of { <em>K</em> x<sub>1</sub> … x<sub>n</sub> -> <em>e</em>; _ -> <em>e′</em> }
  = (\x<sub>1</sub> … x<sub>n</sub> -> <em>e</em>) <em>e<sub>1</sub></em> … <em>e<sub>n</sub></em>
  where <em>K</em> is a data constructor of arity <em>n</em> </code></pre>
 </td>
 </tr>
 <tr>
 <td class="code-number">(r)</td>
 <td><pre><code>case ⊥ of { <em>K</em> x<sub>1</sub> … x<sub>n</sub> -> <em>e</em>; _ -> <em>e′</em> } =  ⊥
  where <em>K</em> is a data constructor of arity <em>n</em></code></pre>
 </td>
 </tr>
</tbody>
</table>

**図 3.2:** case式の意味論、パート2
<div class="separator"></div>

<div class="separator"></div>

<a name="figure-3.3"></a>
<table class="fbox">
<tbody>
<tr>
 <td class="code-number">(s)</td>
 <td><pre><code>case () of { () | <em>g<sub>1</sub></em>, …, <em>g<sub>n</sub></em> -> <em>e</em>; _ -> <em>e′</em> }
= case () of {
     () | <em>g<sub>1</sub></em> -> … case () of {
                    () | <em>g<sub>n</sub></em> -> <em>e</em>;
                    _ -> <em>e′</em> } …
     _ -> <em>e′</em> }
 where <em>y</em> is a new variable </code></pre>
</td>
</tr>
 <td class="code-number">(t)</td>
 <td><pre><code>case() of { () | <em>p</em> <- <em>e<sub>0</sub></em> -> <em>e</em>; _ -> <em>e′</em> }
= case <em>e</em><sub>0</sub> of { <em>p</em> -> <em>e</em>; _ -> <em>e′</em> }</code></pre>
</td>
</tr>
<tr>
<td class="code-number">(u)</td>
<td><pre><code>case () of { () | let <em>decls</em> -> <em>e</em>; _ -> <em>e′</em> }
= let <em>decls</em> in <em>e</em> </code></pre>
</td>
</tr>
<tr>
 <td class="code-number">(v)</td>
 <td><pre><code>case () of { () | <em>e<sub>0</sub></em> -> <em>e</em>; _ -> <em>e′</em> }
  = if <em>e<sub>0</sub></em> then <em>e</em> else <em>e′</em> </code></pre>
</td>
</tr>
</tbody>
</table>

**図 3.3:** case式の意味論、パート3
<div class="separator"></div>

図[3.1]("#figure-3.1")-[3.3]("#figure-3.3")の<em>e, e'</em>と<em>e<sub>i</sub></em>は式で、<em>g<sub>i</sub></em>と<em>gs<sub>i</sub></em>はガードと各々のガードの並びであり、<em>p</em>と<em>p<sub>i</sub></em>はパターン、<em>v, x, x<sub>i</sub></em>は変数、<em>K,K'</em>は代数的データ型`(data)`コンストラクタ(タプルコンストラクタを含む)で、<em>N</em>は<code>newtype</code>コンストラクタである。

ルール(b)は実際にガードを含むかどうかにはかかわらず、一般的な表層ソース言語の<code>case</code>式に適合するものである。もしガードが書かれていなければ、その時、<code>True</code>が形式<em>match<sub>i</sub></em>内のガード<em>gs<sub>i,j</sub></em>に代用される。各々の識別子はもっと簡単な形式へと<code>case</code>式の結果を操作する。

図[3.2]("#figure-3.2")のルール(h)はオーバロードされた<code>==</code>演算子を起動し、パターンマッチングの意味をオーバーロードされた定数に対して定義するというルールである。

これらの識別子は静的な意味論を全て保存する。ルール(d)、(e)、(j)、(q)は<code>let</code>ではなくラムダを使っていて、これは<code>case</code>によって束縛された変数が単相型ということを示す(セクション[4.1.4](./4-declarations-and-bindings.md)を参照)。
