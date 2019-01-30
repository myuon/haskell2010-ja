[WIP]

# 式

この章では、私たちはHaskellの式の構文と非形式的な意味論を説明する。また必要であらばHaskellカーネルへの変換についても説明する。`let`式の場合を除いて、これらの変換は静的、動的な意味論の両方を保存する。これらの変換を使った束縛されていない変数とコンストラクタは常に`Prelude`によって定義された実体を参照する。例えば、リスト内包表記の変換(セクション[3.11])で使われる`"concatMap"`は`Prelude`によって定義された`concatMap`を意味する。これは識別子`"concatMap"`がリスト内包表記で使われているスコープ内にあるかないかは関係なく、また、(もしスコープ内にあったとしても)束縛されていても関係はない。

<pre>
<em>exp</em>        →	<em>infixexp</em> :: [context =>] type	    (expression type signature)
            |	<em>infixexp</em>

<em>infixexp</em>   →	<em>lexp</em> qop <em>infixexp</em>	            (infix operator application)
            |	- <em>infixexp</em>	                    (prefix negation)
            |	<em>lexp</em>

<em>lexp</em>       →	\ apat1 … apatn -> <em>exp</em>             (lambda abstraction, n ≥ 1)
            |	<tt>let</tt> <em>decls</em> <tt>in</tt> <em>exp</em>	            (let expression)
            |	<tt>if</tt> <em>exp</em> [;] <tt>then</tt> <em>exp</em> [;] <tt>else</tt> <em>exp</em>    (conditional)
            |	<tt>case</tt> <em>exp</em> of { <em>alts</em> }	            (case expression)
            |	<tt>do</tt> { <em>stmts</em> }	                    (do expression)
            |	<em>fexp</em>
<em>fexp</em>       →	[<em>fexp</em>] <em>aexp</em>	                    (function application)

<em>aexp</em>	    →	<em>qvar</em>                                (variable)
            |	<em>gcon</em>                                (general constructor)
            |	<em>literal</em>
            |	( <em>exp</em> )                             (parenthesized expression)
            |	( <em>exp<sub>1</sub></em> , … , <em>exp<sub>k</sub></em> )                (tuple, k ≥ 2)
            |	[ <em>exp<sub>1</sub></em> , … , <em>exp<sub>k</sub></em> ]	            (list, k ≥ 1)
            |	[ <em>exp<sub>1</sub></em> [, <em>exp<sub>2</sub></em>] .. [<em>exp<sub>3</sub></em>] ]	    (arithmetic sequence)
            |	[ <em>exp</em> | <em>qual<sub>1</sub></em> , … , <em>qual<sub>n</sub></em> ]	    (list comprehension, n ≥ 1)
            |	( <em>infixexp</em> <em>qop</em> )	            (left section)
            |	( <em>qop<sub>⟨-⟩</sub></em> <em>infixexp</em> )	            (right section)
            |	<em>qcon</em> { <em>fbind<sub>1</sub></em> , … , <em>fbind<sub>n</sub></em> }	    (labeled construction, n ≥ 0)
            |	<em>aexp<sub>⟨qcon⟩</sub></em> { <em>fbind<sub>1</sub></em> , … , <em>fbind<sub>n</sub></em> }   (labeled update, n  ≥  1)
</pre>

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

<pre><code>error     :: String -> a
undefined :: a
</code></pre>

`error`の呼び出しはプログラムの実行を終了させ、OSに適切なエラー表示を返す。そのエラー表示にはシステム依存の方法で文字列を画面に表示するべきである。`undefined`が使われたとき、そのエラーメッセージはコンパイラーによって作成される。

Haskellの式の変換は実行時エラーが発生したことを明示的に表示するため`error`と`undefined`を使用する。エラーが発生した際の実際のプログラムの振舞は実装次第である。そのメッセージはこれらの変換のみ提案するため`error`関数へ渡される。エラー発生時、詳しい情報または乏しい情報を表示することを実装側は選択するかもしれない。

## 変数、コンストラクタ、演算子、リテラル

<pre>
<em>aexp</em>	→	<em>qvar</em>	    (variable)
        |	<em>gcon</em>	    (general constructor)
        |	<em>literal</em>
<em>gcon</em>	→	()
        |	[]
        |	(,{,})
        |	<em>qcon</em>

<em>var</em>	→	<em>varid</em>| ( <em>varsym</em> )	    (variable)
<em>qvar</em>	→	<em>qvarid</em> | ( <em>qvarsym</em> )	    (qualified variable)
<em>con</em>	→	<em>conid</em> | ( <em>consym</em> )	    (constructor)
<em>qcon</em>	→	<em>qconid</em> | ( <em>gconsym</em> )	    (qualified constructor)
<em>varop</em>	→	<em>varsym</em> | `  <em>varid</em>`	    (variable operator)
<em>qvarop</em>	→	<em>qvarsym</em> | `  <em>qvarid</em> `	    (qualified variable operator)
<em>conop</em>	→	<em>consym</em> | `  <em>conid</em> `	    (constructor operator)
<em>qconop</em>	→	<em>gconsym</em> | `  <em>qconid</em> `	    (qualified constructor operator)
<em>op</em>	→	<em>varop</em> | <em>conop</em>	            (operator)
<em>qop</em>	→	<em>qvarop</em> | <em>qconop</em>	            (qualified operator)
<em>gconsym</em>	→	: | <em>qconsym</em>
</pre>

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

<pre>
<em>fexp</em>	→	[<em>fexp</em>] <em>aexp</em>	            (function application)
<em>lexp</em>	→	\ <em>apat<sub>1</sub></em> … <em>apat<sub>n</sub></em> -> <em>exp</em>	    (lambda abstraction, n ≥ 1)
</pre>

関数適用は<code>e<sub>1</sub> e<sub>2</sub></code>と書く。適用は左結合性をもつので、`(f x) y`の括弧は省略することができる。`e1`はデータ構成子である可能性もあるため、データ構成子の部分的な適用は許されている。

ラムダ抽象は<code>\ p<sub>1</sub> … p<sub>n</sub> -> e</code>と書き、<code>p<sub>i</sub></code>はパターンである。`\x:xs->x`のような式は構文的に正しくない。`\(x:xs)->x`と書くのが正しい。

パターンの集合は線形でなければならない。つまり、変数は集合の中で2回以上出現してはいけない。

<div class="column">

**変換:** 以下の等式が成り立つ。

<code>p<sub>1</sub> … p<sub>n</sub> -> e = \ X<sub>1</sub> … X<sub>n</sub> -> case (X<sub>1</sub>, …, X<sub>n</sub>) of (p<sub>1</sub>, …, p<sub>n</sub>) -> e</code>

<code>X<sub>i</sub></code>は新しい識別子である。

</div>

この変換がセクション[3.17.3]("#3.17.3")で説明するcase式とパターンマッチの意味論と組み合わさって与えられたとき、もしもパターンマッチに失敗すれば結果は⊥となる。

## 演算子適用

<pre>
<em>infixexp</em>   → <em>lexp</em> <em>qop</em> <em>infixexp</em>
            |  - <em>infixexp</em>	    (prefix negation)
            | <em>lexp</em>
<em>qop</em>        → <em>qvarop</em> | <em>qconop</em>	    (qualified operator)
</pre>

<code>e<sub>1</sub> qop e<sub>2</sub></code>という形式は二項演算子`qop`の式<code>e<sub>1</sub></code>と<code>e<sub>2</sub></code>への中置適用である。

特殊な形式`-e`は前置の符号反転演算子を表す。この演算子はHaskellにおける唯一の前置演算子であり、`negate (e)`という意味の構文である。二項演算子`-`はPrelude内の`-`の定義への参照を必要とせず、モジュールシステムによって再束縛されるかもしれない。しかしながら、単項演算子`-`はPrelude内で定義された`negate`関数を常に参照する。`-`演算子の局所的な意味と単項の符号反転演算との間には何の関連もない。

前置の符号反転演算子はPrelude内(表[4.1](./4-declarations-and-bindings.md)を参照)で定義された中置演算子`-`と同じ優先順位を持つ。`e1-e2`は二項演算子`-`の中置表現解析されるため、前置の符号反転演算子を使うには構文解析に代わって`e1(-e2)`と書かなければいけない。同様に、`(-)`は中置演算子と同様に`(\ x y -> x-y)`のための構文であるが、`(\ x -> -x)`を表せず、そのためには`negate`を使う必要がある。

<div class="column">

**変換：** 以下の等式が成り立つ。
<pre>
e<sub>1</sub> op e<sub>2</sub>  =	(op) e<sub>1</sub> e<sub>2</sub>
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

<pre><code>
(let n = 10 in n +)
</code></pre>

セクション[3]("./3-expressions.md")にあるように、letとラムダに関するメタルールにより誤りである。次の式は

<pre><code>
(let n = 10 in n + x)
</code></pre>

以下のように解析され

<pre><code>
(let n = 10 in (n + x))
</code></pre>

次のようにはならない

<pre><code>
((let n = 10 in n) + x)
</code></pre>

なぜなら、`-`は文法内で特別に扱われるからだ。前のセクションで説明したように、`(- exp)`はセクションではなく、前置の符号反転演算子の適用である。しかしながら、Prelude名で定義された`subtract`関数があり、それによって<code>(<em>subtract</em> exp)</code>が不正なセクション(**訳注**: (- exp)のこと)と同じ意味となる。式`(+ (- exp))`は同じ用途で役立つことができる。

<div class="column">

**変換:** 以下の等式が成り立つ。

<pre><code>(op e)  =       \ x -> x op e
(e op)  =       \ X -> e op x
</code></pre>

`op`は二項演算子で、`e`は式である。and x is a variable that does not occur free in e.
</div>

## 条件文
<pre>
<em>lexp</em>	→	<tt>if</tt> <em>exp</em> [;] <tt>then</tt> <em>exp</em> [;] <tt>else</tt> <em>exp</em>
</pre>

条件式は<code><tt>if</tt> e<sub>1</sub> <tt>then</tt> e<sub>2</sub> <tt>else</tt> e<sub>3</sub></code>の形式をとり、もし<code>e<sub>1</sub></code>が真なら、<code>e<sub>2</sub></code>を返し、<code>e<sub>1</sub></code>が偽なら<code>e<sub>3</sub></code>を返し、それ以外なら`⊥`を返す。

<div class="column">

**変換:** 以下の等式が成り立つ。

<pre><code><tt>if</tt> e<sub>1</sub> <tt>then</tt> e<sub>2</sub> <tt>else</tt> e<sub>3</sub>(op e) = <tt>case</tt> e<sub>1</sub> <tt>of</tt> { <tt>True</tt> -> e<sub>2</sub> <tt>;</tt> <tt>False</tt> -> e<sub>3</sub> }
</code></pre>

<tt>True</tt>と<tt>False</tt>はPrelude内で定義されている<tt>Bool</tt>型の2つの引数のないコンストラクタである。<code>e<sub>1</sub></code>は<tt>Bool型</tt>でなければならず、<code>e<sub>2</sub></code>と<code>e<sub>3</sub></code>も同じ型でなければならない。条件式全体の型も同様である。
</div>

## リスト

<pre>
<em>infixexp</em>  →     <em>exp1</em> <em>qop</em> <em>exp2</em>
<em>aexp</em>	  →	[ <em>exp<sub>1</sub></em> , … , <em>exp<sub>k</sub></em> ]	    (k ≥ 1)
          |	<em>gcon</em>
<em>gcon</em>	  →	<tt>[]</tt>
          |	<em>qcon</em>
<em>qcon</em>	  →	( <em>gconsym</em> )
<em>qop</em>	  →	<em>qconop</em>
<em>qconop</em>	  →	<em>gconsym</em>
<em>gconsym</em>	  →	<tt>:</tt>

</pre>

**List**は`k ≥ 1`として、<code>[e<sub>1</sub>, …, e<sub>k</sub>]</code>のように書く。リストコンストラクタは `:`であり、空リストは`[]`で表記される。リストの標準操作はPrelude内で与えられる(セクション[6.1.3]("./6-predefined-types-and-classes.md")と[9章]("./9-standard-prelude.md")の特にセクション[9.1]("./9-standard-prelude.md")を参照)。

<div class="column">

**変換:** 以下の等式が成り立つ。

<code>[e<sub>1</sub>, …, e<sub>k</sub>]</code> = <code>e<sub>1</sub> : (e<sub>2</sub> : ( … (e<sub>k</sub> : [])))</code>

`:`と`[]`はPredule内(セクション[6.1.3]("./6-predefined-types-and-classes.md"))で定義されたリストのコンストラクタである。<code>e<sub>1</sub></code>から<code>e<sub>k</sub></code>までの型は同じでなければならない(それを`t`と呼ぶ)。式全体の型は`[t]`になる(セクション[4.1.2]("./4-declarations-and-bindings.md"))。
</div>

コンストラクタ`:`はリストコンストラクタとしてのみ予約されており、言語構文の一部と見做されている。また、それは隠すことも再定義することもできない。`:`は優先順位レベル5の右結合演算子である(セクション[4.4.2]("./4-declarations-and-bindings.md"))。

## タプル
<pre>
<em>aexp</em>	→	( <em>exp<sub>1</sub></em> , … , <em>exp<sub>k</sub></em> )	    (k ≥ 2)
        |	<em>qcon</em>
<em>qcon</em>	→	(,{,})
</pre>

**タプル**は`k ≥ 2`以上の<code>(e<sub>1</sub>, …, e<sub>k</sub></code>のように書く。`n-tuple`のコンストラクタは`(,…,)`と表記され、`n-1`のコンマがある。従って、`(a,b,c)`と`(,,) a b c`は同じ値を表す。タプルの標準操作はPrelude内で定義されている(セクション[6.1.4]("./6-predefined-types-and-classes.md")と[9章]("./9-standard-prelude.md"))。

<div class="column">

**変換:** `k ≥ 2`のときの<code>(e<sub>1</sub>, …, e<sub>k</sub></code>はPrelude内で定義された`k-tuple`のインスタンスになり、変換は要求されない。もし、<code>t<sub>1</sub></code>から<code>t<sub>k</sub></code>はそれぞれ<code>e<sub>1</sub></code>から<code>e<sub>k</sub></code>の型があり、最終的なタプルの型は<code>t<sub>1</sub>,…,t<sub>k</sub></code>になる(セクション[4.1.2]("./4-declarations-and-bindings.md"))。

</div>

## 単位式と括弧付き式

<pre>
<em>aexp</em>	→	<em>gcon</em>
        |	( <em>exp</em> )
<em>gcon</em>	→	()
</pre>

`(e)`の形式はシンプルに**括弧付き式**であり、`e`と等しい。**ユニット(unit)**式`()`は`()`型を持つ(セクション[4.1.2]("./4-declarations-and-bindings.md")を参照)。それは⊥以外の型のメンバのみで、"引数のないタプル"のように考えられる(セクション[6.1.5]("./6-predefined-types-and-classes.md")を参照)。

<div class="column">

**変換:** `(e)`は`e`と等しい。
</div>

## 数列

<pre>
<em>aexp</em>	→	[ <em>exp<sub>1</sub></em> [, <em>exp<sub>2</sub></em>] .. [<em>exp<sub>3</sub></em>] ]
</pre>

**数列**[e<sub>1</sub>,e<sub>2</sub> .. e<sub>3</sub>]は型`t`の値のリストを表し、各<code>e<sub>i</sub></code>は型`t`を持ち、`t`は`Enum`クラスのインスタンスである。

<div class="column">

**変換:**　数列はこれらの等式を満たす。

<pre>
[ <em>e<sub>1</sub></em>.. ]	=	<tt>enumFrom</tt> <em>e<sub>1</sub></em>
[ <em>e<sub>1</sub></em>,<em>e<sub>2</sub></em>.. ]	=	<tt>enumFromThen</tt> <em>e<sub>1</sub></em> <em>e<sub>2</sub></em>
[ <em>e<sub>1</sub></em>..<em>e<sub>3</sub></em> ]	=	<tt>enumFromTo</tt> <em>e<sub>1</sub></em> <em>e<sub>3</sub></em>
[ <em>e<sub>1</sub></em>,<em>e<sub>2</sub></em>..<em>e<sub>3</sub></em> ]	=	<tt>enumFromThenTo</tt> <em>e<sub>1</sub></em> <em>e<sub>2</sub></em> <em>e<sub>3</sub></em>
</pre>

`enumForm`、`enumFormThen`、`enumFormTo`、`enumFormThenTo`はPrelude内で定義されている`Enum`クラスのクラスメソッドになる。
</div>

故に数列の意味論は型`t`のインスタンス宣言に完全に依存している。どの`Prelude`型が`Enum`型にあるか、そしてそれらの意味論についてのより詳しいことについてはセクション[6.3.4]("./6-predefined-types-and-classes.md")を参照すること。

## リスト内包表記

<pre>
<em>aexp</em>	→	[ <em>exp</em> | <em>qual<sub>1</sub></em> , … , <em>qual<sub>n</sub></em> ]     (list comprehension, n ≥ 1)
<em>qual</em>	→	<em>pat</em> <- <em>exp</em>	                (generator)
        |	<tt>let</tt> <em>decls</em>	                (local declaration)
        |	<em>exp</em>	                        (boolean guard)
</pre>

**リスト内包表記**は<code>[ <em>e</em> | q<sub>1</sub>, …, q<sub>n</sub> ]</code>、`n ≥ 1`形式を持ち、<code>q<sub>i</sub></code>修飾子は次のいずれかである。

- 形式`p <- e`の**ジェネレータ**。`p`は型`t`のパターン(セクション[3.17](#3.17))であり、`e`は型`[t]`の式である。
- 生成された式eで、あるいは後方のブーリアンガードとジェネレータで使われる新しい定義を提供するローカル束縛。
- **ブーリアンガード**。Bool型の任意の式を表すことができる。

このようなリスト内包表記は修飾子リスト内のジェネレータのネストされた深さ優先探索の評価によって作成された連続した環境で`e`を評価することによって生成された要素のリストを返す。変数の束縛は通常のパターンマッチングルール(セクション[3.17]("#3.17"))に従って発生し、もし一致に失敗したら、その時はそのリストの要素は単純にスキップされる。従って、

<pre>
[ x |  xs   <- [ [(1,2),(3,4)], [(5,4),(3,2)] ],  
      (3,x) <- xs ]
</pre>

リスト[4,2]を返す。もし修飾子がboolean guardなら、成功した前のパターンマッチのために真と評価しなけれないけない。通常通り、リスト内法表記における束縛は外部スコープの束縛をシャドーイングできる。例えば以下のようになる。

<pre>
[ x | x <- x, x <- x ] = [ z | y <- x, z <- y]
</pre>

<div class="column">

**変換:** リスト内包表記はこれらの等式を満たし、これらの等式はカーネルへの変換として使われる可能性がある。

<pre>
[  e | <tt>True</tt> ]	        =	[e]
[  e | q ]	        =	[  e | q, <tt>True</tt> ]
[  e | b,  Q  ]	        =	<tt>if</tt> b <tt>then</tt> [  e | Q ] <tt>else</tt> []
[  e | p <- l,  Q ]	=	<tt>let</tt> <tt>ok</tt> p = [  e | Q ]
                                        <tt>ok</tt> _ = []
                                <tt>in</tt> <tt>concatMap</tt> ok  l
[  e | <tt>let</tt> <em>decls</em>,  Q ]	=	<tt>let</tt> <em>decls</em> <tt>in</tt> [  e | Q ]
</pre>

<em>e</em>は式にわたる範囲で、`p`はパターンにわたり、`l`はリスト値式にわたり、`b`はブーリアン式にわたり、**decls**は宣言リストにわたり、`q`は修飾子にわたり、`Q`は修飾子の列にわたる範囲をもつ。`ok`は新しい変数である。関数`concatMap`とブーリアン値`True`はPrelude内で定義されている。

</div>

リスト内法表記の変換で示した通り、letによって束縛された変数は最大限多相的な型を持つ一方で<-によって束縛されたものはラムダ束縛であり、よって単相的になる。 (セクション[4.5.4]("./4-declarations-and-bindings.md")を参照).

## Let式

<pre>
<em>lexp</em>	→	<tt>let</tt> <em>decls</em> <tt>in</tt> <em>exp</em>
</pre>

**let**式は一般的な形式<code><tt>let</tt> { <em>d<sub>1</sub></em> ; … ; <em>d<sub>n</sub></em> } <tt>in</tt> <em>e</em></code>を持ち、ネストされたレキシカルスコープをもつ相互再帰的な宣言のリストを導入する(<tt>let</tt>は他の言語で<tt>letrc</tt>としばしば呼ばれる)。宣言の範囲は式`e`と宣言の右側である。宣言は[4章]("./4-declarations-and-bindings.md")で説明される。パターン束縛のマッチは遅延され、暗黙的な`~`がこれらのパターンを反駁不可にする。 例えば、

<pre><code><tt>let</tt> (x,y) = <tt>undefined</tt> <tt>in</tt> e
</code></pre>

は`x`または`y`が評価されるまでランタイムエラーをもたらさない。

<div class="column">

**変換:** 式<code><tt>let</tt> { <em>d<sub>1</sub></em> ; … ; <em>d<sub>n</sub></em>} in <em>e<sub>0</sub></em></code>の動的な意味論は次の変換によって捕捉される。全ての型シグネチャを取り除いた後、それぞれの宣言<em>d<sub>i</sub></em>は<code>p<sub>i</sub> = e<sub>i</sub></code>の形の等式へと変換される。<code>p<sub>i</sub></code>と<code>e<sub>i</sub></code>はセクション[4.4.3]("./4-declarations-and-bindings.md")での変換を使用する、各々のパターンと式である。一度この変換が終われば、次のような等式が成り立つ。この等式はカーネルへの変換として使われる場合がある。

<pre>
<tt>let</tt> {<em>p<sub>1</sub></em> = <em>e<sub>1</sub></em>;  ... ; <em>p<sub>n</sub></em> = <em>e<sub>n</sub></em>} <tt>in</tt> <em>e<sub>0</sub></em>	=	<tt>let</tt> (<tt>~</tt><em>p<sub>1</sub></em>, ... ,<tt>~</tt><em>p<sub>n</sub></em>) = (<em>e<sub>1</sub></em>, ... ,<em>e<sub>n</sub></em>) in <em>e<sub>0</sub></em>
<tt>let</tt> <em>p</em> = <em>e<sub>1</sub></em>  <tt>in</tt>  e<sub>0</sub>	                =	<tt>case</tt> <em>e<sub>1</sub></em> of <tt>~</tt><em>p</em> -> <em>e<sub>0</sub></em>
                                                where no variable in <em>p</em> appears free in <em>e<sub>1</sub></em>
<tt>let</tt> <em>p</em> = <em>e<sub>1</sub></em>  <tt>in</tt>  <em>e<sub>0</sub></em>	                =	<tt>let</tt> <em>p</em> = <tt>fix</tt> ( \ <tt>~</tt><em>p</em> -> <em>e<sub>1</sub></em>) in <em>e<sub>0</sub></em>
</pre>
`fix`は最小不動点演算子である。反駁不可パターン`~p`の使用は注意すべきだ。この変換は静的な意味論を保存しない。なぜなら、caseを使用すると束縛変数が完全な多相型へ型付けされなくなるからである。`let`式で束縛された静的な意味論はセクション[4.4.3]("./4-declarations-and-bindings.md")で説明される。
</div>

## Case式

<pre>
<em>lexp</em>	→	<tt>case</tt> <em>exp</em> <tt>of</tt> { <em>alts</em> }
<em>alts</em>	→	<em>alt<sub>1</sub></em> ; … ; <em>alt<sub>n</sub></em>	    (n ≥ 1)
<em>alt</em>	→	<em>pat</em> -> <em>exp</em> [<tt>where</tt> <em>decls</em>]
        |	<em>pat</em> <em>gdpat</em> [<tt>where</tt> <em>decls</em>]
        |		                    (empty alternative)

<em>gdpat</em>	→	<em>guards</em> -> <em>exp</em> [ <em>gdpat</em> ]
<em>guards</em>	→	| <em>guard<sub>1</sub></em>, …, <em>guard<sub>n</sub></em>	    (n ≥ 1)
<em>guard</em>	→	<em>pat</em> <- <em>infixexp</em>	            (pattern guard)
        |	<tt>let</tt> <em>decls</em>	            (local declaration)
        |	<em>infixexp</em>	            (boolean guard)
</pre>

**case**式は一般的な形式<code><tt>case</tt> e <tt>of</tt> { <em>p<sub>1</sub></em> <em>match<sub>1</sub></em> ; … ; <em>p<sub>n</sub></em> <em>match<sub>n</sub></em> }</code>を持つ。各<code><em>match</em><sub>i</sub></code>は一般的な形式

<pre>
| <em>gs<sub>i1</sub></em>    -> <em>e<sub>i1</sub></em>
…
| <em>gs<sub>imi</sub></em>   -> <em>e<sub>imi</sub></em>
<tt>where</tt> decls<sub>i</sub>
</pre>

(**ガード**の構文ルールについて注目して欲しい。`|`は区切りを表す構文的なメタシンボルではなく終端記号である。)各選択子<code><em>p<sub>i</sub> match<sub>i</sub></em></code>はパターン<em>p<sub>i</sub></em>から成り、<code>match<sub>i</sub></code>と一致する。各マッチは順繰りにガード<code>gs<sub>ij</sub></code>と本体<code>e<sub>ij</sub></code>のペアの列から成り、代替となる全てのガードと式上の範囲での付加的な束縛(<code>decls<sub>i</sub></code>)に従う。

**ガード**は次の形式をの一つを持つ。

- **パターンガード**は形式`p <- e`で、`p`は型`t`のパターンで、`e`は式の種類`t`である。もし、式`e`がパターン`p`に一致するなら成功し、パターンの束縛をその環境にもたらす。
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

<pre><code>case x of { (a,_) | let b = not a in b :: Bool -> a }
</code></pre>

これを正しく構文解析するには用心しなければならない。ただ一つの曖昧さのない構文解析は、すなわち次のようにすることである。

<pre><code>case x of { (a,_) | (let b = not a in b :: Bool) -> a }
</code></pre>

しかしながら、`Bool -> a`というフレーズは型として構文的に正当であり、先読みが制限されているパーサーは、この選択に誤ってコミットする可能性があり、それゆえプログラムは拒否する。故に、プログラマーは型シグネチャで終わるガードを避けるように勧められる。これは実際にガードが`exp`ではなく`infixexp`を含んでいる理由になる。

## Do式

<pre>
<em>lexp</em> 	→ 	<tt>do</tt> { <em>stmts</em> } 	           (do expression)
<em>stmts</em> 	→ 	<em>stmt<sub>1</sub></em> … <em>stmt<sub>n</sub></em> <em>exp</em> [<tt>;</tt>] 	   (n ≥ 0)
<em>stmt</em> 	→ 	<em>exp</em> ;
	| 	<em>pat</em> <- <em>exp</em> ;
	| 	<tt>let</tt> <em>decls</em> ;
	| 	<tt>;</tt> 	                   (empty statement)
</pre>

do式はモナドのプログラミングのためのより従来的な構文を提供する。それは以下のような式を許す。

<pre><code>putStr "x: "    >>  
getLine         >>= \l ->  
return (words l)
</code></pre>

より、旧来の方法による書き方は次のものになる。

<pre><code>do putStr "x: "  
   l <- getLine  
   return (words l)
</code></pre>

<div class="column">

**変換：** Do式はこれらの等式を満たし、排除した空の`stmts`の後にカーネルの中への変換のように使われるかもしれない。

<pre>
do {e} 	= 	e
do {e;stmts} 	= 	e >> do {stmts}
do {p <- e; stmts} 	= 	let ok p = do {stmts}
		    ok _ = fail "..."
		  in e >>= ok
do {let decls; stmts} 	= 	let decls in do {stmts}
</pre>

コンパイラが生成したエラーメッセージを表す省略記号`"..."`の部分は`fail`へ渡され、そして可能であればパターンマッチに失敗した場所を表示する。関数`>>`,`>>=`と`fail`はPreludeで定義されたクラス`Monad`の操作であり、`ok`は新しい識別子である。

</div>

`do`の変換でも示したように、`let`に束縛された変数は完全に多相的な型をもつ一方で`<-`によって定義された変数はラムダ束縛であり、ゆえに単相的である。

## フィールドラベル付きのデータ型

データ型の宣言はフィールドラベルを必要に応じて定義してもよい。(セクション[4.2.1]("./4-declarations-and-bindings.md")を参照)これらのフィールドラベルは構築、形式の選択、データ型全体の構造に依存した方法でのフィールドの更新することに使用される。

異なるデータ型は同じスコープの共通のフィールドラベルを共有することはできない。フィールドラベルはコンストラクタ内で高々一度だけ、使用することができる。しかしながら、データ型の中で、あるフィールドがすべてのコンストラクタ内で同じ型を持つときに限り1つのフィールドを複数のコンストラクタで使用することができる。最後の点については次が良い例である:

<pre><code>data S = S1 { x :: Int } | S2 { x :: Int }   -- OK  
data T = T1 { y :: Int } | T2 { y :: Bool }  -- BAD
</code></pre>

ここでの`s`は正当であるが`T`はそうではない。また`y`は後者では矛盾する型付けが与えられている。

### フィールドセレクション

<pre>
aexp 	→ 	qvar
</pre>

フィールドラベルはセレクタ関数のように使用される。変数のように使われる際は、フィールドラベルはオブジェクトからフィールドを抽出する関数のように振る舞う。セレクタはトップレベルの束縛であり、よってローカル変数によってシャドーイングされる場合があるが、しかし他のトップレベルの束縛で同じ名前のものと衝突してはならない。この覆いはセレクタ関数にのみ影響を及ぼし、レコード作成(セクション[3.15.2]("#3.15.2"))及びに更新(セクション[3.15.3]("#3.15.3"))、フィールドラベルは通常の変数と混合されることはない。

<div class="column">

**変換:** フィールドラベル`f`は次のようなセレクタ関数を生成する。

<code>f x 	= 	case x of { C<sub>1</sub> p<sub>11</sub> … p<sub>1k</sub>  ->  e<sub>1</sub> ;… ; C<sub>n</sub> p<sub>n1</sub> … p<sub>nk</sub>  ->  e<sub>n</sub> }
</code>

<code>C<sub>1</sub> ... C<sub>n</sub></code>は全て<code>f</code>とラベルされたフィールドを含むデータ型のコンストラクタで、<code>p<sub>ij</sub></code>
</pre>は`f`が<code>C<sub>i</sub></code>の要素の`j`番目、または`_`をラベルした時の`y`であり、<code>e<code>i</code></code>は<code>C<sub>i</sub></code>のフィールドが`f`または`undefined`のラベルを持つ時の`y`である。

</div>

### フィールドラベルを用いた生成

<pre>
aexp 	→ 	qcon { fbind<sub>1</sub> , … , fbind<sub>n</sub> } 	    (labeled construction, n ≥ 0)
fbind 	→ 	qvar = exp
</pre>

ラベル付けされたフィールドを使うコンストラクタが値の生成に使われる場合があるが、その時には各コンポーネントは位置ではなく名前によって指定する。宣言リストの中で使われる中括弧とは異なりレイアウトの対象にならない。`{`と`}`の文字は明示しなければならない。(これはフィールドの更新、フィールドパターンにおいても正しい。)フィールドラベルを使用する構築は次の制約に応じる。

- 指定されたコンストラクタで宣言されたフィールドラベルのみ言及してよい。
- フィールドラベルは複数回言及してはならない。
- 言及されないフィールドは`⊥`で初期化される。
- 正格なフィールド(宣言された型のフィールドの接頭語に`!`が付けられている)が生成の際に省略された時はコンパイルエラーが発生する。厳格なフィールドはセクション[4.2.1]("./4-declarations-and-bindings.md")で説明される。

式`F {}`は、`F`はデータコンストラクタであり、<strong><code>F</code>がレコード構文により宣言されたかどうかに関わらず</strong>、正当である(ただし`F`が正格フィールドを持たない時に限る。上の4番目の箇条書きを参照)。それは<code>F ⊥<sub>1</sub> … ⊥<sub>n</sub></code>を表し、`n`は`F`の引数の数である。

<div class="column">

**変換：** `f = v`の束縛で、フィールド`f`は`v`でラベルする。
<pre><code>C { bs } 	= 	C (pick<sub>1</sub><sup>C</sup> bs <sub>undefined</sub>) … (pick<sub>k</sub><sup>C</sup> bs <sub>undefined</sub>)
</code></pre>
`k`は`C`の引数の数である。

補助関数<code>pick<sub>i</sub><sup>C</sup> bs d</code>は次にように定義される。
<p>
もし、コンストラクタ`C`の`i`番目の要素がフィールドラベル`f`を持ち、`if f=v`は束縛された`bs`に表示されるなら、その時は<code>pick<sub>i</sub><sup>C</sup> bs d</code>は`v`である。言い換えると<code>pick<sub>i</sub><sup>C</sup> bs d</code>はデフォルト値`d`である。
</p>
</div>

### フィールドラベルを使用した更新

<pre>
aexp 	→ 	aexp⟨qcon⟩ { fbind<sub>1</sub> , … , fbind<sub>n</sub> } 	    (labeled update, n ≥ 1)
</pre>

フィールドラベルを使ったデータ型に所属する値は非破壊的に更新されるかもしれない。これは元々存在していた値を指定されたフィールドの値で書き換えた新しい値を生成する。更新は次の方法に制限される。

- 全てのラベルは同じデータ型から取られなければいけない。
- 少なくともあるコンストラクタは更新の中で全ての言及されたラベルを定義しなければいけない。
- 2回以上言及されるラベルがあってはならない。
- 実行エラーは更新された値が全ての明記されたラベルを含まない時に発生する。

<div class="column">

**変換:** 以下は以前の`pick`の定義を使用する。

[未対応：subタグの子subタグがある]
<pre><code>
e { bs } = <tt>case</tt> e <tt>of</tt>
		        C<sub>1</sub> v<sub>1</sub> … v<sub>k1</sub> -> C<sub>1</sub> (pick<sub>1</sub><sup>C1</sup> bs v<sub>1</sub>) … (pick<sub>k 1</sub><sup>C1</sup> bs v <sub>k1</sub>)
		             ...
		        C<sub>j</sub> v<sub>1</sub> … v<sub>kj</sub> -> C<sub>j</sub> (pick<sub>1</sub><sup>Cj</sup> bs v<sub>1</sub>) … (pick<sub>k j</sub><sup>Cj</sup> bs v <sub>k<sub>j</sub></sub>)
		        _ -> error "Update error"
</code></pre>

<code>{ C<sub>1</sub>,...,C<sub>j</sub>}</code>は`bs`内の全てのラベルを含むコンストラクタの集合で、ｋ<sub>i</sub>はC<sub>i</sub>の引数の数である。

</div>

これはラベル付けされたフィールドを使用している例である。

<pre><code>data T    = C1 {f1,f2 :: Int}  
          | C2 {f1 :: Int,  
                f3,f4 :: Char}
</code></pre>

|式|変換|
|--|--|
| C1 {f1 = 3} | C1 3 <tt>undefined</tt> |
|  C2 {f1 = 1, f4 = 'A', f3 = 'B'} | C2 1 'B' 'A' |
| x {f1 = 1} | <pre><tt>case</tt> x <tt>of</tt> C1 _ f2    -> C1 1 f2<br>          C2 _ f3 f4 -> C2 1 f3 f4</pre> |

フィールド`f1`は両方の`T`のコンストラクタに共通である。この例では、フィールドラベル表記でコンストラクタを使った式をフィールドラベルを使わない同じコンストラクタを使った同値な式へと変換している。もし、`x {f2 = 1, f3 = 'x'}`のように、どのコンストラクタも、更新で使われたフィールドラベルの集合を定義していないのであれば、コンパイル時エラーという結果になるだろう。

## 式の型シグネチャ

<pre><em>exp</em> 	→ 	<em>exp</em> <tt>::</tt> [<em>context</em> =>] <em>type</em>
</pre>

式の型シグネチャは形式`e :: t`を持つ。`e`は式で、`t`は型(セクション[4.1.2]("./4-declarations-and-bindings.md"))であり、それらは明示的に式を分類することに使用され、オーバーロード(セクション[4.1.2](”./4-declarations-and-bindings.md”)を参照)するために曖昧な型付けを解決することに使われるかもしれない。式の値は`exp`の値である。通常の型シグネチャと同様に(セクション[4.4.1](”./4-declarations-and-bindings.md”)を参照)、宣言された型は`exp`から導出可能な主要な型より具体的になるかもしれないが、主要な型より一般的なまたは同程度な型を与えることはエラーである。

<div class="column">

**変換：**

<pre><em>e</em> :: <em>t</em> 	= 	<tt>let</tt> { <em>v</em> :: <em>t</em>;  <em>v</em> = <em>e</em> } <tt>in</tt> <em>v</em></pre>

</div>

## パターンマッチング

**パターン**はラムダ抽象や関数定義、パターン束縛、リスト内包表記、do式、case式内で現れる。しかしながら、はじめの5つは最終的にcase式に変換されるので、パターンマッチの意味論はcase式のときのみ定めれば十分である。

### パターン

パターンはこの構文を持つ。

<pre>

<em>pat</em> 	→ 	<em>lpat</em> qconop <em>pat</em> 	    (infix constructor)
	| 	<em>lpat</em>

<em>lpat</em> 	→ 	<em>apat</em>
	| 	<tt>-</tt> (<em>integer</em> | <em>float</em>) 	    (negative literal)
	| 	<em>gcon</em> <em>apat<sub>1</sub></em> … <em>apat<sub>k</sub></em> 	    (arity gcon  =  k, k ≥ 1)

<em>apat</em> 	→ 	<em>var</em> [ <tt>@</tt> <em>apat</em>]               (as pattern)
	| 	<em>gcon</em>                        (arity gcon  =  0)
	| 	<em>qcon</em> { <em>fpat1</em> , … , <em>fpatk</em> }  (labeled pattern, k ≥ 0)
	| 	<em>literal</em>
	| 	<tt>_</tt> 	                    (wildcard)
	| 	( <em>pat</em> )                     (parenthesized pattern)
	| 	( <em>pat<sub>1</sub></em> , … , <em>pat<sub>k</sub></em> ) 	    (tuple pattern, k ≥ 2)
	| 	[ <em>pat<sub>1</sub></em> , … , <em>pat<sub>k</sub></em> ] 	    (list pattern, k ≥ 1)
	| 	<tt>~</tt> <em>apat</em> 	    (irrefutable pattern)

<em>fpat</em> 	→ 	<em>qvar</em> <tt>=</tt> <em>pat</em>

</pre>

コンストラクタの引数の数はそれに関係するサブパターンの数と一致しなければいけない。部分的に適用されるコンストラクタに反して一致することはできない。

全てのパターンは**線形**でなければならない。変数は2回以上現れないかもしれない。例として、この定義は不正である。

<pre><code>f (x,x) = x     -- ILLEGAL; x used twice in pattern
</code></pre>

形式`var@pat`のパターンは`as-patterns`と呼ばれ、`var`を`pat`によってマッチされた値に付ける名前として使うことができる。例えば以下のものは、

<pre><code>case e of { xs@(x:rest) -> if x==0 then rest else xs }
</code></pre>

は次のものと等しい。

<pre><code>let { xs = e } in  
  case xs of { (x:rest) -> if x==0 then rest else xs }
</code></pre>

形式`_`のパターンは`ワイルドカード`であり、パターンのいくつかの部分が右手側で参照されない時に便利である。それは他の場所で使われない識別子がその場所に置かれているかのようである。例えば、以下は、

<pre><code>case e of { [x,_,_]  ->  if x==0 then True else False }
</code></pre>

は次のものと等しい。

<pre><code>case e of { [x,y,z]  ->  if x==0 then True else False } </code></pre>


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
     - もし値が形式<code>con' v<sub>1</sub> ... v<sub>m</sub></code>であるなら、`con`は`con'`への異なるコンストラクタである、そのマッチングは失敗する。
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
    <pre>
    (\ ~(x,y) -> 0) ⊥    ⇒    0
    (\  (x,y) -> 0) ⊥    ⇒    ⊥
    (\ ~[x] -> 0) []    ⇒    0
    (\ ~[x] -> x) []    ⇒    ⊥
    (\ ~[x,~(a,b)] -> x) [(0,1),⊥]    ⇒    (0,1)
    (\ ~[x, (a,b)] -> x) [(0,1),⊥]    ⇒    ⊥
    (\  (x:xs) -> x:x:xs) ⊥   ⇒   ⊥
    (\ ~(x:xs) -> x:x:xs) ⊥   ⇒   ⊥:⊥:⊥
    </pre>
1. 次の宣言を考えてほしい。
    <pre>
    newtype N = N Bool  
    data    D = D !Bool
    </pre>
    これらの例は`data`と`newtype`によって定義された型においてのパターンマッチングの違いを説明する。
    <pre>
    (\  (N True) -> True) ⊥     ⇒    ⊥
    (\  (D True) -> True) ⊥     ⇒    ⊥
    (\ ~(D True) -> True) ⊥     ⇒    True
    </pre>
    追加の例はセクション[4.2.3]("./4-declarations-and-bindings.md")で見つかるかもしれない。

関数内のcase式内の最上位パターンと最上位パターンの集合またはパターン束縛は0以上の`ガード`に関係する持つかもしれない。ガードの構文と意味論についてはセクション[3.13]("#3.13")を参照してもらいたい。

ガード意味論は関数またはcase式の厳密な特徴への影響を持つ。特に、他の反駁できないパターンがガードのために評価されるかもしれない。例えば、次の

<pre><code>f :: (Int,Int,Int) -> [Int] -> Int  
f ~(x,y,z) [a] | (a == y) = 1
</code></pre>

`a`と`y`の両方はガードの`==`によって評価される。

### パターンマッチングの正式な意味論

case式を除くすべてのパターンマッチの意味論は、パターンマッチの構成と`case`式との間を関連付ける等式を与えることで定められる(**訳注**: パターンマッチの意味論は一旦case式を使って定義し、そのあとcase式の意味論に従って処理を行う)。<tt>case</tt>式の意味論自体は図[3.1]("#figure-3.1")、[3.3](#figure-3.3)の、一連の識別子のように順番に与えられる。どんな実装でもこれらの識別子を保持するために振る舞わなければならず、かなり非効率的なコードを生成することから、それはそれらを直接使用することは期待されない。

<a name="figure-3.1"></a>
<table class="fbox">
<tbody>
 <tr>
  <td class="code-number">(a)</td>
  <td><pre>
<tt>case</tt> e <tt>of</tt> { alts } = (\v <tt>-></tt> <tt>case</tt> v <tt>of</tt> { alts }) e
<tt>where</tt> v <tt>is</tt> a <tt>new</tt> <tt>variable</tt> </pre></td>
 </tr>
 <tr>
  <td class="code-number">(b)</td>
  <td><pre>
<tt>case</tt>  v <tt>of</tt> {  p <sub>1</sub>  match<sub>1</sub>;  … ; p<sub>n</sub>  match<sub>n</sub> }
=  <tt>case</tt> v <tt>of</tt> { p<sub>1</sub>  match<sub>1</sub> ;
               _  -> … <tt>case</tt> v <tt>of</tt> {
                         p<sub>n</sub>  match<sub>n</sub> ;
                         _  -> <tt>error</tt> "No match" }…}
<tt>where</tt> <tt>each</tt> match<sub>i</sub> <tt>has the form:</tt>
 | gs<sub>i,1</sub>  -> e<sub>i,1</sub> ; … ; | gs<sub>i,m<sub>i</sub></sub> -> e<sub>i,m<sub>i</sub></sub> <tt>where</tt> { decls<sub>i</sub> } </pre></td>
 </tr>
 <tr>
  <td class="code-number">(c)</td>
  <td><pre>
<tt>case</tt> v <tt>of</tt> { p | gs<sub>1</sub> -> e<sub>1</sub> ; …
             | gs<sub>n</sub> -> e<sub>n</sub> <tt>where</tt> { decls }
            _     -> e′ }
= <tt>case</tt> e′ <tt>of</tt> { y ->
   <tt>case</tt> v <tt>of</tt> {
     p -> <tt>let</tt> { decls } <tt>in</tt>
          <tt>case</tt> () <tt>of</tt> {
            () | gs<sub>1</sub> -> e<sub>1</sub>;
            _ -> … <tt>case</tt> () <tt>of</tt> {
                       () | gs<sub>n</sub> -> e<sub>n</sub>;
                       _  -> y } … }
     _ -> y }}
<tt>where</tt> y <tt>is a new variable</tt></pre></td>
 </tr>
 <tr>
 <td class="code-number">(d)</td>
 <td><pre>
<tt>case</tt> v <tt>of</tt> { ~p -> e; _ -> e′ }
= (\x<sub>1</sub> … x<sub>n</sub> -> e ) (<tt>case</tt> v <tt>of</tt> { p-> x<sub>1</sub> })… (<tt>case</tt> v <tt>of</tt> { p -> x<sub>n</sub>})
<tt>where</tt> x<sub>1</sub>,…,x<sub>n</sub> <tt>are all the variables in</tt> p</pre></td>
 </tr>
 <tr>
 <td class="code-number">(e)</td>
 <td><pre>
<tt>case</tt> v <tt>of</tt> { x<tt>@</tt>p -> e; _ -> e′ }
=  <tt>case</tt> v <tt>of</tt> { p -> ( \ x -> e ) v ; _ -> e′ }</pre></td>
 </tr>
 <tr>
 <td class="code-number">(f)</td>
 <td><pre>
<tt>case</tt> v <tt>of</tt> { _ -> e; _ -> e′ } = e </pre></td>
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
  <td><pre><tt>case</tt> v <tt>of</tt> { K p<sub>1</sub>…p<sub>n</sub> -> e; _ -> e′ }
	= <tt>case</tt> v <tt>of</tt> {
	     K x<sub>1</sub>…x<sub>n</sub> -> <tt>case</tt> x<sub>1</sub> <tt>of</tt> {
	                    p<sub>1</sub> -> … <tt>case</tt> xn <tt>of</tt> { p<sub>n</sub> -> e ; _ -> e′ } …
	                    _  -> e′ }
	     _ -> e′ }
	<tt>at least one of</tt> p<sub>1</sub>,…,p<sub>n</sub> <tt>is not a variable;</tt> x<sub>1</sub>,…,x<sub>n</sub> <tt>are new variables</tt> </pre></td>
 </tr>
 <tr>
  <td class="code-number">(h)</td>
  <td><pre><tt>case</tt> v <tt>of</tt> { k -> e; _ -> e′ } = <tt>if</tt> (v==k) <tt>then</tt> e <tt>else</tt> e′
	<tt>where</tt> k <tt>is a numeric, character, or string literal</tt></pre></td>
 </tr>
 <tr>
  <td class="code-number">(i)</td>
  <td><pre><tt>case</tt> v <tt>of</tt> { x -> e; _ -> e′ } = <tt>case</tt> v <tt>of</tt> { x -> e }</pre></td>
 </tr>
 <tr>
 <td class="code-number">(j)</td>
 <td><pre><tt>case</tt> v <tt>of</tt> { x -> e } = ( \ x -> e ) v </pre></td>
 </tr>
 <tr>
 <td class="code-number">(k)</td>
 <td><pre><tt>case</tt> N v <tt>of</tt> { N p -> e; _ -> e′ }
	= <tt>case</tt> v <tt>of</tt> { p -> e; _ -> e′ }
	<tt>where</tt> N <tt>is a newtype constructor</tt> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(l)</td>
 <td><pre><tt>case</tt> ⊥ <tt>of</tt> { N p -> e; _ -> e′ } = <tt>case</tt> ⊥ <tt>of</tt> { p -> e }
	<tt>where</tt> N <tt>is a newtype constructor</tt> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(m)</td>
 <td><pre><tt>case<tt>  v  <tt>of<tt> {  K  { f<sub>1</sub>  =  p<sub>1</sub>  ,  f<sub>2</sub>  =  p<sub>2</sub>  , … } ->  e ; _ ->  e′ }
	=  <tt>case</tt> e′ <tt>of</tt> {
	    y ->
	    <tt>case</tt>  v  <tt>of</tt> {
	      K  {  f<sub>1</sub>  =  p<sub>1</sub>  } ->
	            <tt>case</tt>  v  <tt>of</tt> { K  { f<sub>2</sub>  =  p<sub>2</sub>  , …  } ->  e ; _ ->  y  };
	            _ ->  y  }}
	<tt>where</tt> f<sub>1</sub>, f<sub>2</sub>, … <tt>are fields of constructor</tt> K; y <tt>is a new variable</tt> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(n)</td>
 <td><pre><tt>case</tt>  v  <tt>of</tt> {  K  { f  =  p } ->  e ; _ ->  e′ }
  = <tt>case</tt>  v  <tt>of</tt> {
       K p<sub>1</sub> … p<sub>n</sub>  ->  e ; _ ->  e′ }
  <tt>where</tt> p<sub>i</sub> <tt>is</tt> p <tt>if f labels the ith component of</tt> K, _ <tt>otherwise</tt> </pre></td>
 </tr>
 <tr>
 <td class="code-number">(o)</td>
 <td><pre><tt>case</tt>  v  <tt>of</tt> {  K  {} ->  e ; _ ->  e′ }
  = <tt>case</tt>  v  <tt>of</tt> {
       K _… _ ->  e ; _ ->  e′ }</pre></td>
 </tr>
 <tr>
 <td class="code-number">(p)</td>
 <td><pre><tt>case</tt> (K′ e<sub>1</sub> … em) <tt>of</tt> { K x<sub>1</sub> … x<sub>n</sub> -> e; _ -> e′ } = e′
  <tt>where</tt> K <tt>and</tt> K′ <tt>are distinct data constructors of arity</tt> n <tt>and</tt> m<tt>, respectively</tt></pre></td>
 </tr>
 <tr>
 <td class="code-number">(q)</td>
 <td><pre><tt>case</tt> (K e<sub>1</sub> … e<sub>n</sub>) <tt>of</tt> { K x<sub>1</sub> … x<sub>n</sub> -> e; _ -> e′ }
  = (\x<sub>1</sub> … x<sub>n</sub> -> e) e<sub>1</sub> … e<sub>n</sub>
  <tt>where</tt> K <tt>is a data constructor of arity</tt> n </pre></td>
 </tr>
 <tr>
 <td class="code-number">(r)</td>
 <td><pre><tt>case</tt> ⊥ <tt>of</tt> { K x<sub>1</sub> … x<sub>n</sub> -> e; _ -> e′ } =  ⊥
  <tt>where</tt> K <tt>is a data constructor of arity</tt> n</pre></td>
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
 <td><pre><tt>case</tt> () <tt>of</tt> { () | g<sub>1</sub>, …, g<sub>n</sub> -> e; _ -> e′ }
= <tt>case</tt> () <tt>of</tt> {
     () | g<sub>1</sub> -> … <tt>case</tt> () <tt>of</tt> {
                    () | g<sub>n</sub> -> e;
                    _ -> e′ } …
     _ -> e′ }
 where y is a new variable </pre></td>
</tr>
 <td class="code-number">(t)</td>
 <td><pre>case () <tt>of</tt> { () | p <- e<sub>0</sub> -> e; _ -> e′ }
= <tt>case</tt> e<sub>0</sub> <tt>of</tt> { p -> e; _ -> e′ }</pre></td>
</tr>
<tr>
<td class="code-number">(u)</td>
<td><pre><tt>case</tt> () <tt>of</tt> { () | <tt>let</tt> decls -> e; _ -> e′ }
= <tt>let</tt> decls in e </pre></td>
</tr>
<tr>
 <td class="code-number">(v)</td>
 <td><pre><tt>case</tt> () <tt>of</tt> { () | e<sub>0</sub> -> e; _ -> e′ }
  = if e<sub>0</sub> then e else e′ </pre></td>
</tr>
</tobdy>
</table>

**図 3.3:** case式の意味論、パート3
<div class="separator"></div>

図[3.1]("#figure-3.1")-[3.3]("#figure-3.3")の<code>e, e'とe<sub>i</sub></code>は式で、<code>g<sub>i</sub>とgs<sub>i</sub></code>はガードと各々のガードの並びであり、<code>pとp<sub>i</sub></code>はパターン、<code>v, x, x<sub>i</sub></code>は変数、`K,K'`は代数的データ型`(data)`コンストラクタ(タプルコンストラクタを含む)で、`N`は`newtype`コンストラクタである。

ルール(b)は実際にガードを含むかどうかにはかかわらず、一般的な表層ソース言語の`case`式に適合するものである。もしガードが書かれていなければ、その時、`True`が形式<code>match<sub>i</sub></code>内のガード<code>gs<sub>i,j</sub></code>に代用される。各々の識別子はもっと簡単な形式へと`case`式の結果を操作する。

図[3.2]("#figure-3.2")のルール(h)はオーバロードされた`==`演算子を起動し、パターンマッチングの意味をオーバーロードされた定数に対して定義するというルールである。

これらの識別子は静的な意味論を全て保存する。ルール(d)、(e)、(j)、(q)は`let`ではなくラムダを使っていて、これは`case`によって束縛された変数が単相型ということを示す(セクション[4.1.4]("./4-declarations-and-bindings.md")を参照)。
