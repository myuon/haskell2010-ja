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

Haskellは中置記法に対応するため特別な構文を提供している。**演算子**は中置構文を用いて適用が可能である(セクション[3.4]("#3.4"))か、**セクション**(セクション[3.5]("#3.5"))を用いて部分的に適応が可能な関数のことである。

**演算子**は、`+`や`$$`といった**演算子シンボル**か、`op`のようにグレイブ・アクセント(バッククォート)で囲まれた通常の識別子かのいずれかである。例えば、`op x y`という前置適用を書く代わりに、<code>x \`op\` y</code>という中置適応を書くことができる。もし、`op`に対して結合性が宣言されていない場合には、優先順位は最高で左結合をデフォルトとする。(セクション[4.4.2]("./4-declarations-andbindings.md")参照)。

対照的に、演算子シンボルは括弧で閉じられた普通の識別子へ変換可能である。例として、`(+) x y`は`x + y`に等しく、`foldr (⋆) 1 xs`は`foldr (\x y -> x⋆y) 1 xs`に等しくなる。

一部の組み込み型のコンストラクタの名前をつけるのに特別な構文がつかわれているものがあり、実際に`qcon`や`literal`で見ることができる。これらについてはセクション[6.1]("./6-predefined-types-and-classes.md")で説明される。

整数リテラルは`fromInteger`関数を`Integer`型の適した値への適用を表す。同様に、浮動小数点リテラルは`Rational`型(つまり、`Ratio Integer`)の値に`fromRational`を適応することを表す。

<div class="column">

**変換:** 整数リテラル`i`は`fromInteger i`に等しく、`fromInteger`は`Num`クラスのメソッドである。(セクション[6.4.1]("./6-predefined-types-and-classes,md"))

浮動小数点リテラル`f`は`fromRational (n Ratio.% d)`に等しく、`fromRational`は`Fractional`クラスのメソッドで、`Ratio.%`は`Ratio`ライブラリで定義されており、2つの整数から有理数を構築する。整数`n`と`d`は`n/d = f`を満たすものとして選ばれる。

</div>

## カリー化された適用とラムダ抽象

<pre>
<em>fexp</em>	→	[<em>fexp</em>] <em>aexp</em>	            (function application)
<em>lexp</em>	→	\ <em>apat<sub>1</sub></em> … <em>apat<sub>n</sub></em> -> <em>exp</em>	    (lambda abstraction, n ≥ 1)
</pre>

関数適応は<code>e<sub>1</sub> e<sub>2</sub></code>と書く。適応は左結合性をもつので、`(f x) y`の括弧は省略することができる。`e1`はデータ構成子である可能性もあるため、データ構成子の部分的な適応は許されている。

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

<code>e<sub>1</sub> qop e<sub>2</sub></code>という形式は二項演算子`qop`の式<code>e<sub>1</sub></code>と<code>e<sub>2</sub></code>への中置適応である。

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

**セクション**は<code>( <em>op</em> <em>e</em> )</code>や<code>( <em>e</em> <em>op</em> )</code>のように書かれる。このときの`op`は二項演算子で`e`は式である。セクションは二項演算子を部分的に適応する便利な構文である。

シンタックスの先行ルールは次のとおりのセクションへ適応する。(op e) is legal if and only if (x op e) parses in the same way as (x op (e)); and similarly for (e op).例えば、`(⋆a+b)`は構文的に不当であるが、`(+a⋆b)`と`(⋆(a+b))`は有効である。なぜなら`(+)`は左に関連し、`(a+b+)`は構文的に正しいが、`(+a+b)`はそうではない。後者は`(+(a+b))`のように書かれるのが正当である。他の例として、次の式は

<pre><code>
(let n = 10 in n +)
</code></pre>

let/ラムダのメタルール(セクション[3]("./3-expressions.md"))により、誤りである。次の式は

<pre><code>
(let n = 10 in n + x)
</code></pre>

以下のように解析され

<pre><code>
(let n = 10 in (n + x))
</code></pre>

次のようにはならない

<pre><code>
((let n = 10 in n9 + x)
</code></pre>

なぜなら、`-`は文法内で特別に扱われるからだ。前のセクションで説明したように、`(- exp)`はセクションではなく、前置の符号反転演算子の適用である。しかしながら、<code>(<em>subtract</em> exp)</code>は不当なセクションであるように、Prelude内で定義された`subtract`関数はある。式`(+ (- exp))`は同じ用途で役立つことができる。

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

<tt>True</tt>と<tt>False</tt>はPrelude内で定義されている<tt>Bool</tt>型の2つの引数のないコンストラクタである。<code>e<sub>1</sub></code>は<tt>Bool</tt>でなければならず、<code>e<sub>2</sub></code>と<code>e<sub>3</sub></code>も同じ型でなければならない。条件式全体の型も同様である。
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

コンストラクタ`:`はリストコンストラクタとしてのみ予約されており、言語構文の一部と見做されている。また、それは隠すまたは再定義できない。`:`は優先順位レベル5の右結合演算子である(セクション[4.4.2]("./4-declarations-and-bindings.md"))。

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

`(e)`の形式はシンプルに**括弧付き式**であり、`e`と等しい。**単位**式`()`は`()`型を持つ(セクション[4.1.2]("./4-declarations-and-bindings.md")を参照)。それは⊥以外の型のメンバのみで、"引数のないタプル"のように考えられる(セクション[6.1.5]("./6-predefined-types-and-classes.md")を参照)。

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

- 形式`p <- e`の生成機。`p`は型`t`のパターン(セクション[3.17](#3.17))であり、`e`は型`[t]`の式である。
- local bindings that provide new definitions for use in the generated expression e or subsequent boolean guards and generators
- boolean guards, which are arbitrary expressions of type Bool.

このようなリスト内包表記は修飾子リスト内のジェネレータのネストされた深さ優先探索の評価によって作成された連続した環境で`e`を評価することによって生成された要素のリストを返す。変数の束縛は通常のパターンマッチングルール(セクション[3.17]("#3.17"))に従って発生し、もし一致に失敗したら、その時はそのリストの要素は単純にスキップされる。従って、

<pre>
[ x |  xs   <- [ [(1,2),(3,4)], [(5,4),(3,2)] ],  
      (3,x) <- xs ]
</pre>

リスト[4,2]を返す。もし修飾子がboolean guardなら、成功した前のパターンマッチのために真と評価しなけれないけない。As usual, bindings in list comprehensions can shadow those in outer scopes;　例えば以下のようになる。

<pre>
[ x | x <- x, x <- x ] = [ z | y <- x, z <- y]
</pre>

<div class="column">

**変換:** リスト内包表記はこれらの等式を満たし、カーネルの中へ変換するように使われるだろう。

<pre>
[  e | <tt>True</tt> ]	        =	[e]
[  e | q ]	        =	[  e | q, <tt>True</tt> ]
[  e | b,  Q  ]	        =	<tt>if</tt> b <tt>then</tt> [  e | Q ] <tt>else</tt> []
[  e | p <- l,  Q ]	=	<tt>let</tt> <tt>ok</tt> p = [  e | Q ]
                                        <tt>ok</tt> _ = []
                                <tt>in</tt> <tt>concatMap</tt> ok  l
[  e | <tt>let</tt> <em>decls</em>,  Q ]	=	<tt>let</tt> <em>decls</em> <tt>in</tt> [  e | Q ]
</pre>

<em>e</em>は式にわたる範囲で、`p`はパターンにわたり、`l`はリスト値式にわたり、`b`はブーリアン式にわたり、**decls**は宣言リストにわたり、`q`は修飾子にわたり、`Q`は修飾子の順序にわたる範囲をもつ。`ok`は新しい変数である。関数`concatMap`とブーリアン値`True`はPrelude内で定義されている。

</div>

As indicated by the translation of list comprehensions, variables bound by let have fully polymorphic types while those defined by <- are lambda bound and are thus monomorphic (セクション[4.5.4]("./4-declarations-and-bindings.md")を参照).

## Let式

<pre>
<em>lexp</em>	→	<tt>let</tt> <em>decls</em> <tt>in</tt> <em>exp</em>
</pre>

**let**式は一般的な形式<code><tt>let</tt> { <em>d<sub>1</sub></em> ; … ; <em>d<sub>n</sub></em> } <tt>in</tt> <em>e</em></code>を持ち、宣言(<tt>let</tt>は他の言語で<tt>letrc</tt>としばしば呼ばれる)のネスト、語彙的なスコープ、相互再帰的なリストを併せる。宣言の範囲は式`e`と宣言の右側である。宣言は[4章]("./4-declarations-and-bindings.md")で説明される。パターン束縛は遅延的に一致される。an implicit ~ makes these patterns irrefutable. 例えば、

<pre><code><tt>let</tt> (x,y) = <tt>undefined</tt> <tt>in</tt> e
</code></pre>

は`x`または`y`が評価されるまでランタイムエラーをもたらさない。

<div class="column">

**変換:** 式<code><tt>let</tt> { <em>d<sub>1</sub></em> ; … ; <em>d<sub>n</sub></em>} in <em>e<sub>0</sub></em></code>の動的な意味論は次の変換によって捕捉される。全ての型著名を取り除いた後、それぞれの宣言<em>d<sub>i</sub></em>は形式<code>p<sub>i</sub> = e<sub>i</sub></code>の方程式へ変換される。<code>p<sub>i</sub></code>と<code>e<sub>i</sub></code>はセクション[4.4.3]("./4-declarations-and-bindings.md")での変換を使用する、各々のパターンと式である。一度行うと、これらの等式を満たし、カーネルへと変換のように使われるかもしれない。

<pre>
<tt>let</tt> {<em>p<sub>1</sub></em> = <em>e<sub>1</sub></em>;  ... ; <em>p<sub>n</sub></em> = <em>e<sub>n</sub></em>} <tt>in</tt> <em>e<sub>0</sub></em>	=	<tt>let</tt> (<tt>~</tt><em>p<sub>1</sub></em>, ... ,<tt>~</tt><em>p<sub>n</sub></em>) = (<em>e<sub>1</sub></em>, ... ,<em>e<sub>n</sub></em>) in <em>e<sub>0</sub></em>
<tt>let</tt> <em>p</em> = <em>e<sub>1</sub></em>  <tt>in</tt>  e<sub>0</sub>	                =	<tt>case</tt> <em>e<sub>1</sub></em> of <tt>~</tt><em>p</em> -> <em>e<sub>0</sub></em>
                                                where no variable in <em>p</em> appears free in <em>e<sub>1</sub></em>
<tt>let</tt> <em>p</em> = <em>e<sub>1</sub></em>  <tt>in</tt>  <em>e<sub>0</sub></em>	                =	<tt>let</tt> <em>p</em> = <tt>fix</tt> ( \ <tt>~</tt><em>p</em> -> <em>e<sub>1</sub></em>) in <em>e<sub>0</sub></em>
</pre>
`fix`は最小固定小数点演算子である。反論の余地のないパターン`~p`の使用は注意すべきだ。この変換は`case`の使用が束縛される変数の完全な多様体型を排除するため静的な意味論を保存しない。`let`式で束縛された静的な意味論はセクション[4.4.3]("./4-declarations-and-bindings.md")で説明される。
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

(**ガード**の構文ルールについて注目して欲しい。`|`は一つ置きの構文的なメタシンボルではなく終端記号である。)各選択子<code><em>p<sub>i</sub> match<sub>i</sub></em></code>はパターン<em>p<sub>i</sub></em>から成り、<code>match<sub>i</sub></code>と一致する。各一致は順繰りにガード<code>gs<sub>ij</sub></code>と本体<code>e<sub>ij</sub></code>のペアの列から成り、代替となる全てのガードと式上の範囲での付加的な束縛(<code>decls<sub>i</sub></code>)に従う。

**ガード**は次の形式をの一つを持つ。

- **パータンガード**は形式`p <- e`で、`p`は型`t`のパターンで、`e`は式の種類`t`である。もし、式`e`がパターン`p`に一致するなら成功し、パターンの束縛をその環境にもたらす。
- **局地的束縛**は形式<code><tt>let</tt> <em>decls</em></code>である。それらは常に成功し、その環境に`decls`と定義した名前をもたらす。
- **ブーリアンガード**は`Bool`型の数式である。もし、式が`True`と評価するなら成功し、その環境に新しい名前をもたらさない。ブーリアンガード`g`はパターンガード`True <- g`に意味的に等しい。

形式`pat -> exp where decls`の代わりの以下の簡略記法が扱われる。

<pre>
<em>pat</em> | <tt>True</tt> -> <em>exp</em>
<tt>where</tt> <em>decls</em>
</pre>

ケース式は少なくとも1つの選択肢を持たなければならず、各選択肢は一つの実体を持たないといけない。各実体は同じ型を持たなければならず、式全体の型はその型になる。

ケース式は式`e`が個々の選択肢に反するパターンマッチングによって評価される。その選択子は上から下へ連続的に試される。もし、`e`が選択肢のパターンと一致したら、そのとき選択肢のガード式は始めにパターンの一致の間に生成された束縛によって展開されたケース式の環境内で上から下へ連続的に試される。その時、`where`句内の<code>decls<sub>i</sub></code>のよって、その選択肢は関連付けられる。

各ガード式のためにコンマ区切りのガードは左から右へ連続的に試される。もし、そのすべてに成功したなら、そのときは対応する式はガードによって生成された束縛で展開された環境で評価される。すなわち、(let句かパターンガードのいずれかを使った)ガードによって生成された束縛は続くガードと対応する式のスコープ内にある。もし、あらゆるガードが失敗したら、その時はこのガード式は失敗し、次のガード式を試す。

もし選択肢に与えられたガード式の`none`が成功したら、その時マッチングは次の選択肢を続行する。もし、どの選択肢も成功しなければ、そのときの結果は`⊥`となる。パターンマッチングはセクション[3.17]("#3.17")で説明され、ケース式の正式な意味論はセクション[3.17.3]("#3.17.3")で説明される。

解析についての注意点。以下の式は

<pre><code>case x of { (a,_) | let b = not a in b :: Bool -> a }
</code></pre>

正しく解析することが難しい。1つの明白な解析を持っており、すなわち、以下のものになる。

<pre><code>case x of { (a,_) | (let b = not a in b :: Bool) -> a }
</code></pre>

しかしながら、`Bool -> a`というフレーズは型として構文的に正当であり、先読みが制限されているパーサーは、この選択に誤ってコミットする可能性があり、それゆえプログラムは拒否する。故に、プログラマーは型シグネチャで終わるガードを避けるように勧められる。これは実際にガードが`exp`ではなく`infixexp`を含んでいる理由になる。

## Do式

## フィールドラベル付きのデータ型

## 式の型シグネチャ

## パターンマッチング