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

**変換：** 以下の同一性は保持される。
<pre>
e<sub>1</sub> op e<sub>2</sub>  =	(op) e<sub>1</sub> e<sub>2</sub>
-e        =	<em>negate</em> (e)
</pre>

</div>

## セクション

## 条件文

## リスト

## タプル

## 単位式と括弧付き式

## 数列

## リスト内包

## Let式

## Case式

## Do式

## フィールドラベル付きのデータ型

## 式の型シグネチャ

## パターンマッチング