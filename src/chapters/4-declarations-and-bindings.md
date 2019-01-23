[WIP]

# 宣言と束縛

この章では、Haskellの宣言の構文と簡略した意味論を説明する。

<pre>
module 	→ 	<tt>module</tt> modid [exports] <tt>where</tt> body
	| 	body
body 	→ 	{ impdecls ; topdecls }
	| 	{ impdecls }
	| 	{ topdecls }

topdecls→ 	topdecl<sub>1</sub> ; … ; topdecl<sub>n</sub> 	    (n ≥ 1)
topdecl → 	<tt>type</tt> simpletype = type
	| 	<tt>data</tt> [context =>] simpletype [= constrs] [deriving]
	| 	<tt>newtype</tt> [context =>] simpletype = newconstr [deriving]
	| 	<tt>class</tt> [scontext =>] tycls tyvar [<tt>where</tt> cdecls]
	| 	<tt>instance</tt> [scontext =>] qtycls inst [<tt>where</tt> idecls]
	| 	<tt>default</tt> (type<sub>1</sub> , … , type<sub>n</sub>) 	      (n ≥ 0)
	| 	<tt>foreign</tt> fdecl
	| 	decl

decls 	→ 	{ decl<sub>1</sub> ; … ; decl<sub>n</sub> } 	    (n ≥ 0)
decl 	→ 	gendecl
	| 	(funlhs | pat) rhs

cdecls 	→ 	{ cdecl<sub>1</sub> ; … ; cdecl<sub>n</sub> } 	    (n ≥ 0)
cdecl 	→ 	gendecl
	| 	(funlhs | var) rhs

idecls 	→ 	{ idecl<sub>1</sub> ; … ; idecl<sub>n</sub> } 	    (n ≥ 0)
idecl 	→ 	(funlhs | var) rhs
	| 		    (empty)

gendecl 	→ 	vars :: [context <tt>=></tt>] type 	    (type signature)
	| 	fixity [integer] ops 	    (fixity declaration)
	| 		    (empty declaration)

ops 	→ 	op<sub>1</sub> , … , op<sub>n</sub> 	    (n ≥ 1)
vars 	→ 	var<sub>1</sub> , … , var<sub>n</sub> 	    (n ≥ 1)
fixity 	→ 	<tt>infixl</tt> | <tt>infixr</tt> | <tt>infix</tt>
</pre>

**topdecls**構文的なカテゴリの宣言はHaskellモジュール([5章]("./5-modules.md"))の最上位のみ許す一方で**decls**は最上位またはネストされたスコープ(例えば、`let`か`where`の内で`topdecls`を構築する)のいずれかで使われるかもしれない。

説明のため、`type`と`newtype`、`data`宣言からなるユーザー定義のデータ型(セクション[4.2](”#4.2”))と`class`と`instance`、`default`宣言からなる型クラスとオーバーロード(セクション[4.3]("#4.3"))、値束縛と型シグネチャ、固定の宣言からなるネストされた宣言(セクション[4.4]("#4.4"))の3つのグループに宣言を分割する。

haskellは(整数や浮動小数点数のような)"ハード・ワイヤード"であるいくつかのプリミティブなデータ型を持つ。しかし、多くの"ビルドイン"なデータ型は通常のHaskellコードによって定義されていて、通常<code>type</code>や<code>data</code>宣言に使われる。これらの"ビルドイン"のデータ型はセクション[6.1]("./6-predefined-types-and-classes.md")で詳細に説明される。

## 型とクラスの概要

Haskellは静的型意味論[4.6]("#4.6")を提供するために伝統的なHindley-Milner多形型システムを使用するが、その型システムは構造化された歩法にオーバーロード関数を導入するために提供する**型クラス**(または**クラス**)で拡張されている。

<code><tt>class</tt></code>宣言(セクション[4.3.1](”#4.3.1”))は新しい**型クラス**とあらゆるそのクラスのインスタンスの型によってもサポートされなければいけないオーバーロードされた操作を導入する。<code><tt>instance</tt></code>宣言(セクション[4.3.2]("#4.3.2"))は型がクラスのインスタンスであり、名前付き型でインスタンス化されたオーバーロードされた操作、クラスメソッドと呼ぶ、の定義を含むということを宣言する。

例えば、型`Int`と`Float`で操作`(+)`と`negate`をオーバーロードしたいと考えたとしよう。`Num`と呼ばれる新しい型クラスを導入する。

<pre><code>class Num a  where          -- simplified class declaration for Num  
  (+)    :: a -> a -> a     -- (Num is defined in the Prelude)  
  negate :: a -> a
</code></pre>

この宣言は「型`a`がもし与えられた型で定義されたクラスメソッド`(+)`と`negate`があるならクラス`Num`のインスタンスである」と読まれるかもしれない。

このクラスのインスタンス化するときに`Int`と`Float`をその時宣言するかもしれない。

<pre><code>instance Num Int  where     -- simplified instance of Num Int  
   x + y       =  addInt x y  
   negate x    =  negateInt x  

 instance Num Float  where   -- simplified instance of Num Float  
   x + y       =  addFloat x y  
   negate x    =  negateFloat x
</code></pre>

`addInt`や`negateInt`、`addFloat`、`negateFloat`はこのケースでプリミティブ関数で想定されるが、一般的にはユーザー定義関数になり得る。上のはじめの宣言は「`Int`は`(+)`と`negate`という定義が見られるようにクラス`Num`のインスタンスである」と読まれるかもしれない。

型クラスのより多くの例はJones[8]("./../bibliography.md")かWadlerとBlott[13]("./../bibliography.md")による論文で見つけられる。用語'型クラス'はオリジナルのHaskell1.0型システムを記述するために使われてあって、'コンストラクタクラス'はオリジナルの型クラスへ拡張を記述することに使われていた。ふたつの異なる用語を使う理由はもはやなく、このリポートでは、'型クラス'がJonesによって導入されたオリジナルのHaskell型クラスとコンストラクタクラスの両方を含んでいる。

### 種類

宣言が正常であることが確かであることは、型式は異なる`種類`へと分類され、以下の2つの可能な形式の内、1つを取る。

- シンボル`*`は全ての引数のない型コンストラクタの種類を意味する。
- もし<code>K<sub>1</sub></code>と<code>K<sub>2</sub></code>が種類ならば、<code>K<sub>1</sub> → K<sub>2</sub></code>は種類<code>K<sub>1</sub></code>の型を取り、<code>K<sub>2</sub></code>の型を返す型の種類である。

種類の推論は型推論が値式の正常性を確認するのと同様に型式の正常性を確認する。しかしながら、型のようではなく、種類は完全に暗黙に示され、言語の可視化された部分はない。種類の推論はセクション[4.6]("#4.6")で議論される。

### 型の構文

<pre>
type 	→ 	btype [-> type]         (function type)

btype 	→ 	[btype] atype           (type application)

atype 	→ 	gtycon
	| 	tyvar
	| 	( type1 , … , typek )   (tuple type, k ≥ 2)
	| 	[ type ] 	        (list type)
	| 	( type ) 	        (parenthesised constructor)

gtycon 	→ 	qtycon
	| 	() 	                (unit type)
	| 	[] 	                (list constructor)
	| 	(->) 	                (function constructor)
	| 	(,{,}) 	                (tupling constructors)
</pre>

Haskellの型式のための構文は上に与えられる。データ値と同じようにデータコンストラクタを使って作られる、型値は`型コンストラクタ`から作られる。データコンストラクタと同様に、型コンストラクタの名前は大文字で始められる。データコンストラクタとは違い、中置型コンストラクタは許されない(`(->)以外`)。

型式の主要な形式は次のものになる。

1. 小文字で始まる識別子のように書かれた型変数。変数の種類は現れた文脈によって暗黙的に決定される。
1. 型コンストラクタ。多くの型コンストラクタは大文字から始まる識別子のように書かれている。
    例えば、
    - `Char`や`Int`,`Float`,`Double`、`Bool`は種類`*`で構築される型である。
    - `Maybe`と`IO`は単項型コンストラクタで、種類`*→*`を使った型として扱われる。
    - 宣言`data T ...`または`newtype T ...`は型の単語編に型コンストラクタ`T`を追加する。`T`の種類は種類の推論によって決定される。

    特殊な構文は特定のビルドインの型コンストラクタに提供される。
    - `ありふれた型`は`()`のように書かれ、種類`*`を持つ。それは`引数のないタプル`型を示し、しっかりとある値を持ち、`()`とも書かれる(セクション[3.9]("#3.9")と[6.1.5]("./6-predefined-types-and-classes.md")を参照)。
    - `関数型`は`(->)`のように書かれ、種類`∗→∗→∗`を持つ。
    - `リスト型`は`[]`のように書かれ、種類`∗→∗`を持つ。
    - `タプル型`は`(,), (,,)`等のように書かれる。それらの種類は`∗→∗→∗, ∗→∗→∗→ ∗`などである。

    `(->)`と`[]`の定数の使用は下でより詳しく説明される。
1. 型適応。もし、<code>t<sub>1</sub></code>が種類<code>K<sub>1</sub> → K<sub>2</sub></code>の型で<code>t<sub>2</sub></code>が種類<code>K<sub>1</sub></code>の型であるなら、その時<code>t<sub>1</sub>, t<sub>2</sub></code>は種類<code>K<sub>2</sub></code>の型式である。
1. `括弧つき型`、形式`(t)`を持つ、型`t`と同一である。

例えば、型式`IO a`は変数`a`に定数`IO`への適応のように理解されることができる。`IO`型コンストラクタは種類`∗→∗`を持ち、変数`a`と式全体の両方を従え、式`IO a`は種類`*`を持たなければならない。一般的に`型の推論`(セクション[4.6]("#4.6"))の処理は適切な種類をユーザー定義のデータ型や型の同意語、クラスへ決定することを必要とされる。

特別な構文は特定の型式がより伝統的なスタイルで書かれることを許すために提供される。

1. `関数型`は形式<code>t<sub>1</sub> -> t<sub>2</sub></code>を持ち、型<code>(->) t<sub>1</sub> t<sub>2</sub></code>に等しい。アロー関数は左に関連づける。例えば、`Int -> Int -> Float`は`Int -> (Int -> Float)`を意味する。
1. `タプル型`は`K ≥ 2`の形式<code>t<sub>1</sub>, ..., t<sub>k</sub></code>を持ち、括弧の間に`k-1`個のカンマがある型<code>(,…,) t<sub>1</sub> … t<sub>k</sub></code>と等しい。それは型<code>t<sub>1</sub></code>をはじめの要素に、型<code>t<sub>2</sub></code>を2番目の要素に持つ等など、`k要素のタプル`の型を示す(セクション[3.8]("./3-expressions.md")と[6.1.4]("./6-predefined-types-and-classes.md")を参照)。
1. `リスト型`は形式`[t]`を持ち、型`[] t`と等しい。それは型`t`の要素に持つリストの方を示す(セクション[3.7]("./3-expressions.md")と[6.1.3]("./6-predefined-types-and-classes.md")を参照)。

これらの特別な構文的形式はスコープに関係なく関数、タプル、リストのビルドイン型のコンストラクタを常に示す。同様な方法で、接頭型のコンストラクタ`(->), [], (), (,)`等はビルドイン型のコンストラクタを常に示す。それらは資格が与えらることができず、そしてまたリストのインポートまたはエクスポートにに言及されることもできない([5章]("./5-modules.md"))。(上述の特殊な生産"gtycon"から)

リストとタプル型が特別な構文を持つのだが、それらの意味論はユーザー定義された代数データ型と同じである。

式と型は一貫した構文を持つことに注意してください。もし、<code>t<sub>i</sub></code>は式またはパターン<code>e<sub>i</sub></code>の型なら、その時式<code>(\ e<sub>1</sub> -> e<sub>2</sub>), [e<sub>1</sub>]</code>と<code>(e<sub>1</sub>,e<sub>2</sub>)</code>は各々型<code>(t<sub>1</sub> -> t<sub>2</sub>), [t<sub>1</sub>]</code>と<code>(t<sub>1</sub>,t<sub>2</sub>)</code>を持つ。

ひとつの例外(クラス宣言内の顕著な型変数のこと(セクション[4.3.1]("#4.3.1")))を除いて、Haskellの型式内の型変数は例外なく量で定められると全て仮定され、一般的な定量化のための明示的な構文はない[4]("./../bibliography.md")。例えば、型式`a -> a`は型`∀ a. a  →  a`を示す。明確にするために、しかしながら、Haskellプログラムの方を議論する時に明示的な定量化をしばしば書く。明示的に数量化型を書く時、`∀`のスコープは可能な限り左側へ拡張する。例として、`∀ a. a  →  a `は`∀ a. (a  →  a)`を意味する。

### クラス表明と文脈の構文

<pre>
context → 	class
	| 	( class1 , … , classn ) 	    (n ≥ 0)
class 	→ 	qtycls tyvar
	| 	qtycls ( tyvar atype1 … atypen )    (n ≥ 1)
qtycls 	→ 	[ modid . ] tycls
tycls 	→ 	conid
tyvar 	→ 	varid
</pre>

**クラス表明** は形式**qtycls tyvar**を持ち、クラス`qtycls`の型**tyvar**のメンバを示す。クラス識別子は大文字で始める。`内容`は0個以上のクラス表明からなり、<code>( C<sub>1</sub>, …, C<sub>n</sub></code>がクラス識別子である一般的な形式<code>( C<sub>1</sub> u<sub>1</sub>, …, C<sub>n</sub> u<sub>n</sub> )</code>を持つ。<code>( u<sub>1</sub>, …, u<sub>n</sub></code>の各々は変数型か一つ以上の型への変数型の適応のいずれかである。括弧の外側は`n = 1`のとき省かれるかもしれない。一般的に、内容を示すために`cx`を使用し、`cx => t`を内容`cx`によって型制限された型`t`を示すために書く。内容`cx`は`t`によって参照される変数型のみを含まなければいけない。利便性のために、内容`cx`が空であっても、具体的な構文は`=>`を含まないケースであるが、`cx => t`を書く。

### 型とクラスの意味論

このセクションは、型システムの簡略的な詳細を提供する。(WadlerとBlott[[13]]("./../bibliography.md")、Jones[[8]]("./../bibliography.md")は各々より詳細に型とコンストラクタクラスを議論している。)

Haskellの型システムは`型`をプログラム内の各式に帰する。一般的に、型は形式`∀ u. cx  ⇒  t`である。`u`は変数型の集合<code>u<sub>1</sub>, ..., u<sub>n</sub></code>である。どのような型でも、`cx`の中で開放される一般的な定量化された変数型<code>u<sub>i</sub></code>は`t`の中でも開放されなければならない。その上、内容`cx`はセクション[4.1.3]("#4.1.3")上で与えられた形式でなければならない。例として、ここにいくつかの正常な型がある。

<pre><code>Eq a => a -> a  
(Eq a, Show a, Eq b) => [a] -> [b] -> String  
(Eq (f a), Functor f) => (a -> b) -> f a -> f b -> Bool
</code></pre>

3つの型で、拘束`Eq (f a)`は`f`が一般的に定量化されているためもっと単純にしなければならない。

式`e`の型は型を`e`に開放する変数へ与える**型環境**とどの型がどのクラスのインスタンスであるかを宣言する**クラス環境**に依存する。(型は`インスタンス`宣言または`導出`節の存在経由のみのクラスのインスタンスになる。)

## ユーザー定義のデータ型

### 代数データ型宣言

### 型同意語の宣言

### データ型のリネイム

## 型クラスとオーバーロード

### クラス宣言

### インスタンス宣言

### 派生インスタンス

### 不明瞭な型とオーバーロードされた数値オペレータの既定値

## ネストされた宣言

### 型シグネチャ

### 固定の宣言

### 関数とパターン束縛

#### 関数束縛

#### パターン束縛

## 関数とパターン束縛の静的な意味論

### 依存の解析

### 一般化

### コンテキストの削減エラー

### 単相性

### 単相性の制限

## 種類の推論
