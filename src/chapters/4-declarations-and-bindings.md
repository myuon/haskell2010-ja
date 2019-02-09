# 宣言と束縛

この章では、Haskellの<em>宣言</em>の構文と簡略した意味論を説明する。

|||||
|---|---|---|---|
|<em>module</em>| → |`module` <em>modid</em> [<em>exports</em>] `where` <em>body</em>| |
|      |&#124;|<em>body</em>| |
|  <em>body</em>| → |{ <em>impdecls</em> ; <em>topdecls</em> }| |
|      |&#124;|{ <em>impdecls</em> }| |
|      |&#124;|{ <em>topdecls</em> }| |
|||||
  |<em>topdecls</em>| → |<em>topdecl<sub>1</sub></em> ; … ; <em>topdecl<sub>n</sub></em>|(<em>n</em> ≥ 1)|
| <em>topdecl</em>| → |`type` <em>simpletype</em> = <em>type</em>| |
|        |&#124;|`data` [<em>context</em> =>] <em>simpletype</em> [= <em>constrs</em>] [<em>deriving</em>]| |
|        |&#124;|`newtype` [<em>context</em> =>] <em>simpletype</em> = <em>newconstr</em> [<em>deriving</em>]| |
|        |&#124;|`class` [<em>scontext</em> =>] <em>tycls</em> <em>tyvar</em> [`where` <em>cdecls</em>]| |
|        |&#124;|`instance` [<em>scontext</em> =>] <em>qtycls</em> <em>inst</em> [`where` <em>idecls</em>]| |
|        |&#124;|`default` (<em>type<sub>1</sub></em> , … , <em>type<sub>n</sub></em>)|(<em>n</em> ≥ 0)|
|        |&#124;|`foreign` <em>fdecl</em>| |
|        |&#124;|<em>decl</em>| |
|||||
|  <em>decls</em>| → |{ <em>decl<sub>1</sub></em> ; … ; <em>decl<sub>n</sub></em> }|(<em>n</em> ≥ 0)|
|   <em>decl</em>| → |<em>gendecl</em>| |
|       |&#124;|(<em>funlhs</em> &#124; <em>pat</em>) <em>rhs</em>| |
|||||
| <em>cdecls</em>| → |{ <em>cdecl<sub>1</sub></em> ; … ; <em>cdecl<sub>n</sub></em> }|(<em>n</em> ≥ 0)|
|  <em>cdecl</em>| → |<em>gendecl</em>| |
|       |&#124;|(<em>funlhs</em> &#124; <em>var</em>) <em>rhs</em>| |
|||||
| <em>idecls</em>| → |{ <em>idecl<sub>1</sub></em> ; … ; <em>idecl<sub>n</sub></em> }|(<em>n</em> ≥ 0)|
|  <em>idecl</em>| → |(<em>funlhs</em> &#124; <em>var</em>) <em>rhs</em>| |
|       |&#124;|  |(empty)|
|<em>gendecl</em>|→|<em>vars</em> :: [<em>context</em> <tt>=></tt>] <em>type</em>|(type signature)|
|       |&#124;|<em>fixity</em> [<em>integer</em>] <em>ops</em>|(fixity declaration)|
|       |&#124;|  |(empty declaration)|
|||||
|    <em>ops</em>|→|<em>op<sub>1</sub></em> , … , <em>op<sub>n</sub></em>|(<em>n</em> ≥ 1)|
|   <em>vars</em>|→|<em>var<sub>1</sub></em> , … , <em>var<sub>n</sub></em>|(<em>n</em> ≥ 1)|
| <em>fixity</em>|→|`infixl` &#124; `infixr` &#124; `infix`| |

構文的カテゴリ<em>topdecls</em>に属する宣言はHaskellモジュール([5章](./5-modules.md))の最上位のみ許す一方で <em>decls</em>は最上位またはネストされたスコープのいずれかで使ってもよい(例えば、`let`か`where`の内で<em>topdecls</em>を構築する)。

説明のため、`type`と`newtype`、`data`宣言からなるユーザー定義のデータ型(セクション[4.2](#aユーザー定義のデータ型))と`class`と`instance`、`default`宣言からなる型クラスとオーバーロード(セクション[4.3](#a型クラスとオーバーロード))、値束縛と型シグネチャ、固定の宣言からなるネストされた宣言(セクション[4.4](#aネストされた宣言))の3つのグループに宣言を分割する。

haskellは(整数や浮動小数点数のような)"ハードウェアで実現された"であるいくつかのプリミティブなデータ型を持つ。しかし、多くの"ビルドイン"なデータ型は通常のHaskellコードによって定義されていて、通常`type`や`data`宣言に使われる。これらの"ビルドイン"のデータ型はセクション[6.1](./6-predefined-types-and-classes.md#a標準Haskell型)で詳細に説明される。

## 型とクラスの概要

Haskellは静的型意味論[4](../bibliography.md),[6](../bibliography.md)を提供するために伝統的なHindley-Milner多相型システムを使用するが、その型システムは構造化された手法にオーバーロード関数を導入するために提供する<em>型クラス</em>(または<em>クラス</em>)で拡張されている。

`class`宣言(セクション[4.3.1](#aクラス宣言))は新しい<em>型クラス</em>とあらゆるそのクラスのインスタンスの型によってもサポートされなければいけないオーバーロードされた操作を導入する。`instance`宣言(セクション[4.3.2](#インスタンス宣言))は型がクラスの<em>インスタンス</em>であり、名付けられた型でインスタンス化されるオーバーロードされたオペレーション、クラスメソッドと呼ばれる、の定義を含むということを宣言する。

例えば、型`Int`と`Float`で操作`(+)`と`negate`をオーバーロードしたいと考えたとしよう。`Num`と呼ばれる新しい型クラスを導入する。

```hs
class Num a  where          -- Numの単純化されたクラス宣言
  (+)    :: a -> a -> a     -- (NumはPreludeで定義されている)  
  negate :: a -> a
```

この宣言は「型`a`がもし与えられた型で定義されたクラスメソッド`(+)`と`negate`があるならクラス`Num`のインスタンスである」と読めるであろう。

このクラスのインスタンス化のときに`Int`と`Float`をその際に宣言できる。

```hs
instance Num Int  where     -- Num Intの単純化されたインスタンス
   x + y       =  addInt x y  
   negate x    =  negateInt x  

 instance Num Float  where   -- Num Floatの単純化されたインスタンス
   x + y       =  addFloat x y  
   negate x    =  negateFloat x
```

`addInt`や`negateInt`、`addFloat`、`negateFloat`はこのケースでプリミティブ関数で想定されるが、一般的にはユーザー定義関数になり得る。
上のはじめの宣言は「`Int`はクラス`Num`のインスタンスであり、その証拠として(クラスメソッド)`(+)`と`negate`が定義されている」と読まれる。

型クラスのより多くの例はJones[8](../bibliography.md)かWadlerとBlott[13](../bibliography.md)による論文で見つけられる。用語'型クラス'はオリジナルのHaskell1.0型システムを記述するために使われてあって、'コンストラクタクラス'はオリジナルの型クラスへ拡張を記述することに使われていた。ふたつの異なる用語を使う理由はもはやなく、この規格書において、'型クラス'という単語は元々のHaskell型クラスとJonesによって導入されたコンストラクタクラスの両方を含んでいる。

### 種

型の表現が有効である確証を得るために、型の表現を異なる<em>種(カインド, kind)</em>へと分類され、以下の2つの可能な形式の内、1つを取る。

- シンボル`*`は全ての引数のない型コンストラクタの種を意味する。
- もし<em>K<sub>1</sub></em>と<em>K<sub>2</sub></em>が種ならば、<em>K<sub>1</sub> → K<sub>2</sub></em>は種<em>K<sub>1</sub></em>の型を取り、<em>K<sub>2</sub></em>の型を返す型の種である。

型推論が値の表現の正当性をチェックするのと同様にして、種推論は型の表現の正当性をチェックする。しかしながら、型とは違い、種は完全に暗黙的であり、言語の見て分かる部分には存在しない。種の推論はセクション[4.6](#a種の推論)で議論される。

### 型の構文

|||||
|---|---|---|---|
|<em>type</em>| → |<em>btype</em> [-> <em>type</em>]|(function type)|
|||||
|<em>btype</em>|→|[<em>btype</em>] <em>atype</em>|(type application)|
|||||
|<em>atype</em>|→|<em>gtycon</em>| |
|  |&#124;|<em>tyvar</em>| |
|  |&#124;|( <em>type<sub>1</sub></em> , … , <em>type<sub>k</sub></em> )|(tuple type, <em>k</em> ≥ 2)|
|  |&#124;|[ <em>type</em> ]|(list type)|
|  |&#124;|( <em>type</em> )|(parenthesised constructor)|
|||||
|<em>gtycon</em>|→|<em>qtycon</em>| |
|  |&#124;|()|(unit type)|
|  |&#124;|[]|(list constructor)|
|  |&#124;|(->)|(function constructor)|
|  |&#124;|(,{,})|(tupling constructors)|

Haskellの型の表現のための構文は上に与えられる。データ値と同じようにデータコンストラクタを使って作られ、型の値(訳注: 型の表現でそれ以上簡約出来ないもののこと)は<em>型コンストラクタ</em>から作られる。データコンストラクタと同様に、型コンストラクタの名前は大文字で始められる。データコンストラクタとは違い、中置型コンストラクタは許されない(`(->)`以外)。

型の表現の主要な形式は次のものになる。

1. 小文字で始まる識別子のように書かれた型変数。変数の種は現れた文脈によって暗黙的に決定される。
1. 型コンストラクタ。多くの型コンストラクタは大文字から始まる識別子のように書かれている。
    例えば、
    - `Char`や`Int`,`Float`,`Double`、`Bool`は種`*`で構築される型である。
    - `Maybe`と`IO`は単項型コンストラクタで、種`*→*`をもつ型として扱われる。
    - 宣言`data T ...`または`newtype T ...`は型のボキャブラリーに型コンストラクタ`T`を追加する。`T`の種は種の推論によって決定される。

    特殊な構文は特定のビルドインの型コンストラクタに提供される。
    - <em>自明型</em>は`()`のように書かれ、種`*`を持つ。それは"引数のないタプル"型を示し、`()`と書かれるが、値をちゃんと持つ(セクション[3.9](./3-expressions.md#a単位式と括弧付き式)と[6.1.5](./6-predefined-types-and-classes.md#a)を参照)。
    - <em>関数型</em>は`(->)`のように書かれ、種`∗→∗→∗`を持つ。
    - <em>リスト型</em>は`[]`のように書かれ、種`∗→∗`を持つ。
    - <em>タプル型</em>は`(,), (,,)`等のように書かれる。それらの種は`∗→∗→∗, ∗→∗→∗→ ∗`などである。

    `(->)`と`[]`の定数の使用は下でより詳しく説明される。
1. 型適用。もし、<em>t<sub>1</sub></em>が種<em>K<sub>1</sub> → K<sub>2</sub></em>の型で<em>t<sub>2</sub></em>が種<em>K<sub>1</sub></em>の型であるなら、その時<em>t<sub>1</sub>, t<sub>2</sub></em>は種<em>K<sub>2</sub></em>の型の表現である。
1. <em>括弧つき型</em>、形式<em>(t)</em>を持つ、型<em>t</em>と同一である。

例えば、型の表現`IO a`は変数`a`に定数`IO`への適用のように理解されることが可能だ。`IO`型コンストラクタは種`∗→∗`を持ち、変数`a`と式全体の両方を従え、式`IO a`は種`*`を持たなければならない。一般的に<em>型の推論</em>(セクション[4.6](#a種の推論))の処理は適切な種をユーザー定義のデータ型や型のシノニム、クラスへ決定することを必要とされる。

特別な構文は特定の型の表現がより伝統的なスタイルで書かれることを許すために提供される。

1. <em>関数型</em>は形式<em>t<sub>1</sub> -> t<sub>2</sub></em>を持ち、型<em>(->) t<sub>1</sub> t<sub>2</sub></em>に等しい。アロー関数は左に関連づける。例えば、`Int -> Int -> Float`は`Int -> (Int -> Float)`を意味する。
1. <em>タプル型</em>は<em>K</em> ≥ 2である形式<em>t<sub>1</sub>, ..., t<sub>k</sub></em>を持ち、括弧の間に<em>k-1</em>個のカンマがある型<em>(,…,) t<sub>1</sub> … t<sub>k</sub></em>と等しい。それは型<em>t<sub>1</sub></em>をはじめの要素に、型<em>t<sub>2</sub></em>を2番目の要素に持つなど、<em>k</em>要素のタプルの型を示す(セクション[3.8](./3-expressions.md#aタプル)と[6.1.4](./6-predefined-types-and-classes.md#a)を参照)。
1. <em>リスト型</em>は形式<em>[t]</em>を持ち、型<em>[] t</em>と等しい。それは型<em>t</em>の要素を伴うリストの型を示す(セクション[3.7](./3-expressions.md#aリスト)と[6.1.3](./6-predefined-types-and-classes.md#a)を参照)。

これらの特別な構文的形式は何がスコープに入っているかにかかわらず関数、タプル、リストのビルドイン型のコンストラクタを常に示す。同様な方法で、プリフィックスな型コンストラクタ`(->), [], (), (,)`等はビルドインの型コンストラクタを常に示す。それらは修飾子を付けることはできず、そしてまたリストのimport/exportするもののリストに入れることもできない([5章](./5-modules.md#a))。(上述の特定の生成規則、"gtycon"から)

リストとタプル型が特別な構文を持つのだが、それらの意味論はユーザー定義された代数データ型と同じである。

式と型は一貫した構文を持つことに注意すること。もし、<em>t<sub>i</sub></em>は式またはパターン<em>e<sub>i</sub></em>の型ならその時、式<em>(\ e<sub>1</sub> -> e<sub>2</sub>), [e<sub>1</sub>]</em>と<em>(e<sub>1</sub>,e<sub>2</sub>)</em>は各々、型<em>(t<sub>1</sub> -> t<sub>2</sub>), [t<sub>1</sub>]</em>と<em>(t<sub>1</sub>,t<sub>2</sub>)</em>を持つ。

ひとつの例外(クラス宣言内の区別された型変数のこと(セクション[4.3.1](#aクラス宣言)))を除いて、Haskellの型の表現内の型変数は一般に全て全称量化されていると仮定され、全称量化のための明示的な文法はない[4](./../bibliography.md)。例えば、型の表現`a -> a`は型`∀ a. a  →  a`を示す。明確にするために、しかしながら、Haskellプログラムの型を議論する時に明示的な個々の区分をしばしば書く。明示的に個々に区分された型を書く時、`∀`のスコープは可能な限り左側へ拡張する。例として、`∀ a. a  →  a `は`∀ a. (a  →  a)`を意味する。

### クラス表明と文脈の構文

|||||
|--|--|--|--|
|<em>context</em>| → |<em>class</em>| |
|       | &#124; |( <em>class<sub>1</sub></em> , … , <em>class<sub>n</sub></em> )|(<em>n</em> ≥ 0)|
| <em>class</em> | → |<em>qtycls</em> <em>tyvar</em>| |
|       | &#124; |<em>qtycls</em> ( <em>tyvar</em> <em>atype<sub>1</sub></em> … <em>atype<sub>n</sub></em> )|(<em>n</em> ≥ 1)|
| <em>qtycls</em>| → |[ <em>modid</em> . ] <em>tycls</em>| |
|  <em>tycls</em>| → |<em>conid</em>| |
|  <em>tyvar</em>| → |<em>varid</em>| |

<em>クラス表明</em>は形式<em>qtycls tyvar</em>を持ち、クラス<em>qtycls</em>の型<em>tyvar</em>のメンバを示す。クラス識別子は大文字で始める。<em>文脈</em>は0個以上のクラス表明からなり、一般に<em>( C<sub>1</sub> u<sub>1</sub>, …, C<sub>n</sub> u<sub>n</sub> )</em>の形式をもつ。ここで<em>C<sub>1</sub>, …, C<sub>n</sub></em>はクラス識別子であり、<em>( u<sub>1</sub>, …, u<sub>n</sub>)</em>はそれぞれ変数型または一つ以上の型への変数型の適用のいずれかである。<em>n = 1</em>のとき括弧の外側は省かれるであろう。一般的に、文脈を示すために<em>cx</em>を使用し、<em>cx => t</em>を文脈<em>cx</em>によって制限された型<em>t</em>を示すために書く。文脈<em>cx</em>は<em>t</em>によって参照される変数型のみを含まなければいけない。利便性のために、文脈<em>cx</em>が空であっても、具体的な構文は<em>=></em>を含まないケースであるが、<em>cx => t</em>を書く。

### 型とクラスの意味論

このセクションは、型システムの簡略的な詳細を提供する。(WadlerとBlott[[13]](../bibliography.md)、Jones[[8]](../bibliography.md)は各々より詳細に型とコンストラクタクラスを議論している。)

Haskellの型システムは<em>型</em>をプログラム内の各式に割り当てる。一般的に、型は形式<code>∀ <em><span class="overline">u</span>. cx  ⇒  t</em></code>である。<em><span class="overline">u</span></em>は変数型の集合<em>u<sub>1</sub>, ..., u<sub>n</sub></em>である。どのような型であっても、<em>cx</em>に束縛がない一般的な個々に区別された変数型<em>u<sub>i</sub></em>は<em>t</em>でも束縛がないものでなければならない。その上、内容<em>cx</em>はセクション[4.1.3](#aクラス表明と文脈の構文)上で与えられた形式でなければならない。例として、ここにいくつかの正常な型がある。

```hs
Eq a => a -> a  
(Eq a, Show a, Eq b) => [a] -> [b] -> String  
(Eq (f a), Functor f) => (a -> b) -> f a -> f b -> Bool
```

3つの型で、制約`Eq (f a)`は`f`が全称量化されているためもっと単純にはできない。

式<em>e</em>の型は型を<em>e</em>に束縛のない変数へ与えるため<em>型環境</em>に依存し、いずれかの型を宣言する<em>クラス環境</em>はいずれかのクラスのインスタンスである。(型は`インスタンス`宣言または`派生`節の存在によってのみクラスのインスタンスになる。)

型は一般化による半順序集合(下に明記される)で関連する。多くの一般的な型は、一般化の先行順によって同等まで導かれ、(与えられた環境の)個々の式は<em>主要な型</em>と呼ばれるものに割り当てられる。Haskellの拡張されたHindley-Milner型システムは全式の主要な型を推論でき、オーバーロードされたクラスメソッドの妥当な使用を含んでいる(セクション[4.3.4](#a不明瞭な型とオーバーロードされた数値オペレータの既定値)で説明するように、確実に曖昧なオーバーロードが起こり得るのだが)。したがって、明示的な型付け(型シグネチャと呼ぶ)は通常、オプションである(セクション[3.16](./3-expressions,md#a式の型シグネチャ)と[4.4.1](#a型シグネチャ)を参照)。

型<code>∀ <em><span class="overline">u</span>. cx<sub>1</sub>  ⇒  t<sub>1</sub></em></code>は領域が以下のような<em><span class="overline">u</span></em>の代用<em>S</em>がある場合に限り、型<code>∀ <em><span class="overline">w</span>. cx<sub>2</sub>  ⇒  t<sub>2</sub></em></code><em>より一般的</em>である。

- <em>t<sub>2</sub></em>は<em>S(t<sub>1</sub>)</em>と同じである。
- <em>cx<sub>2</sub></em>はそのクラスの環境を保持し、<em>S(cx<sub>1</sub>)</em>も保持する。

型<code>∀ <em><span class="overline">u</span>. cx  ⇒  t</em></code>の値は内容<em>cx[<span class="overline">s</span>;/<span class="overline">u</span>]</em>を保持する場合に限り型<em><span class="overline">s</span></em>でインスタンス化されるかもしれない。例えば、関数`double`について考えてみる。

```hs
double x = x + x
```

`double`の最も一般的な型は<code>∀ <em>a. Num a⇒  a  →  a</em></code>である。`double`は(`Int`にインスタンス化する)型`Int`の値に適用されるかもしれない、なぜなら`Num Int`が成り立つ、すなわち`Int`はクラス`Num`のインスタンスだからである。しかしながら、`double`が型`Char`の値に通常の意味で適用されることはないであろう。なぜなら、`Char`は通常クラス`Num`のインスタンスではないからだ。ユーザーはインスタンスのような宣言を選択するかもしれない。その場合、`double`が型`Char`の値に通常の意味で適用されることはないであろう。

## ユーザー定義のデータ型

このセクションでは、代数のデータ型(`data`宣言)や新たに命名するデータ型(`newtype`宣言)、型のシノニム(`type`宣言)を説明する。これらの宣言はモジュールのトップレベルでのみ現れてよい。

### 代数データ型宣言

|||||
|---|---|---|---|
|<em>topdecl</em>|→|`data` [<em>context</em> =>] <em>simpletype</em> [= <em>constrs</em>] [<em>deriving</em>]| |
|||||
|<em>simpletype</em>|→|<em>tycon</em> <em>tyvar<sub>1</sub></em> … <em>tyvar<sub>k</sub></em>|(<em>k</em> ≥ 0)|
|||||
|<em>constrs</em>|→|<em>constr<sub>1</sub></em> &#124; … &#124; <em>constr<sub>n</sub></em>|(<em>n</em> ≥ 1)|
| <em>constr</em>|→|<em>con</em> [`!`] <em>atype<sub>1</sub></em> … [`!`] <em>atype<sub>k</sub></em>|(arity <em>con</em> = <em>k</em>, <em>k</em> ≥ 0)|
|  |&#124;|(<em>btype</em> &#124; `!` <em>atype</em>) <em>conop</em> (<em>btype</em> &#124; `!` <em>atype</em>)|(infix <em>conop</em>)|
|  |&#124;|<em>con</em> { <em>fielddecl<sub>1</sub></em> , … , <em>fielddecl<sub>n</sub></em> }|(<em>n</em> ≥ 0)|
|<em>fielddecl</em>|→|<em>vars</em> `::` (<em>type</em> &#124; `!` <em>atype</em>)| |
|||||
| <em>deriving</em>|→|`deriving` (<em>dclass</em> &#124; (<em>dclass<sub>1</sub></em>, … , <em>dclass<sub>n</sub></em>))|(<em>n</em> ≥ 0)|
|<em>dclass</em>|→|<em>qtycls</em>| |

<em>constr</em>の優先順位は式と同じである。通常のコンストラクタの適用が中置コンストラクタの適用より高い優先順位を持つ(そのため`a : Foo a`は`a : (Foo a)`のように解析する)。

代数的なデータ型の宣言は、<em>cx</em>が内容である形式<code>data <em>cs => T u<sub>1</sub> ... u<sub>k</sub> = K<sub>1</sub> t<sub>1 1</sub> ... t<sub>1k<sub>1</sub></sub> | ... | K<sub>n</sub> t<sub>n1</sub> ... t<sub>nk<sub>n</sub></sub></em></code>を持つ。
この宣言は0個以上の構成要素である<em>データコンストラクタ K<sub>1</sub>, …, K<sub>n</sub></code></em>をもつような新しい<em>型コンストラクタT</em>を導入する。
このリポートで、修飾されていない用語"コンストラクタ"は"データコンストラクタ"を常に意味する。

データコンストラクタの型は<code><em>K<sub>i</sub></em>  ::  ∀ <em>u<sub>1</sub> … u<sub>k</sub>.  cx<sub>i</sub>  ⇒  t<sub>i1</sub>  →  ⋅⋅⋅  →  t<sub>ik<sub>i</sub></sub>  →  (T u<sub>1</sub> … u<sub>k</sub>)</em></code>によって与えられる。
ここで<em>cx<sub>i</sub></em>は、<em>t<sub>i1</sub>, …, t<sub>ik<sub>i</sub></sub></em>の型に自由に出現する型変数たちのみを含むような<em>cx</em>の部分集合の中で最大なものである。
型変数<em>u<sub>1</sub></em>から<em>u<sub>k</sub></em>は互いに異なるものでなければならず、<em>cx</em>と<em>t<sub>ij</sub></em>に出現してもよい。
また、他の型変数が<em>cx</em>やそれより右側に出現すると静的なエラーとなる。
新しい型定数<em>T</em>は引数の変数<em>u<sub>i</sub></em>の種<em>κ<sub>i</sub></em>がセクション[4.6](#a種の推論)で説明される種推論によって決定される形式<em>κ<sub>1</sub> →… → κ<sub>k</sub> →∗</em>の種を持つ。
これは<em>T</em>が0から<em>k</em>引数のどこでも型の表現に使われるかもしれないということを意味する。

例えば、以下の宣言は

```hs
data Eq a => Set a = NilSet | ConsSet a (Set a)
```

種`∗→∗`の型コンストラクタ`Set`を導入し、型ありのコンストラクタ`NilSet`と`ConsSet`は以下のものである。

```hs
NilSet  ::  ∀ a.  Set  a
ConsSet ::  ∀ a.  Eq   a  ⇒  a  →  Set   a  →  Set   a
```

与えられた例では、`ConsSet`にオーバーロードされた型は`ConsSet`は型がクラス`Eq`のインスタンスである値に提供されることのみ可能であることを保証する。`ConsSet`に対照したパターンマッチングは`Eq a`拘束にも発生する。例えば、

```hs
f (ConsSet a s) = a
```

関数`f`は推論された型`Eq a => Set a -> a`を持つ。`data`宣言の内容は他に何も効果を持たない。

データ型のコンストラクタの可視性(すなわちデータ型の"抽象度")は、それが定義されたモジュールの外では、セクション[5.8](./5-modules.md#a)で説明されるエクスポートリスト内のデータ型の名前の形式によって制御される。

`data`宣言の内容は他に何も効果を持たない付加的な`deriving`の部分は<em>派生されたインスタンス</em>と関係しており、セクション[4.3.3](#a派生インスタンス)で説明される。

**ラベル付けされたフィールド** 引数<em>k</em>個とるデータコンストラクタは<em>k</em>要素のオブジェクトを作成する。これらの要素は通常、式またはパターンの中のコンストラクタへの引数のように位置付けして呼び出される。巨大なデータ型のために、データオブジェクトの要素に<em>フィールドラベル</em>を割り当てることは便利である。これはコンストラクタ内でその位置を独立して参照されるために明記するフィールドを許す。

`data`宣言のコンストラクタ定義はラベルを記録構文`(C { ... })`を使用するコンストラクタのフィールドに割り当てられるだろう。フィールドラベルを使用するコンストラクタはそれらなしにコンストラクタを自由に組み合わされるかもしれない。フィールドラベルに関係するコンストラクタは通常のコンストラクタのようにまだ使われるだろう。ラベルを使う機能は基礎となる位置上のコンストラクタを使う操作のための単純な簡略記法である。その位置上のコンストラクタの引数はラベル付けされたフィールドのように同じ順序で発生する。例えば、以下の宣言は

```hs
data C = F { f1,f2 :: Int, f3 :: Bool }
```

下のように生成されるものと同一な型とコンストラクタを定義する。

```hs
data C = F Int Int Bool
```

フィールドラベルを使用する操作はセクション[3.15](./3-expressions.md#aフィールドラベル付きのデータ型)で説明される。`data`宣言においては、型シノニムを展開した後にそのフィールドが使われている場所すべてで同じ型がつく場合に限り、同じフィールドラベルを複数のコンストラクタで使ってもよい。ラベルはスコープ内の型以上で共有されることはできない。フィールド名は通常の変数とクラスメソッドを使う最上位の名前空間を共有し、スコープ内の他の最上位の名前と衝突してはいけない。

パターン`F {}`はコンストラクタ`F`が<em>記録構文を使って宣言されているかどうかにかかわらず</em>、`F`によって構築された任意の値と一致する。

**正格なフラグ** データコンストラクタが適用されるたびに、代数的データ型の宣言に対応する型が感嘆符`!`で表される正格なフラグを持つ場合に限り、コンストラクタの各引数は評価される。
`"!"`は通常のvarsymであって、<em>reservedop</em>としては字句解析されない。
それはデータ宣言の引数の型の内容にのみ特別な意味を持つ。

<div class="column">

**変換:** 各<em>s<sub>i</sub></em>が形式<em>!t<sub>i</sub></em>か<em>t<sub>i</sub></em>のいずれかである形式<code>data <em>cx => T u<sub>1</sub> … u<sub>k</sub> = … | K s<sub>1</sub> … s<sub>n</sub> | …</em></code>の宣言は<em>(\ x<sub>1</sub> … x<sub>n</sub> -> ( ((K op<sub>1</sub> x<sub>1</sub>) op<sub>2</sub> x<sub>2</sub>) … ) op<sub>n</sub> x<sub>n</sub>)</em>という式の中の<em>K</em>の全ての発生を置き換える。
<em>op<sub>i</sub></em>はもし<em>s<sub>i</sub></em>が形式<em>t<sub>i</sub></em>なら、正格ではない適用関数`$`であり、<em>op<sub>i</sub></em>はもし<em>s<sub>i</sub></em>が形式<code>！ <em>t<sub>i</sub></em></code>であるなら正格に適用する関数`$!`である(セクション[6.2](./6-predefined-types-and-classes.md#a)を参照)。
<em>K</em>上のパターンマッチングは正格なフラグによる影響を受けられない。
</div>

### 型シノニムの宣言

|||||
|---|---|---|---|
|   <em>topdecl</em>|→|`type` <em>simpletype</em> `=` <em>type</em>| |
|<em>simpletype</em>|→|<em>tycon</em> <em>tyvar<sub>1</sub></em> … <em>tyvar<sub>k</sub></em>|(<em>k</em> ≥ 0)|

型シノニムの宣言は古い型と等しい新しい型を生成する。
それは新しいコンストラクタ<em>T</em>を生成する形式`type` <em>T u<sub>1</sub> ... u<sub>k</sub> = t</em>を持つ。
型<em>(T t<sub>1</sub> …t<sub>k</sub>)</em>は型<em>t[t<sub>1</sub>∕u<sub>1</sub>, …, t<sub>k</sub>∕u<sub>k</sub>]</em>に等しい。
型変数<em>u<sub>1</sub></em>から<em>u<sub>k</sub></em>は互いに異なるものでなければならず、<em>t</em>上のみにスコープされる。
そしてその<em>t</em>の中に他の型変数が現れたら静的エラーになる。
新しい型コンストラクタ<em>T</em>の種は引数<em>u<sub>i</sub></em>の種<em>κ<sub>i</sub></em>は形式<em>κ<sub>1</sub> →… → κ<sub>k</sub> → κ</em>であり、<em>t</em>の右側の<em>κ</em>はセクション[4.6](#a種の推論)で説明される種の推論によって決定される。
例えば、次の定義はリスト型のコンストラクタを書く方法の代替案を提供することに使用されることができる。

```hs
type List = []
```

型シノニムの宣言によって生成された型コンストラクタのシンボル<em>T</em>は一部のみを提供されることはできない。十分な数の引数なしに<em>T</em>を使うことは静的エラーになる。

再帰的と相互再帰的なデータ型は許されるのだが、<em>代数的データ型</em> が入り込む限り、型シノニムではそうではない。例えば、

```hs
type Rec a   =  [Circ a]  
data Circ a  =  Tag [Rec a]
```

は許されるが、それに反して、

```hs
type Rec a   =  [Circ a]        -- invalid  
type Circ a  =  [Rec a]         -- invalid
```

はそうではない。似たもので、`type Rec a = [Rec a]`も許されない。

型シノニムはより読みやすい型シグネチャを作る便利な、しかし厳密な構文的仕組みである。同義語とその定義は`instance`宣言のインスタンス型を除いて、完全に置き換えできる(セクション[4.3.2](#aインスタンス宣言)を参照)。

### データ型の改名

|||||
|---|---|---|---|
| <em>topdecl</em>|→|`newtype` [<em>context</em> =>] <em>simpletype</em> = <em>newconstr</em> [<em>deriving</em>]| |
| <em>newconstr</em>|→|<em>con</em> <em>atype</em>| |
|  |&#124;|<em>con</em> { <em>var</em> :: <em>type</em> }| |
|<em>simpletype</em>|→|<em>tycon</em> <em>tyvar<sub>1</sub></em> … <em>tyvar<sub>k</sub></em>|(<em>k</em> ≥ 0)|

<code>newtype <em>cs => T u<sub>1</sub> ... u<sub>k</sub> = N t</em></code>の形の宣言は新しい型を導入し、その表現はすでに存在している型と同じである(**訳注**: TはNから新たに作られた型であるが、実行時表現が等しい)。
型<em>(T u<sub>1</sub>… u<sub>k</sub>)</em>はデータ型<em>t</em>を改名する。
それは型シノニムからオリジナルな型からまたはその型へ明示的に強制されなければならない厳密な型を作成することとは異なる。
また型シノニムと異なり、`newtype`は再帰的な型を定義することに使用されるかもしれない。
式の中のコンストラクタ<em>N</em>は型<em>t</em>から型<em>(T u<sub>1</sub> … u<sub>k</sub>)</em>へ値を強制する。
パターンの中の<em>N</em>は型<em>(T u<sub>1</sub> … u<sub>k</sub>)</em>から型<em>t</em>へ値を強制する。
これらの強制は実行時のオーバーヘッドなしに実装されるかもしれない。
`newtype`はオブジェクトの根底にある表現を変更しない。

新しいインスタンス(セクション[4.3.2](#aインスタンス宣言)を参照)は`newtype`によって定義された型に定義されることができるが、型シノニムに定義されることはないかもしれない。
`newtype`によって作成された型は代数的データ型が追加の間接レベルを持つ表現内の代数的データ型とは異なる。
この差は効率が悪い表現にアクセスするかもしれない。
この差はパターンマッチングのための異なるルールに反映される(セクション[3.17](./3-expressions.md#aパターンマッチング)を参照)。
代数的なデータ型とは異なり、新しい型コンストラクタ<em>N</em>は<em>リフトしない</em>、そのため、<code><em>N</em> ⊥</code>は`⊥`と同じである。

次の例は`data`(代数的データ型)と`type`(型シノニム)、`newtype`(型の改名)との差を明確にする。以下の宣言が与えられる。

```hs
data D1 = D1 Int  
data D2 = D2 !Int  
type S = Int  
newtype N = N Int  
d1 (D1 i) = 42  
d2 (D2 i) = 42  
s i = 42  
n (N i) = 42
```

式`(d1 ⊥)`と`(d2 ⊥)`、`(d2 (D2 ⊥))`は`⊥`と全て等しい。一方で、`(n ⊥)`と`(n (N ⊥))`、`(d1 (D1 ⊥))`、`(s ⊥)`は`42`と全て等しくなる。特別に、`(N ⊥)`は`(D1 ⊥)`が`⊥`と等しくないときは`⊥`と等しくなる。

`newtype`宣言のオプション的に派生部分は`data`宣言の派生要素と同じ方法で扱われる。セクション[4.3.3]("#4.3.3")を参照すること。

`newtype`宣言はフィールド名をつける構文を使用するかもしれず、もちろんそのフィールドしかないかもしれない。従って、

```hs
newtype Age = Age { unAge :: Int }
```

はコンストラクタとデコンストラクタの両方をスコープに持ち込む。

```hs
Age   :: Int -> Age  
unAge :: Age -> Int
```

## 型クラスとオーバーロード

### クラス宣言

|||||
|---|---|---|---|
| <em>topdecl</em>|→|`class` [<em>scontext</em> =>] <em>tycls</em> tyvar [`where` <em>cdecls</em>]| |
|<em>scontext</em>|→|<em>simpleclass</em>| |
| |&#124;|( <em>simpleclass<sub>1</sub></em> `,` … `,` <em>simpleclass<sub>n</sub></em> )|(<em>n</em> ≥ 0)|
|<em>simpleclass</em>|→|<em>qtycls</em> <em>tyvar</em>| |
|  <em>cdecls</em>|→|{ <em>cdecl<sub>1</sub></em> `;` … `;` <em>cdecl<sub>n</sub></em> }|(<em>n</em> ≥ 0)|
|   <em>cdecl</em>|→|<em>gendecl</em> |
| |&#124;|(<em>funlhs</em> &#124; <em>var</em>) <em>rhs</em>| |

<em>クラス宣言</em> は新しいクラスとその中のオペレーション(<em>クラスメソッド</em>)を生成する。クラス宣言は次の一般的な形式を持つ。

```hs
class cx => C u where cdecls
```

これは新しいクラスの名前<em>C</em>を生成し、型変数<em>u</em>はそのクラスの本体のクラスメソッドシグネチャ上でのみスコープされる。内容<em>cx</em>は下で説明する<em>C</em>のスーパークラスを明記する。<em>cx</em>の中で参照されるであろう型変数のみが<em>u</em>である。

スーパークラスの関係は循環してはいけない。例)指示された非環式のグラフを構成しなければいけない。

`class`宣言の<em>cdecls</em>部分は宣言の3つの種を含む。
- クラス宣言は新しい<em>クラスメソッドv<sub>i</sub></em>を生成し、スコープは<em>class</em>宣言の外側に展開する。
    クラス宣言のクラスメソッドは<em>cdecls</em>内の明示的な型シグネチャ<em>v<sub>i</sub> :: cx<sub>i</sub> => t<sub>i</sub></em>にある<em>v<sub>i</sub></em>そのものである。
    クラスメソッドは変数束縛とフィールド名と一緒に最上位の名前空間を共有する。
    それらはスコープの他の最上位の束縛と衝突してはならない。
    そのため、クラスメソッドは最上位の定義やフィールド名、他のクラスメソッドのように同じ名前を持つことはできない。
    <br><br>
		トップレベルのクラスメソッド<em>v<sub>i</sub></em>の型は<em>v<sub>i</sub> :: ∀u,<span class="overline">w</span>.(Cu,cx<sub>i</sub>) ⇒ t<sub>i</sub></em>である。
    <em>t<sub>i</sub></em>は<em>u</em>を言及しなければいけないし、<em>u</em>より型変数<em><span class="overline">w</span></em>を言及するかもしれない。
    その場合、<em>v<sub>i</sub></em>の型は<em>u</em>と<em><span class="overline">w</span></em>の両方にポリモフィックである。
    <em>cx<sub>i</sub></em>は<em><span class="overline">w</span></em>のみ束縛するだろう。
    特に、<em>cx<sub>i</sub></em>は<em>u</em>を束縛しないだろう。
    例えば、
    ```hs
    class Foo a where
    op :: Num b => a -> b -> a
    ```
    <br>
    ここでの`op`型は<code>∀ <em>a</em>, <em>b</em>.(`Foo` <em>a</em>, Num <em>b</em>)   ⇒  <em>a</em>  →  <em>b</em>  →  <em>a</em>.</code>である。
- <em>cdecls</em>は(他の値ではなく)クラスメソッドのいずれかへの<em>結合性宣言</em>も含めるだろう。
    しかしながら、クラスメソッドはトップレベルの値を宣言することから、クラスメソッドのための固定宣言はクラス宣言の外側、或いはトップレベルを表現するだろう。
- 最後に、<em>cdecls</em>は<em>v<sub>i</sub></em>のいずれかの<em>デフォルトクラスメソッド</em>を含められる(セクション[4.3.2](#aインスタンス宣言))。
    デフォルトメソッドの宣言は通常、左手側が変数か関数定義のみであろうことを除いて値の定義である。
    例えば、
    ```hs
    class Foo a where  
    op1, op2 :: a -> a  
    (op1, op2) = ...
    ```
    <br>
		は、許可されない。デフォルト宣言の左手側がパターンだからだ。

これらのケース以外に、<em>cdecls</em>内の宣言は許されない。

`where`部を伴わない`class`宣言はオリジナルの全てのクラスメソッドを継承する巨大なクラスのコレクションを合成することに便利かもしれない。
例えば、
```hs
class  (Read a, Show a) => Textual a
```

このような場合、型が全スーパークラスのインスタンスであるなら、たとえサブクラスが直ちにクラスメソッドを持たなくても、サブクラスのインスタンスに<em>自動的にはならず</em>、`instance`宣言は`where`部を伴わず明示的に与えられなければならない。

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

## 種の推論
