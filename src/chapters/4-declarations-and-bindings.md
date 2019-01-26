[WIP]

# 宣言と束縛

この章では、Haskellの宣言の構文と簡略した意味論を説明する。

|||||
|---|---|---|---|
|module| → |<tt>module</tt> modid [exports] <tt>where</tt> body| |
|      |&#124;|body| |
|  body| → |{ impdecls ; topdecls }| |
|      |&#124;|{ impdecls }| |
|      |&#124;|{ topdecls }| |
|||||
|topdecls| → |topdecl<sub>1</sub> ; … ; topdecl<sub>n</sub>|(n ≥ 1)|
| topdecl| → |<tt>type</tt> simpletype = type| |
|        |&#124;|<tt>data</tt> [context =>] simpletype [= constrs] [deriving]| |
|        |&#124;|<tt>newtype</tt> [context =>] simpletype = newconstr [deriving]| |
|        |&#124;|<tt>class</tt> [scontext =>] tycls tyvar [<tt>where</tt> cdecls]| |
|        |&#124;|<tt>instance</tt> [scontext =>] qtycls inst [<tt>where</tt> idecls]| |
|        |&#124;|<tt>default</tt> (type<sub>1</sub> , … , type<sub>n</sub>)|(n ≥ 0)|
|        |&#124;|<tt>foreign</tt> fdecl||
|        |&#124;|decl| |
|||||
|  decls| → |{ decl<sub>1</sub> ; … ; decl<sub>n</sub> }|(n ≥ 0)|
|   decl| → |gendecl| |
|       |&#124;|(funlhs &#124; pat) rhs| |
|||||
| cdecls| → |{ cdecl<sub>1</sub> ; … ; cdecl<sub>n</sub> }|(n ≥ 0)|
|  cdecl| → |gendecl||
|       |&#124;|(funlhs | var) rhs||
|||||
| idecls| → |{ idecl<sub>1</sub> ; … ; idecl<sub>n</sub> }|(n ≥ 0)|
|  idecl| → |(funlhs &#124; var) rhs||
|       |&#124;|  |(empty)|
|gendecl|→|vars :: [context <tt>=></tt>] type|(type signature)|
|       |&#124;|fixity [integer] ops|(fixity declaration)|
|       |&#124;|  |(empty declaration)|
|||||
|    ops|→|op<sub>1</sub> , … , op<sub>n</sub>|(n ≥ 1)|
|   vars|→|var<sub>1</sub> , … , var<sub>n</sub>|(n ≥ 1)|
| fixity|→|<tt>infixl</tt> &#124; <tt>infixr</tt> &#124; <tt>infix</tt>| |

**topdecls**構文的なカテゴリの宣言はHaskellモジュール([5章]("./5-modules.md"))の最上位のみ許す一方で**decls**は最上位またはネストされたスコープのいずれかで使われるかもしれない(例えば、`let`か`where`の内で`topdecls`を構築する)。

説明のため、`type`と`newtype`、`data`宣言からなるユーザー定義のデータ型(セクション[4.2](”#4.2”))と`class`と`instance`、`default`宣言からなる型クラスとオーバーロード(セクション[4.3]("#4.3"))、値束縛と型シグネチャ、固定の宣言からなるネストされた宣言(セクション[4.4]("#4.4"))の3つのグループに宣言を分割する。

haskellは(整数や浮動小数点数のような)"ハード・ワイヤード"であるいくつかのプリミティブなデータ型を持つ。しかし、多くの"ビルドイン"なデータ型は通常のHaskellコードによって定義されていて、通常<code>type</code>や<code>data</code>宣言に使われる。これらの"ビルドイン"のデータ型はセクション[6.1]("./6-predefined-types-and-classes.md")で詳細に説明される。

## 型とクラスの概要

Haskellは静的型セマンティクス[4.6]("#4.6")を提供するために伝統的なHindley-Milner多形型システムを使用するが、その型システムは構造化された手法にオーバーロード関数を導入するために提供する**型クラス**(または**クラス**)で拡張されている。

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

種類の推論は型推論がバリュー式の正常性を確認するのと同様に型式の正常性を確認する。しかしながら、型のようではなく、種類は完全に暗黙に示され、言語の可視化された部分はない。種類の推論はセクション[4.6]("#4.6")で議論される。

### 型の構文

|||||
|---|---|---|---|
|type| → |btype [-> type]|(function type)|
|||||
|btype|→|[btype] atype|(type application)|
|||||
|atype|→|gtycon| |
|  |&#124;|tyvar| |
|  |&#124;|( type<sub>1</sub> , … , type<sub>k</sub> )|(tuple type, k ≥ 2)|
|  |&#124;|[ type ]|(list type)|
|  |&#124;|( type )|(parenthesised constructor)|
|||||
|gtycon|→|qtycon| |
|  |&#124;|()|(unit type)|
|  |&#124;|[]|(list constructor)|
|  |&#124;|(->)|(function constructor)|
|  |&#124;|(,{,})|(tupling constructors)|

Haskellの型式のための構文は上に与えられる。データ値と同じようにデータコンストラクタを使って作られ、型バリューは`型コンストラクタ`から作られる。データコンストラクタと同様に、型コンストラクタの名前は大文字で始められる。データコンストラクタとは違い、中置型コンストラクタは許されない(`(->)以外`)。

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

例えば、型式`IO a`は変数`a`に定数`IO`への適応のように理解されることができる。`IO`型コンストラクタは種類`∗→∗`を持ち、変数`a`と式全体の両方を従え、式`IO a`は種類`*`を持たなければならない。一般的に`型の推論`(セクション[4.6]("#4.6"))の処理は適切な種類をユーザー定義のデータ型や型のシノニム、クラスへ決定することを必要とされる。

特別な構文は特定の型式がより伝統的なスタイルで書かれることを許すために提供される。

1. `関数型`は形式<code>t<sub>1</sub> -> t<sub>2</sub></code>を持ち、型<code>(->) t<sub>1</sub> t<sub>2</sub></code>に等しい。アロー関数は左に関連づける。例えば、`Int -> Int -> Float`は`Int -> (Int -> Float)`を意味する。
1. `タプル型`は`K ≥ 2`の形式<code>t<sub>1</sub>, ..., t<sub>k</sub></code>を持ち、括弧の間に`k-1`個のカンマがある型<code>(,…,) t<sub>1</sub> … t<sub>k</sub></code>と等しい。それは型<code>t<sub>1</sub></code>をはじめの要素に、型<code>t<sub>2</sub></code>を2番目の要素に持つ等など、`k要素のタプル`の型を示す(セクション[3.8]("./3-expressions.md")と[6.1.4]("./6-predefined-types-and-classes.md")を参照)。
1. `リスト型`は形式`[t]`を持ち、型`[] t`と等しい。それは型`t`の要素に持つリストの方を示す(セクション[3.7]("./3-expressions.md")と[6.1.3]("./6-predefined-types-and-classes.md")を参照)。

これらの特別な構文的形式はスコープに関係なく関数、タプル、リストのビルドイン型のコンストラクタを常に示す。同様な方法で、接頭型のコンストラクタ`(->), [], (), (,)`等はビルドイン型のコンストラクタを常に示す。それらは資格が与えらることができず、そしてまたリストのインポートまたはエクスポートにに言及されることもできない([5章]("./5-modules.md"))。(上述の特殊な生産"gtycon"から)

リストとタプル型が特別な構文を持つのだが、それらのセマンティクスはユーザー定義された代数データ型と同じである。

式と型は一貫した構文を持つことに注意してください。もし、<code>t<sub>i</sub></code>は式またはパターン<code>e<sub>i</sub></code>の型なら、その時式<code>(\ e<sub>1</sub> -> e<sub>2</sub>), [e<sub>1</sub>]</code>と<code>(e<sub>1</sub>,e<sub>2</sub>)</code>は各々型<code>(t<sub>1</sub> -> t<sub>2</sub>), [t<sub>1</sub>]</code>と<code>(t<sub>1</sub>,t<sub>2</sub>)</code>を持つ。

ひとつの例外(クラス宣言内の顕著な型変数のこと(セクション[4.3.1]("#4.3.1")))を除いて、Haskellの型式内の型変数は例外なく量で定められると全て仮定され、一般的な定量化のための明示的な構文はない[4]("./../bibliography.md")。例えば、型式`a -> a`は型`∀ a. a  →  a`を示す。明確にするために、しかしながら、Haskellプログラムの方を議論する時に明示的な定量化をしばしば書く。明示的に数量化型を書く時、`∀`のスコープは可能な限り左側へ拡張する。例として、`∀ a. a  →  a `は`∀ a. (a  →  a)`を意味する。

### クラス表明と文脈の構文

|||||
|--|--|--|--|
|context| → |class| |
|       | &#124; |( class<sub>1</sub> , … , class<sub>n</sub> )|(n ≥ 0)|
| class | → |qtycls tyvar| |
|       | &#124; |qtycls ( tyvar atype<sub>1</sub> … atype<sub>n</sub> )|(n ≥ 1)|
| qtycls| → |[ modid . ] tycls| |
|  tycls| → |conid| |
|  tyvar| → |varid| |

**クラス表明** は形式**qtycls tyvar**を持ち、クラス`qtycls`の型**tyvar**のメンバを示す。クラス識別子は大文字で始める。`内容`は0個以上のクラス表明からなり、<code>( C<sub>1</sub>, …, C<sub>n</sub></code>がクラス識別子である一般的な形式<code>( C<sub>1</sub> u<sub>1</sub>, …, C<sub>n</sub> u<sub>n</sub> )</code>を持つ。<code>( u<sub>1</sub>, …, u<sub>n</sub></code>の各々は変数型か一つ以上の型への変数型の適応のいずれかである。括弧の外側は`n = 1`のとき省かれるかもしれない。一般的に、内容を示すために`cx`を使用し、`cx => t`を内容`cx`によって型制限された型`t`を示すために書く。内容`cx`は`t`によって参照される変数型のみを含まなければいけない。利便性のために、内容`cx`が空であっても、具体的な構文は`=>`を含まないケースであるが、`cx => t`を書く。

### 型とクラスのセマンティクス

このセクションは、型システムの簡略的な詳細を提供する。(WadlerとBlott[[13]]("./../bibliography.md")、Jones[[8]]("./../bibliography.md")は各々より詳細に型とコンストラクタクラスを議論している。)

Haskellの型システムは`型`をプログラム内の各式に帰する。一般的に、型は形式`∀ u. cx  ⇒  t`である。`u`は変数型の集合<code>u<sub>1</sub>, ..., u<sub>n</sub></code>である。どのような型でも、`cx`の中で開放される一般的な定量化された変数型<code>u<sub>i</sub></code>は`t`の中でも開放されなければならない。その上、内容`cx`はセクション[4.1.3]("#4.1.3")上で与えられた形式でなければならない。例として、ここにいくつかの正常な型がある。

<pre><code>Eq a => a -> a  
(Eq a, Show a, Eq b) => [a] -> [b] -> String  
(Eq (f a), Functor f) => (a -> b) -> f a -> f b -> Bool
</code></pre>

3つの型で、拘束`Eq (f a)`は`f`が一般的に定量化されているためもっと単純にしなければならない。

式`e`の型は型を`e`に開放する変数へ与える**型環境**とどの型がどのクラスのインスタンスであるかを宣言する**クラス環境**に依存する。(型は`インスタンス`宣言または`導出`節の存在経由のみのクラスのインスタンスになる。)

型は一般化の先行順(下に明記されている)によって関連する。多くの一般的な型は、一般化の先行順によって同等まで導かれ、(与えられた環境の)個々の式は**主要な型**と呼ばれるものに割り当てられる。Haskellの拡張されたHindley-Milner型システムは全式の主要な型を推論でき、オーバーロードされたクラスメソッドの妥当な使用を含んでいる(セクション[4.3.4]("#4.3.4")で説明するように、確実に曖昧なオーバーロードが起こり得るのだが)。したがって、明示的な型付け(型署名と呼ぶ)は通常、オプションである(セクション[3.16]("./3-expressions,md")と[4.4.1]("#4.4.1")を参照)。

型<code>∀ <span class="overline">u</span>;. cx<sub>1</sub>  ⇒  t<sub>1</sub></code>は領域が以下のような`u`の代用`S`がある場合に限り、型<code>∀ <span class="overline">w</span>;. cx<sub>2</sub>  ⇒  t<sub>2</sub></code>**より一般的**である。

- <code>t<sub>2</sub></code>は<code>S(t<sub>1</sub>)</code>と同じである。
- <code>cx<sub>2</sub></code>はそのクラスの環境を保持し、<code>S(cx<sub>1</sub>)</code>も保持する。

型<code>∀ <span class="overline">u</span>. cx  ⇒  t</code>の値は内容<code>cx[<span class="overline">s</span>;/<span class="overline">u</span>]</code>を保持する場合に限り型<code><span class="overline">s</span></code>でインスタンス化されるかもしれない。例えば、関数`double`について考えてみる。

<pre><code>double x = x + x</code></pre>

`double`の多くの一般的な型は`∀ a`である。`Num   a⇒  a  →  a`。`double`は(`Int`にインスタンス化する)型`Int`の値に提供されるかもしれない。`Num Int`が保持するゆえに、`Int`はクラス`Num`のインスタンスである。しかしながら、`double`は型`Char`の値に通常提供されないかもしれない。なぜなら、`Char`は通常、クラス`Num`のインスタンスではないからだ。ユーザーはインスタンスのような宣言を選択するかもしれない。その場合、`double`は実際に`Char`へ提供されるかもしれない。

## ユーザー定義のデータ型

このセクションでは、代数のデータ型(`data`宣言)や新たに命名するデータ型(`newtype`宣言)、型のシノニム(`type`宣言)を説明する。これらの宣言はモジュールの最上位で現れるかもしれない。

### 代数データ型宣言

|||||
|---|---|---|---|
|topdecl|→|<tt>data</tt> [context =>] simpletype [= constrs] [deriving]| |
|||||
|simpletype|→|tycon tyvar<sub>1</sub> … tyvar<sub>k</sub>|(k ≥ 0)|
|||||
|constrs|→|constr<sub>1</sub> &#124; … &#124; constr<sub>n</sub>|(n ≥ 1)|
| constr|→|con [<tt>!</tt>] atype<sub>1</sub> … [<tt>!</tt>] atype<sub>k</sub>|(arity con  =  k, k ≥ 0)|
|  |&#124;|(btype &#124; <tt>!</tt> atype) conop (btype &#124; <tt>!</tt> atype)|(infix conop)|
|  |&#124;|con { fielddecl<sub>1</sub> , … , fielddecl<sub>n</sub> }|(n ≥ 0)|
|fielddecl|→|vars :: (type &#124; <tt>!</tt> atype)| |
|||||
| deriving|→|<tt>deriving</tt> (dclass &#124; (dclass<sub>1</sub>, … , dclass<sub>n</sub>))|(n ≥ 0)|
|dclass|→|qtycls| |

**constr**の優先順位は式と同じである。通常のコンストラクタの適用が中置コンストラクタの適用より高い優先順位を持つ(そのため`a : Foo a`は`a : (Foo a)`のように解析する)。

代数的なデータ型の宣言は、`cx`が内容である形式<code>data cs => T u<sub>1</sub> ... u<sub>k</sub> = K<sub>1</sub> t<sub>1 1</sub> ... t<sub>1k<sub>1</sub></sub> | ... | K<sub>n</sub> t<sub>n1</sub> ... t<sub>nk<sub>n</sub></sub></code>を持つ。この宣言は一つ以上の構成要素<code>データコンストラクタ　K<sub>1</sub>, …, K<sub>n</sub></code>を使う新しい`型コンストラクタT`を紹介する。このリポートで、修飾されていない用語"コンストラクタ"は"データコンストラクタ"を常に意味する。

データコンストラクタの型は<code>K<sub>i</sub>  ::  ∀ u<sub>1</sub> … u<sub>k</sub>.  cx<sub>i</sub>  ⇒  t<sub>i1</sub>  →  ⋅⋅⋅  →  t<sub>ik<sub>i</sub></sub>  →  (T u<sub>1</sub> … u<sub>k</sub>)</code>によって与えられる。<code>cx<sub>i</sub></code>は型`t<sub>i1</sub>, …, t<sub>ik<sub>i</sub></sub>`にこれらの型変数の開放のみを強制する`cx`の巨大な部分集合である。型変数<code>u<sub>1</sub></code>から<code>u<sub>k</sub></code>ははっきりとしなければならず、`cx`と<code>t<sub>ij</sub></code>に現れるかもしれない。`cx`またはその右手側に現れる他の型変数への静的なエラーである。新しい型定数`T`は<code>κ<sub>i</sub></code>形式<code>κ<sub>1</sub> →… → κ<sub>k</sub> →∗</code>種類を持つ。引数の変数<code>u<sub>i</sub></code>の種類<code>κ<sub>i</sub></code>はセクション[4.6]("#4.6")で説明される種類推論によって決定される。これは`T`が0からk引数のどこでも型式に使われるかもしれないということを意味する。

例えば、以下の宣言は

<pre><code>data Eq a => Set a = NilSet | ConsSet a (Set a)</code></pre>

種類`∗→∗`の型コンストラクタ`Set`を導入し、型ありのコンストラクタ`NilSet`と`ConsSet`は以下のものである。

<pre><code>NilSet  ::  ∀ a.  Set  a
ConsSet ::  ∀ a.  Eq   a  ⇒  a  →  Set   a  →  Set   a
</code></pre>

与えられた例では、`ConsSet`にオーバーロードされた型は`ConsSet`は型がクラス`Eq`のインスタンスである値に提供されることのみ可能であることを保証する。`ConsSet`に対照したパターンマッチングは`Eq a`拘束にも発生する。例えば、

<pre><code>f (ConsSet a s) = a</code></pre>
関数`f`は推論された型`Eq a => Set a -> a`を持つ。`data`宣言の内容は他に何も効果を持たない。

データ型が定義される中でモジュールの外側のデータ型のコンストラクタ(例：データ型の”抽象度”)の可視性はセクション[5.8]("./5-modules.md")で説明されるエクスポートリスト内のデータ型の名前の形式によって制御される。

`data`宣言の内容は他に何も効果を持たない付加的な`deriving`の部分は`派生されたインスタンス`と関係しており、セクション[4.3.3]("#4.3.3")で説明される。

<strong class="strong-point">ラベル付けされたフィールド</strong> 引数`k`個とるデータコンストラクタは`k`要素のオブジェクトを作成する。これらの要素は通常、式またはパターンの中のコンストラクタへの引数のように位置付けして呼び出される。巨大なデータ型のために、データオブジェクトの要素に**フィールドラベル**を割り当てることは便利である。これはコンストラクタ内でその位置を独立して参照されるために明記するフィールドを許す。

`data`宣言のコンストラクタ定義はラベルを記録構文`(C { ... })`を使用するコンストラクタのフィールドに割り当てるかもしれない。フィールドラベルを使用するコンストラクタはそれらなしにコンストラクタを自由に組み合わされるかもしれない。フィールドラベルに関係するコンストラクタは通常のコンストラクタのようにまだ使われるかもしれない。ラベルを使う機能は基礎となる位置上のコンストラクタを使う操作のための単純な簡略記法である。その位置上のコンストラクタの引数はラベル付されたフィールドのように同じ順序で発生する。例えば、以下の宣言は

<pre><code>data C = F { f1,f2 :: Int, f3 :: Bool }</code></pre>

下のように生成されるものと同一な型とコンストラクタを定義する。

<pre><code>data C = F Int Int Bool </code></pre>

フィールドラベルを使用する操作はセクション[3.15]("./3-expressions.md")で説明される。`data`宣言はフィールドの型付けが型のシノニムの拡張語の全てのケースと同じであるのと同様に複数のコンストラクタで同じフィードラベルを使うかもしれない。ラベルはスコープ内の型以上で共有されることはできない。フィールド名は通常の変数とクラスメソッドを使う最上位の名前空間を共有し、スコープ内の他の最上位の名前と衝突してはいけない。

パターン`F {}`はコンストラクタ`F`が記録構文を使って宣言されているかどうかにかかわらず`F`によって構築された任意の値と一致する。

<strong class="strong-point">厳格なフラグ</strong> データコンストラクタが適用されるたびに、代数的データ型の宣言に対応する型が感嘆符`!`で表される厳格なフラグを持つ場合に限り、コンストラクタの各引数は評価される。語彙的に、`"!"`は**reservedop**ではない普通のvarsymである。それはデータ宣言の引数の型の内容にのみ特別な意味を持つ。

<div class="column">

**変換** 各<code>s<sub>i</sub></code>が形式<code>!t<sub>i</sub></code>か<code>t<sub>i</sub></code>のいずれかである形式<code><tt>data</tt> cx => T u<sub>1</sub> … u<sub>k</sub> = … | K s<sub>1</sub> … s<sub>n</sub> | …</code>の宣言は<code>(\ x<sub>1</sub> … x<sub>n</sub> -> ( ((K op<sub>1</sub> x<sub>1</sub>) op<sub>2</sub> x<sub>2</sub>) … ) op<sub>n</sub> x<sub>n</sub>)</code>という式の中の`K`の全ての発生を置き換える。<code>op<sub>i</sub></code>はもし<code>s<sub>i</sub></code>が形式<code>t<sub>i</sub></code>なら、厳格ではない適用関数`$`であり、<code>op<sub>i</sub></code>はもし<code>s<sub>i</sub></code>が形式<code>！ t<sub>i</sub></code>であるなら厳格に起用する関数`$!`である(セクション[6.2]("./6-predefined-types-and-classes.md")を参照)。
</div>

### 型シノニムの宣言

|||||
|---|---|---|---|
|   topdecl|→|type simpletype = type| |
|simpletype|→|tycon tyvar<sub>1</sub> … tyvar<sub>k</sub>|(k ≥ 0)|

型シノニムの宣言は古い型と等しい新しい型を生成する。それは新しいコンストラクタ`T`を生成する形式<code><tt>type</tt> T u<sub>1</sub> ... u<sub>k</sub> = t</code>を持つ。型<code>(T t<sub>1</sub> …t<sub>k</sub>)</code>は型<code>t[t<sub>1</sub>∕u<sub>1</sub>, …, t<sub>k</sub>∕u<sub>k</sub>]</code>に等しい。型変数<code>u<sub>1</sub></code>から<code>u<sub>k</sub></code>は明確でなければならず、`t`上のみにスコープされる。`t`の中に他の型変数が現れたら静的エラーになる。新しい型コンストラクタ`T`の種類は引数<code>u<sub>i</sub></code>の種類<code>κ<sub>i</sub></code>は形式<code>κ<sub>1</sub> →… → κ<sub>k</sub> → κ</code>であり、`t`の右側の`κ`はセクション[4.6]("#4.6")で説明される種類の推論によって決定される。例えば、次の定義はリスト型のコンストラクタを書く方法の代替案を提供することに使用されることができる。

<pre><code>type List = []</code></pre>

型シノニムの宣言によって生成された型コンストラクタのシンボル`T`は一部のみを提供されることはできない。十分な数の引数なしに`T`を使うことは静的エラーになる。

再帰的と相互再帰的なデータ型は許されるのだが、**代数的データ型** が入り込む限り、型シノニムではそうではない。例えば、

<pre><code>type Rec a   =  [Circ a]  
data Circ a  =  Tag [Rec a]
</code></pre>

は許されるが、それに反して、

<pre><code>type Rec a   =  [Circ a]        -- invalid  
type Circ a  =  [Rec a]         -- invalid
</code></pre>

はそうではない。似たもので、<code>type Rec a = [Rec a]</code>も許されない。

型シノニムはより読みやすい型著名を作る便利な、しかし厳密な構文的で仕組みである。同義語とその定義は`instance`宣言ののインスタンス型を除いて、完全に置き換えできる(セクション[4.3.2]("./#4.3.2")を参照)。

### データ型のリネーム

|||||
|---|---|---|---|
| topdecl|→|<tt>newtype</tt> [context =>] simpletype = newconstr [deriving]| |
| newconstr|→|con atype| |
|  |&#124;|con { var :: type }| |
|simpletype|→|tycon tyvar<sub>1</sub> … tyvar<sub>k</sub>|(k ≥ 0)|

形式<code>newtype cs => T u<sub>1</sub> ... u<sub>k</sub> = N t</code>の宣言は表記法が存在している型と同じである新しい型を生成する。型<code>(T u<sub>1</sub>… u<sub>k</sub>)</code>はデータ型`t`を改名する。それは型シノニムからオリジナルな型からまたはその型へ明示的に強制されなければならない厳密な型を作成することとは異なる。また型シノニムと異なり、`newtype`は再帰的な方を定義することに使用されるかもしれない。式の中のコンストラクタ`N`は型`t`から型<code>(T u<sub>1</sub> … u<sub>k</sub>)</code>へ値を強制する。パターンの中の`N`は型<code>(T u<sub>1</sub> … u<sub>k</sub>)</code>から型`t`へ値を強制する。これらの強制は実行時のオーバーヘッドなしに実装されるかもしれない。`newtype`はオブジェクトの根底にある表現を変更しない。

新しいインスタンス(セクション[4.3.2]("#4.3.2")を参照)は`newtype`によって定義された型に定義されることができるが、型シノニムに定義されることはないかもしれない。`newtype`によって作成された型は代数的データ型が追加の間接レベルを持つ表現内の代数的データ型とは異なる。この差は効率が悪い表現にアクセスするかもしれない。この差はパターンマッチングのための異なるルールに反映される(セクション[3.17]("./3-expressions.md")を参照)。代数的なデータ型とは異なり、新しい型コンストラクタ`N`はリフトしない、そのため、`N ⊥`は`⊥`と同じである。

次の例は`data`(代数的データ型)と`type`(型シノニム)、`newtype`(型のリネーム)との差を明確にする。以下の宣言が与えられる。

<pre><code>
data D1 = D1 Int  
data D2 = D2 !Int  
type S = Int  
newtype N = N Int  
d1 (D1 i) = 42  
d2 (D2 i) = 42  
s i = 42  
n (N i) = 42
</code></pre>

式`(d1 ⊥)`と`(d2 ⊥)`、`(d2 (D2 ⊥))`は`⊥`と全て等しい。一方で、`(n ⊥)`と`(n (N ⊥))`、`(d1 (D1 ⊥))`、`(s ⊥)`は`42`と全て等しくなる。特別に、`(N ⊥)`は`(D1 ⊥)`が`⊥`と等しくないときは`⊥`と等しくなる。

`newtype`宣言のオプション的に派生部分は`data`宣言の派生要素と同じ方法で扱われる。セクション[4.3.3]("#4.3.3")を参照すること。

`newtype`宣言はフィールド名をつける構文を使用するかもしれず、もちろんそのフィールドしかないかもしれない。従って、

<pre><code>newtype Age = Age { unAge :: Int }</code></pre>

はコンストラクタとデコンストラクタの両方をスコープに持ち込む。

<pre><code>Age   :: Int -> Age  
unAge :: Age -> Int
</code></pre>

## 型クラスとオーバーロード

### クラス宣言

|||||
|---|---|---|---|
| topdecl|→|<tt>class</tt> [scontext =>] tycls tyvar [<tt>where</tt> cdecls]| |
|scontext|→|simpleclass| |
| |&#124;|( simpleclass<sub>1</sub> , … , simpleclass<sub>n</sub> )|(n ≥ 0)|
|simpleclass|→|qtycls tyvar| |
|  cdecls|→|{ cdecl<sub>1</sub> ; … ; cdecl<sub>n</sub> }|(n ≥ 0)|
|   cdecl|→|gendecl| |
| |&#124;|(funlhs &#124; var) rhs||

**クラス宣言**は新しいクラスとその中のオペレーション(**クラスメソッド**)を生成する。クラス宣言は次の一般的な形式を持つ。

<pre><code><tt>class</tt> cx => C u <tt>where</tt> cdecls
</code></pre>

これは新しいクラスの名前`C`を生成し、型変数`u`はそのクラスの本体のクラスメソッドシグネチャ上でのみスコープされる。内容`cx`は下で説明する`C`のスーパークラスを明記する。`cx`の中で参照されるであろう型変数のみが`u`である。

スーパークラスの関係は循環してはいけない。例)指示された非環式のグラフを構成しなければいけない。

`class`宣言の**cdecls**部分は3つの宣言の種類を含む。
- クラス宣言は新しい<code>クラスメソッドv<sub>i</sub></code>生成し、スコープは`class`宣言の外側に展開する。
    クラス宣言のクラスメソッドは`cdecls`内の明示的な型シグネチャ<code>v<sub>i</sub> :: cx<sub>i</sub> => t<sub>i</sub></code>にある<code>v<sub>i</sub></code>そのものである。クラスメソッドは変数束縛とフィールド名と一緒に最上位の名前空間を共有する。それらはスコープの他の最上位の束縛と衝突してはならない。そのため、クラスメソッドは最上位の定義やフィールド名、他のクラスメソッドのように同じ名前を持つことはできない。
    <br><br>
		最上位のクラスメソッド<code>v<sub>i</sub></code>の型は<code>v<sub>i</sub> :: ∀u,<span class="overline">w</span>.</code>である。<code>(Cu,cx<sub>i</sub>) ⇒ t<sub>i</sub></code>(訳注:誤字なのかピリオドがなかった)。<code>t<sub>i</sub></code>は`u`を言及しなければいけない。それは<code>u</code>より型変数<code><span class="overline">w</span></code>を言及するかもしれない。その場合、<code>v<sub>i</sub></code>の型は`u`と<code><span class="overline">w</span></code>の両方にポリモフィックである。<code>cx<sub>i</sub></code>は<code><span class="overline">w</span></code>のみ束縛するだろう。特に、<code>cx<sub>i</sub></code>は`u`を束縛しないだろう。例えば、
		<pre><code>
<tt>class</tt> Foo a <tt>where</tt>
  op :: Num b => a -> b -> a
		</code></pre>
		ここでの`op`型は`∀ a, b`である。`(Foo a, Num b)   ⇒  a  →  b  →  a. `
- **cdecls**は(他の値ではなく)クラスメソッドのいずれかへの**fixity declarations**も含めるだろう。しかしながら、クラスメソッドは最上位の値を宣言することから、クラスメソッドのためのfixity declarationsはクラス宣言の外側、或いは最上位を表現するだろう。
- 最後に、**cdecls**は<code>v<sub>i</sub></code>のいずれかの**デフォルトクラスメソッド**を含むだろう(セクション[4.3.2]("#4.3.2"))。デフォルトメソッドの宣言は通常、左手側が変数か関数定義のみであろうことを除いて値の定義である。例えば、
    <pre><code>class Foo a where  
  op1, op2 :: a -> a  
  (op1, op2) = ...
		</code></pre>
		は、許可されない。デフォルト宣言の左手側がパターンだからだ。

これらのケース以外に、`cdecls`内の宣言は許されない。

`where`部を伴わない`class`宣言はオリジナルの全てのクラスメソッドを継承する巨大なクラスのコレクションを合成することに便利であろう。例えば、
<pre><code>class  (Read a, Show a) => Textual a</code></pre>

このような場合、型が全スーパークラスのインスタンスであるなら、たとえサブクラスが直ちにクラスメソッドを持たなくても、
自動的にサブクラスのインスタンスではない。｀instance｀宣言は`where`部を伴わず明示的に与えられなければならない。

### インスタンス宣言

### 派生インスタンス

### 不明瞭な型とオーバーロードされた数値オペレータの既定値

## ネストされた宣言

### 型シグネチャ

### 固定の宣言

### 関数とパターン束縛

#### 関数束縛

#### パターン束縛

## 関数とパターン束縛の静的なセマンティクス

### 依存の解析

### 一般化

### コンテキストの削減エラー

### 単相性

### 単相性の制限

## 種類の推論
