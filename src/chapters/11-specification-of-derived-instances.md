# インスタンス導出の仕様

インスタンス導出とは、データまたはnewtype宣言から自動的に生成されるインスタンス宣言である。インスタンス導出のコード本体は対応する型の定義から文法的に導出される。インスタンス導出はコンパイラが知っているクラスに対してのみ可能である。このようなクラスはPreludeまたは標準ライブラリで定義されている。この章では、Preludeによって定義されたクラスの導出について説明する。

`T`が次のように定義された代数的データ型であるとする:

```haskell
data cx => T u1 ... uk = K1 t11 ... t1k1 | ... | Kn tn1 ... tnkn
    deriving (C1, ..., Cm)
```

(ここで、`m ≥ 0`であるとする。また、`m = 1`のときは括弧は省略できる)

そしてクラス`C`に対するインスタンス導出宣言は次の条件を満たすときに可能である:

1. `C`は`Eq, Ord, Enum, Bounded, Show, Read`のいずれかである
2. 文脈`cx'`であって、`cx' => C tij`が構成に使われているすべての型`tij`について成り立つようなものが存在する
3. `C`が`Bounded`であるとき、型は列挙型である(どのコンストラクタもパラメーターを持たない)か、唯一のコンストラクタをもつ
4. `C`が`Enum`であるとき、型は列挙型である
5. `T u1 ... uk`が`C`のインスタンスになるような明示的なインスタンス宣言がプログラムのどこにも存在しない
6. データ宣言がコンストラクタをもたないとき(上で`n = 0`のとき)、どのクラスも導出可能でない(`m = 0`である)

インスタンスを導出するという目的から、`newtype`宣言は`data`宣言で1つコンストラクタをもつものとして扱われる。

`deriving`の形があるとき、各クラス`Ci`に対する`T u1 ... uk`のインスタンス宣言は自動的に生成される。どれかの`Ci`に対してインスタンス宣言が導出不可能であるとき、静的エラーが返る。インスタンス導出が必要でないときは、`deriving`の形は省略されるか、`deriving ()`が使われることもある。

それぞれの導出されたインスタンス宣言は `instance (cx, cx') => Ci (T u1 ... uk) where { d }` の形をしている。ただし`d`は`Ci`と`T`のデータ型宣言に依存して自動的に導出される(これについてはこのセクションの残りで説明する))。

コンテキスト `cx'`は上の(2)を満たす最小のコンテキストである。相互再帰データ型でこれを計算するためにコンパイラは不動点の計算を行う必要があることがある。

Preludeのクラスで導出可能なそれぞれについてのインスタンス導出の詳細をここで与える。ここでの説明で登場する自由変数とコンストラクタはすべて`Prelude`で定義されたものを指している。

### `Eq`と`Ord`のインスタンス導出

`Eq`と`Ord`のインスタンス導出によって自動的に導入されるクラスメソッドは `(==), (/=), compare, (<), (<=), (>), (>=), max, min`である。最後の7つの演算子は各コンストラクタの集合ごとに辞書順で比較を行うように定義される。すなわち、データ型宣言で先に書かれたコンストラクタは後に書かれたコンストラクタよりも小さい値として扱われる。例えば、`Bool`データ型に対して、`(True > False) == True`が成り立つ。

導出された比較はコンストラクタを常に左から右に向かって走査する。このことは次の例から確かめられる:

```haskell
(1,undefined) == (2,undefined) =>    False
(undefined,1) == (undefined,2) =>    ⊥
```

導出されたクラス`Eq`と`Ord`の演算はいずれも引数に対して正格である。例えば、`False`は`Bool`型の最初のコンストラクタであるが`False <= ⊥`は`⊥`である。

### `Enum`のインスタンス導出

`Enum`のインスタンス導出宣言は列挙型(すべてのコンストラクタが引数をとらないようなデータ型)に対してのみ可能である。

引数をとらないコンストラクタはは左から右に向かって`0`から`n-1`までナンバリングされていると仮定する。このナンバリングの仕組みの下で`succ`と`pred`は値の次の値と前の値を与える演算子である。`succ`を最大の要素に適用したときと`pred`を最小の要素に適用したときはエラーになる。

`toEnum`と`fromEnum`は列挙された値を`Int`へ、または`Int`から変換する演算子である。`toEnum`は`Int`の引数に一致するコンストラクタの番号がない場合は実行時エラーになる。

他のメソッドの定義は次である:

```haskell
enumFrom x           = enumFromTo x lastCon
enumFromThen x y     = enumFromThenTo x y bound
    where  
        bound
            | fromEnum y >= fromEnum x = lastCon
            | otherwise                = firstCon
enumFromTo x y       = map toEnum [fromEnum x .. fromEnum y]
enumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]
```

ここで、`firstCon`と`lastCon`はそれぞれ`data`宣言にある最初と最後のコンストラクタである。例として、データ型

```haskell
data Color = Red | Orange | Yellow | Green deriving (Enum)
```

が与えられたとき、次のようになる:

```haskell
[Orange ..]         ==  [Orange, Yellow, Green]  
fromEnum Yellow     ==  2
```

### `Bounded`のインスタンス導出

`Bounded`クラスは`minBound`と`maxBound`をクラスメソッドとして導入するが、これはそれぞれその型の最小、最大の要素を定めるメソッドである。列挙型に対しては、`data`宣言にある最初と最後のコンストラクタが境界になる。コンストラクタが1つの型に対しては、コンストラクタを構成型の教会に適用したものになる。例として、次のデータ型を考える。

```haskell
data Pair a b = Pair a b deriving Bounded
```

これは次のような`Bounded`インスタンスを生成する:

```haskell
instance (Bounded a,Bounded b) => Bounded (Pair a b) where  
    minBound = Pair minBound minBound  
    maxBound = Pair maxBound maxBound
```

### `Read`と`Show`のインスタンス導出

`Read`と`Show`のインスタンス導出で自動的に導入されるクラスメソッドは`showsPrec, readsPrec, showList, readList`である。これらは値を文字列に変換し、文字列をパースして値にするために使われる。

関数`showsPrec d x r`は優先度`d`(`0`から`11`の間の数値をとる)、値`x`、文字列`r`を受ける。これは`r`に`x`の文字列表現を結合したものを返す。`showsPrec`は次の規則を満たす: `showsPrec d x r ++ s == showsPred d x (r ++ s)` この表現は、`x`のトップレベルコンストラクタの優先度が`d`よりも小さいときは括弧で囲われる。追加のパラメータ`r`は木などの構造を木のサイズに対して二次関数的でなく線形時間で出力するためには必須になる。

関数`readsPrec d s`は優先度`d`(`0`から`10`の数値をとる)と文字列`s`を受け取り、文字列の先頭から値をパースすることを試み、(パースされた値, 残りの文字列)なるペアのリストを返す。パースに成功することがない場合、返却されるリストは空になる。括弧のない中置演算子の適用のパースが成功するのは演算子の優先度が`d`以上の時だけである。

次が成り立つ。

```haskell
(x,"") は (readsPrec d (showsPrec d x "")) の要素である
```

つまり、`readsPrec`は`showsPrec`によって生成された文字列をパースでき、`showsPrec`に初めに渡された値が得られるべきである。

`showList`と`readList`は標準的でない表示を使ってオブジェクトのリストを表現できるようにする。

`readsPrec`は文字列以外の標準的な型のどんな有効な表現もパースできる。文字列に対してはクォートされた文字列のみが許され、他にはリストに対しては、ブラケットで囲まれた形`[...]`のみが許される。詳細は9章(**[訳注]** リンク)を見よ。

`show`の結果は、結合性の宣言がその型が宣言された時点で有効になっている場合であれば、定数のみを含んだHaskellの文法的に正しい式になる。戻り値は、データ型で定義されたコンストラクタの名前と括弧、スペースのみを含む。ラベル付きコンストラクタフィールドが使われているときは、波括弧、コンマ、フィールド名、等号も使われる。括弧は**結合性を無視して**必要なところでだけ追加される。`show`の戻り値は、すべてのコンポーネント型がread可能ならば`read`できる。(これはPreludeで定義されたすべてのインスタンスに対しては正しいが、ユーザーによって定義されたインスタンスではそうとは限らない。)

`Read`のインスタンス導出によって、次のようなことが仮定される。そしてこれらの過程は`Show`のインスタンス導出も従う:

- コンストラクタが中置演算子として定義されているならば、`Read`インスタンスの導出はコンストラクタの(前置形ではなく)中置適用のみをパースする。
- 結合性は括弧を減らすためには使われないが、優先度はそうと使われる場合がある。例として

    ```haskell
    infixr 4 :$
    data T = Int :$ T | NT
    ```

    があったとき、

    - `show (1 :$ 2 :$ NT)`は文字列`"1 :$ (2 :$ NT)"`を生成する。
    - `read "1 :$ (2 :$ NT)"`は成功し、当然の結果を返す。
    - `read 1 :$ 2 :$ NT`は失敗する。

- コンストラクタがレコード構文により定義されている場合、導出された`Read`はレコード構文の形のみパースし、さらにフィールドは元々の宣言と同じ順番でしか与えることはできない。
- `Read`インスタンスの導出は入力文字列のトークンの間に任意のHaskellの空白を許す。余分な括弧も許される。

`Read`と`Show`インスタンスの導出が不適切なケースもある。次のような問題がある:

- 循環構造はこれらのインスタンスによっては出力、読み取りできない。
- 出力によって部分構造の共有が失われる。つまり、オブジェクトが出力された表現は必要より遥かに大きくなる場合がある。
- `read`で使われるパースの方法は非常に非効率的であるので、巨大な構造の読み取りは非常に遅い場合がある。
- Preludeで定義された型の出力はユーザーによる制御ができない。例えば、浮動小数点数のフォーマットを変える方法がない。

### 例

完結した例として木構造を考えよう:

```haskell
data Tree a = Leaf a | Tree a :^: Tree a
    deriving (Eq, Ord, Read, Show)
```

`Tree`は列挙型でもなければコンストラクタが1つでもないので、`Bounded`と`Enum`のインスタンスの自動導出はできない。`Tree`のインスタンス宣言の完全なものはFigure 11.1にある。デフォルトクラスメソッド定義に注意せよ。例えば、`Ord`に対しては`<=`のみが定義されており、ほかのクラスメソッド(`<,>,>=,max,min`)はFigure 6.1に示されている、クラス宣言で与えられたデフォルト値によって与えられている。

**Figure11.1**: インスタンス導出の例
```haskell
infixr 5 :^:  
data Tree a =  Leaf a  |  Tree a :^: Tree a  
 
instance (Eq a) => Eq (Tree a) where  
        Leaf m == Leaf n  =  m==n  
        u:^:v  == x:^:y   =  u==x && v==y  
             _ == _       =  False  
 
instance (Ord a) => Ord (Tree a) where  
        Leaf m <= Leaf n  =  m<=n  
        Leaf m <= x:^:y   =  True  
        u:^:v  <= Leaf n  =  False  
        u:^:v  <= x:^:y   =  u<x || u==x && v<=y  
 
instance (Show a) => Show (Tree a) where  
 
        showsPrec d (Leaf m) = showParen (d > app_prec) showStr  
          where  
             showStr = showString "Leaf " . showsPrec (app_prec+1) m  
 
        showsPrec d (u :^: v) = showParen (d > up_prec) showStr  
          where  
             showStr = showsPrec (up_prec+1) u .  
                       showString " :^: "      .  
                       showsPrec (up_prec+1) v  
                -- Note: right-associativity of :^: ignored  
 
instance (Read a) => Read (Tree a) where  
 
        readsPrec d r =  readParen (d > up_prec)  
                         (\r -> [(u:^:v,w) |  
                                 (u,s) <- readsPrec (up_prec+1) r,  
                                 (":^:",t) <- lex s,  
                                 (v,w) <- readsPrec (up_prec+1) t]) r  
 
                      ++ readParen (d > app_prec)  
                         (\r -> [(Leaf m,t) |  
                                 ("Leaf",s) <- lex r,  
                                 (m,t) <- readsPrec (app_prec+1) s]) r  
 
up_prec  = 5    -- :^: の結合優先度
app_prec = 10   -- 適用はどんな結合優先度の高い演算子よりも優先度が高い
```
