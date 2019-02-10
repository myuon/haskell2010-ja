# Control.Monad

```hs
module Control.Monad (  
    Functor(fmap),  Monad((>>=), (>>), return, fail),  MonadPlus(mzero, mplus),  
    mapM,  mapM_,  forM,  forM_,  sequence,  sequence_,  (=<<),  (>=>),  (<=<),  
    forever,  void,  join,  msum,  filterM,  mapAndUnzipM,  zipWithM,  
    zipWithM_,  foldM,  foldM_,  replicateM,  replicateM_,  guard,  when,  
    unless,  liftM,  liftM2,  liftM3,  liftM4,  liftM5,  ap  
  ) where
```

`Control.Monad`モジュールは`Functor`, `Monad`, `MonadPlus`クラスと、いくつかのモナド上の便利な演算子を提供する。

(**訳注**: 以下の説明は現状のGHCの提供するものとは異なっています。)

## FunctorとMonadクラス

#### 型クラス

```hs
class Functor f where
```

`Functor`クラスはそれ上にマップできるような型に使われる。`Functor`インスタンスは次の法則を満たすべきである:

- `fmap id  ==  id`
- `fmap (f . g)  ==  fmap f . fmap g`

リスト、`Data.Maybe.Maybe`、`System.IO.IO`に対する`Functor`のインスタンスはこの法則を満たす。

#### メソッド

```hs
fmap :: (a -> b) -> f a -> f b
```

#### インスタンス

```hs
instance Functor []
instance Functor IO
instance Functor Maybe
instance Ix i => Functor (Array i)
```

#### 型クラス

```hs
class Monad f where
```

`Monad`クラスはモナド上の基本的な演算子を定義する。**モナド**は**圏論**と呼ばれる数学の一分野から来た概念である。しかしながら、Haskellプログラマの観点からはモナドはアクションの**抽象データ型**のことであると思うのが最適である。Haskellの`do`式はモナディックな式を書くための便利な構文を提供している。

最小完全定義: `>>=`と`return`

`Monad`のインスタンスは次の法則をみたすべきである:

- `return a >>= k  ==  k a`
- `m >>= return  ==  m`
- `m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h`

`Monad`と`Functor`のインスタンスは追加で次の法則も満たすべきである。

- `fmap f xs  ==  xs >>= return . f`

Preludeで定義されているリスト、`Data.Maybe.Maybe`、`System.IO.IO`に対する`Monad`のインスタンスはこの法則を満たす。

#### メソッド

```hs
(>>=) :: m a -> (a -> m b) -> m b
```

最初のアクションによって生成された値を2つ目のアクションの引数として渡すことで連続して2つのアクションを合成する。

```hs
(>>) :: m a -> m b -> m b
```

最初のアクションによって生成された値を捨てることで連続して2つのアクションを合成する。これは手続き型言語での順列を表す演算子(例えばセミコロン)のようなものである。

```hs
return :: a -> m a
```

値をモナディックな型に注入する。

```hs
fail :: String -> m a
```

メッセージとともに失敗する。この演算子はモナドの数学的な定義の一部ではないが、`do`式の中でパターンマッチに失敗した際に呼ばれる。

#### インスタンス

```hs
instance Monad []
instance Monad IO
instance Monad Maybe
```

#### 型クラス

```hs
class Monad m => MonadPlus m where
```

モナドであって、選択と失敗を備えたもの。

#### メソッド

```hs
mzero :: m a
```

`mplus`の単位元。さらに次の法則も満たすべきである:

- `mzero >>= f  =  mzero`
- `v >> mzero   =  mzero`

```hs
mplus :: m a -> m a -> m a
```

結合的な演算子。

#### インスタンス

```hs
instance MonadPlus []
instance MonadPlus Maybe
```

## 関数

### 名前付けの慣習

このライブラリの関数は次のような名前付けの慣習に従っている。

- `M`が後ろにつくものはクライスリ圏での関数を表す。すなわち、モナド型コンストラクタ`m`が関数の結果(カリー化していない形)に対して付けられ、それ以外にはつかない。よって、例としては次:

```hs 
filter  ::              (a ->   Bool) -> [a] ->   [a]  
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
```

- `_`が後ろにつくものは結果の型が`(m a)`から`(m ())`になる。よって、例としては次:

```hs
sequence  :: Monad m => [m a] -> m [a]  
sequence_ :: Monad m => [m a] -> m ()
```

- `m`が先頭につくものは、すでに存在する関数をモナディックな形に一般化したものである。よって、例としては次:

```hs
sum  :: Num a       => [a]   -> a  
msum :: MonadPlus m => [m a] -> m a
```

### `Monad`の基本的な関数

```hs
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
```

`mapM f`は`sequence . map f`と同値である。

```hs
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
```

`mapM_ f`は`sequence_ . map f`と同値である。

```hs
forM :: Monad m => [a] -> (a -> m b) -> m [b]
```

`forM`は`mapM`の引数を逆にしたものである。

```hs
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
```

`forM_`は`mapM_`の引数を逆にしたものである。

```hs
sequence :: Monad m => [m a] -> m [a]
```

各アクションを左から右に順番に評価し、結果を集める。

```hs
sequence_ :: Monad m => [m a] -> m ()
```

各アクションを左から右に順番に評価し、結果を無視する。

```hs
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

`>>=`と同じで、引数が入れ替えたもの。

```hs
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

左から右へのモナドのクライスリ合成。

```hs
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
```

右から左へのモナドのクライスリ合成。`>>=`の引数を逆にしたもの。

```hs
forever :: Monad m => m a -> m b
```

`forever act`はアクションを無限に繰り返す。

```hs
void :: Functor f => f a -> f ()
```

`void value`は`IO`アクションの結果の値といった評価の結果を捨て、または無視する。

### リストの関数の一般化

```hs
join :: Monad m => m (m a) -> m a
```

`join`関数はモナドの従来のjoin演算子である。モナドの構造を1レベル取り除き、(束縛されている)引数をより外側のレベルへ射影するために使われる。

```hs
msum :: MonadPlus m => [m a] -> m a
```

リストに対する`concat`関数の一般化である。

```hs
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
```

リストに対する`filter`関数の一般化である。

```hs
mapAndUnzipM :: Monad m => (a -> m (b, c)) -> [a] -> m ([b], [c])
```

`mapAndUnzipM`関数は第一引数をリストに適用し、ペアのリストを結果として返す。この関数は複雑なデータや状態変化を行うようなモナドで主に使われる。

```hs
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
```

`zipWithM`関数は`zipWith`を任意のモナドに一般化したものである。

```hs
zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
```

`zipWithM_`は`zipWithM`の拡張で、最後の結果を無視するものである。

```hs
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
```

`foldM`関数は`foldl`に似たものであるが、結果がモナドに包まれているところが異なる。`foldM`は引数であるリストを左から右に向かって走査することに注意せよ。このことは、`>>`と"畳み込み関数"が可換でない場合に問題になりうる。

```hs
       foldM f a1 [x1, x2, ..., xm]
==

       do  
         a2 <- f a1 x1  
         a3 <- f a2 x2  
         ...  
         f am xm
```

右から左に向かった評価が必要であれば、入力のリストを反転させればよい。

```hs
foldM_ :: Monad m => (a -> b -> m a) -> a -> [b] -> m ()
```

`foldM`に近いが、結果を捨てる。

```hs
replicateM :: Monad m => Int -> m a -> m [a]
```

`replicateM n act`はアクションを`n`回行い、結果を集める。

```hs
replicateM_ :: Monad m => Int -> m a -> m ()
```

`replicateM`に近いが、結果を捨てる。

### モナディックな式の条件付き実行

```hs
guard :: MonadPlus m => Bool -> m ()
```

`guard b`は`b`が`True`であれば`return ()`であり、`b`が`False`であれば`mzero`である。

```hs
when :: Monad m => Bool -> m () -> m ()
```

モナディックな式の条件付き実行である。例えば、

```hs
       when debug (putStr "Debugging\n")
```

はブール値`debug`が`True`であれば文字列`Debugging\n`を出力し、そうでなければ何もしない。

```hs
unless :: Monad m => Bool -> m () -> m ()
```

`when`の逆である。

### モナディックな持ち上げ演算子

```hs
liftM :: Monad m => (a1 -> r) -> m a1 -> m r
```

関数をモナドに持ち上げる。

```hs
liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
```

関数をモナドに持ち上げ、モナディックな引数を左から右へと走査する。例えば、

```hs
    liftM2 (+) [0,1] [0,2] = [0,2,1,3]  
    liftM2 (+) (Just 1) Nothing = Nothing
```

```hs
liftM3 :: Monad m => (a1 -> a2 -> a3 -> r)
                     -> m a1 -> m a2 -> m a3 -> m r
```

関数をモナドに持ち上げ、モナディックな引数を左から右へと走査する。(`liftM2`を参照)

```hs
liftM4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> r)
                     -> m a1 -> m a2 -> m a3 -> m a4 -> m r
```

関数をモナドに持ち上げ、モナディックな引数を左から右へと走査する。(`liftM2`を参照)

```hs
liftM5 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> r)
                     -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
```

関数をモナドに持ち上げ、モナディックな引数を左から右へと走査する。(`liftM2`を参照)

```hs
ap :: Monad m => m (a -> b) -> m a -> m b
```

多く場合`liftM`演算子は、(関数適用を持ち上げる)`ap`を使用したものに置き換えることができる。

```hs
       return f ‘ap‘ x1 ‘ap‘ ... ‘ap‘ xn
```

は次と等しい。

```hs
       liftMn f x1 x2 ... xn
```
