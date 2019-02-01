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

