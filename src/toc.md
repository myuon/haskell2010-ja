
# Haskell2010 Language Report 日本語訳

翻訳元: [Haskell2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)

Simon Marlow (editor)

著作権表示
著者と発行者はこのレポートを Haskell コミュニティ全体へ帰属させることを意図しており、この表示を含め文章全体が再現される限りにおいてはコピーと配布をいかなる目的でも許可している。このレポートの改変も、改変がそうと明示されており、Haskell2010 言語の定義であることを主張しなければ、いかなる目的でもコピーや再配布することができる。

> Copyright notice.
> The authors and publisher intend this Report to belong to the entire Haskell community, and grant permission to copy and distribute it for any purpose, provided that it is reproduced in its entirety, including this Notice. Modified versions of this Report may also be copied and distributed for any purpose, provided that the modified version is clearly presented as such, and that it does not claim to be a definition of the language Haskell 2010.

- 目次
- 前書き
  - 目標
  - Haskell2010: 言語とライブラリ
  - Haskell98 からの拡張
  - リソース
  - 言語の作成

### I. Haskell 2010 言語

1. [序説](./chapters/1-introduction.md)
   - プログラムの構造
   - Haskell のコア
   - 値と型
   - 名前空間
2. [字句構造](./chapters/2-lexical-structure.md)
   - 慣習的表記
   - 字句の構造
   - コメント
   - 識別子と演算子
   - 数値リテラル
   - 文字と文字列リテラル
   - レイアウト
3. [式](./chapters/3-expressions.md)
   - エラー
   - 変数、コンストラクタ、演算子、リテラル
   - カリー化された適用とラムダ抽象
   - 演算子の適用
   - セクション
   - 条件
   - リスト
   - タプル
   - ユニット式と括弧つき式
   - 算術列
   - リスト内法表記
   - let 式
   - case 式
   - do 式
   - フィールドラベルのついたデータ型
   - 型シグネチャ
   - パターンマッチ
4. [宣言と束縛](./chapters/4-declarations-and-bindings.md)
   - 型とクラスの概観
   - ユーザー定義データ型
   - 型クラスとオーバーロード
   - ネストされた宣言
   - 関数とパターン束縛の静的な意味論
   - カインド推論
5. [モジュール](./chapters/5-modules.md)
   - モジュール構造
   - export リスト
   - import 宣言
   - インスタンス宣言の import と export
   - 名前衝突とクロージャー
   - 標準 Prelude
   - 分割コンパイル
   - 抽象データ型
6. 事前定義された型とクラス
   - 標準的な Haskell の型
   - 正格評価
   - 標準的な Haskell のクラス
   - 数値
7. 入出力の基本
   - 標準的な I/O 関数
   - I/O 操作列
   - I/O モナドにおける例外処理
8. Foreign Function Interface
   - 外部言語
   - コンテキスト
   - 字句構造
   - 外部宣言
   - 外部エンティティの規格
   - マーシャリング
   - 外部 C インターフェイス
9. 標準 Prelude
   - Prelude `PreludeList`
   - Prelude `PreludeText`
   - Prelude `PreludeIO`
10. [文法リファレンス](./chapters/10-syntax-reference.md)
    - 慣習的表記
    - 字句文法
    - レイアウト
    - 文芸的コメント
    - 文脈自由文法
    - 結合の解決
11. [インスタンス導出の仕様](./chapters/11-specification-of-derived-instances.md)
    - `Eq`と`Ord`のインスタンス導出
    - `Enum`のインスタンス導出
    - `Bounded`のインスタンス導出
    - `Read`と`Show`のインスタンス導出
    - 例
12. [コンパイラプラグマ](./chapters/12-compiler-pragmas.md)
    - インライン化
    - 限定化
    - 言語拡張

### Haskell 2010 ライブラリ

13. [Control.Monad](./chapters/13-control-monad.md)
    - ファンクターとモナドクラス
    - 関数
14. `Data.Array`
    - 不変非正格配列
    - 配列の構成
    - 配列へのアクセス
    - 配列の逐次更新
    - 導出された配列
    - 規格
15. `Data.Bits`
16. `Data.Char`
    - 文字と文字列
    - 文字の分類
    - 複合語の慣習
    - 単一の数値文字
    - 数値の表現
    - 文字列の表現
17. `Data.Complex`
    - 直行形式
    - 極形式
    - 共役
    - 規格
18. `Data.Int`
    - 符号付き整数型
19. `Data.Ix`
    - `Ix`クラス
    - `Ix`インスタンスの導出
20. `Data.List`
    - 基本的な関数
    - リスト変換
    - リストの縮小(畳み込み)
    - リストの作成
    - 部分リスト
    - リストの検索
    - リストの添え字アクセス
    - リストの zip/unzip
    - 特別なリスト
    - 一般化された関数
21. `Data.Maybe`
    - `Maybe`型と操作
    - 規格
22. `Data.Ratio`
    - 規格
23. `Data.Word`
    - 符号なし整数型
24. `Foreign`
25. `Foreign.C`
26. `Foreign.C.Error`
    - `errno`値の Haskell における表現
27. `Foreign.C.String`
    - C の文字列
    - C のワイド文字列
28. `Foreign.C.Types`
    - C の型の表現
29. `Foreign.ForeignPtr`
    - ファイナライズされたデータポインター
30. `Foreign.Marshal`
31. `Foreign.Marshal.Alloc`
    - メモリ確保
32. `Foregin.Marshal.Array`
    - 配列のマーシャリング
33. `Foreign.Marshal.Error`
34. `Foreign.Marshal.Utils`
    - 一般的なマーシャリングのユーティリティ
35. `Foregin.Ptr`
    - データポインタ
    - 関数ポインタ
    - 整数型とポインタのロスのない相互変換
36. `Foregin.StablePtr`
    - Haskell の値への安定参照
37. `Foregin.Storable`
38. `Numeric`
    - 表示
    - 読み取り
    - その他
39. `System.Environment`
40. `Sytem.Exit`
41. `System.IO`
    - IO モナド
    - ファイルとハンドル
    - ファイルのオープンとクローズ
    - ハンドルに対する捜査
    - テキストの入力と出力
42. `System.IO.Error`
    - I/O エラー
    - I/O エラーの型
    - I/O エラーの送出と捕捉

参考文献
