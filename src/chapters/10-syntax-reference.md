# 文法リファレンス

## 慣習的表記

以下の慣習的表記が文法を表現するのにつかわれる:

- <code>[<em>pattern</em>]</code> 省略可能
- <code>{<em>pattern</em>}</code> 0回以上の繰り返し
- <code>(<em>pattern</em>)</code> グループ化
- <code><em>pat<sub>1</sub></em> | <em>pat<sub>2</sub></em></code> 選択
- <code>pat<sub>&lt;pat'&gt;</sub></code> 差(`pat`によって生成された要素で、`pat'`で生成されたものを除いたもの)
- <tt>fibonacci</tt> タイプライターフォントで表記される終端文法

BNFのような文法をレポートを通して用いる。文法の生成は次のような形をしている:

<pre>
nonterm -> alt<sub>1</sub> | alt<sub>2</sub> | .. | alt<sub>n</sub>
</pre>

字句文法、文脈自由文法いずれも曖昧さが残るが、これは文法語句をできる限り長く取り、左から右に進む(shift-reduceパースでは、shift/reduceコンフリクトはシフトを取ることで解決する)ことで解決するものとする。字句文法では、これは「最長一致」と呼ばれるルールである。文脈自由文法では、これは条件式やlet式、ラムダ抽象などが右方向に取れるだけ長くとることを表す。

## 字句文法

|||||
|--|--|--|--|
|<em>program</em>|→|{ <em>lexeme</em> &#124; <em>whitespace</em> }| |
|<em>lexeme</em>|→|<em>qvarid</em> &#124; <em>qconid</em> &#124; <em>qvarsym</em> &#124; <em>qconsym</em>| |
|		|&#124;| <em>literal</em> &#124; <em>special</em> &#124; <em>reservedop</em> &#124; <em>reservedid</em>| |
|<em>literal</em>|→|<em>integer</em> &#124; <em>float</em> &#124; <em>char</em> &#124; <em>string</em>| |
|<em>special</em>|→|( &#124; ) &#124; , &#124; ; &#124; [ &#124; ] &#124; ` &#124; { &#124; } | |
|||||
|<em>whitespace</em>|→|<em>whitestuff</em> {<em>whitestuff</em>| |
|<em>whitestuff</em>|→|<em>whitechar</em> &#124; <em>comment</em> &#124; <em>ncomment</em>| |
|<em>whitechar</em>|→|<em>newline</em> &#124; <em>vertab</em> &#124; <em>space</em> &#124; <em>tab</em> &#124; <em>uniWhite</em>| |
|<em>newline</em>|→|<em>return</em> <em>linefeed</em> &#124; <em>return</em> &#124; <em>linefeed</em> &#124; <em>formfeed</em>| |
|<em>return</em>|→|a carriage return| |
|<em>linefeed</em>|→|a line feed| |
|<em>vertab</em>|→|a vertical tab| |
|<em>formfeed</em>|→|a form feed| |
|<em>space</em>|→|a space| |
|<em>tab</em>|→|a horizontal tab| |
|<em>uniWhite</em>|→|any Unicode character defined as whitespace| |
|||||
|<em>comment</em>|→|dashes [ any⟨symbol⟩ {any} ] newline| |
|<em>dashes</em>|→|-- {-}| |
|<em>opencom</em>|→|{-| |
|<em>closecom</em>|→|-}| |
|<em>ncomment</em>|→|<em>opencom</em> <em>ANY seq</em> {<em>ncomment</em> <em>ANY seq</em>} <em>closecom</em>| |
|<em>ANY seq</em>|→|{<em>ANY</em>}<sub>⟨{<em>ANY</em>} ( <em>opencom</em> &#124; <em>closecom</em> ) {<em>ANY</em>}⟩</sub>| |
|<em>ANY</em>|→|<em>graphic</em> &#124; <em>whitechar</em>| |
|<em>any</em>|→|<em>graphic</em> &#124; <em>space</em> &#124; <em>tab</em>| |
|<em>graphic</em>|→|<em>small</em> &#124; <em>large</em> &#124; <em>symbol</em> &#124; <em>digit</em> &#124; <em>special</em> &#124; " &#124; '| |
|||||
|<em>small</em>|→|<em>ascSmall</em> &#124; <em>uniSmall</em> &#124; _| |
|<em>ascSmall</em>|→|<tt>a</tt> &#124; <tt>b</tt> &#124; … &#124; <tt>z</tt>| |
|<em>uniSmall</em>|→|any Unicode lowercase letter| |
|||||
|<em>large</em>|→|<em>ascLarge</em> &#124; <em>uniLarge</em>| |
|<em>ascLarge</em>|→|<tt>A</tt> &#124; <tt>B</tt> &#124; … &#124; <tt>Z</tt>| |
|<em>uniLarge</em>|→|any uppercase or titlecase Unicode letter| |
|<em>symbol</em>|→|<em>ascSymbol</em> &#124; <em>uniSymbol</em><sub>⟨<em>special</em> &#124; _ &#124; " &#124; '⟩</sub>| |
|||||
|<em>ascSymbol</em>|→|<tt>!</tt> &#124; <tt>#</tt> &#124; <tt>$</tt> &#124; <tt>%</tt> &#124; <tt>&</tt> &#124; <tt>⋆</tt> &#124; <tt>+</tt> &#124; <tt>.</tt> &#124; <tt>/</tt> &#124; <tt><</tt> &#124; <tt>=</tt> &#124; <tt>></tt> &#124; <tt>?</tt> &#124; <tt>@</tt>| |
|                |&#124;| <tt>\\</tt> &#124; <tt>^</tt> &#124; <tt>&#124;</tt> &#124; <tt>-</tt> &#124; <tt>~</tt> &#124; <tt>:</tt>| |
|<em>uniSymbol</em>|→|any Unicode symbol or punctuation| |
|<em>digit</em>|→|<em>ascDigit</em> &#124; <em>uniDigit</em>| |
|<em>ascDigit</em>|→|<tt>0</tt> &#124; <tt>1</tt> &#124; … &#124; <tt>9</tt>| |
|<em>uniDigit</em>|→|any Unicode decimal digit| |
|<em>octit</em>|→|<tt>0</tt> &#124; <tt>1</tt> &#124; … &#124; <tt>7</tt>| |
|<em>hexit</em>|→|<em>digit</em> &#124; <tt>A</tt> &#124; … &#124; <tt>F</tt> &#124; <tt>a</tt> &#124; … &#124; <tt>f</tt>| |
|||||
|<em>varid</em>|→|(<em>small</em> {<em>small</em> &#124; <em>large</em> &#124; <em>digit</em> &#124; <tt>'</tt> })<sub>⟨reservedid⟩</sub>| |
|<em>conid</em>|→|<em>large</em> {<em>small</em> &#124; <em>large</em> &#124; <em>digit</em> &#124; <tt>'</tt> }| |
|<em>reservedid</em>|→|<tt>case</tt> &#124; <tt>class</tt> &#124; <tt>data</tt> &#124; <tt>default</tt> &#124; <tt>deriving</tt> &#124; <tt>do</tt> &#124; <tt>else</tt>| |
|		|&#124;| <tt>foreign</tt> &#124; <tt>if</tt> &#124; <tt>import</tt> &#124; <tt>in</tt> &#124; <tt>infix</tt> &#124; <tt>infixl</tt>| |
|		|&#124;| <tt>infixr</tt> &#124; <tt>instance</tt> &#124; <tt>let</tt> &#124; <tt>module</tt> &#124; <tt>newtype</tt> &#124; <tt>of</tt>| |
|		|&#124;| <tt>then</tt> &#124; <tt>type</tt> &#124; <tt>where</tt> &#124; <tt>_</tt>| |
|||||
|<em>varsym</em>|→|( <em>symbol</em><sub>⟨<tt>:</tt>⟩</sub> {<em>symbol</em>} )<sub>⟨reservedop &#124; dashes⟩</sub>| |
|<em>consym</em>|→|( <tt>:</tt> {<em>symbol</em>})<sub>⟨reservedop⟩</sub>| |
|<em>reservedop</em>|→|<tt>..</tt> &#124; <tt>:</tt> &#124; <tt>::</tt> &#124; <tt>=</tt> &#124; <tt>\\</tt> &#124; <tt>&#124;</tt> &#124; <tt><-</tt> &#124; <tt>-></tt> &#124;  <tt>@</tt> &#124; <tt>~</tt> &#124; <tt>=></tt>| |
|||||
|<em>varid</em>| | |(variables)|
|<em>conid</em>| | |(constructors)|
|<em>tyvar</em>|→|<em>varid</em>|(type variables)|
|<em>tycon</em>|→|<em>conid</em>|(type constructors)|
|<em>tycls</em>|→|<em>conid</em>|(type classes)|
|<em>modid</em>|→|{<em>conid</em> <tt>.</tt>} <em>conid</em>|(modules)|
|||||
|<em>qvarid</em>|→|[ <em>modid</em> <tt>.</tt> ] <em>varid</em>| |
|<em>qconid</em>|→|[ <em>modid</em> <tt>.</tt> ] <em>conid</em>| |
|<em>qtycon</em>|→|[ <em>modid</em> <tt>.</tt> ] <em>tycon</em>| |
|<em>qtycls</em>|→|[ <em>modid</em> <tt>.</tt> ] <em>tycls</em>| |
|<em>qvarsym</em>|→|[ <em>modid</em> <tt>.</tt> ] <em>varsym</em>| |
|<em>qconsym</em>|→|[ <em>modid</em> <tt>.</tt> ] <em>consym</em>| |
|||||
|<em>decimal</em>|→|<em>digit</em>{<em>digit</em>| |
|<em>octal</em>|→|<em>octit</em>{<em>octit</em>| |
|<em>hexadecimal</em>|→|<em>hexit</em>{<em>hexit</em>| |
|||||
|<em>integer</em>|→|<em>decimal</em>| |
|		|&#124;| <tt>0o</tt> <em>octal</em> &#124; <tt>0O</tt> <em>octal</em>| |
|		|&#124;| <tt>0x</tt> <em>hexadecimal</em> &#124; <tt>0X</tt> <em>hexadecimal</em>| |
|<em>float</em>|→|<em>decimal</em> <tt>.</tt> <em>decimal</em> [<em>exponent</em>| |
|		|&#124;|	<em>decimal</em> <em>exponent</em>| |
|<em>exponent</em>|→|(<tt>e</tt> &#124; <tt>E</tt>) [<tt>+</tt> &#124; <tt>-</tt>] <em>decimal</em>| |
|||||
|<em>char</em>|→|<tt>'</tt> (<em>graphic</em><sub>⟨<tt>'</tt> &#124; <tt>\\</tt>⟩</sub> &#124; <em>space</em> &#124; <em>escape</em><sub>⟨<tt>\&</tt>⟩</sub>) <tt>'</tt>| |
|<em>string</em>|→|<tt>"</tt> {<em>graphic</em><sub>⟨<tt>"</tt> &#124; <tt>\\</tt>⟩</sub> &#124; <em>space</em> &#124; <em>escape</em> &#124; <em>gap</em>} <tt>"</tt>| |
|<em>escape</em>|→|<tt>\\</tt> ( <em>charesc</em> &#124; <em>ascii</em> &#124; <em>decimal</em> &#124; <tt>o</tt> <em>octal</em> &#124; <tt>x</tt> <em>hexadecimal</em> )| |
|<em>charesc</em>|→|<tt>a</tt> &#124; <tt>b</tt> &#124; <tt>f</tt> &#124; <tt>n</tt> &#124; <tt>r</tt> &#124; <tt>t</tt> &#124; <tt>v</tt> &#124; <tt>\\</tt> &#124; <tt>"</tt> &#124; <tt>'</tt> &#124; <tt>&</tt>| |
|<em>ascii</em>|→|<tt>^</tt><em>cntrl</em> &#124; <tt>NUL</tt> &#124; <tt>SOH</tt> &#124; <tt>STX</tt> &#124; <tt>ETX</tt> &#124; <tt>EOT</tt> &#124; <tt>ENQ</tt> &#124; <tt>ACK</tt>| |
|		|&#124;| <tt>BEL</tt> &#124; <tt>BS</tt> &#124; <tt>HT</tt> &#124; <tt>LF</tt> &#124; <tt>VT</tt> &#124; <tt>FF</tt> &#124; <tt>CR</tt> &#124; <tt>SO</tt> &#124; <tt>SI</tt> &#124; <tt>DLE</tt>| |
|		|&#124;| <tt>DC1</tt> &#124; <tt>DC2</tt> &#124; <tt>DC3</tt> &#124; <tt>DC4</tt> &#124; <tt>NAK</tt> &#124; <tt>SYN</tt> &#124; <tt>ETB</tt> &#124; <tt>CAN</tt>| |
|		|&#124;| <tt>EM</tt> &#124; <tt>SUB</tt> &#124; <tt>ESC</tt> &#124; <tt>FS</tt> &#124; <tt>GS</tt> &#124; <tt>RS</tt> &#124; <tt>US</tt> &#124; <tt>SP</tt> &#124; <tt>DEL</tt>| |
|<em>cntrl</em>|→|<em>ascLarge</em> &#124; <tt>@</tt> &#124; <tt>[</tt> &#124; <tt>\\</tt> &#124; <tt>]</tt> &#124; <tt>^</tt> &#124; <tt>_</tt>| |
|<em>gap</em>|→|<tt>\\</tt> <em>whitechar</em> {<em>whitechar</em>} <tt>\\</tt>| |

## レイアウト

セクション2.7(**[訳注]** TODO:リンク)ではレイアウトルールに対する非形式的な議論を見た。このセクションではより正確に定義をする。

Haskellプログラムの意味はその**レイアウト**に依存する場合がある。レイアウトが意味に与える効果は波括弧とセミコロンをレイアウトによって決定される位置に追加することで完全に説明できる。このようにして追加されたプログラムの意味は今やレイアウトによって影響を受けない。

レイアウトがプログラムに対して与える影響は、このセクションで波括弧とセミコロンをどのように追加するかを説明することで指定される。仕様は、プログラムの返還を行う関数`L`の形で与えられる。`L`の入力は次のようなものである:

- Haskellレポートにある字句文法によって定められた語句の列であって、さらに次のような追加の語句を含む:
    - `let, where, do, of`キーワードの後に`{`が続かない場合、トークン{n}がキーワードの後に挿入される。ただしnは、続くトークンがあればそのインデントを表し、ファイルの終端に達した場合は0を表す。
    - モジュールの最初のトークンが`{`でも`module`でもない場合、そのトークンのインデントをnとすると、{n}が先行する。
    - 同じ行で空白のみが最初のトークンに先行する場合、`<n>`がこの語句に先行する。ここでnは語句のインデントであり、もしインデントが存在しない場合には先の2つのルールの結果として{n}が先行することとなる。(注意: 文字列リテラルはセクション[2.6](./2-lexical-structure.md)で説明したように、複数行に及ぶこともある。よって次のコードにおいては、`\Bill`の前に`<n>`が挿入されることはない。なぜなら、これは完全な語句の開始でもなければ`,`の前でもなく、単純に空白文字によって先行されているだけだからである。)

```haskell
f = ("Hello \  
        \Bill", "Jake")    
```

- "レイアウト文脈"のスタックで、各要素が次のいずれかであるもの:
    - 文脈の囲みを明示することを表すゼロ(すなわち、プログラマは開いた波括弧を書いていた場合) 最も内側の文脈が0である場合、レイアウトトークンは文脈の囲いが終了するか新しい文脈が追加されるまで挿入されない。
    - レイアウト文脈を囲うインデントの段数を表す正の整数

語句の"インデント"とは、語句の最初の文字の段数である。そしてある行のインデントとは、最も左にある語句のインデントである。段数を決定するには、次の慣習に従う固定幅フォントを使っていると仮定する。
    - 次の文字 `newline`, `return`, `linefeed`, `formfeed` は新しい行を開始する。
    - 最初の段数は0ではなく、1とする。
    - タブは8文字分の幅だけ次の文字位置までの間が空く。
    - タブ文字は現在の位置をタブの次の文字位置まで揃えるためにそれに足りるだけのスペースを挿入させる。

レイアウトルールのために、ソースコード中のユニコード文字もASCII文字と同じ固定された幅をもつものとみなされる。しかし見た目の混乱を防ぐために、プログラマーは見た目に分からないレイアウトの意味を、空白でない文字幅に依存させるようなプログラムを書くことは避けるべきである。

関数適用 `L tokens []`  は、`tokens`をレイアウトに依存しないものへの変換である。ここで`tokens`はモジュールを字句解析して得られた結果で、上で説明したような段数を表す数字を追加している。`L`の定義は次のようになっている。ここで、`:`をストリームのコンストラクタ演算子として、`[]`を空のストリームとして使っている。

```haskell
L (< n >: ts) (m : ms)  = ;  :  (L ts (m : ms))             if m = n
                        = }  :  (L (< n >: ts) ms)          if n < m
L (< n >: ts) ms        = L ts ms
L ({n} : ts) (m : ms)   = {  :  (L ts (n : m : ms))         if n > m (ノート) 1)
L ({n} : ts) []         = {  :  (L ts [n])                  if n > 0 (ノート 1)
L ({n} : ts) ms         = {  :  }  :  (L (< n >: ts) ms)    (ノート 2)
L (} : ts) (0 : ms)     = }  :  (L ts ms)                   (ノート 3)
L (} : ts) ms           = parse-error                       (ノート 3)
L ({ : ts) ms           = {  :  (L ts (0 : ms))             (ノート 4)
L (t : ts) (m : ms)     = }  :  (L (t : ts) ms)             if m≠0 and parse-error(t)
                                                            (ノート 5)
L (t : ts) ms           = t  :  (L ts ms)
L [] []                 = []
L [] (m : ms)           = }  :  L [] ms                     if m≠0 (ノート 6)
```

**ノート1**
ネストされたコンテキストは文脈の囲い`(n > m)`よりも深くインデントされなければならない。そうでなければ、`L`は失敗し、コンパイラはレイアウトエラーを示すだろう。次は例である。

```haskell
  f x = let  
           h y = let  
    p z = z  
                 in p  
        in h
```
ここで、`p`の定義は、ここでは`h`の定義によって定まっている文脈の囲いのインデントより浅くインデントされている。

**ノート2**
(例えば)`where`の後に出現する最初のトークンがレイアウト文脈の囲いよりもインデントされていなかった場合、その`where`のブロックは空でなければならず、よって中身のないの波括弧が挿入される。`{n}`トークンは中身のない波括弧が明示されたを模倣して`<n>`によって置き換えられる。

**ノート3**
現在のレイアウト文脈を0と比べることで、明示された閉じ波括弧が明示された開き波括弧のみと対応していることを保証できる。明示された閉じ波括弧が明示されていない開き波括弧と対応している場合はパースエラーが出力される。

**ノート4**
この句は、ラベル付き構成と更新を含めた(セクション[3.15](./3-expressions.md))すべての波括弧の組が明示的なレイアウト文脈として扱われるようにするためのものである。この式はHaskell1.4とは異なっている。

**ノート5**
横の条件`parse-error(t)`は次のように解釈される: `L`によってこれまでに生成された次のトークン`t`をもつトークン列がHaskell文法において無効なものから始まっていることを表しており、また`L`によってこれまでに生成された`"}"`に続くトークン列がHaskell文法において有効なものから始まっていることを表している場合、`parse-error(t)`は真である。

m≠0は暗黙的に追加された閉じ波括弧が明示されていない開き波括弧と対応することを確認している。

**ノート6**
入力に終わりに、保留されている閉じ波括弧が挿入される。非レイアウト文脈に含まれている場合(すなわち、m = 0)、ここでエラーになる。

上のルールのいずれもマッチしない場合、このアルゴリズムは失敗する。例えば入力の最後に到達したときに、非レイアウト文脈が有効であれば、閉じ波括弧が存在しないので失敗することになる。このアルゴリズムによって検知されないエラー条件も一部存在する。例えば、`let }`である。

ノート1はレイアウト処理がパースエラーによって途中で停止する可能性があることを言っている。例えば

```haskell
let x = e; y = x in e'
```

は有効である。なぜならこれは次のように変換されるからである。

```haskell
let { x = e; y = x } in e'
```

閉じ波括弧は上のパースエラーのルールにより挿入される。


## 文芸的コメント

「文芸的コメント」の慣習は、リチャード・バードとフィリップ・ワドラーらがOrwell言語のために初めて導入し、そして次にドナルド・クヌースの「文芸的プログラミング」に影響を与えたものであるが、Haskellのソースコードを記述するためのもう一つのスタイルである。文芸的スタイルはコメントを書くことを、それをデフォルトとすることで推奨している。始めの文字が">"である行はプログラムの一部として扱われ、それ以外の行はすべてコメントとなる。

プログラムの本文は">"で始まる行のみを拾い、">"とそれに続く空白を置き換えることで復元することができる。その結果残る本文の中では、レイアウトやコメントは[10章](./10-syntax-reference.md)で説明したとおりに適用される。

">"を間違って省略してしまった場合に備えて、空でないコメント行に隣接するプログラム行はエラーになる。ここで空のコメント行とは、空白しか含まないもののことである。

慣習的に、コメントのスタイルはファイル拡張子によって指定される。".hs"であれば通常のHaskellファイルであり、".lhs"であれば文芸的Haskellファイルである。このスタイルを用いると、階乗の簡単なプログラムは次のようになる。

(**訳注**: 文芸的Haskellに対応するsyntax highlighterがないので通常のHaskellハイライトで代用しています。本来コメントとして扱われる`This literate...`や`This is the factorial...`などに色が付いていますがここは上でも説明があった通りコメントです。)

```hs
   This literate program prompts the user for a number  
   and prints the factorial of that number:  

> main :: IO ()  

> main = do putStr "Enter a number: "  
>           l <- readLine  
>           putStr "n!= "  
>           print (fact (read l))  

  This is the factorial function.  

> fact :: Integer -> Integer  
> fact 0 = 1  
> fact n = n ⋆ fact (n-1)
```

文芸的プログラミングという代替スタイルは、文章処理システムのLaTeXを使う際に特に適している。この慣習の下では、`\begin{code}...\end{code}`デリミタで囲まれた部分全体が文芸的プログラムのプログラム本文として扱われ、その他の行はすべてコメントである。より正確には:

- プログラムコードは`\begin{code}`に続く次の行から始まり
- プログラムコードは`\end{code}`で始まる行の直前で終わる (文字列リテラルは当然除く)

これらデリミタの前後に余分な空白行を挿入する必要は必ずしもないが、スタイルとしてはそれが望ましいであろう。例えば次のようになる。

```tex
\documentstyle{article}  

\begin{document}  

\chapter{Introduction}  

This is a trivial program that prints the first 20 factorials.  

\begin{code}  
main :: IO ()  
main =  print [ (n, product [1..n]) | n <- [1..20]]  
\end{code}  

\end{document}
```

このスタイルは同じファイル拡張子を用いる。同じファイルに対してこれら2つのスタイルを混ぜるのはおすすめできない。

## 文脈自由構文

|||||
|--|--|--|--|
|<em>module</em>|→|<tt>module</tt> <em>modid</em> [<em>exports</em>] <tt>where</tt> <em>body</em>| |
| |&#124;|	<em>body</em>| |
|<em>body</em>|→|{ <em>impdecls</em> <tt>;</tt> <em>topdecls</em> }| |
| |&#124;|	{ <em>impdecls</em> }| |
| |&#124;|	{ <em>topdecls</em> }| |
|||||
|<em>impdecls</em>|→|<em>impdecl<sub>1</sub></em> <tt>;</tt> … <tt>;</tt> <em>impdecl<sub>n</sub></em>|(<em>n</em> ≥ 1)|
|||||
|<em>exports</em>|→|( <em>export<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>export<sub>n</sub></em> [ <tt>,</tt> ] )|(<em>n</em> ≥ 0)|
|||||
|<em>export</em>|→|<em>qvar</em>| |
| |&#124;|	<em>qtycon</em> [(..) &#124; ( <em>cname<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>cname<sub>n</sub></em> )]|(<em>n</em> ≥ 0)|
| |&#124;|	<em>qtycls</em> [(..) &#124; ( <em>qvar<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>qvar<sub>n</sub></em> )]|(<em>n</em> ≥ 0)|
| |&#124;|	<tt>module</tt> <em>modid</em>| |
|||||
|<em>impdecl</em>|→|<tt>import</tt> [<tt>qualified</tt>] <em>modid</em> [<tt>as</tt> <em>modid</em>] [<em>impspec</em>]| |
| |&#124;| |(empty declaration)|
|||||
|<em>impspec</em>|→|( <em>import<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>import<sub>n</sub></em> [ <tt>,</tt> ] ) |(<em>n</em> ≥ 0)|
| |&#124;|	<tt>hiding</tt> ( <em>import<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>import<sub>n</sub></em> [ <tt>,</tt> ] )	    |(<em>n</em> ≥ 0)|
|||||
|<em>import</em>|→|<em>var</em>| |
| |&#124;|	<em>tycon</em> [ (..) &#124; ( <em>cname<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>cname<sub>n</sub></em> )]	    |(<em>n</em> ≥ 0)|
| |&#124;|	<em>tycls</em> [(..) &#124; ( <em>var<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>var<sub>n</sub></em> )]	    |(<em>n</em> ≥ 0)|
|<em>cname</em>|→|<em>var</em> &#124; <em>con</em>| |
|||||
|<em>topdecls</em>|→|<em>topdecl<sub>1</sub></em> <tt>;</tt> … <tt>;</tt> <em>topdecl<sub>n</sub></em>	    |(<em>n</em> ≥ 0)|
|<em>topdecl</em>|→|<tt>type</tt> <em>simpletype</em> = <em>type</em>| |
| |&#124;|	<tt>data</tt> [<em>context</em> =>] <em>simpletype</em> [= <em>constrs</em>] [<em>deriving</em>]| |
| |&#124;|	<tt>newtype</tt> [<em>context</em> =>] <em>simpletype</em> = <em>newconstr</em> [<em>deriving</em>]| |
| |&#124;|	<tt>class</tt> [<em>scontext</em> =>] <em>tycls</em> <em>tyvar</em> [<tt>where</tt> <em>cdecls</em>]| |
| |&#124;|	<tt>instance</tt> [<em>scontext</em> =>] <em>qtycls</em> <em>inst</em> [<tt>where</tt> <em>idecls</em>]| |
| |&#124;|	<tt>default</tt> (type1 <tt>,</tt> … <tt>,</tt> <em>type<sub>n</sub></em>)	    |(<em>n</em> ≥ 0)|
| |&#124;|	<tt>foreign</tt> <em>fdecl</em>| |
| |&#124;|	<em>decl</em>| |
|||||
|<em>decls</em>|→|{ <em>decl<sub>1</sub></em> <tt>;</tt> … <tt>;</tt> <em>decl<sub>n</sub></em> }	    |(<em>n</em> ≥ 0)|
|<em>decl</em>|→|<em>gendecl</em>| |
| |&#124;|	(<em>funlhs</em> &#124; <em>pat</em>) <em>rhs</em>| |
|||||
|<em>cdecls</em>|→|{ <em>cdecl<sub>1</sub></em> <tt>;</tt> … <tt>;</tt> <em>cdecl<sub>n</sub></em> }	    |(<em>n</em> ≥ 0)|
|<em>cdecl</em>|→|<em>gendecl</em>| |
| |&#124;|	(<em>funlhs</em> &#124; <em>var</em>) <em>rhs</em>| |
|||||
|<em>idecls</em>|→|{ <em>idecl<sub>1</sub></em> <tt>;</tt> … <tt>;</tt> <em>idecl<sub>n</sub></em> }	    |(<em>n</em> ≥ 0)|
|<em>idecl</em>|→|(<em>funlhs</em> &#124; <em>var</em>) <em>rhs</em>| |
| |&#124;|		    |(empty)|
|||||
|<em>gendecl</em>|→|<em>vars</em> <tt>::</tt> [<em>context</em> =>] <em>type</em>	    |(type signature)|
| |&#124;|	<em>fixity</em> [<em>integer</em>] <em>ops</em>	    |(fixity declaration)|
| |&#124;|		    |(empty declaration)|
|||||
|<em>ops</em>|→|<em>op<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>op<sub>n</sub></em>	    |(<em>n</em> ≥ 1)|
|<em>vars</em>|→|<em>var<sub>1</sub></em> <tt>,</tt> …, <em>var<sub>n</sub></em>	    |(<em>n</em> ≥ 1)|
|<em>fixity</em>|→|<tt>infixl</tt> &#124; <tt>infixr</tt> &#124; <em>infix</em>| |
|||||
|<em>type</em>|→|<em>btype</em> [-> <em>type</em>]	    |(function type)|
|||||
|<em>btype</em>|→|[<em>btype</em>] <em>atype</em>	    |(type application)
|||||
|<em>atype</em>|→|<em>gtycon</em>| |
| |&#124;|	<em>tyvar</em>| |
| |&#124;|	( <em>type<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>type<sub>k</sub></em> )	    |(tuple type, <em>k</em> ≥ 2)|
| |&#124;|	[ <em>type</em> ]	    |(list type)
| |&#124;|	( <em>type</em> )	    |(parenthesized constructor)
|||||
|<em>gtycon</em>|→|<em>qtycon</em>| |
| |&#124;|	<tt>()</tt>	    |(unit type)
| |&#124;|	<tt>[]</tt>	    |(list constructor)
| |&#124;|	<tt>(->)</tt>	    |(function constructor)
| |&#124;|	<tt>(,{,})</tt>	    |(tupling constructors)
|||||
|<em>context</em>|→|<em>class</em>| |
| |&#124;|	( <em>class<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>class<sub>n</sub></em> )	    |(<em>n</em> ≥ 0)|
|<em>class</em>|→|<em>qtycls</em> <em>tyvar</em>| |
| |&#124;|	<em>qtycls</em> ( <em>tyvar</em> <em>atype<sub>1</sub></em> … <em>atype<sub>n</sub></em> )	    |(<em>n</em> ≥ 1)|
|<em>scontext</em>|→|<em>simpleclass</em>| |
| |&#124;|	( <em>simpleclass<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>simpleclass<sub>n</sub></em> )	    |(<em>n</em> ≥ 0)|
|<em>simpleclass</em>|→|<em>qtycls</em> <em>tyvar</em>| |
|||||
|<em>simpletype</em>|→|<em>tycon</em> <em>tyvar<sub>1</sub></em> … <em>tyvar<sub>k</sub></em>	    |(<em>k</em> ≥ 0)|
|<em>constrs</em>|→|<em>constr<sub>1</sub></em> &#124; … &#124; <em>constr<sub>n</sub></em>	    |(<em>n</em> ≥ 1)|
|<em>constr</em>|→|<em>con</em> [!] <em>atype<sub>1</sub></em> … [!] <em>atype<sub>k</sub></em>	    |(arity <em>con</em>  =  <em>k</em>, <em>k</em> ≥ 0)|
| |&#124;|	(<em>btype</em> &#124; ! <em>atype</em>) <em>conop</em> (<em>btype</em> &#124; ! <em>atype</em>)	    |(infix <em>conop</em>)
| |&#124;|	<em>con</em> { <em>fielddecl<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>fielddecl<sub>n</sub></em> }	    |(<em>n</em> ≥ 0)|
|<em>newconstr</em>|→|<em>con</em> <em>atype</em>| |
| |&#124;|	<em>con</em> { <em>var</em> <tt>::</tt> <em>type</em> }| |
|<em>fielddecl</em>|→|<em>vars</em> <tt>::</tt> (<em>type</em> &#124; ! <em>atype</em>) |
|<em>deriving</em>|→|<tt>deriving</tt> (dclass &#124; (<em>dclass<tt>1</tt></em>, … <tt>,</tt> <em>dclass<sub>n</sub></em>))	    |(<em>n</em> ≥ 0)|
|<em>dclass</em>|→|<em>qtycls</em>| |
|||||
|<em>inst</em>|→|<em>gtycon</em>| |
| |&#124;|	( <em>gtycon</em> <em>tyvar<sub>1</sub></em> … <em>tyvar<sub>k</sub></em> )	    |(<em>k</em> ≥ 0, <em>tyvars</em> distinct)
| |&#124;|	( <em>tyvar<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>tyvar<sub>k</sub></em> )	    |(<em>k</em> ≥ 2, <em>tyvars</em> distinct)
| |&#124;|	[ <em>tyvar</em> ]| |
| |&#124;|	( <em>tyvar<sub>1</sub></em> -> <em>tyvar<sub>2</sub></em> )	    |<em>tyvar<sub>1</sub></em> and <em>tyvar<sub>2</sub></em> distinct|
|||||
|<em>fdecl</em>|→|<tt>import</tt> <em>callconv</em> [<em>safety</em>] <em>impent</em> <em>var</em> <tt>::</tt> <em>ftype</em>	    |(define variable)|
| |&#124;|	<tt>export</tt> <em>callconv</em> <em>expent</em> <em>var</em> <tt>::</tt> <em>ftype</em>	    |(expose variable)|
|<em>callconv</em>|→|<tt>ccall</tt> &#124; <tt>stdcall</tt> &#124; <tt>cplusplus</tt>	    |(calling convention)|
| |&#124;|	<tt>jvm</tt> &#124; <tt>dotnet</tt>| |
| |&#124;|	 **system-specific calling conventions**| |
|<em>impent</em>|→|[<em>string</em>]	    |(see Section [8.5.1]("./8-foreign-function-interface.md"))|
|<em>expent</em>|→|[<em>string</em>]	    |(see Section [8.5.1]("./8-foreign-function-interface.md"))|
|<em>safety</em>|→|<tt>unsafe</tt> &#124; <tt>safe</tt>| |
|||||
|<em>ftype</em>|→|<em>frtype</em>| |
| |&#124;|	<em>fatype</em> → <em>ftype</em>| |
|<em>frtype</em>|→|<em>fatype</em>| |
| |&#124;|	()| |
|<em>fatype</em>|→|<em>qtycon</em> <em>atype<sub>1</sub></em> … <em>atype<sub>k</sub></em>	    |(<em>k</em>  ≥  0)|
|||||
|<em>funlhs</em>|→|<em>var</em> <em>apat</em> { <em>apat</em> }| |
| |&#124;|	<em>pat</em> <em>varop</em> <em>pat</em>| |
| |&#124;|	( <em>funlhs</em> ) <em>apat</em> { <em>apat</em> }| |
|||||
|<em>rhs</em>|→|= <em>exp</em> [<tt>where</tt> <em>decls</em>]| |
| |&#124;|	<em>gdrhs</em> [<tt>where</tt> <em>decls</em>]| |
|||||
|<em>gdrhs</em>|→|<em>guards</em> = <em>exp</em> [<em>gdrhs</em>]| |
|||||
|<em>guards</em>|→|&#124; <em>guard<sub>1</sub></em>, …, <em>guard<sub>n</sub></em>	    |(<em>n</em> ≥ 1)|
|<em>guard</em>|→|<em>pat</em> <- <em>infixexp</em>	    |(pattern guard)|
| |&#124;|	<tt>let</tt> <em>decls</em>	    |(local declaration)|
| |&#124;|	<em>infixexp</em>	    |(boolean guard)|
|||||
|<em>exp</em>|→|<em>infixexp</em> <tt>::</tt> [<em>context</em> =>] <em>type</em>	    |(expression type signature)|
| |&#124;|	<em>infixexp</em>| |
|||||
|<em>infixexp</em>|→|<em>lexp</em> <em>qop</em> <em>infixexp</em>	    |(infix operator application)|
| |&#124;|	- <em>infixexp</em>	    |(prefix negation)|
| |&#124;|	<em>lexp</em>| |
|||||
|<em>lexp</em>|→|\ <em>apat<sub>1</sub></em> … <em>apat<sub>n</sub></em> -> <em>exp</em>	    |(lambda abstraction, <em>n</em> ≥ 1)|
| |&#124;|	<tt>let</tt> <em>decls</em> <tt>in</tt> <em>exp</em>	    |(let expression)|
| |&#124;|	<tt>if</tt> <em>exp</em> [<tt>;</tt>] <tt>then</tt> <em>exp</em> [<tt>;</tt>] <tt>else</tt> <em>exp</em>	    |(conditional)|
| |&#124;|	<tt>case</tt> <em>exp</em> <tt>of</tt> { <em>alts</em> }	    |(case expression)|
| |&#124;|	<tt>do</tt> { <em>stmts</em> }	    |(do expression)|
| |&#124;|	<em>fexp</em>| |
|<em>fexp</em>|→|[<em>fexp</em>] <em>aexp</em>	    |(function application)|
|||||
|<em>aexp</em>|→|<em>qvar</em>	    |(variable)|
| |&#124;|	<em>gcon</em>	    |(general constructor)|
| |&#124;|	<em>literal</em>| |
| |&#124;|	( <em>exp</em> )	    |(parenthesized expression)|
| |&#124;|	( <em>exp<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>exp<sub>k</sub></em> )	    |(tuple, <em>k</em> ≥ 2)|
| |&#124;|	[ <em>exp<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>exp<sub>k</sub></em> ]	    |(list, <em>k</em> ≥ 1)|
| |&#124;|	[ <em>exp<sub>1</sub></em> [<tt>,</tt> <em>exp<sub>2</sub></em>] .. [<em>exp<sub>3</sub></em>] ]	    |(arithmetic sequence)|
| |&#124;|	[ <em>exp</em> &#124; <em>qual<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>qual<sub>n</sub></em> ]	    |(list comprehension, <em>n</em> ≥ 1)|
| |&#124;|	( <em>infixexp</em> <em>qop</em> )	    |(left section)
| |&#124;|	( <em>qop</em>⟨-⟩ <em>infixexp</em> )	    |(right section)
| |&#124;|	<em>qcon</em> { <em>fbind<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>fbind<sub>n</sub></em> }	    |(labeled construction, <em>n</em> ≥ 0)|
| |&#124;|	<em>aexp</em>⟨qcon⟩ { <em>fbind<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>fbind<sub>n</sub></em> }	    |(labeled update, <em>n</em>  ≥  1)|
|||||
|<em>qual</em>|→|<em>pat</em> <- <em>exp</em>	    |(generator)|
| |&#124;|	<tt>let</tt> <em>decls</em>	    |(local declaration)
| |&#124;|	<em>exp</em>	    |(guard)|
|||||
|<em>alts</em>|→|<em>alt<sub>1</sub></em> <tt>;</tt> … <tt>;</tt> <em>alt<sub>n</sub></em>	    |(<em>n</em> ≥ 1)|
|<em>alt</em>|→|<em>pat</em> -> <em>exp</em> [<tt>where</tt> <em>decls</em>]| |
| |&#124;|	<em>pat</em> <em>gdpat</em> [<tt>where</tt> <em>decls</em>]| |
| |&#124;|		    |(empty alternative)|
|||||
|<em>gdpat</em>|→|<em>guards</em> -> <em>exp</em> [ <em>gdpat</em> ]| |
|||||
|<em>stmts</em>|→|<em>stmt<sub>1</sub></em> … <em>stmt<sub>n</sub></em> <em>exp</em> [<tt>;</tt>]	    |(<em>n</em> ≥ 0)|
|<em>stmt</em>|→|<em>exp</em> ;| |
| |&#124;|	<em>pat</em> <- <em>exp</em> ;| |
| |&#124;|	<tt>let</tt> <em>decls</em> ;| |
| |&#124;|	<tt>;</tt>	    |(<em>empty statement</em>)|
|||||
|<em>fbind</em>|→|<em>qvar</em> = <em>exp</em>| |
|||||
|<em>pat</em>|→|<em>lpat</em> <em>qconop</em> <em>pat</em>	    |(infix constructor)|
| |&#124;|	<em>lpat</em>| |
|||||
|<em>lpat</em>|→|<em>apat</em>| |
| |&#124;|	- (integer &#124; <em>float</em>)	    |(negative literal)
| |&#124;|	<em>gcon</em> <em>apat<sub>1</sub></em> … <em>apat<sub>k</sub></em>	    |(arity <em>gcon</em>  =  <em>k</em>, <em>k</em> ≥ 1)|
|||||
|<em>apat</em>|→|<em>var</em> [ @ <em>apat</em>]	    |(as pattern)
| |&#124;|	<em>gcon</em>	    |(arity <em>gcon</em>  =  0)|
| |&#124;|	<em>qcon</em> { <em>fpat<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>fpat<sub>k</sub></em> }	    |(labeled pattern, <em>k</em> ≥ 0)|
| |&#124;|	<em>literal</em>| |
| |&#124;|	<em>_</em>	    |(wildcard)|
| |&#124;|	( <em>pat</em> )	    |(parenthesized pattern)
| |&#124;|	( <em>pat<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>pat<sub>k</sub></em> )	    |(tuple pattern, <em>k</em> ≥ 2)|
| |&#124;|	[ <em>pat<sub>1</sub></em> <tt>,</tt> … <tt>,</tt> <em>pat<sub>k</sub></em> ]	    |(list pattern, <em>k</em> ≥ 1)|
| |&#124;|	~ <em>apat</em>	    |(irrefutable pattern)
|||||
|<em>fpat</em>|→|<em>qvar</em> = <em>pat</em>| |
|||||
|<em>gcon</em>|→|()| |
| |&#124;|	[<em></em>]| |
| |&#124;|	(,{,})| |
| |&#124;|	<em>qcon</em>| |
|||||
|<em>var</em>|→|<em>varid</em> &#124; ( <em>varsym</em> )	    |(variable)|
|<em>qvar</em>|→|<em>qvarid</em> &#124; ( <em>qvarsym</em> )	    |(qualified variable)
|<em>con</em>|→|<em>conid</em> &#124; ( <em>consym</em> )	    |(constructor)|
|<em>qcon</em>|→|<em>qconid</em> &#124; ( <em>gconsym</em> )	    |(qualified constructor)
|<em>varop</em>|→|<em>varsym</em> &#124; <em>\`</em>  <em>varid</em> <em>\`</em>	    |(variable operator)
|<em>qvarop</em>|→|<em>qvarsym</em> &#124; <em>\`</em>  <em>qvarid</em> <em>\`</em>	    |(qualified <em>variable</em> operator)
|<em>conop</em>|→|<em>consym</em> &#124; <em>\`</em>  <em>conid</em> <em>\`</em>	    |(constructor operator)
|<em>qconop</em>|→|<em>gconsym</em> &#124; <em>\`</em>  <em>qconid</em> <em>\`</em>	    |(qualified <em>constructor</em> operator)
|<em>op</em>|→|<em>varop</em> &#124; <em>conop</em>	    |(operator)|
|<em>qop</em>|→|<em>qvarop</em> &#124; <em>qconop</em>	    |(qualified operator)
|<em>gconsym</em>|→|<tt>:</tt> &#124; <em>qconsym</em>| |

## 結合性解決

次はHaskellの式における結合性解決の実装例である。結合性解決はHaskellのパターンに対しても適用可能であるが、パターンは式のサブセットであるので、以下では簡単のため式のみを考えることにする。

`resolve`関数はリストであって、それぞれの要素が式または演算子であるもの、すなわち、文脈自由文法において非終端記号`infixexp`のインスタンスとなるもの(**訳注**: 上の文法表で<em>infixexp</em>に当てはまるもの、の意味)を受け取る。`resolve`は`Just e`(ここで`e`は解決された式)または、入力が意味のある式を表現していない場合には`Nothing`を返す。もちろん、コンパイラにおいては有益なエラーメッセージを生成するという目的のためには関係する演算子についての情報をより多く返す方がよいであろうが、ここではアルゴリズムを説明するのに`Maybe`型で十分である。

```hs
import Control.Monad  

type Prec   = Int  
type Var    = String  

data Op = Op String Prec Fixity  
  deriving (Eq,Show)  

data Fixity = Leftfix | Rightfix | Nonfix  
  deriving (Eq,Show)  

data Exp = Var Var | OpApp Exp Op Exp | Neg Exp  
  deriving (Eq,Show)  

data Tok = TExp Exp | TOp Op | TNeg  
  deriving (Eq,Show)  

resolve :: [Tok] -> Maybe Exp  
resolve tokens = fmap fst $ parseNeg (Op "" (-1) Nonfix) tokens  
  where  
    parseNeg :: Op -> [Tok] -> Maybe (Exp,[Tok])  
    parseNeg op1 (TExp e1 : rest)  
       = parse op1 e1 rest  
    parseNeg op1 (TNeg : rest)  
       = do guard (prec1 < 6)  
            (r, rest') <- parseNeg (Op "-" 6 Leftfix) rest  
            parse op1 (Neg r) rest'  
       where  
          Op _ prec1 fix1 = op1  

    parse :: Op -> Exp -> [Tok] -> Maybe (Exp, [Tok])  
    parse _   e1 [] = Just (e1, [])  
    parse op1 e1 (TOp op2 : rest)  
       -- case (1): 不当な式をチェック
       | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix)  
       = Nothing  

       -- case (2): op1とop2は左結合であるべきである
       | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix)  
       = Just (e1, TOp op2 : rest)  

       -- case (3): op1とop2は右結合であるべきである
       | otherwise  
       = do (r,rest') <- parseNeg op2 rest  
            parse op1 (OpApp e1 op2 r) rest'  
       where  
         Op _ prec1 fix1 = op1  
         Op _ prec2 fix2 = op2
```

このアルゴリズムは次のように働く。各段階において関数呼び出し

```hs
parse op1 E1 (op2 : tokens)
```

があるが、これは次のような式であることを確かめていることを意味している。(ここで、呼び出し側がE0を保持している)

```hs
E0 ‘op1‘ E1 ‘op2‘ ...     (1)
```

`parse`の仕事は`op1`の右側の式を構築し、入力の残りの部分を返すことである。

3つの場合を考慮する必要がある:

1. `op1`と`op2`が同じ優先度であるが、同じ結合性を持たない場合あるいは不結合(nonfix)であることが宣言されている場合には、式は不当である。
2. `op1`が`op2`よりも高い優先度であるか`op1`と`op2`が左結合である場合には、`op1`の右側の式は`E1`であることがわかり、よってこれを呼び出し側に返却する。
3. いずれでもない場合、``E1 `op2` R``の形の式が求めているものであることがわかる。`R`を見つけるには、`parseNeg op2 tokens`を呼び出して`op2`の右側の式を計算し、これを今は`R`と名前を付ける(`parseNeg`以下の部分についての詳細は以下に示すが、本質的には、もしも`tokens`が`(E2 : tokens)`の形をしている場合、`parseNeg`は`parse op2 E2 rest`と同値である)。さて、``E0 `op1` (E1 `op2` R) `op3` ...``の形を得た、ここで`op3`は入力に出現する次の演算子である。これは上の(1)の場合の具体的な例になっているから、``E1 == (E1 `op2` R)``と新たに置いて`parse`を続いて呼び出す。

アルゴリズムを初期化するには、`op1`は他の何よりも優先度の低い仮想的な演算子であるとおく。そして`parse`は入力全体を消費し結果の式を返す。

前置マイナス演算子の`-`の扱いは若干込み入っている。前置のマイナスは中置のマイナスと同じ結合性であったことを思い出そう、いずれも左結合で優先度は6だ。`-`の左にくる演算子は、もし存在すれば、式が正当であるためには優先度が6より低くなければならない。マイナス演算子そのものは同じ結合性を持つ演算子に対しては左結合的にはたらく(例: `+`)。よって例えば`-a + b`は正当であり`(-a) + b`として解決されるが、`a + -b`は不当である。

関数`parseNeg`は前置マイナスを処理する。前置演算子に遭遇して、それがその位置で正当であれば(左側にある演算子の優先度が6より低ければ)、上の(3)のケースと同様にして進めていく。つまり、`-`の引数を`parseNeg`を再帰的に呼ぶことで計算し、そして`parse`を呼んで続けていく。

このアルゴリズムは優先度の範囲と解決には影響されないことに注意せよ。原則として、Haskellの優先度が1から10までの範囲の整数に制限される理由は何もない。より広い範囲や分数の値を使うことで特別に難しくなるわけではない。
