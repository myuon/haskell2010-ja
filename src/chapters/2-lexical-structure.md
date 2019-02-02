# 字句構造

この章では、Haskellの低レベルな字句構造を説明する。このレポートを初めて読む場合には、詳細はほとんど読み飛ばしてもよいだろう。

## 表記法

以下の表記法は構文を表すために使用される。

|||
|:----|:----|
| [pattern]                         | 任意 |
| {pattern}                         | 0、またはそれ以上の繰り返し |
| (pattern)                         | グルーピング |
| pat<sub>1</sub> &#124; pat<sub>2</sub> | 選択 |
| pat<sub>(pat')</sub> |  相違 ー pat'によって生成されたものを除いた、patによって生成された要素|
| <sub>fibonacci</sub> | タイプライターフォントの終端構文 |

このセクション内の構文は字句構造を説明しているため、全ての空白は明示的に表現されているが、並置されたシンボル間には暗黙的な空白はない。BNFのような構文は今後使われ、次の形式を取る。

||||
|---|---|---|
| nonterm | → | alt<sub>1</sub> &#124; alt<sub>2</sub> &#124; … &#124; alt<sub>n</sub> |

通常は文脈によって区別が明確になるが、
<sub>'|'</sub>や<sub>'[…]'</sub>のような(タイプライターフォントで指定された)具体的な終端構文から'|'や'[…]'のようなメタデータ構文の区別には注意が必要である。

HaskellはUnicode[[2](./bibliogrphy.md)]文字セットを使っている。しかしながら、プログラムソースは現在、以前のHaskellバージョンで使われていたASCII文字セットに偏っている。

この構文はUnicodeコンソーシアムによって定義されているUnicode文字の文字符号化スキームによって異なる。Haskellコンパイラーは新しいバージョンのUnicodeが利用可能になるにつれてそれらを利用されることが期待されている。

## 字句プログラム構造

||||
|--|--|--|
| program |→| {lexeme &#124; whietespace}|
| lexeme |→| qvarid &#124; qconid &#124; qvarsym &#124; qconsym &#124; literal &#124; special &#124; reservedop &#124; reservedid |
| literal |→| integer &#124; float &#124; char &#124; string |
| special |→| (&#124;) &#124; , &#124; ; &#124; [ &#124; ] &#124; ` &#124; { &#124; } |
| | | |
| whitespace |→| whitestuff {whitestuff}|
| whitestuff |→| whitechar &#124; comment &#124; ncomment |
| whitechar |→| newline &#124; vertab &#124; space &#124; tab &#124; uniWhite |
| newline |→| return linefeed &#124; return &#124; linefeed &#124; formfeed |
| return |→| キャレッジ⏎ |
| linefeed |→| 改行 |
| vertab |→| 垂直タブ |
| formfeed |→| 改ページ |
| space |→| 空白 |
| tab |→| 水平タブ |
| uniWhite |→|	空白として定義されたUnicode文字 |
| | | |
|comment |→| dashes [ any<sub>symbol</sub> {any} ] newline |
|dashes |→| -- {-} |
|opencom |→| {- |
|closecom |→| -} |
|ncomment |→| opencom ANY seq {ncomment ANY seq} closecom |
|ANY seq |→| {ANY }<sub>⟨{ANY } ( opencom &#124; closecom ) {ANY }⟩</sub> |
|ANY |→| graphic &#124; whitechar |
|any |→| graphic &#124; space &#124; tab |
|graphic |→| small &#124; large &#124; symbol &#124; digit &#124; special &#124; " &#124; ' |
| | | |
|small |→| ascSmall &#124; uniSmall &#124; _ |
|ascSmall |→| <sub>a</sub> &#124; <sub>b</sub> &#124; … &#124; <sub>z</sub> |
|uniSmall |→| 小文字Unicode |
| | | |
|large |→| ascLarge &#124; uniLarge |
|ascLarge |→| A &#124; B &#124; … &#124; Z |
|uniLarge |→| any uppercase or titlecase Unicode letter |
|symbol |→| ascSymbol &#124; uniSymbol<sub>⟨special &#124; _ &#124; " &#124; '⟩</sub> |
| | | |
|ascSymbol |→| <sub>!</sub> &#124; <sub>#</sub> &#124; <sub>$</sub> &#124; <sub>%</sub> &#124; <sub>&</sub> &#124; <sub>⋆</sub> &#124; <sub>+</sub> &#124; <sub>.</sub> &#124; <sub>/</sub> &#124; <sub><</sub> &#124; <sub>=</sub> &#124; <sub>></sub> &#124; <sub>?</sub> &#124; <sub>@</sub> &#124; <sub>\\</sub> &#124; <sub>^</sub> &#124; <sub>&#124;</sub> &#124; <sub>-</sub> &#124; <sub>~</sub> &#124; <sub>:</sub>|
|uniSymbol |→| Unicodeのシンボル、または句読点 |
|digit |→| ascDigit &#124; uniDigit |
|ascDigit |→| <sub>0</sub> &#124; <sub>1</sub> &#124; <sub>…</sub> &#124; <sub>9</sub> |
|uniDigit |→| 10進数Unicode |
|octit |→| <sub>0</sub> &#124; <sub>1</sub> &#124; <sub>…</sub> &#124; <sub>7</sub> |
|hexit |→| <sub>digit</sub> &#124; <sub>A</sub> &#124; <sub>…</sub> &#124; <sub>F</sub> &#124; <sub>a</sub> &#124; <sub>…</sub> &#124; <sub>f</sub> |

字句解析は"maximal munch"規則に従うべきである。すなわち、語彙素生成規則を満たす可能な限り長くとった語彙素が読み取られる([訳注]: "longest match" ともいう)。したがって、caseは予約語だが、casesは予約語ではない。同様に=は予約されているが、==と~=は予約されていない。

空白はどんなものでも正しい字句の区切り文字である。

**Any**カテゴリではない文字はHaskellプログラム内では有効ではなく、字句解析エラーを結果にすべきである。

## コメント

コメントは有効な空白である。

普通のコメントは二つ以上の連続したダッシュ(**--**など)で始まり、次の改行まで及ぶ。連続したダッシュは正当な語彙素の一部を形成してはいけない。例として、"-->"や"|--"はコメントの開始としては見なさない。なぜならこれらは正当な語彙素だからだ。しかしながら、"--foo"はコメントの開始として見なされる。

ネストされたコメントは"{-"で始まり、"-}"で終わる。"{-"は違法な語彙素ではない。それゆえ、例えば"{---"は末尾に余分なダッシュがあるがネストされたコメントの始まりである。

コメントそれ自体は語彙的に解析されない。代わりに、初めに文字列"-}"が現れた前の部分までがネストされたコメントの範囲である。ネストされたコメントは任意の深さにネストできる。ネストされたコメント内に文字列"{-"があると新しいネストされたコメントが始まり、"-}"によって閉じられる。ネストされたコメント内では、各"{-"は対応する"-}"の出現によって照合される。

普通のコメント内では"{-"と"-}"の文字の並びは特別な意味を持たず、一方でネストされたコメント内ではダッシュの並びは特別な意味を持たない。

ネストされたコメントはコンパイラープラグマのためにも使われる。それについては[12章](./chapters/12-compiler-pragmas.md)で説明される。

もし、いくつかのコードがネストされたコメントによってコメントアウトされていたら、その時、そのコード内の文字列内または行末コメントに"{-"と"-}"があるとネストされたコメントに干渉する。

## 識別子と演算子

||||
|--|--|--|
|varid | → | (small {small &#124; large &#124; digit &#124; ' })<sub>⟨reservedid⟩</sub>|
|conid | → | large {small &#124; large &#124; digit &#124; ' }|
|reservedid | → | <sub>case</sub> &#124; <sub>class</sub> &#124; <sub>data</sub> &#124; <sub>default</sub> &#124; <sub>deriving</sub> &#124; <sub>do</sub> &#124; <sub>else</sub> &#124; <sub>foreign</sub> &#124; <sub>if</sub> &#124; <sub>import</sub> &#124; <sub>in</sub> &#124; <sub>infix</sub> &#124; <sub>infixl</sub> &#124; <sub>infixr</sub> &#124; <sub>instance</sub> &#124; <sub>let</sub> &#124; <sub>module</sub> &#124; <sub>newtype</sub> &#124; <sub>of</sub> &#124; <sub>then</sub> &#124; <sub>type</sub> &#124; <sub>where</sub> &#124; <sub>_</sub>|

識別子は0個以上の文字、数字、アンダースコア、およびシングルクォートで構成される。識別子は字句的に小文字で始まる字句(変数識別子)と大文字から始まる字句(コンストラクタ識別子)の二つの名前空間に区別される。(セクション[1.4](./chapter1-introduction.md))これらの識別子は大文字と小文字を区別する。name、naMe、Nameは3つの判然たる識別子である。(初め2つは変数識別子で、最後のはコンストラクタ識別子である。)

アンダースコア("\_")は小文字として扱われ、小文字が許されるところならどこでも使用可能だ。しかしながら、全て"\_"なものはパターンのワイルドカードのように使われる識別子として予約されている。未使用の識別子に対して警告を出すコンパイラーはアンダースコアで始まる識別子に対しては警告を抑制することが推奨される。これはプログラマーが未使用であると予想されるパラメータに"_foo"を使うことを許可している。

||||
|--|--|--|
|varsym | → | ( symbol⟨:⟩ {symbol} )<sub>⟨reservedop &#124; dashes⟩</sub>|
|consym | → | ( : {symbol})⟨reservedop⟩|
|reservedop | → | <sub> .. </sub> &#124; <sub> : </sub> &#124; <sub> :: </sub> &#124; <sub> = </sub> &#124; <sub> \ </sub> &#124; <sub> </sub> &#124; <sub> </sub> &#124; <sub> <- </sub> &#124; <sub> -> </sub> &#124; <sub> @ </sub> &#124; <sub> ~ </sub> &#124; <sub> => </sub>|

演算子シンボルは上で定義したように、1つ以上の記号文字から形成され、2つの名前空間に字句的に区別される。(セクション[1.4](./chapters/1-introduction.md))

- コロンから始まる演算子シンボルはコンストラクタである。
- 他の文字から始まる演算子シンボルは普通の識別子である。

コロン(":")はHaskellリストのコンストラクタとして使用されるためだけに予約されている。これにより、"[]"や"[a,b]"のようなリスト構文の他の部分との扱いが統一される。

接頭辞否定の特殊な構文を除き、全ての演算子は中置である。ただし、各中置演算子をセクション内で使用して、部分的に適応される演算子を生成することができる。(セクション[3.5](./chapters/3-expressions.md)を参照)標準の中置演算子はすべて定義済みのシンボルであり、リバウンドすることがある。

レポートの残りの部分では、6種類の名前が使用される。

|||||
|--|--|--|--|
|varid | |   | (variables)|
|conid | |   | (constructors)|
|tyvar | → | varid |           (type variables)|
|tycon | → | conid |           (type constructors)|
|tycls | → | conid |           (type classes)|
|modid | → | {conid .} conid | (modules)|

変数と型変数は小文字で始まる識別子によって表され、そのほかは大文字で始まる識別子によって表される。また、変数とコンストラクタには中置形式があるが、他の4つにはない。モジュール名はドットで区切られた一連のconidである。名前空間についてはセクション[1.4](./chapters/1-introduction.md)でも説明している。

特定の状況では、名前の前にモジュール識別子を付けることで、名前をオプションで修飾できる。これは変数、コンストラクタ、型コンストラクタ、型クラス、型クラス名に適応されるが、型変数やモジュール名には適応されない。修飾名については、チャプター[5](./chapters/5-modules.md)で詳しく説明する。

||||
|--|--|--|
|qvarid	| → | [modid .] varid|
|qconid	| → | [modid .] conid|
|qtycon	| → | [modid .] tycon|
|qtycls	| → | [modid .] tycls|
|qvarsym	| → | [modid .] varsym|
|qconsym	| → | [modid .] consym|

修飾名は語彙素なので、修飾子と名前の間には空白を入れることはできない。サンプルの語彙解析を以下に示す。

| This | Lexes as this |
| -- | -- |
| f.g | f . g (3トークン)|
| F.g | F.g (修飾された'g')|
| f.. | f .. (2トークン)|
| F.. | F.. (修飾された'.') |
| F.  | F . (2トークン) |

修飾子は名前の構文上の扱いを変更しない。例えば、Prelude.+はPrelude(セクション[4.4.2](./chapters/4-declarations-and-bindings.md))での+の定義と同じ固定性を持つ中置演算子である。

## 数値リテラル
||||
|--|--|--|
|decimal | → | digit{digit}|
|octal | → | octit{octit}|
|hexadecimal | → | hexit{hexit}|
|integer | → | decimal &#124; <sub>0o</sub> octal &#124; <sub>0O</sub> octal &#124; <sub>0x</sub> hexadecimal &#124; <sub>0X</sub> hexadecimal|
|float | → | decimal . decimal [exponent] &#124; decimal exponent|
|exponent | → | (e &#124; E) [+ &#124; -] decimal|

数値リテラルには整数と浮動小数点の2種類ある。整数リテラルは10進数(デフォルト)、8進数(0oまたは0Oを先頭に付ける)、16進数表記(0xか0Xを先頭に付ける)で指定できる。浮動小数点リテラルは常に10進数である。浮動小数点リテラルは小数点の前後に数値を含まなければいけない。これは小数点が他のドット文字の使い方に誤って認識されないことを保証するためだ。負数リテラルはセクション[3.4](./chapters/3-expressions.md)で論じられる。数値リテラルの型はセクション[6.4.1](./chapters/6-predefined-types-and-classes.md)で論じられる。

## 文字と文字列リテラル

||||
| -- | --| --|
|char	| → |	' (graphic<sub>⟨' &#124; \⟩</sub> &#124; space &#124; escape<sub>⟨\&⟩</sub>) '|
|string	| → |	" {graphic<sub>⟨" &#124; \⟩</sub> &#124; space &#124; escape &#124; gap} "|
|escape	| → |	\ ( charesc &#124; ascii &#124; decimal &#124; o octal &#124; x hexadecimal )|
|charesc	| → |	<sub>a</sub> &#124; <sub>b</sub> &#124; <sub>f</sub> &#124; <sub>n</sub> &#124; <sub>r</sub> &#124; <sub>t</sub> &#124; <sub>v</sub> &#124; <sub>\\</sub> &#124; <sub>"</sub> &#124; <sub>'</sub> &#124; <sub>&</sub>|
|ascii	| → |	<sub>^</sub>cntrl &#124; <sub>NUL</sub> &#124; <sub>SOH</sub> &#124; <sub>STX</sub> &#124; <sub>ETX</sub> &#124; <sub>EOT</sub> &#124; <sub>ENQ</sub> &#124; <sub>ACK</sub> &#124;	<sub>BEL</sub> &#124; <sub>BS</sub> &#124; <sub>HT</sub> &#124; <sub>LF</sub> &#124; <sub>VT</sub> &#124; <sub>FF</sub> &#124; <sub>CR</sub> &#124; <sub>SO</sub> &#124; <sub>SI</sub> &#124; <sub>DLE</sub> &#124;	<sub>DC1</sub> &#124; <sub>DC2</sub> &#124; <sub>DC3</sub> &#124; <sub>DC4</sub> &#124; <sub>NAK</sub> &#124; <sub>SYN</sub> &#124; <sub>ETB</sub> &#124; <sub>CAN</sub> &#124; <sub>EM</sub> &#124; <sub>SUB</sub> &#124; <sub>ESC</sub> &#124; <sub>FS</sub> &#124; <sub>GS</sub> &#124; <sub>RS</sub> &#124; <sub>US</sub> &#124; <sub>SP</sub> &#124; <sub>DEL</sub>|
|cntrl	| → |	ascLarge &#124; <sub>@</sub> &#124; <sub>[</sub> &#124; <sub>\\</sub> &#124; <sub>]</sub> &#124; <sub>^</sub> &#124; <sub>_</sub>|
|gap	| → |	<sub>\\</sub> whitechar {whitechar} <sub>\\</sub>|

文字リテラルは'a'のようにシングルクォーテーションで囲まれたものであり、文字列リテラルは"Hello"のようにダブルクォーテーションで囲まれたものである。

エスケープコードは特殊文字の表現のために文字と文字列で使われる。注意すべきことはシングルクォーテーション(')は文字列においても使われるが、文字でエスケープする必要があることだ。同様に、ダブルクォーテーションは文字の中で使われるが、その際は文字列でエスケープする必要がある。\は常にエスケープしないといけない。**charesc**カテゴリには"アラート"(\a)、"バックスペース"(\b)、"改ページ"(\f)、"改行"(\n)、"キャリッジ・リターン"(\r)、"水平タブ"(\t)、"垂直タブ"(\v)といった文字のポータブル表現も含まれている。

\\^Xのような制御文字を含むUnicode文字セットのエスケープ文字も用意している。\137のような数値エスケープは10進数表現の137で文字を指定するために使用され、同様に8進数(例:\o137)や16進数(例:\x37)の表現も可能である。

“maximal munch”に従って、文字列内の数字のエスケープ文字は全て連続した数字で構成され、任意の長さにすることができる。同様に、"\SOH"のような奇妙なASCIIエスケープコードは長さ1の文字列としてパ－スされる。'\&'エスケープ文字は"\137\&9"や"SO\&H"のような文字列(共に長さは2)が構成できるように"ヌル文字"として提供される。その代わり"\&"は""に等しく、'\&'文字は許されない。文字の等価性はセクション[6.1.2](./chapters/6-predefined-types-and-classes.md)で定義されている。

文字列は無視される"ギャップ"(白い文字を囲む2つのバックスラント)を含むかもしれない。これにより1行の終わりと次の行の始めにバックスラントを書くことによって、複数の行に長い文字列を書くことが可能だ。例としては以下のものになる。
``` hs
"Here is a backslant \\ as well as \137, \  
    \a numeric escape character, and \^X, a control character."
```

文字列リテラルは実際には文字のリストの略記である。(セクション[3.7](./chapters/3-expressions.md)を参照)

## レイアウト

Haskellはレイアウトを使用して同じ情報を伝えることによって、いくつかの文法で使用されている中括弧とセミコロンの省略を許可している。これによりレイアウトに依存するもの、しないものの両方のコーディングスタイルが可能になり、1つのプログラム内で自由に混在させることができる。レイアウトが必要ではないため、Haskellプログラムは他のプログラムによって簡単に作成することができる。

レイアウトがHaskellプログラムの意味に与える影響は、レイアウトによって決定される場所に中括弧とセミコロンを追加することによって完全に指定できる。この拡張プログラムの意味はレイアウトに影響されなくなった。

非公式には中括弧とセミコロンは次のように挿入される。キーワードのwhere、let、do、またはofの後に開き括弧が省略されると、レイアウト(または"オフサイド")規則が有効になる。これが起こると、次の語彙素のインデントが(新しい行にあるかどうかにかかわらず)記録され、省略された開き括弧が挿入される(語彙素の前の空白にはコメントが含まれる場合がある)。後続の行について、空白のみが含まれている場合、またはそれ以上のインデントされている場合は、前の項目が続行される。(何も挿入されない)同じ量だけインデントされている場合は新しい項目が始まる(セミコロンが挿入される)。またインデントが小さくなると、レイアウトリストは終了する(閉じ括弧が挿入される)。where、let、doまたはofの直後のノンブレース語彙素のインデントが現在のインデントレベル以下である場合は、レイアウトを開始する代わりに、空のリスト"{}"が挿入され、現在のレベルでレイアウト処理が発生する(つまり、セミコロンまたは閉じ括弧を挿入する)。レイアウトリストを含む構文カテゴリが終了するたびに、閉じ括弧も挿入される。つまり、閉じ括弧が合法となる点で違法な語彙素が検出された場合は閉じ括弧が挿入される。レイアウトルールはそれが挿入した開いている中括弧にだけ一致する。明示的な開き括弧は、明示的に閉じ括弧と一致しなければならない。これらの明示的な開き括弧内では、たとえ行が以前の暗黙の開き括弧の左側に字下げされていても、括弧の外側の構成要素に対してレイアウト処理は実行されない。

セクション[10.3](./chapters/10-syntax-reference.md#3)ではレイアウトルールのより正確な定義を示す。

これらの規則を考えると、1つの改行で実際に複数のレイアウトリストを終了させることができる。
これらの規則は以下のコードを許す。
``` hs
f x = let a = 1; b = 2  
          g y = exp2  
       in exp1
```
生成したa, b, gは全て同じレイアウトリストの一部である。

例として、図[2.1](#figure2.1)は(ややわざとらしい)モジュールを示し、図[2.2](#figure2.2)はそのレイアウトルールを適応した結果を示している。次の部分に注意: (a) "}};pop"で行が開始している個所において、前の行が終了すると、ネストしたwhere区の深さ(3)に対応する3つレイアウトルールの利用が呼び出される。(b)where句の閉じ括弧はタプルとcase式にネストされており、タプルの終了を検出されたため挿入された。(c)一番最後の閉じ括弧は、Eofトークンの0列のインデントにより挿入された。

``` hs
module AStack( Stack, push, pop, top, size ) where  
data Stack a = Empty  
             | MkStack a (Stack a)  

push :: a -> Stack a -> Stack a  
push x s = MkStack x s  

size :: Stack a -> Int  
size s = length (stkToLst s)  where  
           stkToLst  Empty         = []  
           stkToLst (MkStack x s)  = x:xs where xs = stkToLst s  

pop :: Stack a -> (a, Stack a)  
pop (MkStack x s)  
  = (x, case s of r -> i r where i x = x) -- (pop Empty) is an error  

top :: Stack a -> a  
top (MkStack x s) = x                     -- (top Empty) is an error
```

<a name="figure2.1">図2.1:</a> サンプルプログラム

``` hs
module AStack( Stack, push, pop, top, size ) where  
{data Stack a = Empty  
             | MkStack a (Stack a)  

;push :: a -> Stack a -> Stack a  
;push x s = MkStack x s  

;size :: Stack a -> Int  
;size s = length (stkToLst s)  where  
           {stkToLst  Empty         = []  
           ;stkToLst (MkStack x s)  = x:xs where {xs = stkToLst s  

}};pop :: Stack a -> (a, Stack a)  
;pop (MkStack x s)  
  = (x, case s of {r -> i r where {i x = x}}) -- (pop Empty) is an error  

;top :: Stack a -> a  
;top (MkStack x s) = x                        -- (top Empty) is an error  
}
```

<a name="figure2.2">図2.2:</a> レイアウトを展開したサンプルプログラム
