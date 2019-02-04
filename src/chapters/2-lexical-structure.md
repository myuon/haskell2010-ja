# 字句構造

この章では、Haskellの低レベルな字句構造を説明する。このレポートを初めて読む場合には、詳細はほとんど読み飛ばしてもよいだろう。

## 表記法

以下の表記法は構文を表すために使用される。

|||
|:----|:----|
| [<em>pattern</em>]                         | 任意 |
| {<em>pattern</em>}                         | 0、またはそれ以上の繰り返し |
| (<em>pattern</em>)                         | グルーピング |
| <em>pat<sub>1</sub></em> &#124; <em>pat<sub>2</sub></em> | 選択 |
| pat<sub>(pat')</sub> |  相違 ー <em>pat'</em>によって生成されたものを除いた、<em>pat</em>によって生成された要素|
| <code>fibonacci</code> | タイプライターフォントの終端構文 |

このセクション内の構文は字句構造を説明しているため、全ての空白は明示的に表現されているが、並置されたシンボル間には暗黙的な空白はない。BNFのような構文は今後使われ、次の形式を取る。

||||
|---|---|---|
| <em>nonterm</em> | → | <em>alt<sub>1</sub></em> &#124; <em>alt<sub>2</sub></em> &#124; … &#124; <em>alt<sub>n</sub></em> |

通常は文脈によって区別が明確になるが、
<code>'|'</code>や<code>'[…]'</code>のような(タイプライターフォントで指定された)具体的な終端構文から'|'や'[…]'のようなメタデータ構文の区別には注意が必要である。

HaskellはUnicode[[2](./bibliogrphy.md)]文字セットを使っている。しかしながら、プログラムソースは現在、以前のHaskellバージョンで使われていたASCII文字セットに偏っている。

この構文はUnicodeコンソーシアムによって定義されているUnicode文字の文字符号化スキームによって異なる。Haskellコンパイラーは新しいバージョンのUnicodeが利用可能になるにつれてそれらを利用されることが期待されている。

## 字句プログラム構造

||||
|--|--|--|
| <em>program</em> |→| { <em>lexeme</em> &#124; <em>whietespace</em> }|
| <em>lexeme</em> |→| <em>qvarid</em> &#124; <em>qconid</em> &#124; <em>qvarsym</em> &#124; <em>qconsym</em> &#124; <em>literal</em> &#124; <em>special</em> &#124; <em>reservedop</em> &#124; <em>reservedid</em> |
| <em>literal</em> |→| <em>integer</em> &#124; <em>float</em> &#124; <em>char</em> &#124; <em>string</em> |
| <em>special</em> |→| (&#124;) &#124; , &#124; ; &#124; [ &#124; ] &#124; ` &#124; { &#124; } |
| | | |
| <em>whitespace</em> |→| <em>whitestuff</em> {<em>whitestuff</em>}|
| <em>whitestuff</em> |→| <em>whitechar</em> &#124; <em>comment</em> &#124; <em>ncomment</em> |
| <em>whitechar</em> |→| <em>newline</em> &#124; <em>vertab</em> &#124; <em>space</em> &#124; <em>tab</em> &#124; <em>uniWhite</em> |
| <em>newline</em> |→| <em>return</em> <em>linefeed</em> &#124; <em>return</em> &#124; <em>linefeed</em> &#124; <em>formfeed</em> |
| <em>return</em> |→| キャレッジ⏎ |
| <em>linefeed</em> |→| 改行 |
| <em>vertab</em> |→| 垂直タブ |
| <em>formfeed</em> |→| 改ページ |
| <em>space</em> |→| 空白 |
| <em>tab</em> |→| 水平タブ |
| <em>uniWhite</em> |→|	空白として定義されたUnicode文字 |
| | | |
|<em>comment</em> |→| <em>dashes</em> [ <em>any<sub>symbol</sub></em> {<em>any</em>} ] <em>newline</em> |
|<em>dashes</em> |→| <code>--</code> <code>{-}</code> |
|<em>opencom</em> |→| <code>{-</code> |
|<em>closecom</em> |→| <code>-}</code> |
|<em>ncomment</em> |→| <em>opencom</em> <em>ANY</em> <em>seq</em> {<em>ncomment</em> <em>ANY</em> <em>seq</em>} <em>closecom</em> |
|<em>ANY</em> <em>seq</em> |→| {<em>ANY</em> }<sub>⟨{<em>ANY</em> } ( <em>opencom</em> &#124; <em>closecom</em> ) {<em>ANY</em> }⟩</sub> |
|<em>ANY</em> |→| <em>graphic</em> &#124; <em>whitechar</em> |
|<em>any</em> |→| <em>graphic</em> &#124; <em>space</em> &#124; <em>tab</em> |
|<em>graphic</em> |→| <em>small</em> &#124; <em>large</em> &#124; <em>symbol</em> &#124; <em>digit</em> &#124; <em>special</em> &#124; <code>"</code> &#124; <code>'</code> |
| | | |
|<em>small</em> |→| <em>ascSmall</em> &#124; <em>uniSmall</em> &#124; <code>_</code> |
|<em>ascSmall</em> |→| <code>a</code> &#124; <code>b</code> &#124; … &#124; <code>z</code> |
|<em>uniSmall</em> |→| 小文字Unicode |
| | | |
|<em>large</em> |→| <em>ascLarge</em> &#124; <em>uniLarge</em> |
|<em>ascLarge</em> |→| <code>A</code> &#124; <code>B</code> &#124; … &#124; <code>Z</code> |
|<em>uniLarge</em> |→| 大文字またはタイトルケースのUnicode文字 |
|<em>symbol</em> |→| <em>ascSymbol</em> &#124; uniSymbol<sub>⟨special &#124; <code>_</code> &#124; <code>"</code> &#124; <code>'</code>⟩</sub> |
| | | |
|<em>ascSymbol</em> |→| <sub>!</sub> &#124; <sub>#</sub> &#124; <sub>$</sub> &#124; <sub>%</sub> &#124; <sub>&</sub> &#124; <sub>⋆</sub> &#124; <sub>+</sub> &#124; <sub>.</sub> &#124; <sub>/</sub> &#124; <sub><</sub> &#124; <sub>=</sub> &#124; <sub>></sub> &#124; <sub>?</sub> &#124; <sub>@</sub> &#124; <sub>\\</sub> &#124; <sub>^</sub> &#124; <sub>&#124;</sub> &#124; <sub>-</sub> &#124; <sub>~</sub> &#124; <sub>:</sub>|
|<em>uniSymbol</em> |→| Unicodeのシンボル、または句読点 |
|<em>digit</em> |→| <em>ascDigit</em> &#124; <em>uniDigit</em> |
|<em>ascDigit</em> |→| <code>0</code> &#124; <code>1</code> &#124; <em>…</em> &#124; <code>9</code> |
|<em>uniDigit</em> |→| 10進数Unicode |
|<em>octit</em> |→| <code>0</code> &#124; <code>1</code> &#124; <em>…</em> &#124; <code>7</code> |
|<em>hexit</em> |→| <em>digit</em> &#124; <code>A</code> &#124; <em>…</em> &#124; <code>F</code> &#124; <code>a</code> &#124; <code>…</code> &#124; <code>f</code> |

字句解析は"maximal munch"規則に従うべきである。すなわち、語彙素生成規則を満たす可能な限り長くとった語彙素が読み取られる([訳注]:日本語訳、最長一致。"longest match" ともいう)。したがって、<code>case</code>は予約語だが、<code>cases</code>は予約語ではない。同様に<code>=</code>は予約されているが、<code>==</code>と<code>~=</code>は予約されていない。

空白はどんなものでも正しい字句の区切り文字である。

**Any** カテゴリではない文字はHaskellプログラム内では有効ではなく、字句解析エラーを結果にすべきである。

## コメント

コメントは有効な空白である。

普通のコメントは二つ以上の連続したダッシュ(<code>--</code>など)で始まり、次の改行まで及ぶ。連続したダッシュは正当な語彙素の一部を形成してはいけない。例として、"-->"や"|--"はコメントの開始としては見なさない。なぜならこれらは正当な語彙素だからだ。しかしながら、<code>"--foo"</code>はコメントの開始として見なされる。

ネストされたコメントは<code>"{-"</code>で始まり、<code>"-}"</code>で終わる。<code>"{-"</code>は違法な語彙素ではない。それゆえ、例えば<code>"\{---"</code>は末尾に余分なダッシュがあるがネストされたコメントの始まりである。

コメントそれ自体は語彙的に解析されない。代わりに、初めに文字列<code>"-}"</code>が現れた前の部分までがネストされたコメントの範囲である。ネストされたコメントは任意の深さにネストできる。ネストされたコメント内に文字列<code>"{-"</code>があると新しいネストされたコメントが始まり、<code>"-}"</code>によって閉じられる。ネストされたコメント内では、各<code>"{-"</code>は対応する<code>"-}"</code>の出現によって照合される。

普通のコメント内では<code>"{-"</code>と<code>"-}"</code>の文字の並びは特別な意味を持たず、一方でネストされたコメント内ではダッシュの並びは特別な意味を持たない。

ネストされたコメントはコンパイラープラグマのためにも使われる。それについては[12章](./chapters/12-compiler-pragmas.md)で説明される。

もし、いくつかのコードがネストされたコメントによってコメントアウトされていたら、その時、そのコード内の文字列内または行末コメントに<code>"{-"</code>と<code>"-}"</code>があるとネストされたコメントに干渉する。

## 識別子と演算子

||||
|--|--|--|
|<em>varid</em> | → | (<em>small</em> {<em>small</em> &#124; <em>large</em> &#124; <em>digit</em> &#124; <code>'</code> })<sub>⟨reservedid⟩</sub>|
|<em>conid</em> | → | <em>large</em> {<em>small</em> &#124; <em>large</em> &#124; <em>digit</em> &#124; <code>'</code> }|
|<em>reservedid</em> | → | <code>case</code> &#124; <code>class</code> &#124; <code>data</code> &#124; <code>default</code> &#124; <code>deriving</code> &#124; <code>do</code> &#124; <code>else</code>|
| |&#124;| <code>foreign</code> &#124; <code>if</code> &#124; <code>import</code> &#124; <code>in</code> &#124; <code>infix</code> &#124; <code>infixl</code>|
| |&#124;| <code>infixr</code> &#124; <code>instance</code> &#124; <code>let</code> &#124; <code>module</code> &#124; <code>newtype</code> &#124; <code>of</code>|
| |&#124;| <code>then</code> &#124; <code>type</code> &#124; <code>where</code> &#124; <code>_</code>|

識別子は0個以上の文字、数字、アンダースコア、およびシングルクォートで構成される。識別子は字句的に小文字で始まる字句(変数識別子)と大文字から始まる字句(コンストラクタ識別子)の二つの名前空間に区別される。(セクション[1.4](./chapter1-introduction.md))これらの識別子は大文字と小文字を区別する。<code>name</code>、<code>naMe</code>、<code>Name</code>は3つの判然たる識別子である。(初め2つは変数識別子で、最後のはコンストラクタ識別子である。)

アンダースコア(<code>"\_"</code>)は小文字として扱われ、小文字が許されるところならどこでも使用可能だ。しかしながら、全て<code>"\_"</code>なものはパターンのワイルドカードのように使われる識別子として予約されている。未使用の識別子に対して警告を出すコンパイラーはアンダースコアで始まる識別子に対しては警告を抑制することが推奨される。これはプログラマーが未使用であると予想されるパラメータに<code>"\_foo"</code>を使うことを許可している。

||||
|--|--|--|
|<em>varsym</em> | → | ( <em>symbol</em><sub>⟨:⟩</sub> {<em>symbol</em>} )<sub>⟨<em>reservedop</em> &#124; <em>dashes</em>⟩</sub>|
|<em>consym</em> | → | ( <code>:</code> {<em>symbol</em>})<sub>⟨<em>reservedop</em>⟩</sub>|
|<em>reservedop</em> | → | <code> .. </code> &#124; <code> : </code> &#124; <code> :: </code> &#124; <code> = </code> &#124; <code> \ </code> &#124; <code> </code> &#124; <code> </code> &#124; <code> <- </code> &#124; <code> -> </code> &#124; <code> @ </code> &#124; <code> ~ </code> &#124; <code> => </code>|

演算子シンボルは上で定義したように、1つ以上の記号文字から形成され、2つの名前空間に字句的に区別される。(セクション[1.4](./chapters/1-introduction.md))

- コロンから始まる演算子シンボルはコンストラクタである。
- 他の文字から始まる演算子シンボルは普通の識別子である。

コロン(<code>":"</code>)はHaskellリストのコンストラクタとして使用されるためだけに予約されている。これにより、<code>"[]"</code>や<code>"[a,b]"</code>のようなリスト構文の他の部分との扱いが統一される。

接頭辞否定の特殊な構文を除き、全ての演算子は中置である。ただし、各中置演算子をセクション内で使用して、部分的に適応される演算子を生成することができる。(セクション[3.5](./chapters/3-expressions.md)を参照)標準の中置演算子はすべて定義済みのシンボルであり、リバウンドすることがある。

レポートの残りの部分では、6種類の名前が使用される。

|||||
|--|--|--|--|
|<em>varid</em> | |   | (variables)|
|<em>conid</em> | |   | (constructors)|
|<em>tyvar</em> | → | <em>varid</em> |           (type variables)|
|<em>tycon</em> | → | <em>conid</em> |           (type constructors)|
|<em>tycls</em> | → | <em>conid</em> |           (type classes)|
|<em>modid</em> | → | {<em>conid</em> <code>.</code>} <em>conid</em> | (modules)|

変数と型変数は小文字で始まる識別子によって表され、そのほかは大文字で始まる識別子によって表される。また、変数とコンストラクタには中置形式があるが、他の4つにはない。モジュール名はドットで区切られた一連の<em>conid</em>である。名前空間についてはセクション[1.4](./chapters/1-introduction.md)でも説明している。

特定の状況では、名前の前にモジュール識別子を付けることで、名前をオプションで修飾できる。これは変数、コンストラクタ、型コンストラクタ、型クラス、型クラス名に適応されるが、型変数やモジュール名には適応されない。修飾名については、チャプター[5](./chapters/5-modules.md)で詳しく説明する。

||||
|--|--|--|
|<em>qvarid</em>	| → | [modid <code>.</code>] <em>varid</em>|
|<em>qconid</em>	| → | [modid <code>.</code>] <em>conid</em>|
|<em>qtycon</em>	| → | [modid <code>.</code>] <em>tycon</em>|
|<em>qtycls</em>	| → | [modid <code>.</code>] <em>tycls</em>|
|<em>qvarsym</em>	| → | [modid <code>.</code>] <em>varsym</em>|
|<em>qconsym</em>	| → | [modid <code>.</code>] <em>consym</em>|

修飾名は語彙素なので、修飾子と名前の間には空白を入れることはできない。サンプルの語彙解析を以下に示す。

| これは | このような語彙 |
| -- | -- |
| <code>f.g</code> | <code>f . g</code> (3トークン)|
| <code>F.g</code> | <code>F.g</code> (修飾された'g')|
| <code>f..</code> | <code>f ..</code> (2トークン)|
| <code>F..</code> | <code>F..</code> (修飾された'.') |
| <code>F.</code>  | <code>F .</code> (2トークン) |

修飾子は名前の構文上の扱いを変更しない。例えば、<code>Prelude.+</code>はPrelude(セクション[4.4.2](./chapters/4-declarations-and-bindings.md))での<code>+</code>の定義と同じ固定性を持つ中置演算子である。

## 数値リテラル
||||
|--|--|--|
|<em>decimal</em> | → | <em>digit</em>{<em>digit</em>}|
|<em>octal</em> | → | <em>octit</em>{<em>octit</em>}|
|<em>hexadecimal</em> | → | <em>hexit</em>{<em>hexit</em>}|
||||
|<em>integer</em> | → | <em>decimal</em>|
| |&#124;| <code>0o</code> <em>octal</em> &#124; <code>0O</code> <em>octal</em>|
| |&#124;| <code>0x</code> <em>hexadecimal</em> &#124; <code>0X</code> <em>hexadecimal</em>|
||||
|<em>float</em> | → | <em>decimal</em> . <em>decimal</em> [<em>exponent</em>]
| |&#124;| <em>decimal</em> <em>exponent</em>|
||||
|<em>exponent</em> | → | (<code>e</code> &#124; <code>E</code>) [<code>+</code> &#124; <code>-</code>] <em>decimal</em>|

数値リテラルには整数と浮動小数点の2種類ある。整数リテラルは10進数(デフォルト)、8進数(<code>0o</code>または<code>0O</code>を先頭に付ける)、16進数表記(<code>0x</code>か<code>0X</code>を先頭に付ける)で指定できる。浮動小数点リテラルは常に10進数である。浮動小数点リテラルは小数点の前後に数値を含まなければいけない。これは小数点が他のドット文字の使い方に誤って認識されないことを保証するためだ。負数リテラルはセクション[3.4](./chapters/3-expressions.md)で論じられる。数値リテラルの型はセクション[6.4.1](./chapters/6-predefined-types-and-classes.md)で論じられる。

## 文字と文字列リテラル

||||
| -- | --| --|
|<em>char</em>	| → |	<code>'</code> (<em>graphic<sub>⟨' &#124; \⟩</sub></em> &#124; <em>space</em> &#124; <em>escape<sub>⟨\\&⟩</sub></em>) <code>'</code>|
|<em>string</em>	| → |	<code>"</code> {<em>graphic<sub>⟨" &#124; \⟩</sub></em> &#124; <em>space</em> &#124; <em>escape</em> &#124; <em>gap</em>} <code>"</code>|
|<em>escape</em>	| → |	<code>\\</code> ( <em>charesc</em> &#124; <em>ascii</em> &#124; <em>decimal</em> &#124; <code>o</code> <em>octal</em> &#124; <code>x</code> <em>hexadecimal</em> )|
|<em>charesc</em>	| → |	<code>a</code> &#124; <code>b</code> &#124; <code>f</code> &#124; <code>n</code> &#124; <code>r</code> &#124; <code>t</code> &#124; <code>v</code> &#124; <sub>\\</sub> &#124; <sub>"</sub> &#124; <sub>'</sub> &#124; <sub>&</sub>|
|<em>ascii</em>	| → |	<em>^cntrl</em> &#124; <code>NUL</code> &#124; <code>SOH</code> &#124; <code>STX</code> &#124; <code>ETX</code> &#124; <code>EOT</code> &#124; <code>ENQ</code> &#124; <code>ACK</code>|
| |&#124;|	<code>BEL</code> &#124; <code>BS</code> &#124; <code>HT</code> &#124; <code>LF</code> &#124; <code>VT</code> &#124; <code>FF</code> &#124; <code>CR</code> &#124; <code>SO</code> &#124; <code>SI</code> &#124; <code>DLE</code>|
| |&#124;|	<code>DC1</code> &#124; <code>DC2</code> &#124; <code>DC3</code> &#124; <code>DC4</code> &#124; <code>NAK</code> &#124; <code>SYN</code> &#124; <code>ETB</code> &#124; <code>CAN</code>|
| |&#124;| <code>EM</code> &#124; <code>SUB</code> &#124; <code>ESC</code> &#124; <code>FS</code> &#124; <code>GS</code> &#124; <code>RS</code> &#124; <code>US</code> &#124; <code>SP</code> &#124; <code>DEL</code>|
|<em>cntrl</em>	| → |	<em>ascLarge</em> &#124; <code>@</code> &#124; <code>[</code> &#124; <code>\\</code> &#124; <code>]</code> &#124; <code>^</code> &#124; <code>_</code>|
|<em>gap</em>	| → |	<code>\\</code> <em>whitechar</em> {<em>whitechar</em>} <code>\\</code>|

文字リテラルは<code>'a'</code>のようにシングルクォーテーションで囲まれたものであり、文字列リテラルは<code>"Hello"</code>のようにダブルクォーテーションで囲まれたものである。

エスケープコードは特殊文字の表現のために文字と文字列で使われる。注意すべきことはシングルクォーテーション(')は文字列においても使われるが、文字でエスケープする必要があることだ。同様に、ダブルクォーテーションは文字の中で使われるが、その際は文字列でエスケープする必要がある。<code>\\</code>は常にエスケープしないといけない。<em>charesc</em>カテゴリには"アラート"(<code>\\a</code>)、"バックスペース"(<code>\\b</code>)、"改ページ"(<code>\\f</code>)、"改行"(<code>\\n</code>)、"キャリッジ・リターン"(<code>\\r</code>)、"水平タブ"(<code>\\t</code>)、"垂直タブ"(<code>\\v</code>)といった文字のポータブル表現も含まれている。

<code>\\^X</code>のような制御文字を含むUnicode文字セットのエスケープ文字も用意している。<code>\\137</code>のような数値エスケープは10進数表現の137で文字を指定するために使用され、同様に8進数(例:<code>\\o137</code>)や16進数(例:<code>\\x37</code>)の表現も可能である。

“maximal munch”に従って、文字列内の数字のエスケープ文字は全て連続した数字で構成され、任意の長さにすることができる。同様に、<code>"\\SOH"</code>のような奇妙なASCIIエスケープコードは長さ1の文字列としてパ－スされる。<code>'\\&'</code>エスケープ文字は<code>"\\137\\&9"</code>や<code>"SO\\&H"</code>のような文字列(共に長さは2)が構成できるように"ヌル文字"として提供される。その代わり<code>"\\&"</code>は<code>""</code>に等しく、<code>'\\&'</code>文字は許されない。文字の等価性はセクション[6.1.2](./chapters/6-predefined-types-and-classes.md)で定義されている。

文字列は無視される"ギャップ"(白い文字を囲む2つのバックスラント)を含むかもしれない。これにより1行の終わりと次の行の始めにバックスラントを書くことによって、複数の行に長い文字列を書くことが可能だ。例としては以下のものになる。
``` hs
"Here is a backslant \\ as well as \137, \  
    \a numeric escape character, and \^X, a control character."
```

文字列リテラルは実際には文字のリストの略記である。(セクション[3.7](./chapters/3-expressions.md)を参照)

## レイアウト

Haskellはレイアウトを使用して同じ情報を伝えることによって、いくつかの文法で使用されている中括弧とセミコロンの省略を許可している。これによりレイアウトに依存するもの、しないものの両方のコーディングスタイルが可能になり、1つのプログラム内で自由に混在させることができる。レイアウトが必要ではないため、Haskellプログラムは他のプログラムによって簡単に作成することができる。

レイアウトがHaskellプログラムの意味に与える影響は、レイアウトによって決定される場所に中括弧とセミコロンを追加することによって完全に指定できる。この拡張プログラムの意味はレイアウトに影響されなくなった。

非公式には中括弧とセミコロンは次のように挿入される。キーワードの<code>where</code>、<code>let</code>、<code>do</code>、または<code>of</code>の後に開き括弧が省略されると、レイアウト(または"オフサイド")規則が有効になる。これが起こると、次の語彙素のインデントが(新しい行にあるかどうかにかかわらず)記録され、省略された開き括弧が挿入される(語彙素の前の空白にはコメントが含まれる場合がある)。後続の行について、空白のみが含まれている場合、またはそれ以上のインデントされている場合は、前の項目が続行される。(何も挿入されない)同じ量だけインデントされている場合は新しい項目が始まる(セミコロンが挿入される)。またインデントが小さくなると、レイアウトリストは終了する(閉じ括弧が挿入される)。<code>where</code>、<code>let</code>、<code>do</code>または<code>of</code>の直後のノンブレース語彙素のインデントが現在のインデントレベル以下である場合は、レイアウトを開始する代わりに、空のリスト<code>"{}"</code>が挿入され、現在のレベルでレイアウト処理が発生する(つまり、セミコロンまたは閉じ括弧を挿入する)。レイアウトリストを含む構文カテゴリが終了するたびに、閉じ括弧も挿入される。つまり、閉じ括弧が合法となる点で違法な語彙素が検出された場合は閉じ括弧が挿入される。レイアウトルールはそれが挿入した開いている中括弧にだけ一致する。明示的な開き括弧は、明示的に閉じ括弧と一致しなければならない。これらの明示的な開き括弧内では、たとえ行が以前の暗黙の開き括弧の左側に字下げされていても、括弧の外側の構成要素に対してレイアウト処理は実行されない。

セクション[10.3](./chapters/10-syntax-reference.md#3)ではレイアウトルールのより正確な定義を示す。

これらの規則を考えると、1つの改行で実際に複数のレイアウトリストを終了させることができる。
これらの規則は以下のコードを許す。
``` hs
f x = let a = 1; b = 2  
          g y = exp2  
       in exp1
```
生成した<code>a</code>, <code>b</code>, <code>g</code>は全て同じレイアウトリストの一部である。

例として、図[2.1](#figure2.1)は(ややわざとらしい)モジュールを示し、図[2.2](#figure2.2)はそのレイアウトルールを適応した結果を示している。次の部分に注意: (a) <code>}};pop</code>で行が開始している個所において、前の行が終了すると、ネストした<code>where</code>区の深さ(3)に対応する3つレイアウトルールの利用が呼び出される。(b)<code>where</code>句の閉じ括弧はタプルと<code>case</code>式にネストされており、タプルの終了を検出されたため挿入された。(c)一番最後の閉じ括弧は、Eofトークンの0列のインデントにより挿入された。

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
