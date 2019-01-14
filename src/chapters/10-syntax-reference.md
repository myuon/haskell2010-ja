[WIP]

# 文法リファレンス

### 慣習的表記

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

### 字句文法

<pre>
<em>program</em>		→ { <em>lexeme</em> | <em>whitespace</em> }
<em>lexeme</em>		→ <em>qvarid</em> | <em>qconid</em> | <em>qvarsym</em> | <em>qconsym</em>
		| <em>literal</em> | <em>special</em> | <em>reservedop</em> | <em>reservedid</em>
<em>literal</em>		→ <em>integer</em> | <em>float</em> | <em>char</em> | <em>string</em>
<em>special</em>		→ ( | ) | , | ; | [ | ] | ` | { | }
 
<em>whitespace</em>	→ <em>whitestuff</em> {<em>whitestuff</em>}
<em>whitestuff</em>	→ <em>whitechar</em> | <em>comment</em> | <em>ncomment</em>
<em>whitechar</em>	→ <em>newline</em> | <em>vertab</em> | <em>space</em> | <em>tab</em> | <em>uniWhite</em>
<em>newline</em>		→ <em>return</em> <em>linefeed</em> | <em>return</em> | <em>linefeed</em> | <em>formfeed</em>
<em>return</em>		→ a carriage return
<em>linefeed</em>	→ a line feed
<em>vertab</em>		→ a vertical tab
<em>formfeed</em>	→ a form feed
<em>space</em>		→ a space
<em>tab</em>		→ a horizontal tab
<em>uniWhite</em>	→ any Unicode character defined as whitespace
 
<em>comment</em>		→ dashes [ any⟨symbol⟩ {any} ] newline
<em>dashes</em>		→ -- {-}
<em>opencom</em>		→ {-
<em>closecom</em>	→ -}
<em>ncomment</em>	→ <em>opencom</em> <em>ANY seq</em> {<em>ncomment</em> <em>ANY seq</em>} <em>closecom</em>
<em>ANY seq</em>		→ {<em>ANY</em>}<sub>⟨{<em>ANY</em>} ( <em>opencom</em> | <em>closecom</em> ) {<em>ANY</em>}⟩</sub>
<em>ANY</em>		→ <em>graphic</em> | <em>whitechar</em>
<em>any</em>		→ <em>graphic</em> | <em>space</em> | <em>tab</em>
<em>graphic</em>		→ <em>small</em> | <em>large</em> | <em>symbol</em> | <em>digit</em> | <em>special</em> | " | '
 
<em>small</em>		→ <em>ascSmall</em> | <em>uniSmall</em> | _
<em>ascSmall</em>	→ <tt>a</tt> | <tt>b</tt> | … | <tt>z</tt>
<em>uniSmall</em>	→ any Unicode lowercase letter
 
<em>large</em>		→ <em>ascLarge</em> | <em>uniLarge</em>
<em>ascLarge</em>	→ <tt>A</tt> | <tt>B</tt> | … | <tt>Z</tt>
<em>uniLarge</em>	→ any uppercase or titlecase Unicode letter
<em>symbol</em>		→ <em>ascSymbol</em> | <em>uniSymbol</em><sub>⟨<em>special</em> | _ | " | '⟩</sub>
 
<em>ascSymbol</em>	→ <tt>!</tt> | <tt>#</tt> | <tt>$</tt> | <tt>%</tt> | <tt>&</tt> | <tt>⋆</tt> | <tt>+</tt> | <tt>.</tt> | <tt>/</tt> | <tt><</tt> | <tt>=</tt> | <tt>></tt> | <tt>?</tt> | <tt>@</tt>
                | <tt>\</tt> | <tt>^</tt> | <tt>|</tt> | <tt>-</tt> | <tt>~</tt> | <tt>:</tt>
<em>uniSymbol</em>	→ any Unicode symbol or punctuation
<em>digit</em>		→ <em>ascDigit</em> | <em>uniDigit</em>
<em>ascDigit</em>	→ <tt>0</tt> | <tt>1</tt> | … | <tt>9</tt>
<em>uniDigit</em>	→ any Unicode decimal digit
<em>octit</em>		→ <tt>0</tt> | <tt>1</tt> | … | <tt>7</tt>
<em>hexit</em>		→ <em>digit</em> | <tt>A</tt> | … | <tt>F</tt> | <tt>a</tt> | … | <tt>f</tt>
 
<em>varid</em>		→ (<em>small</em> {<em>small</em> | <em>large</em> | <em>digit</em> | <tt>'</tt> })<sub>⟨reservedid⟩</sub>
<em>conid</em>		→ <em>large</em> {<em>small</em> | <em>large</em> | <em>digit</em> | <tt>'</tt> }
<em>reservedid</em>	→ <tt>case</tt> | <tt>class</tt> | <tt>data</tt> | <tt>default</tt> | <tt>deriving</tt> | <tt>do</tt> | <tt>else</tt>
		| <tt>foreign</tt> | <tt>if</tt> | <tt>import</tt> | <tt>in</tt> | <tt>infix</tt> | <tt>infixl</tt>
		| <tt>infixr</tt> | <tt>instance</tt> | <tt>let</tt> | <tt>module</tt> | <tt>newtype</tt> | <tt>of</tt>
		| <tt>then</tt> | <tt>type</tt> | <tt>where</tt> | <tt>_</tt>
 
<em>varsym</em>		→ ( <em>symbol</em><sub>⟨<tt>:</tt>⟩</sub> {<em>symbol</em>} )<sub>⟨reservedop | dashes⟩</sub>
<em>consym</em>		→ ( <tt>:</tt> {<em>symbol</em>})<sub>⟨reservedop⟩</sub>
<em>reservedop</em>	→ <tt>..</tt> | <tt>:</tt> | <tt>::</tt> | <tt>=</tt> | <tt>\</tt> | <tt>|</tt> | <tt><-</tt> | <tt>-></tt> |  <tt>@</tt> | <tt>~</tt> | <tt>=></tt>
 
<em>varid</em>	    	    (variables)
<em>conid</em>	    	    (constructors)
<em>tyvar</em>		→ <em>varid</em>	    (type variables)
<em>tycon</em>		→ <em>conid</em>	    (type constructors)
<em>tycls</em>		→ <em>conid</em>	    (type classes)
<em>modid</em>		→ {<em>conid</em> <tt>.</tt>} <em>conid</em>	    (modules)
 
<em>qvarid</em>		→ [ <em>modid</em> <tt>.</tt> ] <em>varid</em>
<em>qconid</em>		→ [ <em>modid</em> <tt>.</tt> ] <em>conid</em>
<em>qtycon</em>		→ [ <em>modid</em> <tt>.</tt> ] <em>tycon</em>
<em>qtycls</em>		→ [ <em>modid</em> <tt>.</tt> ] <em>tycls</em>
<em>qvarsym</em>		→ [ <em>modid</em> <tt>.</tt> ] <em>varsym</em>
<em>qconsym</em>		→ [ <em>modid</em> <tt>.</tt> ] <em>consym</em>
 
<em>decimal</em>		→ <em>digit</em>{<em>digit</em>}
<em>octal</em>		→ <em>octit</em>{<em>octit</em>}
<em>hexadecimal</em>	→ <em>hexit</em>{<em>hexit</em>}
 
<em>integer</em>		→ <em>decimal</em>
		| <tt>0o</tt> <em>octal</em> | <tt>0O</tt> <em>octal</em>
		| <tt>0x</tt> <em>hexadecimal</em> | <tt>0X</tt> <em>hexadecimal</em>
<em>float</em>		→ <em>decimal</em> <tt>.</tt> <em>decimal</em> [<em>exponent</em>]
		|	<em>decimal</em> <em>exponent</em>
<em>exponent</em>	→ (<tt>e</tt> | <tt>E</tt>) [<tt>+</tt> | <tt>-</tt>] <em>decimal</em>
 
<em>char</em>		→ <tt>'</tt> (<em>graphic</em><sub>⟨<tt>'</tt> | <tt>\</tt>⟩</sub> | <em>space</em> | <em>escape</em><sub>⟨<tt>\&</tt>⟩</sub>) <tt>'</tt>
<em>string</em>		→ <tt>"</tt> {<em>graphic</em><sub>⟨<tt>"</tt> | <tt>\</tt>⟩</sub> | <em>space</em> | <em>escape</em> | <em>gap</em>} <tt>"</tt>
<em>escape</em>		→ <tt>\</tt> ( <em>charesc</em> | <em>ascii</em> | <em>decimal</em> | <tt>o</tt> <em>octal</em> | <tt>x</tt> <em>hexadecimal</em> )
<em>charesc</em>		→ <tt>a</tt> | <tt>b</tt> | <tt>f</tt> | <tt>n</tt> | <tt>r</tt> | <tt>t</tt> | <tt>v</tt> | <tt>\</tt> | <tt>"</tt> | <tt>'</tt> | <tt>&</tt>
<em>ascii</em>		→ <tt>^</tt><em>cntrl</em> | <tt>NUL</tt> | <tt>SOH</tt> | <tt>STX</tt> | <tt>ETX</tt> | <tt>EOT</tt> | <tt>ENQ</tt> | <tt>ACK</tt>
		| <tt>BEL</tt> | <tt>BS</tt> | <tt>HT</tt> | <tt>LF</tt> | <tt>VT</tt> | <tt>FF</tt> | <tt>CR</tt> | <tt>SO</tt> | <tt>SI</tt> | <tt>DLE</tt>
		| <tt>DC1</tt> | <tt>DC2</tt> | <tt>DC3</tt> | <tt>DC4</tt> | <tt>NAK</tt> | <tt>SYN</tt> | <tt>ETB</tt> | <tt>CAN</tt>
		| <tt>EM</tt> | <tt>SUB</tt> | <tt>ESC</tt> | <tt>FS</tt> | <tt>GS</tt> | <tt>RS</tt> | <tt>US</tt> | <tt>SP</tt> | <tt>DEL</tt>
<em>cntrl</em>		→ <em>ascLarge</em> | <tt>@</tt> | <tt>[</tt> | <tt>\</tt> | <tt>]</tt> | <tt>^</tt> | <tt>_</tt>
<em>gap</em>		→ <tt>\</tt> <em>whitechar</em> {<em>whitechar</em>} <tt>\</tt>
</pre>

### レイアウト

セクション2.7(**[訳注]** TODO:リンク)ではレイアウトルールに対する非形式的な議論を見た。このセクションではより正確に定義をする。

Haskellプログラムの意味はその**レイアウト**に依存する場合がある。レイアウトが意味に与える効果は波括弧とセミコロンをレイアウトによって決定される位置に追加することで完全に説明できる。このようにして追加されたプログラムの意味は今やレイアウトによって影響を受けない。

レイアウトがプログラムに対して与える影響は、このセクションで波括弧とセミコロンをどのようについかするかを説明することで指定される。仕様は、プログラムの返還を行う関数`L`の形で与えられる。`L`の入力は次のようなものである:

- Haskellレポートにある字句文法によって定められた語句の列であって、さらに次のような追加の語句を含む:
    - `let, where, do, of`キーワードの後に`{`が続かない場合、トークン{n}がキーワードの後に挿入される。ただしnは、続くトークンがあればそのインデントを表し、ファイルの終端に達した場合は0を表す。
    - モジュールの最初のトークンが`{`でも`module`でもない場合、そのトークンのインデントをnとすると、{n}が先行する。
    - 同じ行で空白のみが最初のトークンに先行する場合、
    