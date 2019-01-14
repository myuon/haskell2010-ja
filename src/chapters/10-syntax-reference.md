[WIP]

# 文法リファレンス

### 慣習的表記

以下の慣習的表記が文法を表現するのにつかわれる:

- <code>[<em>pattern</em>]</code> 省略可能
- <code>{<em>pattern</em>}</code> 0回以上の繰り返し
- <code>(<em>pattern</em>)</code> グループ化
- <code><em>pat<sub>1</sub></em> | <em>pat<sub>2</sub></em></code> 選択
- <code>pat<sub>&lt;pat'&gt;</sub></code> 差(`pat`によって生成された要素で、`pat'`で生成されたものを除いたもの)
- `fibonacci` タイプライターフォントで表記される終端文法

BNFのような文法をレポートを通して用いる。文法の生成は次のような形をしている:

<pre>
nonterm -> alt<sub>1</sub> | alt<sub>2</sub> | .. | alt<sub>n</sub>
</pre>

字句文法、文脈自由文法いずれも曖昧さが残るが、これは文法語句をできる限り長く取り、左から右に進む(shift-reduceパースでは、shift/reduceコンフリクトはシフトを取ることで解決する)ことで解決するものとする。字句文法では、これは「最長一致」と呼ばれるルールである。文脈自由文法では、これは条件式やlet式、ラムダ抽象などが右方向に取れるだけ長くとることを表す。

### 字句文法

<pre>
<em>program</em>	→	{ <em>lexeme</em> | <em>whitespace</em> }
<em>lexeme</em>	→	<em>qvarid</em> | <em>qconid</em> | <em>qvarsym</em> | <em>qconsym</em>
                | <em>literal</em> | <em>special</em> | <em>reservedop</em> | <em>reservedid</em>
<em>literal</em>	→	<em>integer</em> | <em>float</em> | <em>char</em> | <em>string</em>
<em>special</em>	→	( | ) | , | ; | [ | ] | ` | { | }
 
<em>whitespace</em>	→	<em>whitestuff</em> {<em>whitestuff</em>}
<em>whitestuff</em>	→	<em>whitechar</em> | <em>comment</em> | <em>ncomment</em>
<em>whitechar</em>	→	<em>newline</em> | <em>vertab</em> | <em>space</em> | <em>tab</em> | <em>uniWhite</em>
<em>newline</em>	→	<em>return</em> <em>linefeed</em> | <em>return</em> | <em>linefeed</em> | <em>formfeed</em>
<em>return</em>	→	a carriage return
<em>linefeed</em>	→	a line feed
<em>vertab</em>	→	a vertical tab
<em>formfeed</em>	→	a form feed
<em>space</em>	→	a space
<em>tab</em>	→	a horizontal tab
<em>uniWhite</em>	→	any Unicode character defined as whitespace
 
<em>comment</em>	→	dashes [ any⟨symbol⟩ {any} ] newline
<em>dashes</em>	→	-- {-}
<em>opencom</em>	→	{-
<em>closecom</em>	→	-}
<em>ncomment</em>	→	<em>opencom</em> <em>ANY seq</em> {<em>ncomment</em> <em>ANY seq</em>} <em>closecom</em>
<em>ANY seq</em>	→	{<em>ANY</em>}<sub>⟨{<em>ANY</em>} ( <em>opencom</em> | <em>closecom</em> ) {<em>ANY</em>}⟩</sub>
<em>ANY</em>	→	<em>graphic</em> | <em>whitechar</em>
<em>any</em>	→	<em>graphic</em> | <em>space</em> | <em>tab</em>
<em>graphic</em>	→	<em>small</em> | <em>large</em> | <em>symbol</em> | <em>digit</em> | <em>special</em> | " | '
 
<em>small</em>	→	<em>ascSmall</em> | <em>uniSmall</em> | _
<em>ascSmall</em>	→	<tt>a</tt> | <tt>b</tt> | … | <tt>z</tt>
<em>uniSmall</em>	→	any Unicode lowercase letter
 
<em>large</em>	→	<em>ascLarge</em> | <em>uniLarge</em>
<em>ascLarge</em>	→	<tt>A</tt> | <tt>B</tt> | … | <tt>Z</tt>
<em>uniLarge</em>	→	any uppercase or titlecase Unicode letter
<em>symbol</em>	→	<em>ascSymbol</em> | <em>uniSymbol</em><sub>⟨<em>special</em> | _ | " | '⟩</sub>
 
<em>ascSymbol</em>	→ <tt>!</tt> | <tt>#</tt> | <tt>$</tt> | <tt>%</tt> | <tt>&</tt> | <tt>\8902</tt> | <tt>+</tt> | <tt>.</tt> | <tt>/</tt> | <tt><</tt> | <tt>=</tt> | <tt>></tt> | <tt>?</tt> | <tt>@</tt>
                    |	<tt>\</tt> | <tt>^</tt> | <tt>|</tt> | <tt>-</tt> | <tt>~</tt> | <tt>:</tt>
uniSymbol	→	any Unicode symbol or punctuation
digit	→	ascDigit | uniDigit
ascDigit	→	0 | 1 | … | 9
uniDigit	→	any Unicode decimal digit
octit	→	0 | 1 | … | 7
hexit	→	digit | A | … | F | a | … | f
 
varid	→	(small {small | large | digit | ' })⟨reservedid⟩
conid	→	large {small | large | digit | ' }
reservedid	→	case | class | data | default | deriving | do | else
|	foreign | if | import | in | infix | infixl
|	infixr | instance | let | module | newtype | of
|	then | type | where | _
 
varsym	→	( symbol⟨:⟩ {symbol} )⟨reservedop | dashes⟩
consym	→	( : {symbol})⟨reservedop⟩
reservedop	→	.. | : | :: | = | \ | | | <- | -> |  @ | ~ | =>
 
varid	    	    (variables)
conid	    	    (constructors)
tyvar	→	varid	    (type variables)
tycon	→	conid	    (type constructors)
tycls	→	conid	    (type classes)
modid	→	{conid .} conid	    (modules)
 
qvarid	→	[ modid . ] varid
qconid	→	[ modid . ] conid
qtycon	→	[ modid . ] tycon
qtycls	→	[ modid . ] tycls
qvarsym	→	[ modid . ] varsym
qconsym	→	[ modid . ] consym
 
decimal	→	digit{digit}
octal	→	octit{octit}
hexadecimal	→	hexit{hexit}
 
integer	→	decimal
|	0o octal | 0O octal
|	0x hexadecimal | 0X hexadecimal
float	→	decimal . decimal [exponent]
|	decimal exponent
exponent	→	(e | E) [+ | -] decimal
 
char	→	' (graphic⟨' | \⟩ | space | escape⟨\&⟩) '
string	→	" {graphic⟨" | \⟩ | space | escape | gap} "
escape	→	\ ( charesc | ascii | decimal | o octal | x hexadecimal )
charesc	→	a | b | f | n | r | t | v | \ | " | ' | &
ascii	→	^cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
|	BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
|	DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN
|	EM | SUB | ESC | FS | GS | RS | US | SP | DEL
cntrl	→	ascLarge | @ | [ | \ | ] | ^ | _
gap	→	\ whitechar {whitechar} \
</pre>

### レイアウト

セクション2.7(**[訳注]** TODO:リンク)ではレイアウトルールに対する非形式的な議論を見た。このセクションではより正確に定義をする。

Haskellプログラムの意味はその**レイアウト**に依存する場合がある。レイアウトが意味に与える効果は波括弧とセミコロンをレイアウトによって決定される位置に追加することで完全に説明できる。このようにして追加されたプログラムの意味は今やレイアウトによって影響を受けない。

レイアウトがプログラムに対して与える影響は、このセクションで波括弧とセミコロンをどのようについかするかを説明することで指定される。仕様は、プログラムの返還を行う関数`L`の形で与えられる。`L`の入力は次のようなものである:

- Haskellレポートにある字句文法によって定められた語句の列であって、さらに次のような追加の語句を含む:
    - `let, where, do, of`キーワードの後に`{`が続かない場合、トークン{n}がキーワードの後に挿入される。ただしnは、続くトークンがあればそのインデントを表し、ファイルの終端に達した場合は0を表す。
    - モジュールの最初のトークンが`{`でも`module`でもない場合、そのトークンのインデントをnとすると、{n}が先行する。
    - 同じ行で空白のみが最初のトークンに先行する場合、
    