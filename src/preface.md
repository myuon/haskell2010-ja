# 前書き

> 『幾多の人々がコンビネーター論理に関する技術的な文書を残しており、さらにその多くは、われわれのものも含めて誤った状態で出版されている。我々の罪を犯した仲間たちはその時代で最も注意深くて有能な論理学者に属する人々であり、我々はこのことがこの分野が手に負えないことの証拠であると考えている。よって十分な解説は正確さのために必要であるし、極端な要約は(通常よりも遥かに)節約にならないであろう。』
> 
> ハスケル・B・カリー, ロベール・フェイ  
> コンビネーター論理[3]の前書きにて 1956年5月31日

1987年の7月、オレゴン州のポートランドでの関数型言語とコンピューターの設計(FPCA '87)学会にて会議が開かれ、関数型言語コミュニティが置かれている不幸な状況について話し合われた。すなわち、それまで10を超える非正格な純粋関数型言語が実装されており、それらの表現力とベースとなる意味論がどれも似かよっていたことである。その会議で強く意見が一致したこととして、共通となる言語が欠けていることはこの種の関数型言語がより広く使われるようになることの妨げとなっているということであった。新しいアイデアの素早いやりとりの場、現実のアプリケーション開発のための安定した基盤、そして人々が関数型言語を利用することを推奨する売り込みの手段を提供する、共通言語を設計するための委員会が結成されることが決定された。。この文書はその委員会(とそれに続く)の努力の結果である、Haskellと呼ばれる純粋関数型言語を説明したものである。これは我々の多くの部分の論理学的な基礎を提供する仕事を成した論理学者ハスケル・B・カリーにちなんでつけられた。

## 目的

委員会の主たる目的は次の条件を満たすような言語をデザインすることであった。

1. 教育、研究、そして大規模なシステムを作ることも含めたアプリケーションに適したものであること。
2. 公表された形式的な文法および意味論により完全に説明されるものであること。
3. 自由に利用可能なものであること。誰でも言語を実装し、それを配りたい人に配ることが許可されていること。
4. 広く認められたアイデアに基づいているものであること。
5. 関数型プログラミング言語の不要な多様性を減らすこと。

## Haskell 2010: 言語とライブラリ

委員会はHaskellが言語デザインにおいて将来の研究の礎の役割を果たすことを目的とし、実験的な機能を盛り込んだその拡張や変種たる言語が登場することを望んでいた。

Haskellは実際には最初の発表から進化を続けてきている。1997年の中頃までには、言語デザインに5つのバージョンが存在していた(Haskell 1.0から1.4)。1997年にアムステルダムで開かれたHaskellのワークショップにおいて、Haskellの安定した変種が必要であることが決定され、これが"Haskell 98"となり、1999年の2月に発表された。小さなバグを修正したものが2002年に **改訂版** Haskell 98レポートとされた。

2005年のHaskellワークショップにおいて、公式の言語に対する多くの拡張が広く使われているということ(そしてそれらは複数の実装でサポートされていること)と、言語標準を定期的に定義することは意味のあることであることが合意された。それには現在の状態の明文化(とルール化)が必須であった。

このためHaskell Primeの取り組みがHaskell 98の相対的に保守的な拡張であると理解された、新しい機能は皆によく理解され広く合意されたものにしか立脚しないのである。それは"安定した"言語であることも意図されていたが、近年の言語デザインの研究でのかなりの進歩も反映したものであった。

色々な言語デザインを調査し数年の後には、唯一の一枚岩な言語の版でやるには仕事が大きすぎること、進捗を生む最良の方法は言語を小さくゆっくりと段階を踏んだ言語の進化であるので、各版はよく理解された拡張と変更のほんの一部だけを統一したものにすることが決定された。Haskell 2010はこのようにして作られた最初の版であり、新しい版は毎年1つずつ出される見込みである。

## Haskell 98への拡張

Haskell 98と比較してHaskell 2010に入った最も重要な言語の変更点をここに並べる。

新しい言語機能:

- 外部関数インターフェイス(FFI)
- 階層的モジュール名(例: `Data.Bool`)
- パターンガード

削除された言語機能:

- `(n + k)`パターン文法

## Haskellのリソース

Haskellのウェブサイト https://www.haskell.org はたくさんの有益なリソースへのアクセスを可能にしている。次はその一部である:

- オンライン版の言語とライブラリの定義
- Haskellのチュートリアルの資料
- Haskellメーリングリストの詳細
- Haskellの実装
- Haskellに貢献したツールやライブラリ
- Haskellのアプリケーション
- ユーザーが貢献するwikiの記事
- Haskellコミュニティにおけるニュースとイベント

是非あなたもHaskellのメーリングリストやHaskell wikiにて、コメントし、改善を提案し、言語やレポートの現状について批評をしてみてください。

## 言語の構築

Haskellは研究者やアプリケーションプログラマーたちの活発なコミュニティによって作られ、維持されてきた。言語とライブラリ委員会の一員を務める人々が、特に多量の時間とエネルギーをこの言語に捧げてきた。彼らの名前と、その期間での所属をここに記す。

(**訳注** ここでは名前と所属について翻訳をせず原文のまま表示します)

<div style="text-align: center;">
Arvind (MIT)
<br />
Lennart Augustsson (Chalmers University)
<br />
Dave Barton (Mitre Corp)
<br />
Brian Boutel (Victoria University of Wellington)
<br />
Warren Burton (Simon Fraser University)
<br />
Manuel M T Chakravarty (University of New South Wales)
<br />
Duncan Coutts (Well-Typed LLP)
<br />
Jon Fairbairn (University of Cambridge)
<br />
Joseph Fasel (Los Alamos National Laboratory)
<br />
John Goerzen
<br />
Andy Gordon (University of Cambridge)
<br />
Maria Guzman (Yale University)
<br />
Kevin Hammond [編集者] (University of Glasgow)
<br />
Bastiaan Heeren (Utrecht University)
<br />
Ralf Hinze (University of Bonn)
<br />
Paul Hudak [編集者] (Yale University)
<br />
John Hughes [編集者] (University of Glasgow; Chalmers University)
<br />
Thomas Johnsson (Chalmers University)
<br />
Isaac Jones (Galois, inc.)
<br />
Mark Jones (Yale University, University of Nottingham, Oregon Graduate Institute)
<br />
Dick Kieburtz (Oregon Graduate Institute)
<br />
John Launchbury (University of Glasgow; Oregon Graduate Institute; Galois, inc.)
<br />
Andres Löh (Utrecht University)
<br />
Ian Lynagh (Well-Typed LLP)
<br />
Simon Marlow [編集者] (Microsoft Research)
<br />
John Meacham
<br />
Erik Meijer (Utrecht University)
<br />
Ravi Nanavati (Bluespec, inc.)
<br />
Rishiyur Nikhil (MIT)
<br />
Henrik Nilsson (University of Nottingham)
<br />
Ross Paterson (City University, London)
<br />
John Peterson [編集者] (Yale University)
<br />
Simon Peyton Jones [編集者] (University of Glasgow; Microsoft Research Ltd)
<br />
Mike Reeve (Imperial College)
<br />
Alastair Reid (University of Glasgow)
<br />
Colin Runciman (University of York)
<br />
Don Stewart (Galois, inc.)
<br />
Martin Sulzmann (Informatik Consulting Systems AG)
<br />
Audrey Tang
<br />
Simon Thompson (University of Kent)
<br />
Philip Wadler [編集者] (University of Glasgow)
<br />
Malcolm Wallace (University of York)
<br />
Stephanie Weirich (University of Pennsylvania)
<br />
David Wise (Indiana University)
<br />
Jonathan Young (Yale University)
</div>

[編集者]と記された人々はこの言語の1つ以上の版での編集に協力してくれている。

加えて、他にも多くの人々が多かれ少なかれ有益な貢献をしてくれている。以下の人々である。
Hans Aberg, Kris Aerts, Sten Anderson, Richard Bird, Tom Blenko, Stephen Blott, Duke Briscoe, Paul Callaghan, Magnus Carlsson, Mark Carroll, Franklin Chen, Olaf Chitil, Chris Clack, Guy Cousineau, Tony Davie, Craig Dickson, Chris Dornan, Laura Dutton, Chris Fasel, Pat Fasel, Sigbjorn Finne, Michael Fryers, Peter Gammie, Andy Gill, Mike Gunter, Cordy Hall, Mark Hall, Thomas Hallgren, Matt Harden, Klemens Hemm, Fergus Henderson, Dean Herington, Bob Hiromoto, Nic Holt, Ian Holyer, Randy Hudson, Alexander Jacobson, Patrik Jansson, Robert Jeschofnik, Orjan Johansen, Simon B. Jones, Stef Joosten, Mike Joy, Wolfram Kahl, Stefan Kahrs, Antti-Juhani Kaijanaho, Jerzy Karczmarczuk, Kent Karlsson, Martin D. Kealey, Richard Kelsey, Siau-Cheng Khoo, Amir Kishon, Feliks Kluzniak, Jan Kort, Marcin Kowalczyk, Jose Labra, Jeff Lewis, Mark Lillibridge, Bjorn Lisper, Sandra Loosemore, Pablo Lopez, Olaf Lubeck, Christian Maeder, Ketil Malde, Michael Marte, Jim Mattson, John Meacham, Sergey Mechveliani, Gary Memovich, Randy Michelsen, Rick Mohr, Andy Moran, Graeme Moss, Arthur Norman, Nick North, Chris Okasaki, Bjarte M. Østvold, Paul Otto, Sven Panne, Dave Parrott, Larne Pekowsky, Rinus Plasmeijer, Ian Poole, Stephen Price, John Robson, Andreas Rossberg, George Russell, Patrick Sansom, Michael Schneider, Felix Schroeter, Julian Seward, Nimish Shah, Christian Sievers, Libor Skarvada, Jan Skibinski, Lauren Smith, Raman Sundaresh, Josef Svenningsson, Ken Takusagawa, Wolfgang Thaller, Satish Thatte, Tom Thomson, Tommy Thorn, Dylan Thurston, Mike Thyer, Mark Tullsen, David Tweed, Pradeep Varma, Keith Wansbrough, Tony Warnock, Michael Webber, Carl Witty, Stuart Wray, and Bonnie Yantis.

最後に、チャーチ、ロッサー、カリーによる重要で基礎的な仕事とほかの人々のラムダ計算に関する仕事はもちろんのこととして、長年にわたって開発されてきた注目に値する多くのプログラミング言語の影響を認めるのが正しい。様々なアイデアの源をピンポイントに挙げるのは難しいが、次の言語は特に影響を与えている: Lisp (とその現在での姿であるCommon LispとScheme)、ランディンのISWIM、APL、バッカスのFP[1]、MLとStandard ML、Hopeと Hope+、Clean、Id、Gofer、Sisal、そしてターナーのMiranda(Mirandaは有限会社リサーチソフトウェアのトレードマークである)が最後を飾った一連の言語。これらの先駆者たちなしにはHaskellは不可能であっただろう。

サイモン・マーロー  
2010年4月 ケンブリッジ大学にて
