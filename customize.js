window.addEventListener('load', function(){
  document.querySelectorAll('pre code').forEach((code) => {
    //以下のものには処理しない
    // すでにハイライトが適用されているもの
    // 手付でhightlight.jsのクラス名を付けているもの
    if (code.className.includes('language-')) {
      return;
    }

    if(!code.classList.contains('language-hs')) { code.classList.add('language-hs'); }
    if(!code.classList.contains('hljs')) { code.classList.add('hljs'); }
    if(!code.classList.contains('haskell')) { code.classList.add('haskell'); }

    hljs.highlightBlock(code);
  });
})