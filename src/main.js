require('./style.css');
const { Elm } = require('./Main.elm');

var app = Elm.Main.init({
  node: document.getElementById('elm')
});

app.ports.playSound.subscribe(function(data) {
  const x = document.getElementById(data)
  x.muted = false
  x.play()
});

app.ports.openLink.subscribe(function(str) {
  window.open(str)
});

app.ports.copyString.subscribe(function(str) {
  // 空div 生成
  var tmp = document.createElement("div")
  // 選択用のタグ生成
  var pre = document.createElement('pre')

  // 親要素のCSSで user-select: none だとコピーできないので書き換える
  pre.style.webkitUserSelect = 'auto'
  pre.style.userSelect = 'auto'

  tmp.appendChild(pre).textContent = str

  // 要素を画面外へ
  var s = tmp.style
  s.position = 'fixed'
  s.right = '200%'

  // body に追加
  document.body.appendChild(tmp)
  // 要素を選択
  document.getSelection().selectAllChildren(tmp)

  // クリップボードにコピー
  var result = document.execCommand("copy")

  // 要素削除
  document.body.removeChild(tmp)
});
