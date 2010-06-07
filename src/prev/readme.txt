1.字句解析（Lexer.token）トークン列を作る
2.構文解析（Parser.exp）抽象構文木を作る
3.型推論（Typing.f）   型を決める
4.K正規化（KNormal.f） 構造を平たくする
5.α変換（Alpha.f）変数名をユニークにする
6.最適化（iter）
6.1.β簡約（Beta.f）等しい変数を置き換える
6.2.ネストしたletの簡約（Assoc.f）括弧内Letを平たくする
6.3.インライン展開（Inline.f）関数呼び出しの展開
6.4.定数畳み込み（ConstFold.f）値が分かっている計算を計算して定数にする
6.5.不要定義削除（Elim.f）使われない変数、関数の削除
7.クロージャ変換（Closure.f）ネスト関数を平らに
8.仮想マシンコード生成（Virtual.f）
9.SPARCの13 bit即値最適化（Simm13.f）
10.レジスタ割り当て（RegAlloc.f）
11.アセンブリ生成（Emit.f）

全16段階中3まで進んでいます。
完全なのかどうか、完全に理解しているか？というとそうでもないので


    12  m.ml
    25  id.ml
    11  type.ml
    27  syntax.ml
   915  lexer.ml
   853  parser.ml
   166  typing.ml
    11  s.ml
   179  kNormal.ml
    46  alpha.ml
    38  beta.ml
    18  assoc.ml
    32  inline.ml
    50  constFold.ml
    32  elim.ml
   106  closure.ml
   103  x86Asm.ml
   164  virtual_x86.ml
    42  simm13.ml
   155  regAlloc.ml
   325  emit_x86.ml
    45  main.ml
    29  test.ml
    22  anchor.ml


