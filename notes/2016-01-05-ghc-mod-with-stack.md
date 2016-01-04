---
title: ghc-modをstackと一緒に使う時の手順
---

Haskellネタです。

具体的な手順は[stackとghc-modを一緒に使う時の手順](http://qiita.com/siphilia_rn/items/bba4519710bb0513d6f9)としてQiitaの方に投稿しました。

前回のconduitネタの投稿から2年と2か月ちょっと経ってるんだけど、もうそんなに経つんだね…。

cabal-helper-wrapperが初回実行時にCabalをcabal-helper用にインストールする部分ですが、
出てるメッセージを見るとuser空間もしくはglobal空間のどっちかに入っていればそっちを使うっぽい。  
ただstack環境でuser/global空間にライブラリを入れるのはよく分からないしどうなのかなぁって事で、今回はcabal-helper専用にインストールしてもらうことにしました。
