---
title: ghcjsがnode.jsのバージョンによってはうまく動かない事への対処
---

Haskell(ghcjs)ネタです。

最初はQiitaの方に書こうと思っていたのですが、色々試していたらnode.jsのバージョン管理方法に強く依存する事が分かったのでやめてこっちにしました。

---

## エラーと解決方法

stackを使ってghcjsをsetupする時、又はstackで入れたghcjsでbuildする時に`The program 'ghcjs' version >=0.1 is required but the version of .../ghcjs could not be determined.`というエラーを吐く場合があります。  
これは[stack issue #1496](https://github.com/commercialhaskell/stack/issues/1496)に書かれていますが、node.jsのバージョンによって発生するエラーのようです。  
node.jsを4.0未満のバージョンにすると概ねどの環境でも正常に動くようになるみたいですが、4.0以上のバージョンでもエラーせずに動くものもあるようです(4.0とか5.0とか5.1とか)。  
ですので解決策は、エラーが出ないnode.jsのバージョンに差し替えましょう、という事になります。

## node.jsの差し替え

node.jsの差し替えは、既存のnode.jsのバージョン管理システム(n,nvm,nave,nodebrew,nodist,ndenv)を使えばOKです。

## ghcjsの再インストールが必要(?)なケース

stackで入れたghcjsのsetup-exe(例:\~/.stack/setup-exe-cache/x86_64-linux/setup-Simple-Cabal-1.22.4.0-ghcjs-0.2.0.20151230.3_ghc-7.10.2)はビルド時に存在していたnodeコマンドのパスがshebangとしてハードコーディングされていて、~/.ghcjs/x86_64-linux-0.2.0.20151230.3-7.10.2/ghcjs/nodeファイルにもnodeコマンドのパスが書かれています。

ですので、ghcjsとツール群が認識しているnode.jsのパスも一緒に変更しないと、nodeコマンドが見つからないというエラーを吐くようになったり、いつまでも差し替え前のnodeコマンドを参照したりします。

必要最低限の修正に留めるのは調査するのが面倒だったので、今回は下記の3つを削除してからghcjsの再インストールを行う事で新しいnode.jsのパスを認識してもらう事にしました。

* \~/.ghcjs
* \~/.stack/setup-exe-cache/x86_64-linux/setup-Simple-Cabal-1.22.4.0-ghcjs-0.2.0.20151230.3_ghc-7.10.2
* \~/.stack/setup-exe-cache/x86_64-linux/setup-Simple-Cabal-1.22.4.0-ghcjs-0.2.0.20151230.3_ghc-7.10.2.jsexe

これでnode.jsのバージョン管理システムを乗り換えたりしてnodeコマンドのパスが変わっても新しい方のパスを認識させることが出来ます。
