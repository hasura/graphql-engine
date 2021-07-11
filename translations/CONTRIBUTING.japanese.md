# Hasura graphql-engine への貢献

_はじめに_: 貢献の始め方に不安がある場合は、[Discord チャンネル](https://discordapp.com/invite/hasura)の #contrib チャンネルで気軽に質問してください。そのまま貢献していただいても構いません。私たちがフィードバックいたします。最悪の場合でも、何かを変更するように丁寧に求められるだけですので、心配しないでください。私たちはどのような貢献にも感謝し、ルールの障壁が貢献の妨げになるようなことは望んでいません。

しかし、プロジェクトに貢献するための最良の方法についてもう少しガイダンスが必要な方は、このまま読み進めてください。このドキュメントでは、私たちが求めているものについて説明しています。以下の点に対処いただくことで、私たちがあなたの貢献を迅速にマージ、または対応できる可能性が高まります。

## 目次

[1. 行動規範 ](#code-of-conduct)

[2. リポジトリの概要 ](#overview)

[3. 初めてのコントリビューター、歓迎します！ ](#first-timers)

[4. 貢献できる範囲 ](#areas)

[5. 貢献の仕方 ](#ways)

[6. コミットメッセージ ](#commit-messages)

[7. 翻訳 ](#translations)

<a name="code-of-conduct"></a>

## 1. 行動規範

Hasura への貢献については、当社の[行動規範](../code-of-conduct.md)に従ってください。

<a name="overview"></a>

## 2. リポジトリの概要

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) は 3 つのコンポーネントから成る、ひとつのリポジトリです。それぞれに独自の貢献ガイドがあります:

1. [サーバー (Haskell)](../server/CONTRIBUTING.md)

2. [CLI (Go)](../cli/CONTRIBUTING.md)

3. [コンソール (JavaScript)](../console/README.md#contributing-to-hasura-console)

3 つのコンポーネントはすべて単一のバージョンを持ち、 git タグかブランチ名と git commit SHA の組み合わせで示されます。

すべての貢献に対して、プルリクエストが送信される前（または送信後）に[こちら](https://cla-assistant.io/hasura/graphql-engine)の CLA(Contributor License Agreement) に署名する必要があります。ボットは必要に応じて、プルリクエストのコメントで CLA に署名するようにコントリビューターに促します。

<a name="first-timers"></a>

## 3. 初めてのコントリビューター、歓迎します！

私たちは初めてのコントリビューターに感謝し、始め方のサポートを喜んで行います。質問がある場合は、お気軽にお問い合わせください！

初めてのコントリビューターに適したイシューは[こちら](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22)からどうぞ。

<a name="areas"></a>

## 4. 貢献できる範囲

もちろん、 Hasura のすべてのコンポーネントへの貢献に感謝しています。しかしながら、オープンソースの貢献に特に適した 3 つの分野を特定しました。

### ドキュメント

私たちの目標はドキュメントを包括的で、更新された状態に保つことです。そのためにご協力いただける方は、どんな種類の貢献でも歓迎します:

- 欠けている内容の報告

- 既存のドキュメントのエラーの修正

- ドキュメントへの追加の協力

ドキュメントへの貢献のガイドは [docs/CONTRIBUTING.md](../docs/CONTRIBUTING.md) にあります。

### コミュニティコンテンツ

[学習ページ](https://hasura.io/learn/) を立ち上げたので、貢献いただけると嬉しいです:

- 既存の学習チュートリアルのエラー修正

- 新しいチュートリアルの追加（重複を避けるため、アイデアがあれば、お問い合わせください）

学習のリポジトリの README は[こちら](https://github.com/hasura/learn-graphql)にあります。

学習のコンテンツとは別に、技術コミュニティのコンテンツで貢献する方法を3つ挙げます:

- [ボイラープレート](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [サンプルアプリ](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [ツール](../community/tools)

以下のコミュニティに貢献したい場合、

- 技術ブログの投稿

- イベントでの登壇

- ワークショップの開催

私たちの[コミュニティ wiki](https://github.com/hasura/graphql-engine/wiki/Community-Wiki)をご確認ください。

上記のいずれかに関連しない場合でも、何か追加したいものがあれば遠慮なくプルリクエストを提出してください。

### Hasura CLI

CLI にはオープンソースでの貢献に適した課題がいくつかあります。 Go に触れたことのある方や、やりながら学びたい方は以下の[イシュー](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22)をチェックしてみてください。

CLI リポジトリの README は [こちら](https://github.com/hasura/graphql-engine/tree/master/cli) にあります。

<a name="ways"></a>

## 5. 貢献の仕方

### イシューを報告する

- 必ずリリースされている最新のバージョンでテストしてください。あなたが発見したバグはすでに修正されている可能性があります。

- Postgres のバージョン、 graphql-engine のバージョン、実行している環境（ Heroku 、 Docker など）を含めて、問題を再現するための手順を提供してください。

- 該当する場合は、サーバーのログも含めてください。

### イシューに取り組む

- [fork-and-branch git workflow](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/) を採用しています。

- 取り組むタスクに関連したイシューがあるかどうか確認してください。

- イシューに取り組んでいる場合は、他の人が重複して作業するのを防ぐためにも、その旨をコメントしてください。

- コミットをまとめ、コミットメッセージの最後に `fix #<issue-no>` や `close #<issue-no>` を使用して問題を参照してください。
  例: `resolve answers to everything (fix #42)` もしくは、 `resolve answers to everything, fix #42`

- プルリクエストを提出する前に、作業ブランチで master をリベースしてください。

<a name="commit-messages"></a>

## 6. コミットメッセージ

- 最初の行は変更点の概要を 50 文字以内で記述し、その後に変更点の詳細を記載したオプションの本文を記述します。
  良いコミットメッセージの書き方については[こちらのリンク](https://github.com/erlang/otp/wiki/writing-good-commit-messages)を参照してください。

- 命令形の現在形を使用してください: "added/fixed/changed" でも "adds/fixes/changes" でもなく、 "add/fix/change" を使用してください。

- 要約行の最初の文字を大文字にしないでください。

- 要約行の最後にピリオド / ドット (.) を付けないでください。

<a name="translations"></a>

## 7. 翻訳

このドキュメントは、以下の言語で利用可能です。

- [英語 🇬🇧](../CONTRIBUTING.md)
- [フランス語 🇫🇷](CONTRIBUTING.french.md)
- [日本語 🇯🇵](CONTRIBUTING.japanese.md)

(クレジット: いくつかのセクションは https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md から適用しています)
