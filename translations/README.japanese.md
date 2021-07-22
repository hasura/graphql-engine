# Hasura GraphQL Engine

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://hasura.io/newsletter/"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQLエンジンは非常に高速なGraphQLサーバです。 **Postgres上に瞬間的でリアルタイムなGraphQL API**を、データベースイベントによる[**Webhookトリガー**](../event-triggers.md)とビジネスロジック用の[**リモートスキーマ**](../remote-schemas.md)と共に提供します。

Hasuraは、Postgresをバックエンドに持つGraphQLアプリケーションを構築したり、Postgresを使用して既存のアプリケーションをGraphQLに段階的に移行したりするのに役立ちます。

詳細については、 [hasura.io](https://hasura.io) と [docs](https://hasura.io/docs) をご覧ください。

------------------

![Hasura GraphQL エンジンデモ](../assets/demo.gif)

------------------

![Hasura GraphQL エンジンリアルタイムデモ](../assets/realtime.gif)

-------------------

## 機能一覧

* **強力なクエリの作成**: 組み込みフィルタリング、ページ付け、パターン検索、一括挿入、更新、削除などのミューテーション
* **リアルタイム**: subscriptions使って様々なGraphQLクエリをライブクエリに変換
* **リモートスキーマのマージ**: 単一のGraphQLエンジンエンドポイントを介してビジネスロジック用のカスタムGraphQLスキーマにアクセスカスタムのGraphQLスキーマにアクセス [**続きを読む**](../remote-schemas.md).
* **Webhookやサーバーレスファンクションの起動**: Postgresのinsert / update / deleteイベント時 ([続きを読む](../event-triggers.md))
* **既存のライブデータベースと連携**: 既存のPostgresデータベースから、すぐに使えるGraphQL APIが即座に作られます
* **きめ細かいアクセス制御**: 認証システムと統合する動的アクセス制御（例：auth0、firebase-auth）
* **高性能でフットプリントが小さい**: 最大15MBのDockerイメージ。最大50MB RAM @ 1000 req/s。マルチコア対応
* **管理UIとマイグレーション**: 管理UIとRailsから着想を得たスキーマのマイグレーション
* **Postgres** ❤️: Postgresの型（PostGIS/地理位置など）をサポートし、ビューをグラフに変え、ストアドファンクションやストアドプロシージャをミューテーションを通して起動できます

詳細については、 [hasura.io](https://hasura.io) と [docs](https://hasura.io/docs)　をご覧ください。

## 目次
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**目次**

- [クイックスタート:](#クイックスタート)
    - [Hasura Cloudへのワンクリックデプロイ](#hasura-cloudへのワンクリックデプロイ)
    - [ワンクリックでデプロイする他の方法](#ワンクリックでデプロイする他の方法)
    - [その他のデプロイ方法](#その他のデプロイ方法)
- [アーキテクチャ](#アーキテクチャ)
- [クライアント側ツール](#クライアント側ツール)
- [ビジネスロジックの追加](#ビジネスロジックの追加)
    - [リモートスキーマ](#リモートスキーマ)
    - [データベースイベントによるWebhookの起動](#データベースイベントによるwebhooksの起動)
- [デモ](#デモ)
    - [リアルタイムアプリケーション](#リアルタイムアプリケーション)
    - [動画](#動画)
- [サポート&トラブル対応](#サポートトラブル対応)
- [開発への参加](#開発への参加)
- [ブランド資産](#ブランド資産)
- [ライセンス](#ライセンス)
- [翻訳](#翻訳)

<!-- markdown-toc end -->

## クイックスタート:

### Hasura Cloudへのワンクリックデプロイ

Hasuraを試す最も早くて簡単な方法は、[Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html)を使うことです。

1. 次のボタンをクリックして、Postgresアドオンを含むまたは既存のPostgresデータベースを使ってHasuraCloudにGraphQLエンジンをデプロイします。

    [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. Hasuraコンソールを開きます。

   "Launch console"ボタンをクリックして、Hasuraコンソールを開きます。

3. 最初のGraphQLのクエリを発行

   テーブルを作成して、最初のクエリを実行します。こちらの [シンプルなガイド](https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html)を参考にしてください。

### ワンクリックでデプロイする他の方法

以下のワンクリックデプロイの方法もチェックしてみてください:

| **インフラプロバイダ** | **ワンクリックリンク** | **追加情報** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [ドキュメント](https://hasura.io/docs/latest/graphql/core/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [ドキュメント](https://hasura.io/docs/latest/graphql/core/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [ドキュメント](https://hasura.io/docs/latest/graphql/core/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [ドキュメント](https://hasura.io/docs/latest/graphql/core/guides/deployment/render-one-click.html) |

### その他のデプロイ方法

Dockerでのデプロイと高度なカスタマイズオプションについては [デプロイガイド](https://hasura.io/docs/latest/graphql/core/getting-started/index.html) か
[インストールマニフェスト](../install-manifests)を参照してください。

## アーキテクチャ

Hasura GraphQLエンジンはPostgresデータベースインスタンスの前で動作し、クライアントアプリケーションからGraphQLのリクエストを受け取ることができます。既存の認証システムと連携するように設定でき、認証システムからの動的変数を含むフィールドレベルのルールを使用してアクセス制御を処理できます。

リモートのGraphQLスキーマをマージして、統一されたGraphQL APIを提供することもできます。

![Hasura GraphQLエンジンアーキテクチャ](../assets/hasura-arch.svg)

## クライアント側ツール

HasuraはどのGraphQLクライアントでも動作します。[Apolloクライアント](https://github.com/apollographql/apollo-client)を使うことをおすすめします。[awesome-graphql](https://github.com/chentsulin/awesome-graphql)にクライアントのリストがあります。

## ビジネスロジックの追加

GraphQLエンジンは、バックエンドにカスタムビジネスロジックを追加するための、理解しやすくスケーラブルで高性能なメソッドを提供します。

### リモートスキーマ

HasuraのPostgresベースのGraphQLスキーマに加えて、リモートスキーマにカスタムリゾルバを追加します。支払いAPIの実装、データベースに無いデータの問い合わせなどのユースケースに最適です - [続きを読む](../remote-schemas.md)。

### データベースイベントによるWebhookの起動

データベースイベントから起動する非同期ビジネスロジックを追加します。
通知、Postgresからのデータパイプライン、または非同期に最適です - [続きを読む](../event-triggers.md)。

### 派生データまたはデータ変換

Postgresのデータを変換するか、Postgresでビジネスロジックを実行することでGraphQLエンジンを使用して問い合わせることができる別のデータセットを派生させる事ができます。[続きを読む](https://hasura.io/docs/latest/graphql/core/queries/derived-data.html).

## デモ

[コミュニティ・サンプル](../community/sample-apps) ディレクトリにあるサンプルアプリケーションを確認してください。

### リアルタイムアプリケーション

- Reactを使用して構築されたグループチャットアプリケーション。タイピングインジケーター、オンラインユーザー、新しいメッセージ通知が含まれます。
  - [試してみよう](https://realtime-chat.demo.hasura.app/)
  - [チュートリアル](../community/sample-apps/realtime-chat)
  - [APIの参照](https://realtime-chat.demo.hasura.app/console)

- 現在の場所を追跡できるアプリ。地図上を移動する乗り物の現在のGPS座標を表示します。
  - [試してみよう](https://realtime-location-tracking.demo.hasura.app/)
  - [チュートリアル](../community/sample-apps/realtime-location-tracking)
  - [APIの参照](https://realtime-location-tracking.demo.hasura.app/console)

- 常にに変化するデータを集約するリアルタイムダッシュボード。
  - [試してみよう](https://realtime-poll.demo.hasura.app/)
  - [チュートリアル](../community/sample-apps/realtime-poll)
  - [APIの参照](https://realtime-poll.demo.hasura.app/console)

### 動画

* [サーバーにホストされたGitlabのインスタンスにGraphQLを追加](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3分44秒*)
* [Auth0とGraphQLバックエンドに持つTodoアプリ](https://www.youtube.com/watch?v=15ITBYnccgc) (*4分00秒*)
* [GitLab上のGitLab Authと動作するGraphQL](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4分05秒*)
* [GPS位置情報の付いた1000万回の乗車のダッシュボード (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3分06秒*)

## サポート&トラブル対応

ドキュメントとコミュニティは、ほとんどの問題のトラブルシューティングに役立ちます。あなたがバグに遭遇したか、または私達と連絡をとる必要があるならば、あなたは以下の方法の1つを使ってコンタクトすることができます：

* サポート&フィードバック: [Discord](https://discord.gg/hasura)
* 問題&バグトラッキング: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* プロダクトアップデートをフォロー: [@HasuraHQ](https://twitter.com/hasurahq)
* 直接話す [ウェブサイトチャット](https://hasura.io)

私たちはこのコミュニティの中でオープンで心地よい環境を達成することを約束します。 [行動規範](../code-of-onduct.md)をご覧ください。

セキュリティに関する問題はこちらから報告をお願いします。[詳細を読む](../SECURITY.md).

## 開発への参加

[開発者へのガイド](../CONTRIBUTING.md) を参照してください。

## ブランド資産

Hasuraブランドの資産（ロゴ、Hasuraマスコット、バッジなどを使用）[ブランド資産](../assets/brand)フォルダにあります。自由に使用してください。
あなたが作ったアプリに"Powered by Hasura"を追加してもらえるととてもうれしいです！

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- 明るいバックグラウンド用 -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- 暗いバックグラウンド用 -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## ライセンス

GraphQLエンジンのコアは[Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0)（Apache-2.0）の下で提供されています。

全ての **その他のコンテンツ** ([`サーバー`](../server), [`クライアント`](../cli) と
[`コンソール`](../console) ディレクトリ以外) は [MITライセンス](../LICENSE-community)の下に提供されます。
これは [`ドキュメント`](../docs) と [`コミュニティ`](../community) ディレクトリに入っているもの全てを含みます。

## 翻訳

このreadmeは、次の翻訳で入手できます。

- [英語 :us:](../README.md)
- [フランス語 :fr:](README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [ボスニア語 :bosnia_herzegovina:](README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [ロシア語 :ru:](README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [ギリシャ語 🇬🇷](tREADME.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [スペイン語 🇲🇽](README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [インドネシア語 :indonesia:](README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [ブラジルポルトガル語 :brazil:](README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [ドイツ語 🇩🇪](README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [中国語 :cn:](README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [トルコ語 :tr:](README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [韓国語 :kr:](README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))

他の翻訳ファイルは[ここ](./)にあります。
