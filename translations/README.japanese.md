# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL エンジンは **Postgres 上で瞬時にリアルタイムの GraphQL API** を[**ウェブフックトリガー**](../event-triggers.md)をデータベースイベントで起動し、ビジネスロジック用の[**リモートスキーマ**](../remote-schemas.md)を提供する、非常に高速な GraphQL サーバーです。

Hasura は、Postgres をバックエンドに持つ GraphQL アプリケーションを構築したり、Postgres を使用して既存のアプリケーションを GraphQL に段階的に移行したりするのに役立ちます。

こちらと [hasura.io](https://hasura.io) こちらから [docs](https://hasura.io/docs)　詳細を読んでください。

---

![Hasura GraphQL エンジンデモ](../assets/demo.gif)

---

![Hasura GraphQL エンジンリアルタイムデモ](../assets/realtime.gif)

---

## 機能一覧

- **強力なクエリ**: 組み込みフィルタリング、ページ付け、パターン検索、一括挿入、更新、削除などの mutations
- **リアルタイム**: subscriptions 使って様々な GraphQL クエリをライブクエリに変換
- **リモートスキーマのマージ**: 単一の GraphQL エンジンエンドポイントを介してビジネスロジック用のカスタム GraphQL スキーマにアクセスカスタムの GraphQL スキーマにアクセス [**続きを読む**](../remote-schemas.md).
- **ウェブフックやサーバーレス機能を起動する**: Postgres の insert / update / delete イベント時 ([続きを読む](../event-triggers.md))
- **既存のライブデータベースと連携**: 既存の Postgres データベースから、すぐに使える GraphQL API が即座に作られます
- **きめ細かいアクセス制御**: 認証システムと統合する動的アクセス制御（例：auth0、firebase-auth）
- **管理 UI と移行**: 最大 15MB のドッカーイメージ。最大 50MB RAM @ 1000 req/s。マルチコア対応
- **Postgres** ❤️: Postgres の型（PostGIS/地理位置など）をサポートし、ビューをグラフに変え、保存された関数や手順を変更した手続きを起動

こちらと [hasura.io](https://hasura.io) こちらから [docs](https://hasura.io/docs)詳細を読んでください。

## 目次

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**目次**

- [クイックスタート:](#quickstart)
  - [Heroku へのワンクリックでのデプロイ](#one-click-deployment-on-heroku)
  - [その他のデプロイ方法](#other-deployment-methods)
- [アーキテクチャ](#architecture)
- [クライアント側ツール](#client-side-tooling)
- [ビジネスロジックの追加](#add-business-logic)
  - [リモートスキーマー](#remote-schemas)
  - [データベースイベントによるウェブフックの起動](#trigger-webhooks-on-database-events)
- [デモ](#demos)
  - [リアルタイムアプリケーション](#realtime-applications)
  - [動画](#videos)
- [サポート&トラブル対応](#support--troubleshooting)
- [開発への参加](#contributing)
- [ブランド資産](#brand-assets)
- [ライセンス](#license)

<!-- markdown-toc end -->

## クイックスタート:

### Heroku へのワンクリックでのデプロイ

Hasura を試す最も早い方法は Heroku を使うことです。

1. 無料の Postgres アドオン経由で Heroku に GraphQL エンジンをデプロイするには、次のボタンをクリックしてください:

   [![Herokuへのデプロイ](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Hasura コンソールを開きます。

   こちらのリンクから `https://<app-name>.herokuapp.com` (_\<app-name\>を自分のアプリ名で置き換えます_) 管理コンソールにアクセスしてください。

3. 最初の GraphQL のクエリを発行

   テーブルを作成して、最初のクエリを実行します。こちらの [シンプルなガイド](https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html)を参考にしてください。

### 他のワンクリックでデプロイする方法

以下のワンクリックデプロイの方法もチェックしてみてください:

| **インフラ業者** |                                                                                                                        **ワンクリックリンク**                                                                                                                         |                                                                        **追加情報**                                                                        |
| :--------------: | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | :--------------------------------------------------------------------------------------------------------------------------------------------------------: |
|   DigitalOcean   |                  [![DigitalOceanへのデプロイ](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme)                   | [ドキュメント](https://hasura.io/docs/latest/graphql/core/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
|      Azure       | [![Azureへのデプロイ](https://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) |                    [ドキュメント](https://hasura.io/docs/latest/graphql/core/guides/deployment/azure-container-instances-postgres.html)                    |

### 他のデプロイ方法

Docker でのデプロイと高度なカスタマイズオプションについては [デプロイガイド](https://hasura.io/docs/latest/graphql/core/getting-started/index.html) か
[インストールマニフェスト](../install-manifests)を参考にしてください。

## アーキテクチャ

Hasura GraphQL エンジンは Postgres データベースインスタンスの前で動作し、クライアントアプリケーションから GraphQL のリクエストを受け取ることができます。既存の認証システムと連携するように設定でき、認証システムからの動的変数を含むフィールドレベルのルールを使用してアクセス制御を処理できます。

リモートの GraphQL スキーマをマージして、統一された GraphQL API を提供することもできます。

![Hasura GraphQLエンジンアーキテクチャ](../assets/hasura-arch.svg)

## クライアント側ツール

Hasura はどの GraphQL クライアントでも動作します。[Apollo クライアント](https://github.com/apollographql/apollo-client)を使うのをおすすめします。. [awesome-graphql](https://github.com/chentsulin/awesome-graphql)にクライアントのリストがあります。

## ビジネスロジックの追加

GraphQL エンジンは、バックエンドにカスタムビジネスロジックを追加するための、理解しやすくスケーラブルで高性能なメソッドを提供します。

### リモートスキーマ

Hasura の Postgres ベースの GraphQL スキーマに加えて、リモートスキーマにカスタムリゾルバを追加します。支払い API の実装、データベースに無いデータの問い合わせなどのユースケースに最適です - [続きを読む](../remote-schemas.md)。

### データベースイベントによるウェブフックの起動

データベースイベントから起動する非同期ビジネスロジックを追加します。
通知、Postgres からのデータパイプライン、または非同期に最適です - [続きを読む](../event-triggers.md)。

### 派生データまたはデータ変換

PostgreSQL のデータを変換するか、あるいはその上でビジネスロジックを実行することで GraphQL エンジンを使用して問い合わせることができる別のデータセットを派生させる事ができます。[続きを読む](https://hasura.io/docs/latest/graphql/core/queries/derived-data.html).

## デモ

[コミュニティ・サンプル](https://github.com/hasura/sample-apps/tree/main) ディレクトリにあるサンプルアプリケーションをチェックできます。

### リアルタイムアプリケーション

- React を使用して構築されたグループチャットアプリケーション。タイピングインジケーター、オンラインユーザー、新しいメッセージ通知が含まれます。

  - [試してみよう](https://realtime-chat.demo.hasura.io/)
  - [チュートリアル](https://github.com/hasura/sample-apps/tree/main/realtime-chat)
  - [API の参照](https://realtime-chat.demo.hasura.io/console)

- 現在の場所を追跡できるアプリ。地図上を移動する乗り物の現在の GPS 座標を表示します。

  - [試してみよう](https://realtime-location-tracking.demo.hasura.io/)
  - [チュートリアル](https://github.com/hasura/sample-apps/tree/main/realtime-location-tracking)
  - [API の参照](https://realtime-location-tracking.demo.hasura.io/console)

- 常にに変化するデータを集約するリアルタイムダッシュボード。
  - [試してみよう](https://realtime-poll.demo.hasura.io/)
  - [チュートリアル](https://github.com/hasura/sample-apps/tree/main/realtime-poll)
  - [API の参照](https://realtime-poll.demo.hasura.io/console)

### 動画

- [サーバーにホストされた Gitlab のインスタンスに GraphQL を追加](https://www.youtube.com/watch?v=a2AhxKqd82Q) (_3 分 44 秒_)
- [Auth0 と GraphQL バックエンドに持つ Todo アプリ](https://www.youtube.com/watch?v=15ITBYnccgc) (_4 分 00 秒_)
- [GitLab 上の GitLab Auth と動作する GraphQL](https://www.youtube.com/watch?v=m1ChRhRLq7o) (_4 分 05 秒_)
- [GPS 位置情報の付いた 1000 万回の乗車のダッシュボード (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (_3 分 06 秒_)

## サポート&トラブルシューティング

ドキュメントとコミュニティは、ほとんどの問題のトラブルシューティングに役立ちます。あなたがバグに遭遇したか、または私達と連絡をとる必要があるならば、あなたは以下の方法の 1 つを使ってコンタクトすることができます：

- サポート&フィードバック: [Discord](https://discord.gg/vBPpJkS)
- 問題&バグトラッキング: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
- プロダクトアップデートをフォロー: [@HasuraHQ](https://twitter.com/hasurahq)
- 直接話す [ウェブサイトチャット](https://hasura.io)

私たちはこのコミュニティの中でオープンで心地よい環境を達成することを約束します。 [行動規範](../code-of-onduct.md)をご覧ください。

セキュリティに関する問題はこちらから報告をお願いします。[詳細を読む](../SECURITY.md).

## 開発への参加

[開発者へのガイド](../CONTRIBUTING.md) を参照してください。

## ブランド資産

Hasura ブランドの資産（ロゴ、Hasura マスコット、バッジなどを使用）[ブランド資産](../assets/brand)フォルダにあります。自由に使用してください。
あなたが作ったアプリに"Powered by Hasura"を追加してもらえるととてもうれしいです！

<div style="display: flex;">
  <img src="../assets/brand/hasura_logo_primary_lightbg.svg" width="150px"/>
  <img src="../assets/brand/hasura_logo_primary_darkbg.svg" width="150px"/>
</div>

```html
<!-- 明るいバックグラウンド用 -->
<a href="https://hasura.io">
  <img
    width="150px"
    src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1711457032/main-web/hasura_logo_primary_lightbg_n0xhz8.svg"
  />
</a>

<!-- 暗いバックグラウンド用 -->
<a href="https://hasura.io">
  <img
    width="150px"
    src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1711457033/main-web/hasura_logo_primary_darkbg_nussjm.svg"
  />
</a>
```

## ライセンス

GraphQL エンジンのコアは[Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0)（Apache-2.0）の下で提供されています。

全ての **その他のコンテンツ** ([`サーバー`](../server), [`クライアント`](../cli) と
[`コンソール`](../console) ディレクトリ以外) は [MIT ライセンス](../LICENSE-community)の下に提供されます。
これは [`ドキュメント`](../docs) と [`コミュニティ`](../community) ディレクトリに入っているもの全てを含みます。
