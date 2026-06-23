# Hasura GraphQL引擎

[![文檔](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL 應用程式 **Postgres 應用程式 API**，回應資料庫事件的 [**Webhook觸發器**](../event-triggers.md)。

Hasura 已建立 GraphQL 的 Postgres 版本、新版本的 Postgres 和新版本的 GraphQL。

請參閱 [hasura.io](https://hasura.io) 和 [文件](https://hasura.io/docs) 以了解更多內容。

------------------

![Hasura GraphQL 引擎示範](../assets/demo.gif)

------------------

![Hasura GraphQL 引擎即時性示範](../assets/realtime.gif)

-------------------

## 特性

* **強大的查詢功能**：內建過濾，分頁，模式搜索，批量插入，更新，刪除
* **即時**：透過訂閱將任何 GraphQL 查詢轉換為即時查詢
* **繪製圖表**：透過單一GraphQL端點存取其他負責業務邏輯的GraphQL端點 ([了解更多](../remote-schemas.md))
* **Webhooks與Serverless函數**: 在發生Postgres插入/更新/刪除事件時觸發函數 ([了解更多](../event-triggers.md))
* **使用現有的資料庫**：關於 PostgresGraphQL API
* **細粒度的存取控制**：與您的認證系統整合的動態存取控制（例如auth0，firebase-auth）
* **效能與低記憶體佔用**：約15MB的docker映像；〜50MB RAM @ 1000請求/秒； 多核心友好
* **管理介面和模式遷移**: 管理介面和受Rails 模式的遷移
* **Postgres** ❤️: 支援Postgres資料類型（PostGIS / geo-location 等），將表格視圖轉換為*圖*，透過GraphQL變更觸發儲存函數或流程

請造訪 [hasura.io](https://hasura.io) 和 [文件](https://hasura.io/docs) 以了解更多

## 目錄
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**目錄**

- [快速開始](#快速開始)
- [一鍵部署到Heroku](#一鍵部署到Heroku)
- [其他配置方法](#其他配置方法)
- [架構](#架構)
- [客戶端工具](#客戶端工具)
- [新增業務邏輯](#新增業務邏輯)
- [端點Schema](#端點Schema)
- [透過資料庫事件觸發Webhooks](#透過資料庫事件觸發Webhooks)
- [演示](#演示)
- [即時應用](#即時應用)
- [影片](#影片)
- [支援與故障排除](#支援與故障排除)
- [貢獻](#貢獻)
- [品牌資料](#品牌資料)
- [許可證](#許可證)
- [翻譯](#翻譯)

<!-- markdown-toc end -->

##

### 一鍵到部署Heroku

部署到 Heroku 正在嘗試 Hasura 最快的方法

1.點擊以下按鈕，在有免費Postgres附加元件的Heroku上設定GraphQL引擎：

[![部署到Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2.打開Hasura控制台

造訪 `https://<app-name>.herokuapp.com` (*更換 \<應用程式名稱\> 使用您的應用程式名稱*) 開啟管理控制台。

3.進行第一個GraphQL查詢

###

以下一鍵式配置選項的說明：

| **基礎設施建設** | **一鍵式連結** | **附加資訊** |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DigitalOcean | [![部署到 DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasuraactionaction=deploy&refcode=c4d9092m"pignc [文件](https://hasura.io/docs/latest/graphql/core/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![部署到Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2f raw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [文檔](https://hasura.io/docs/latest/graphql/core/guides/deployment/azure-container-instances-postgres.html) |

### 其它部署方法

關於基於 Docker 的部署和其它高級配置選項，請參閱 [部署指南](https://hasura.io/docs/latest/graphql/core/getting-started/index.html) 或者  
[安裝清單](../install-manifests)。

## 架構

Hasura GraphQL 引擎位於 Postgres 資料庫實例的前端，並且可以接受來自客戶端應用程式的 GraphQL 調用。您可以將其設定為與現有的身份驗證系統一同使用，並可根據驗證系統中的動態變數來實現欄位級別的存取控制。

您還可以合併遠端的 GraphQL Schema，以提供統一的 GraphQL API。

![Hasura GraphQL 引擎架構](../assets/hasura-arch.svg)

## 客戶端工具

Hasura 可與任何 GraphQL 客戶端一起使用。我們建議使用 [Apollo Client](https://github.com/apollographql/apollo-client)。  
請參見 [awesome-graphql](https://github.com/chentsulin/awesome-graphql) 瞭解其他客戶端工具。

## 添加商業邏輯

GraphQL Engine 提供了易用、可擴展、高效能的方式將自定義業務邏輯插入後端。

### 遠端 Schema

您可以在遠端 Schema 中編寫自定義的 resolver，然後與 Hasura 基於 Postgres 的 GraphQL Schema 合併。適用於實現支付 API 或查詢資料庫中不存在的資料等場景 - [閱讀更多](../remote-schemas.md)。

### 透過資料庫事件觸發 Webhooks

新增基於資料庫事件觸發的非同步業務邏輯。  
非常適合通知、來源於 Postgres 的資料處理流程或非同步任務處理  
[閱讀更多](../event-triggers.md)。

### 衍生資料或資料處理

在 Postgres 中處理資料或套用業務邏輯來匯出另一套可由 GraphQL Engine 處理的資料集 - [閱讀更多](https://hasura.io/docs/latest/graphql/core/queries/derived-data.html)。

## 示範

請造訪 [hasura/sample-apps](https://github.com/hasura/sample-apps/tree/main) 目錄以查看所有示範專案

### 即時應用

- 使用 React 建構的群聊應用，包含輸入中指示器、線上用戶與新訊息通知
  - [試用連結](https://realtime-chat.demo.hasura.io/)
  - [教學說明](https://github.com/hasura/sample-apps/tree/main/realtime-chat)
  - [瀏覽 API](https://realtime-chat.demo.hasura.io/console)

- 即時位置追蹤應用，展示行駛中車輛持續變更的 GPS 座標並於地圖上移動
  - [試用連結](https://realtime-location-tracking.demo.hasura.io/)
  - [教學說明](https://github.com/hasura/sample-apps/tree/main/realtime-location-tracking)
  - [瀏覽 API](https://realtime-location-tracking.demo.hasura.io/console)

- 即時儀表板，用於聚合不斷變化的資料
  - [試用連結](https://realtime-poll.demo.hasura.io/)
  - [教學說明](https://github.com/hasura/sample-apps/tree/main/realtime-poll)
  - [瀏覽 API](https://realtime-poll.demo.hasura.io/console)

### 影片

* [為自託管 GitLab 實例新增 GraphQL](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 分鐘*)
* [基於 Auth0 與 GraphQL 後端的 Todo 應用](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 分鐘*)
* [整合 GitLab 身份驗證的 GraphQL 實作](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 分鐘*)
* [千萬次騎行位置資料的儀表板（PostGIS、Timescale）](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 分鐘*)


## 支援與故障排除

文件與社群將幫助您解決大多數問題。如果您遇到錯誤或需要聯繫我們，可以透過以下任一方式與我們聯繫：
* 支援與回饋：[Discord](https://discord.gg/vBPpJkS)
* 問題與錯誤追蹤：[GitHub issues](https://github.com/hasura/graphql-engine/issues)
* 關注產品更新：[HasuraHQ Twitter](https://twitter.com/hasurahq)
* 與我們對話：[網站即時聊天](https://hasura.io)

我們致力於在社群中營造開放且友善的環境。請參閱 [行為準則](../code-of-conduct.md)。

若您要回報安全性問題，請參考 [此文件](../SECURITY.md)。

## 貢獻

請參考我們的 [貢獻指南](../CONTRIBUTING.md) 以了解更多細節。

## 品牌素材

您可以在 [assets/brand](../assets/brand) 資料夾中找到 Hasura 的品牌素材（如 Logo、吉祥物、Powered-by 徽章等）。歡迎在您的應用程式/網站等地方使用它們！若能在您的 Hasura 應用中看到 "Powered by Hasura" 徽章，我們將感到非常開心。❤️

<div style="display: flex;">
  <img src="../assets/brand/hasura_logo_primary_darkbg.svg" width="150px"/>
  <img src="../assets/brand/hasura_logo_primary_lightbg.svg" width="150px"/>
</div>

```html
<!-- For light backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1711457032/main-web/hasura_logo_primary_lightbg_n0xhz8.svg" />
</a>

<!-- For dark backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1711457033/main-web/hasura_logo_primary_darkbg_nussjm.svg" />
</a>
```

## 許可證

核心 GraphQL 引擎使用 [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0)。

所有**其他內容**([`server`](../server), [`cli`](../cli) 和
[`console`](../console) 目錄) 使用 [MIT 許可證](../LICENSE-community).
其中包括 [`docs`](../docs) 和 [`community`](../community) 目錄。

## 翻譯

自述文件

- [日文:jp:](../translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [法文:fr:](../translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [中文:cn:](../translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))


可以在[這裡](../translations)找到其他文件的翻譯
