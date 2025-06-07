# 遠端模式（Remote Schemas）

Hasura 為您提供具有授權和存取控制的 CRUD + 即時 GraphQL API。然而，在許多情況下，您需要編寫包含自訂邏輯的 API（查詢、變更）。例如，實作支付 API，或查詢資料庫中不存在的資料。

Hasura 具備合併遠端 GraphQL 模式並提供統一 GraphQL API 的能力。可以將其視為自動化的模式合併。您只需建立自己的 GraphQL 服務，並將其 HTTP 端點提供給 Hasura。您的 GraphQL 服務可以使用任何程式語言或框架編寫。

遠端模式適用於下列場景：

* 自訂變更（*例如：在插入資料之前執行驗證*）
* 支援支付等功能，並提供一致的介面來存取它們，例如：即透過 GraphQL 引擎的 API 進行
* 從其他來源取得不同的資料（*例如：從天氣 API 或另一個資料庫*）

為了支援自訂業務邏輯，您需要建立一個自訂的 GraphQL 伺服器（請參閱 [boilerplates](community/boilerplates/remote-schemas)）並將其模式與 GraphQL 引擎的模式合併。

![遠端模式架構](../assets/remote-schemas-arch.png)

## 演示 (*40 秒*)

[![合併遠端模式的影片示範](https://img.youtube.com/vi/eY4n9aPsi0M/0.jpg)](https://www.youtube.com/watch?v=eY4n9aPsi0M)

[合併遠端 GraphQL 模式（YouTube 連結）](https://youtu.be/eY4n9aPsi0M)

[新增遠端模式](https://youtu.be/01t4t2t4q1c)

## 快速入門

嘗試遠端模式最快的方式是透過 Heroku。

1. 點擊以下按鈕，將 GraphQL 引擎與免費的 Postgres 外掛程式一起部署到 Heroku：

 [![部署到 Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. 打開 Hasura 控制台

 請造訪 `https://<app-name>.herokuapp.com`（*將 \<app-name\> 替換為您的應用程式名稱*）以開啟管理控制台。

3. 合併您的第一個遠端模式並查詢它

 在管理控制台中，開啟 ``Remote Schemas`` 標籤頁並點選 ``Add`` 按鈕。填寫以下資訊：
 * 遠端模式名稱：``countries`` (*此遠端模式的別名*)。 
* GraphQL 伺服器 URL：``https://countries.trevorblades.com/`` (*一個公共的 GraphQL API，我們將用它快速查看此功能；由 [@trevorblades](https://github.com/trevorblades) 維護*)。 
* 忽略其餘的設定設定並點選 ``Add Remote Schema`` 按鈕。 

前往 ``GraphQL`` 標籤頁並執行以下查詢 (*將其貼上到左側的查詢視窗並點擊* ▶️ *(播放) 按鈕*)：

 ```graphql
 {
 countries {
 emoji
 name
 languages {
 name
 native
 }
 }
 }
 ```

 您可以使用 ``GraphQL`` 介面右上角的 ``Docs`` 瀏覽器來探索遠端模式中的 GraphQL 類型。

## 模板

適用於流行語言/框架的自訂 GraphQL 伺服器的範本可用。

* [常規範本](community/boilerplates/graphql-servers)，可以部署到任何地方。
* [無伺服器範本](https://github.com/hasura/graphql-serverless)，可部署至 AWS Lambda 等無伺服器平台。

請注意，更多語言、框架、無伺服器平台等的模板正在不斷更新，社群貢獻非常歡迎。

## 注意事項

**當前的限制**:

* 命名規則：類型名稱和節點名稱在所有合併的模式中必須是唯一的（區分大小寫）。在接下來的幾個版本中，將支援合併名稱和結構相同的類型。
* 來自不同 GraphQL 伺服器的節點無法在同一個查詢/變更（query/mutation）中使用。所有頂層節點必須來自同一個 GraphQL 伺服器。
* 不支援遠端 GraphQL 伺服器的訂閱（subscription）。

這些限制將在後續版本中內解決

## 📄 文檔

閱讀完整[文件](https://hasura.io/docs/latest/graphql/core/remote-schemas/index.html)

## 翻譯

此文件提供以下翻譯版本：

- [French :fr:](translations/remote-schemas.french.md)
- [Hindi :india:](translations/remote-schemas.hindi.md)