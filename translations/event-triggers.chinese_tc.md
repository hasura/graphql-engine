# Postgres上的事件觸發器

使用Hasura GraphQL引擎的事件觸發器在資料庫事件上觸發webhooks。

![Event triggers demo](../assets/event-triggers.gif)

## 重點

* **建立反應式和非同步應用程式/功能**: 觸發非同步無伺服器或雲端功能，以降低伺服器基礎結構成本並簡化開發者的 DevOps 工作。

* **原子和可靠**: 使用本機Postgres觸發器，資料庫上的每個相關操作都被視為事件。即使Hasura故障而關閉或正在更新，事件也會被捕獲並儘快交付，並保證*至少一次*。您甚至可以配置傳遞策略，如“max_retries”和“retry_interval”。

* **可擴展性**: 事件觸發系統是橫向可擴展的－如果你需要處理更多的事件，只需投入更多的資源！

* **適用於現有的即時資料庫**: 將其指向現有的Postgres資料庫，以立即監聽資料中的變更並呼叫webhooks。

* **可觀察性和監控準備就緒**: 產生的事件會自動附加一個事件 ID，而 Hasura 發出的結構化日誌可讓您輕鬆使用您喜歡的工具，在生產環境中運行基於事件驅動的後端。 ([觀看](https://youtu.be/WOPA52r3bzU) 使用概述 [Honeycomb](https://honeycomb.io/)).


## 快速入門:

### 在Heroku上一鍵部署

最簡單嘗試事件觸發器的方法是透過Heroku。

1. 點選以下按鈕，並使用免費的Postgres外掛程式在Heroku上部署GraphQL引擎:

 [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. 打開Hasura控制台

 造訪 `https://<app-name>.herokuapp.com` (*將 \<app-name\> 替換為您的應用程式名稱*) 開啟管理控制台。

3. 設定你的第一個事件觸發器和webhook

 建立表, 配置要用作觸發器的資料庫更新，並透過在表中新增行立即呼叫webhook。 遵循此教學 [簡單指南](https://hasura.io/docs/latest/graphql/core/getting-started/first-event-trigger.html).

### 其他部署方法

用於基於Docker的部署和高級配置選項， 訪問 [部署指南](https://hasura.io/docs/latest/graphql/core/getting-started/index.html).

### 示範 (*30 秒*)

[![Create an event-trigger and webhook in 60 seconds](https://img.youtube.com/vi/EaTUVWnDCvA/0.jpg)](https://www.youtube.com/watch?v=EaTUVWnDCvA)

[在60秒內建立事件觸發器和webhook](https://youtu.be/EaTUVWnDCvA)

### Serverless樣板程式碼

使用其中一個 [serverless觸發器樣板程式碼](community/boilerplates/event-triggers) 部署webhook
它可以捕獲資料庫事件。
無伺服器/雲端功能平台的樣板：

* [AWS Lambda](community/boilerplates/event-triggers/aws-lambda)
* [Google Cloud Functions](community/boilerplates/event-triggers/google-cloud-functions)
* [Azure Functions](community/boilerplates/event-triggers/azure-functions)
* [Zeit Now](community/boilerplates/event-triggers/zeit-now)

## 系統架構

![Event triggers architecture](.././assets/event-triggers-arch.png)

## 演示及教學: 建構響應式和非同步應用/功能

### 通知

根據資料庫事件觸發推播通知和電子郵件。嘗試下面的示範和教程，看看當用戶插入一些資料時，瀏覽器推播通知是如何觸發的：

* [觀看示範](https://www.youtube.com/watch?v=nuSHkzE2-zo)
* [嘗試運行](https://serverless-push.demo.hasura.io/)
* [教學](https://github.com/hasura/sample-apps/tree/main/serverless-push)


<!--
### 非同步業務邏輯

將複雜的、長時間運行的業務邏輯轉換為事件驅動、非同步和故障復原。嘗試下面的示範和教學課程，了解如何非同步執行影像處理作業以將影像轉換為黑白版本：

* [觀看示範](https://some-youtube-demo.com) (*10:00 分鐘*)
* [嘗試運行](https://some-link/)
* [教程](https://some-other-link)

-->

### 資料轉換 (ETL)

將資料轉換並載入到外部資料儲存中。請參閱下面的演示和教程，了解如何轉換Postgres資料以建立和填充Algolia索引：

* [觀看示範](https://youtu.be/kWVEBWdEVAA)
* [嘗試運行](https://serverless-etl.demo.hasura.io/)
* [教學](https://github.com/hasura/sample-apps/tree/main/serverless-etl)

### 使用即時GraphQL為非同步後端建立反應式使用者體驗

透過GraphQL訂閱和即時查詢，可以輕鬆地將事件驅動和非同步資訊傳播到UI客戶端。

![Reactive apps architecture](.././assets/reactive-apps-arch.png)

**觀看**: [建立具有非同步後端的響應式應用程式](https://youtu.be/kTSOxRrtCeI) (*04:15 分鐘*)

## 翻譯

本文檔有以下翻譯版本:

- [French :fr:](translations/event-triggers.french.md)