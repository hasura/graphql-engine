# 借助 GraphQL 承載 100萬活動訂閱（即時查詢）

Hasura 是基於 Postgres資料庫的 GraphQL 引擎，提供可控制權限的開箱即用的 GraphQL API。請至 [hasura.io](https://hasura.io/) 和 [github.com/hasura/graphql-engine](https://github.com/hasura/graphql-engine) 了解更多。

Hasura 可為用戶端提供即時查詢（基於 GraphQL 訂閱）。例如，一個外送應用程式使用即時查詢顯示某特定使用者的訂單即時狀態。

本文檔描述 Hasura 的架構，闡述它是如何支撐百萬個並發即時查詢的。

##### 目錄
- [結論](#結論)
- [GraphQL 與訂閱](#GraphQL-與訂閱)
- [實作 GraphQL 即時查詢](#實作-GraphQL-即時查詢)
 - [再取得 GraphQL 查詢結果](#再取得-GraphQL-查詢結果)
 - [Hasura 的方法](#Hasura-的方法)
- [測試](#測試)
- [本方法的優勢](#本方法的優點)

## 結論

**設定：** 每個用戶端（Web 或 行動應用程式）以認證令牌登入並訂閱一個即時查詢結果。資料存放於 Postgres 資料庫。每秒鐘更新 Postgres 資料庫中的一百萬行數據，並確保推送一個新查詢結果到客戶端。 Hasura 是 GraphQL API 的提供者（包含授權）。

**測試：** Hasura 可以並發處理多少個客戶端的實作訂閱？ Hasura 是否可以縱向或橫向效能伸縮擴展？

<img src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/main-image-fs8.png" width="500px" />

![single-instance-results](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/images2/single-instance-fs8.png)

|單例配置| 活動即時查詢數量 | CPU 平均負載 |
|--------|----------------|--------------|
| 1xCPU, 2GB RAM | 5000 | 60% |
| 2xCPU, 4GB RAM | 10000 | 73% |
| 4xCPU, 8GB RAM | 20000 | 90% |

<img alt="results-horizo​​ntally-scaled" src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/horizo​​ntal-scalability.png" width="80%" />

當一百萬個即時查詢的時候，Postgres 的負載不超過 28%，連接數峰值在 850 左右。

關於配置的說明：
- AWS RDS postgres, Fargate, 使用 ELB 的預設配置，無任何微調
- RDS Postgres: 16xCPU, 64GB RAM, Postgres 11
- Hasura 運行在 Fargate (4xCPU, 8GB RAM per instance) ，採用預設配置

## GraphQL 與訂閱

GraphQL 讓應用程式開發者輕鬆地從 API 中精確地取得他們想要的資料。

例如，我們要創建一個外帶應用程式。 Postgres 的架構看起來像這樣：

<img src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/pg-schema.png" alt="postgres schema for food delivery app" width="700px" style="width: 60%;

應用程式介面會顯示目前使用者訂單的狀態，GraphQL 查詢會取得訂單最新狀態和配送員的定位。

<img alt="order-graphql-query" src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/images2/basic-query-fs8.png" width="700px" style="width: 70%;

在底層，查詢被當作字串傳送給伺服器，經解析、授權後，從資料庫中取得應用程式所需的資料。傳回的 JSON 資料結構與請求時的相同。

進入即時查詢：即時查詢的主意是訂閱特定查詢的最新結果。一旦底層的資料改變，伺服器應該推送最新結果到客戶端。

乍一看，這完美符合 GraphQL 的使用場景，因為 GraphQL 用戶端支援處理大量 websocket 連線。用 subscription 取代 query 就能轉換查詢到即時查詢。就是這麼簡單，如果 GraphQL 伺服器可以實現的話。

<img alt="order-subscription-query" src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/images2/subscription-query-fs8.png" width="700px" style="width: 70%;

## 實作 GraphQL 即時查詢

實現即時查詢是個痛苦的過程。當你的資料庫查詢包含授權規則，那麼當變更事件發生時，就會增加查詢結​​果的計算量。這在 web服務上是一個挑戰。像 Postgres 這樣的資料庫，這相當於在底層表更改時，要保持物化視圖最新一樣困難困難。另一種方法是為特定查詢重新取得全部資料（針對特定授權規則的用戶端）。這是我們目前採取的方法。

其次，建立一個 web服務以一種可伸縮的方式來處理 websockets 有時也有點麻煩，但是某些框架和語言確實使並發程式設計更容易處理。

### 再取得 GraphQL 查詢結果

為了理解為什麼這裡要使用再取得(refetching) GraphQL 查詢不好，我們來看一個典型的 GraphQL 查詢過程：

<img alt="graphql-resolvers" src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/resolvers-fs8.png" width="700px" style="width: 70%; min-000%;

在 GraphQL 查詢中，授權 + 資料取得邏輯必須為每個節點執行一次。這很可怕，因為即使稍大一點的查詢都會輕易拖垮資料庫。運用 ORM 不當時，N+1 問題也是個常見問題，這對資料庫效能不好，且難以最佳化 Postgres 查詢。 Data loader 類型模式可以緩解該問題，但在底層仍然會多次查詢 Postgres 資料庫（減少為 GraphQL 查詢中有多少個節點）。

在即時查詢中，該問題更為嚴重，因為每個客戶端的查詢都有一個獨一無二的再獲取。儘管查詢是相同的，考慮到授權規則創建不同的會話變量，每個客戶端需要單獨的再獲取。

### Hasura 的方法

如何做得更好？如果申明式映射資料模型到 GraphQL 模式，並使用它建立單一 SQL 資料庫查詢？這樣就能避免多次搗鼓資料庫，無論返回中是否有大量項目或 GraphQL 查詢中有很多節點。

#### 主意 #1：編譯 一個 GraphQL 查詢為單一 SQL 查詢

Hasura 部分功能是作為轉譯器，它使用資料模型到 GraphQL 模式的映射資訊的元數據，編譯 GraphQL 查詢為 SQL 查詢，來從資料庫取得資料。

GraphQL 查詢 GraphQL 抽象語法樹（AST） SQL 抽象語法樹 SQL

![graphql-to-sql](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/graphql-2-sql-fs8.png)

這消除了 N+1 查詢問題，並允許資料庫優化資料擷取。

但這不夠，因為解析器（resolver）也應用了授權來強制它只取得權限範圍內的資料。因此我們需要將授權規則嵌入到產生的 SQL 中。

#### 主意 #2：使授權具有宣告性

存取資料時的授權本質上是一種約束，它取決於所取得的資料(或行)的值，以及動態提供的應用程式特定使用者的會話變數。例如最普通的，一行資料包含表示資料所有權的欄位 `user_id`。或有個關聯表 `document_viewers` 來表示使用者可查看哪些文件。在其他場景中，會話變數本身可能包含與行相關的資料所有權信息，例如，帳戶管理員可以存取任何帳戶 [1,2,3]，其中該資訊不存在於當前資料庫中，而是存在於會話變數（可能由其他資料系統提供）。

為了對此建模，我們在 API 層實作了類似於 Postgres RLS 的授權層，以提供用於配置存取控制的聲明性框架。如果您熟悉 RLS，可以類比 SQL查詢中的目前會話變數為來自 cookie、JWTs 或 HTTP頭的 HTTP會話變數。

順便說一句，因為我們在許多年前就開始了 Hasura 工程，所以我們在 Postgres RLS特性加入 Postgres 之前就在應用層實現了這個特性。我們甚至在的 insert 回傳子句中也有與已修復的 Postgres RLS 相同的 bug https://www.postgresql.org/about/news/1614/。

因為在 Hasura 應用層面實現了授權（而不是使用 RLS，傳遞使用者詳情給 Postgres 連接的當前會話），這帶來一個顯著優勢，我們等下會看到。

總而言之，既然授權是聲明性的，並且可以在表、視圖甚至函數（如果函數傳回 SETOF）中使用，那麼就可以建立具有授權規則的單一 SQL查詢。

GraphQL 查詢 GraphQL 抽象語法樹 包含授權規則的內部抽象語法樹 SQL 抽象語法樹 SQL

![graphql-to-sql-with-authorization](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/graphql-2-sql-auth-fs8.png)

#### 主意 #3：在單一 SQL 查詢中批次處理多個即時查詢

僅憑主意 [#1](#主意-1編譯-一個-graphql-查詢為單一-sql-查詢)、[#2](#主意-2使授權具有聲明性) 的實現，我們仍會導致這樣的情況：連接了 100k 個客戶端可能會導致相稱的 100k 個 Postgres 來獲取最新資料（假如 100k 對應一個客戶端更新）對應一個客戶端更新。
然而，考慮到我們在 API層有所有應用程式使用者層級會話變數可用，我們實際上可以建立單一 SQL查詢來一次為許多客戶端再次取得資料！

假設有客戶端運行一個訂閱，以獲取最新的訂單狀態和配送員位置。我們可以在查詢中建立一個關係，其中包含所有作為不同行的查詢變數（訂單id）和會話變數（使用者id）。然後join查詢以獲取具有此關係的實際數據，以確保在單一回應中獲取多個客戶端的最新數據。這將允許同時為多個用戶獲取最新的結果，即使它們提供的參數和會話變數是完全動態的，並且僅在查詢時可用。

![graphql-to-sql-multiplexed](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/multiplexed-fs8.png)

#### 何時執行再獲取（refetch）？

我們嘗試了多種方法，取得底層資料庫更新事件時來再取得查詢。

1. 監聽、通知：需要用觸發器偵測所有表，在消費端（web伺服器）重啟或網路中斷的情況下，被消費事件可能會被丟棄。
2. WAL（譯註：Write-ahead logging，預寫式日誌）：Reliable stream，但是 LR 插槽使得橫向效能擴展非常困難，並且在託管資料庫供應商上通常不可用。繁重的寫入負載會污染 WAL，並且需要在應用層進行節流。

在這些嘗試後，我們目前回退到基於時間間隔的輪詢來再取得查詢。因此，我們不是在有適當事件時再獲取，而是根據一個時間間隔再獲取查詢。這樣做有兩個主要原因：
1. 當即時查詢中使用的宣告權限和條件很簡單時，在某種程度上可以將資料庫事件對應到特定客戶端的動態查詢上（例如 `order_id` = 1 and `user_id` = cookie.`session_id`），但是對於複雜的查詢會變得棘手（例如 `'status' ILIKE 'failed_%'）查詢。聲明性權限有時也可以跨表使用。我們在研究這種方法以及基本的增量更新方面投入了大量的精力，並且在生產中有[幾個小項目](https://www.postgresql.eu/events/pgconfeu2018/sessions/session/2195/slides/144/Implementing%20Incremental%20View%20MMtenance/144/Implementing%20Incremental%20View%20MMtenancePMost%20.pdfPMost%20Most%Post的方法採用了類似的方法。
2. 對於任何應用程序，除非寫入吞吐量很小，否則無論如何，您最終都會在一個時間間隔內對事件進行節流/防抖（throttling/debouncing）。

這種方法的缺點是寫入負載很小時有延遲。再獲取可以立即完成，而不是在幾毫秒後。透過適當地調整再獲取間隔和批量大小，可以輕鬆緩解這個問題。到目前為止，我們首先關注的是消除開銷最大的瓶頸，也就是再取得查詢。也就是說，我們將在接下來的幾個月中繼續專注於改進，特別是使用事件依賴（在適用的情況下），來潛在地減少即時查詢中每隔一段時間再獲取的數量。

請注意，我們在內部有其他基於事件的方法的驅動程序，如果你有一個用例，目前的方法不能滿足你的要求，我們願意與你合作與提供協助！

## 測試

測試基於 Websocket 的即時查詢的效能擴展性與可靠性是一項挑戰。我們花了幾週來建立測試套件和基礎自動化工具。設定如下所示：

1. 一個 nodejs腳本，它運行大量的 GraphQL即時查詢客戶端，並在記憶體中記錄事件，這些事件隨後被儲存到資料庫中。 [\[github\]](https://github.com/hasura/subscription-benchmark)
2. 一個在資料庫上建立寫入負載的腳本，從而導致所有執行即時查詢的客戶端之間發生變更（每秒更新一百萬行）。
3. 一旦測試套件完成運行，驗證腳本就會在資料庫中運行，在該資料庫中提取日誌/事件以驗證沒有錯誤並且所有事件已接收到。
4. 僅在以下情況下才認為測試有效：
 - 收到的有效載荷錯誤數為 0
 - 從建立事件到客戶端接收的平均延遲小於 1000毫秒

<img alt="testing-architecture" src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/subscriptions-images/test-architecture.png" width="700px" />

## 本方法的優勢

Hasura 讓即時查詢觸手可及。查詢的概念很容易擴展到即時查詢，而不需要使用 GraphQL 的開發人員做任何額外工作。這對我們來說是最重要的。

1. 功能特性豐富的即時查詢，全面支援 Postgres operators/aggregations/views/functions 等
2. 性能可估
3. 效能可縱向與橫向伸縮擴展
4. 可運行於所有雲端、資料庫供應商平台

## 未來展望

減少 Postgres 負載，通過：

1. 映射事件到即時查詢
2. 結果集的增量計算