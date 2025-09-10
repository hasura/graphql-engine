# 為 Hasura GraphQL 引擎做出貢獻

第一：如果您對如何開始為V2或V3做出貢獻感到沒有把握，請隨時向我們諮詢
[Discord](https://discordapp.com/invite/hasura)在「#contrib」頻道。你也可以繼續你的
貢獻，我們會給你回饋。別擔心，最糟糕的情況是，你會被禮貌地要求
改變一些東西。我們感謝任何貢獻，我們不希望規則之牆阻礙這一點。

然而，對於那些想要更多關於如何為專案做出貢獻的指導的人，請繼續閱讀
這份文件將涵蓋我們正在尋找的內容。透過解決以下問題，我們可以快速合併或
你的貢獻將會增加。

## 目錄

[1. 行為準則 ](#code-of-conduct)

[2. git倉庫概述 ](#overview)

[3. 歡迎首次貢獻者！ ](#first-timers)

[4. 貢獻領域 ](#areas)

[5. 貢獻方式 ](#ways)

[6. git提交資訊 ](#commit-messages)

[7. 翻譯 ](#translations)

<a name="code-of-conduct"></a>

## 1. 行為準則

請關注我們的 [行為準則](code-of-conduct.chinese.md) 在對Hasura的任何貢獻的背景下。

<a name="overview"></a>

## 2. git倉庫概述

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) 是V2和V3的開源單倉庫Hasura版本。

### V2

這個V2部分由3個部分組成，每個部分都有自己的貢獻指南：

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](frontend/docs/generic-info.md#contributing-to-hasura-console)

這三個元件都有一個版本，由git標籤或分支名稱和git的組合表示 提交SHA。

對於所有貢獻，都需要簽署貢獻者許可協議 CLA（Contributor License Agreement）
[這裡](https://cla-assistant.io/hasura/graphql-engine)在提交拉取請求之前（或之後）。如有必要機器人將
透過PR註釋提示貢獻者簽署CLA。

### V3

V3部分完全是V3引擎，是Hasura的核心，用Rust編寫。

1. [V3 引擎 (Rust)](v3/CONTRIBUTING.md)

查看 [V3 README 在這](/v3/README.md).

<a name="first-timers"></a>

## 3. 歡迎首次貢獻者！

我們感謝首次貢獻者，我們很樂意幫助您開始。如有疑問，請聯絡我們！

您發現所有問題都適合首次貢獻者[這裡](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. 貢獻領域

當然，我們感謝對Hasura所有組成部分的貢獻。然而，我們已經確定了三個領域 特別適合開源貢獻。

### V2 文檔

我們的目標是保持文件的全面性和更新性。如果您願意幫助我們這樣做，我們將感謝任何形式的貢獻：

- 報告缺失內容

- 修復現有文件中的錯誤

- 幫助我們新增文檔

文件的貢獻指南可以在以下網址找到 [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

### V2 社群內容

自從我們推出[學習頁面](https://hasura.io/learn/)，我們對貢獻感到高興：

- 修復現有學習教程中的錯誤

-新增教學（如果您有避免重複單字的想法，請聯絡我們）

學習庫的README可以在[點擊此處](https://github.com/hasura/learn-graphql).

除了學習內容，我們還確定了三種其他方式來貢獻技術社群內容：

- [範本程式碼](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [範例應用程式](https://github.com/hasura/sample-apps/tree/main)

- [工具](community/tools)

如果你想透過以下方式為社群做出貢獻

- 寫一篇技術博客

- 在活動中發言

- 組織工作坊

查看我們的 [社群維基](https://github.com/hasura/graphql-engine/wiki/Community-Wiki).

如果你有什麼要添加的，即使它與上述任何內容都無關，也可以隨時提交拉取請求。

### V2 CLI

我們在CLI上遇到了一些適合開源貢獻的問題。如果你知道如何使用Go或你想透過學習使用，請查看以下內容
[問題](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

CLI 倉庫的 README 可以在[這裡](https://github.com/hasura/graphql-engine/tree/master/cli)找到。

<a name="ways"></a>

## 5. 貢獻方式

### 回報問題

- 確保您針對最新發布版本進行測試。我們可能已經修復了您遇到的 bug。

- 提供重現問題的步驟，包括 Postgres 版本、graphql-engine 版本以及您正在運行的提供者（如 Heroku、Docker 等）。

- 如果相關，請包含伺服器的日誌。

- 建立一個 [問題](https://github.com/hasura/graphql-engine/issues/new/choose).

### 處理一個問題 (issue)

- 我們使用[叉庫與分支的 Git 工作流程](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- 請確保您正在進行的工作有相關問題。

- 如果您正在處理某個問題，請評論您這樣做是為了防止其他人重複工作。

- 壓縮您的提交，並在提交訊息末尾使用 `fix #<issue-no>` 或 `close #<issue-no>` 引用問題。 
例如：`resolve answers to everything (fix #42)` 或`resolve answers to everything, fix #42`

- 在提交拉取請求之前，將主分支與您的分支進行變基。

<a name="commit-messages"></a>

## 6. git提交訊息

- 第一行應為更改的摘要，且不超過 50 個字符，後面可以跟著一個可選的正文，詳細說明更改內容。有關編寫良好提交信息的更多信息，請參考
 [此連結](https://github.com/erlang/otp/wiki/writing-good-commit-messages)

- 使用命令式現在式(imperative present tense): "add/fix/change", not "added/fixed/changed" nor "adds/fixes/changes".

- 不要將摘要行的第一個字母大寫。

- 不要在摘要行的末尾加上句號/點（.）。

<a name="translations"></a>

## 7. 翻譯

本文檔有以下翻譯版本：

- [French 🇫🇷](translations/CONTRIBUTING.french.md)

(致謝：部分章節改編自 https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
