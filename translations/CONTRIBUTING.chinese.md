# 對Hasura graphql-engine作貢獻

首先，如果你對開始貢獻感到不安，請隨時在我們的[Discord頻道](https://discordapp.com/invite/hasura)上的 #contrib 頻道詢問。你也可以直接做出貢獻，而我們會給你評論。不要擔心：在最壞的情況中，你只會收到客氣地要求改變某些東西。我們感謝任何貢獻，不希望把規則造成一堵阻礙貢獻的牆。

然而，對於想要更多關於為項目做出貢獻的最佳方式的指導的人，請繼續閱讀。這份檔案將介紹我們預期的東西。通過處理一下幾點，我們快速合併或處理你的貢獻的機會將新增。

## 目錄

[1. 行為準則](#code-of-conduct)

[2. 倉儲概述](#overview)

[3. 歡迎首次的貢獻者！](#first-timers)

[4. 貢獻範圍](#areas)

[5. 貢獻方式](#ways)

[6. 提交內容](#commit-messages)

[7. 翻譯](#translations)

<a name="code-of-conduct"></a>

## 1. 行為準則

請遵守我們的[行為準則](../code-of-conduct.md)對Hasura作出任何貢獻。

<a name="overview"></a>

## 2. 倉儲改授

[hasura/graphql-engine](https://github.com/hasura/graphql-engine)是一個單獨的倉儲，分成3個組件。每個倉儲都有自己的貢獻指南：

1. [服務器 (Haskell)](../server/CONTRIBUTING.md)

2. [CLI (Go)](../cli/CONTRIBUTING.md)

3. [控制台（JavaScript)](../console/README.md#contributing-to-hasura-console)

這三個組件都有一個版本，由Git標記或分支名稱和Git提交SHA的組合能看得出。

對於所有貢獻，你要在提交請求之前（或之後）在[這裏](https://cla-assistant.io/hasura/graphql-engine)簽署CLA（貢獻者授權合約）。如果有必要，一個bot（機器人）將通過提交請求注釋提示參與者簽署CLA。

<a name="first-timers"></a>

## 3. 歡迎首次的貢獻者！

我們感謝首度貢獻的新手，很樂意幫助你開始。如果有問題，請聯繫我們！

你可以[這裡](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22)找到適合首次貢獻者的所有問題。

<a name="areas"></a>

## 4. 貢獻範圍

我們當然感謝對Hasura所有組成部分的貢獻。然而，我們已經確定了三個特別適合開源貢獻的範圍。

### 檔案

我們的目標是保持全面和更新的檔案全面和更新。如果您願意幫助我們，我們非常感謝任何貢獻，包括：

- 報告缺失內容

- 修復現有檔案中的錯誤

- 幫助我們添加檔案

你可在 [docs/CONTRIBUTING.md](../docs/CONTRIBUTING.md) 找到檔案貢獻指南。

### 社區內容

自從我們推出了[“學習”頁面](https://hasura.io/learn/)後，我們對這些貢獻感到高興：

- 修復現有學習教程中的錯誤

- 添加新教程（如果你有避免重複的單詞的想法，請聯繫我們）

學習庫的README檔案可以在[這裡](https://github.com/hasura/learn-graphql)找到。

除了學習內容，我們也確定了三種額外方式來對科技社區內容作出貢獻：

- [模範（Boilerplates）](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [標本應用](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [工具](community/tools)

如果你想通過

- 撰寫科技博文

- 在活動中發言

- 組織研討會

來做出貢獻，請查看我們的[社區維基](https://github.com/hasura/graphql-engine/wiki/Community-Wiki)。

如果你有什麼要添加的，即使與上面提到的任何內容無關，也可以隨意提交一個 pull request（拉取請求）。

### Hasura CLI

我們在CLI上有一些適合開源貢獻的問題。如果你會Go或者你想通過實踐來學習Go，請看下面的[問題](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22)。

CLI倉儲的README檔案可以在[這裡](https://github.com/hasura/graphql-engine/tree/master/cli)找到。

<a name="ways"></a>

## 5. 貢獻方式

### 報告問題

- 請確保根據最新發佈的版本進行測試。我們有可能已經修復了你遇到的錯誤。

- 提供重現問題的步驟，包括Postgres的版本、graphql-engine的版本和正在運行的提供程式（Heroku、Docker等）。

- 如果相關，請包括服務器的日誌。

### 處理一個問題

- 我們使用 [fork-and-branch 的 Git 工作流](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- 請確保你所做的工作與存在的問題有關。

- 如果你正在處理一個問題，請以點評通知大家，防止別人作出重複的項目。

- 請壓縮提交並在提交內容的末尾使用 `fix #<issue-no>` 或 `close #<issue-no>` 來提出相關的問題。例如：`resolve answers to everything (fix #42)` 或 `resolve answers to everything, fix #42`

- 在提交拉取請求之前，將主服務器與您的分支重新建立基。

<a name="commit-messages"></a>

## 6. 提交內容

- 第一行應該簡介你的修改，不超過50字。接著，你可加上可選的正文，其中包含有關更改的詳細資訊。請參閱[此連結](https://github.com/erlang/otp/wiki/writing-good-commit-messages)來收集有關編寫良好提交消息的更多資訊。

- 運用祈使現在時語氣（imperative present tense）："add/fix/change", 不是 "added/fixed/changed" 或者 "adds/fixes/changes".

- 摘要行的第一個字母不要大寫。

- 不要在摘要行的末尾添加句點。

<a name="translations"></a>

## 7. 翻譯

本檔案有以下翻譯版本：

- [英語 🇬🇧](../CONTRIBUTING.md)
- [法語 🇫🇷](./CONTRIBUTING.french.md)

(榮譽：有些部分章節改編自https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
