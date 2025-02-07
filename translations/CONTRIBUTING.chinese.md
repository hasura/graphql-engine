# 为 Hasura GraphQL 引擎做出贡献

第一：如果您对如何开始为V2或V3做出贡献感到没有把握，请随时向我们咨询
[Discord](https://discordapp.com/invite/hasura)在“#contrib”频道中。你也可以继续你的
贡献，我们会给你反馈。别担心，最糟糕的情况是，你会被礼貌地要求
改变一些东西。我们感谢任何贡献，我们不希望规则之墙阻碍这一点。

然而，对于那些想要更多关于如何为项目做出贡献的指导的人，请继续阅读
这份文件将涵盖我们正在寻找的内容。通过解决以下问题，我们可以快速合并或
你的贡献将会增加。

## 目录

[1. 行为准则 ](#code-of-conduct)

[2. git仓库概述 ](#overview)

[3. 欢迎首次贡献者！ ](#first-timers)

[4. 贡献领域 ](#areas)

[5. 贡献方式 ](#ways)

[6. git提交信息 ](#commit-messages)

[7. 翻译 ](#translations)

<a name="code-of-conduct"></a>

## 1. 行为准则

请关注我们的 [行为准则](code-of-conduct.chinese.md) 在对Hasura的任何贡献的背景下。

<a name="overview"></a>

## 2. git仓库概述

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) 是V2和V3的开源单仓库Hasura版本。

### V2

这个V2部分由3个部分组成，每个部分都有自己的贡献指南：

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](frontend/docs/generic-info.md#contributing-to-hasura-console)

这三个组件都有一个版本，由git标签或分支名称和git的组合表示 提交SHA。

对于所有贡献，都需要签署贡献者许可协议 CLA（Contributor License Agreement）
[这里](https://cla-assistant.io/hasura/graphql-engine)在提交拉取请求之前（或之后）。如有必要机器人将
通过PR注释提示贡献者签署CLA。

### V3

V3部分完全是V3引擎，是Hasura的核心，用Rust编写。

1. [V3 引擎 (Rust)](v3/CONTRIBUTING.md)

查看 [V3 README 在这](/v3/README.md).

<a name="first-timers"></a>

## 3. 欢迎首次贡献者！

我们感谢首次贡献者，我们很乐意帮助您开始。如有疑问，請联系我们！

您发现所有问题都适合首次贡献者[这里](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. 贡献领域

当然，我们感谢对Hasura所有组成部分的贡献。然而，我们已经确定了三个领域 特别适合开源贡献。

### V2 文档

我们的目标是保持文档的全面性和更新性。如果您愿意帮助我们这样做，我们将感謝任何形式的贡献：

- 报告缺失内容

- 修复现有文档中的错误

- 帮助我们添加文档

文档的贡献指南可以在以下网址找到 [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

### V2 社区内容

自从我们推出[学习页面](https://hasura.io/learn/)，我们对贡献感到高兴：

- 修复现有学习教程中的错误

-添加新教程（如果您有避免重复单词的想法，请联系我们）

学习库的README可以在[单击此处](https://github.com/hasura/learn-graphql).

除了学习内容，我们还确定了三种其他方式来贡献技术社区内容：

- [模板代码](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [示例应用程序](https://github.com/hasura/sample-apps/tree/main)

- [工具](community/tools)

如果你想通过以下方式为社区做出贡献

- 写一篇技术博客

- 在活动中发言

- 组织工作坊

查看我们的 [社区维基](https://github.com/hasura/graphql-engine/wiki/Community-Wiki).

如果你有什么要添加的，即使它与上述任何内容都无关，也可以随时提交拉取请求。

### V2 CLI

我们在CLI上遇到了一些适合开源贡献的问题。如果你知道如何使用Go或者你想通过学习使用，查看以下内容
[问题](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

CLI 仓库的 README 可以在[这里](https://github.com/hasura/graphql-engine/tree/master/cli)找到。

<a name="ways"></a>

## 5. 贡献方式

### 报告问题

- 确保您针对最新发布版本进行测试。我们可能已经修复了您遇到的 bug。

- 提供重现问题的步骤，包括 Postgres 版本、graphql-engine 版本以及您正在运行的提供商（如 Heroku、Docker 等）。

- 如果相关，请包括服务器的日志。

- 创建一个 [问题](https://github.com/hasura/graphql-engine/issues/new/choose).

### 处理一个问题 (issue)

- 我们使用[叉库与分支的 Git 工作流](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- 请确保您正在进行的工作存在相关问题。

- 如果您正在处理某个问题，请评论说您这样做是为了防止其他人重复工作。

- 压缩您的提交，并在提交消息末尾使用 `fix #<issue-no>` 或 `close #<issue-no>` 引用问题。
  例如：`resolve answers to everything (fix #42)` 或`resolve answers to everything, fix #42`

- 在提交拉取请求之前，将主分支与您的分支进行变基。

<a name="commit-messages"></a>

## 6. git提交信息

- 第一行应为更改的摘要，且不超过 50 个字符，后面可以跟着一个可选的正文，详细说明更改内容。有关编写良好提交信息的更多信息，请参考
  [这链接](https://github.com/erlang/otp/wiki/writing-good-commit-messages)

- 使用命令式现在时式(imperative present tense): "add/fix/change", not "added/fixed/changed" nor "adds/fixes/changes".

- 不要将摘要行的第一个字母大写。

- 不要在摘要行的末尾添加句号/点（.）。

<a name="translations"></a>

## 7. 翻译

本文档有以下翻译版本：

- [French 🇫🇷](translations/CONTRIBUTING.french.md)

(致谢：部分章节改编自 https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
