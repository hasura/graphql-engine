# Hasura GraphQL 引擎

[![文档](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)

<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL 引擎是一个高性能的 GraphQL 服务器，可为您提供 **Postgres 上开箱即用的实时 GraphQL API**， 响应数据库事件的 [**Webhook 触发器**](../event-triggers.md)，以及用于业务逻辑处理的 [**远端 Schema**](../remote-schemas.md)。

Hasura 可帮助您构建基于 Postgres 的 GraphQL 应用程序，或将使用 Postgres 的现有应用迁移到 GraphQL 上。

请参阅 [hasura.io](https://hasura.io) 和 [文档](https://hasura.io/docs) 了解更多。

---

![Hasura GraphQL 引擎演示](../assets/demo.gif)

---

![Hasura GraphQL 引擎实时性演示](../assets/realtime.gif)

---

## 特性

- **强大的查询功能**: 内置过滤，分页，模式搜索，批量插入，更新，删除
- **实时**: 通过订阅将任何 GraphQL 查询转换为实时查询
- **远端 Schema 聚合**: 通过单个 GraphQL 端点访问其它负责业务逻辑的 GraphQL 端点 ([了解更多](../remote-schemas.md))
- **触发 Webhooks 与 Serverless 函数**: 在发生 Postgres 插入/更新/删除事件时触发函数 ([了解更多](../event-triggers.md))
- **使用现有的数据库**: 将其指向现有的 Postgres 数据库即可立即获得现成的 GraphQL API
- **细粒度的访问控制**: 与您的认证系统集成的动态访问控制（例如 auth0，firebase-auth）
- **高性能和低内存占用**: 约 15MB 的 docker 映像; 〜50MB RAM @ 1000 请求/秒; 多核友好
- **管理界面和 Schema 迁移**: 管理界面和受 Rails 启发的 Schema 迁移
- **Postgres** ❤️: 支持 Postgres 数据类型（PostGIS /地理位置等），将表视图转换为*图*，通过 GraphQL 变更触发存储函数或过程

请访问 [hasura.io](https://hasura.io) 和 [文档](https://hasura.io/docs) 了解更多

## 目录

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**目录**

- [快速开始](#快速开始)
  - [一键部署到 Heroku](#一键部署到Heroku)
  - [其它部署方法](#其它部署方法)
- [架构](#架构)
- [客户端工具](#客户端工具)
- [添加业务逻辑](#添加业务逻辑)
  - [远端 Schema](#远端Schema)
  - [通过数据库事件触发 Webhooks](#通过数据库事件触发Webhooks)
- [演示](#演示)
  - [实时应用](#实时应用)
  - [视频](#视频)
- [支持与故障排除](#支持与故障排除)
- [贡献](#贡献)
- [品牌资料](#品牌资料)
- [许可证](#许可证)
- [翻译](#翻译)

<!-- markdown-toc end -->

## 快速开始

### 一键部署到 Heroku

部署到 Heroku 是尝试 Hasura 最快的方法

1. 单击以下按钮，在带有免费 Postgres 附加组件的 Heroku 上部署 GraphQL Engine:

   [![部署到Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. 打开 Hasura 控制台

   访问 `https://<app-name>.herokuapp.com` (_更换 \<应用名称\> 使用您的应用名称_) 打开管理控制台.

3. 进行第一个 GraphQL 查询

### 其他一键式部署选项

查看以下一键式部署选项的说明：

| **基础设施提供商** |                                                                                                                         **一键式链接**                                                                                                                          |                                                                    **附加信息**                                                                    |
| :----------------: | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | :------------------------------------------------------------------------------------------------------------------------------------------------: |
|    DigitalOcean    |                  [![部署到 DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme)                  | [文档](https://hasura.io/docs/latest/graphql/core/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
|       Azure        | [![部署到 Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) |                    [文档](https://hasura.io/docs/latest/graphql/core/guides/deployment/azure-container-instances-postgres.html)                    |

### 其它部署方法

关于基于 Docker 的部署和其它高级配置选项，请参阅[部署指南](https://hasura.io/docs/latest/graphql/core/getting-started/index.html) 或者
[安装清单](../install-manifests).

## 架构

Hasura GraphQL 引擎位于 Postgres 数据库实例的前面，并且可以接受来自客户端应用程序的 GraphQL 请求。 可以将其配置为与现有的身份验证系统一起使用，并可以基于认证系统的动态变量实现字段粒度的访问控制。

您还可以合并远端的 GraphQL Schema 以提供统一的 GraphQL API。

![Hasura GraphQL引擎架构](../assets/hasura-arch.svg)

## 客户端工具

Hasura 可与任何 GraphQL 客户端一起使用。 我们建议使用[Apollo Client](https://github.com/apollographql/apollo-client). 见[awesome-graphql](https://github.com/chentsulin/awesome-graphql) 了解其它客户端。

## 添加业务逻辑

GraphQL Engine 提供了易用，可伸缩，高性能的往后端插入自定义业务逻辑的方法。

### 远端 Schema

您可以在远端 Schema 中编写自定义 resolver，再和 Hasura 的基于 Postgres 的 GraphQL Schema 合并。 适用于实现支付 API 或查询数据库中不存在的数据等用例 - [阅读更多](../remote-schemas.md).

### 通过数据库事件触发 Webhooks

添加基于数据库事件触发的异步业务逻辑。
通知，源自 Postgres 的数据流水线或异步处理的理想选择
[阅读更多](../event-triggers.md).

### 派生数据或数据处理

在 Postgres 中处理数据或对其做业务逻辑处理来导出另一套可以用 GraphQL Engine 处理的数据集 - [阅读更多](https://hasura.io/docs/latest/graphql/core/queries/derived-data.html).

## 演示

访问[hasura/sample-apps](https://github.com/hasura/sample-apps/tree/main) 目录查看所有的演示

### 实时应用

- 使用 React 构建的群聊应用程序，包括正在输入指示器，在线用户和新消息通知

  - [试试看](https://realtime-chat.demo.hasura.io/)
  - [讲解](https://github.com/hasura/sample-apps/tree/main/realtime-chat)
  - [浏览 API](https://realtime-chat.demo.hasura.io/console)

- 实时位置跟踪应用程序，显示行驶中的车辆正在更改当前 GPS 坐标在地图上移动

  - [试试看](https://realtime-location-tracking.demo.hasura.io/)
  - [讲解](https://github.com/hasura/sample-apps/tree/main/realtime-location-tracking)
  - [浏览 API](https://realtime-location-tracking.demo.hasura.io/console)

- 实时仪表板，用于聚合不断变化的数据
  - [试试看](https://realtime-poll.demo.hasura.io/)
  - [讲解](https://github.com/hasura/sample-apps/tree/main/realtime-poll)
  - [浏览 API](https://realtime-poll.demo.hasura.io/console)

### 视频

- [将 GraphQL 添加到自托管的 GitLab 实例](https://www.youtube.com/watch?v=a2AhxKqd82Q) (_3:44 mins_)
- [基于 Auth0 和 GraphQL 后端的 Todo 应用](https://www.youtube.com/watch?v=15ITBYnccgc) (_4:00 mins_)
- [与 GitLab 身份验证集成的 GitLab 上的 GraphQL](https://www.youtube.com/watch?v=m1ChRhRLq7o) (_4:05 mins_)
- [千万次骑行的位置数据的仪表板（PostGIS，Timescale）](https://www.youtube.com/watch?v=tsY573yyGWA) (_3:06 mins_)

## 支持与故障排除

文档和社区将帮助您解决大多数问题。 如果您遇到错误或需要与我们联系，可以使用以下渠道之一与我们联系:

- 支持与反馈: [Discord](https://discord.gg/vBPpJkS)
- 问题与错误追踪: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
- 关注产品更新: [@HasuraHQ](https://twitter.com/hasurahq)
- 与我们交谈 [网站聊天](https://hasura.io)

我们致力于在社区中营造开放和欢迎的环境。 请参阅[行为准则](../code-of-conduct.md).

如果您要报告安全问题，请参考[该文档](../SECURITY.md).

## 贡献

查阅我们的[贡献指南](../CONTRIBUTING.md) 了解更多细节.

## 品牌资料

你可以在[assets/brand](../assets/brand)文件夹中找到 Hasura 的品牌资产（logo，Hasura 吉祥物，powered-by badges 等）。请随意在你的应用程序/网站等地方使用它们吧！我们将非常高兴看到在你用 Hasura 构建的应用里看到"Powered by Hasura"徽章。❤️

<div style="display: flex;">
  <img src="../assets/brand/hasura_logo_primary_lightbg.svg" width="150px"/>
  <img src="../assets/brand/hasura_logo_primary_darkbg.svg" width="150px"/>
</div>

```html
<!-- For light backgrounds -->
<a href="https://hasura.io">
  <img
    width="150px"
    src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1711457032/main-web/hasura_logo_primary_lightbg_n0xhz8.svg"
  />
</a>

<!-- For dark backgrounds -->
<a href="https://hasura.io">
  <img
    width="150px"
    src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1711457033/main-web/hasura_logo_primary_darkbg_nussjm.svg"
  />
</a>
```

## 许可证

核心 GraphQL 引擎使用[Apache 许可证 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

所有**其他内容**([`server`](../server), [`cli`](../cli) 和
[`console`](../console) 目录除外) 使用 [MIT License](../LICENSE-community).
这包括 [`docs`](../docs) 和 [`community`](../community) 目录.

## 翻译

该 README 还有以下翻译版本：

- [Japanese :jp:](../translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](../translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Chinese :cn:](../translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))

可以在[这里](../translations)找到其他文件的翻译
