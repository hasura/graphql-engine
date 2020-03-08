# Hasura GraphQL引擎

[![文档](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL引擎是一个高性能的GraphQL服务器，可为您提供 **Postgres上开箱即用的实时GraphQL API**， 响应数据库事件的 [**Webhook触发器**](../event-triggers.md)，以及用于业务逻辑处理的 [**远端Schema**](../remote-schemas.md)。

Hasura可帮助您构建基于Postgres的GraphQL应用程序，或将使用Postgres的现有应用迁移到GraphQL上。

请参阅 [hasura.io](https://hasura.io) 和 [文档](https://hasura.io/docs) 了解更多。

------------------

![Hasura GraphQL 引擎演示](../assets/demo.gif)

------------------

![Hasura GraphQL 引擎实时性演示](../assets/realtime.gif)

-------------------

## 特性

* **强大的查询功能**: 内置过滤，分页，模式搜索，批量插入，更新，删除
* **实时**: 通过订阅将任何GraphQL查询转换为实时查询
* **远端Schema聚合**: 通过单个GraphQL端点访问其它负责业务逻辑的GraphQL端点 ([了解更多](../remote-schemas.md))
* **触发Webhooks与Serverless函数**: 在发生Postgres插入/更新/删除事件时触发函数 ([了解更多](../event-triggers.md))
* **使用现有的数据库**: 将其指向现有的Postgres数据库即可立即获得现成的GraphQL API
* **细粒度的访问控制**: 与您的认证系统集成的动态访问控制（例如auth0，firebase-auth）
* **高性能和低内存占用**: 约15MB的docker映像; 〜50MB RAM @ 1000请求/秒; 多核友好
* **管理界面和Schema迁移**: 管理界面和受Rails启发的Schema迁移
* **Postgres** ❤️: 支持Postgres数据类型（PostGIS /地理位置等），将表视图转换为*图*，通过GraphQL变更触发存储函数或过程

请访问 [hasura.io](https://hasura.io) 和 [文档](https://hasura.io/docs) 了解更多

## 目录
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**目录**

- [快速开始](#快速开始)
    - [一键部署到Heroku](#一键部署到Heroku)
    - [其它部署方法](#其它部署方法)
- [架构](#架构)
- [客户端工具](#客户端工具)
- [添加业务逻辑](#添加业务逻辑)
    - [远端Schema](#远端Schema)
    - [通过数据库事件触发Webhooks](#通过数据库事件触发Webhooks)
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

### 一键部署到Heroku

部署到Heroku是尝试Hasura最快的方法

1. 单击以下按钮，在带有免费Postgres附加组件的Heroku上部署GraphQL Engine:

    [![部署到Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. 打开Hasura控制台

   访问 `https://<app-name>.herokuapp.com` (*更换 \<应用名称\> 使用您的应用名称*) 打开管理控制台.

3. 进行第一个GraphQL查询

### 其他一键式部署选项

查看以下一键式部署选项的说明：

| **基础设施提供商** | **一键式链接** | **附加信息** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![部署到 DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [文档](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![部署到 Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [文档](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### 其它部署方法

关于基于Docker的部署和其它高级配置选项，请参阅[部署指南](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) 或者
[安装清单](../install-manifests).

## 架构

Hasura GraphQL引擎位于Postgres数据库实例的前面，并且可以接受来自客户端应用程序的GraphQL请求。 可以将其配置为与现有的身份验证系统一起使用，并可以基于认证系统的动态变量实现字段粒度的访问控制。

您还可以合并远端的GraphQL Schema以提供统一的GraphQL API。

![Hasura GraphQL引擎架构](../assets/hasura-arch.svg)

## 客户端工具

Hasura可与任何GraphQL客户端一起使用。 我们建议使用[Apollo Client](https://github.com/apollographql/apollo-client). 见[awesome-graphql](https://github.com/chentsulin/awesome-graphql) 了解其它客户端。

## 添加业务逻辑

GraphQL Engine提供了易用，可伸缩，高性能的往后端插入自定义业务逻辑的方法。

### 远端Schema

您可以在远端Schema中编写自定义resolver，再和Hasura的基于Postgres的GraphQL Schema合并。 适用于实现支付API或查询数据库中不存在的数据等用例 - [阅读更多](../remote-schemas.md).

### 通过数据库事件触发Webhooks

添加基于数据库事件触发的异步业务逻辑。
通知，源自Postgres的数据流水线或异步处理的理想选择
[阅读更多](../event-triggers.md).

### 派生数据或数据处理

在Postgres中处理数据或对其做业务逻辑处理来导出另一套可以用GraphQL Engine处理的数据集 - [阅读更多](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## 演示

访问[community/sample-apps](../community/sample-apps) 目录查看所有的演示

### 实时应用

- 使用React构建的群聊应用程序，包括正在输入指示器，在线用户和新消息通知
  - [试试看](https://realtime-chat.demo.hasura.app/)
  - [讲解](../community/sample-apps/realtime-chat)
  - [浏览API](https://realtime-chat.demo.hasura.app/console)

- 实时位置跟踪应用程序，显示行驶中的车辆正在更改当前GPS坐标在地图上移动
  - [试试看](https://realtime-location-tracking.demo.hasura.app/)
  - [讲解](../community/sample-apps/realtime-location-tracking)
  - [浏览API](https://realtime-location-tracking.demo.hasura.app/console)

- 实时仪表板，用于聚合不断变化的数据
  - [试试看](https://realtime-poll.demo.hasura.app/)
  - [讲解](../community/sample-apps/realtime-poll)
  - [浏览API](https://realtime-poll.demo.hasura.app/console)

### 视频

* [将GraphQL添加到自托管的GitLab实例](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [基于Auth0和GraphQL后端的Todo应用](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [与GitLab身份验证集成的GitLab上的GraphQL](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [千万次骑行的位置数据的仪表板（PostGIS，Timescale）](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## 支持与故障排除

文档和社区将帮助您解决大多数问题。 如果您遇到错误或需要与我们联系，可以使用以下渠道之一与我们联系:
* 支持与反馈: [Discord](https://discord.gg/vBPpJkS)
* 问题与错误追踪: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* 关注产品更新: [@HasuraHQ](https://twitter.com/hasurahq)
* 与我们交谈 [网站聊天](https://hasura.io)

我们致力于在社区中营造开放和欢迎的环境。 请参阅[行为准则](../code-of-conduct.md).

如果您要报告安全问题，请参考[该文档](../SECURITY.md).

## 贡献

查阅我们的[贡献指南](../CONTRIBUTING.md) 了解更多细节.

## 品牌资料

你可以在[assets/brand](../assets/brand)文件夹中找到Hasura的品牌资产（logo，Hasura吉祥物，powered-by badges等）。请随意在你的应用程序/网站等地方使用它们吧！我们将非常高兴看到在你用Hasura构建的应用里看到"Powered by Hasura"徽章。❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- For light backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- For dark backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## 许可证

核心GraphQL引擎使用[Apache许可证 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

所有**其他内容**([`server`](../server), [`cli`](../cli) 和
[`console`](../console) 目录除外) 使用 [MIT License](../LICENSE-community).
这包括 [`docs`](../docs) 和 [`community`](../community) 目录.

## 翻译

该README还有以下翻译版本：

- [Japanese :jp:](../translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](../translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Chinese :cn:](../translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))


可以在[这里](../translations)找到其他文件的翻译
