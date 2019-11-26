# Hasura GraphQL引擎

[![文件](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL引擎是一种快速的GraphQL服务器，可为您提供 **Postgres上的即时，实时GraphQL API**, 与 [**Webhook触发器**](../event-triggers.md) 关于数据库事件，以及 [**远程模式**](../remote-schemas.md) 用于业务逻辑.

Hasura可帮助您构建由Postgres支持的GraphQL应用程序，或使用Postgres将其逐步移至GraphQL以用于现有应用程序.

阅读更多 [hasura.io](https://hasura.io) 和 [docs](https://docs.hasura.io).

------------------

![Hasura GraphQL 引擎演示](../assets/demo.gif)

------------------

![Hasura GraphQL 引擎实时演示](../assets/realtime.gif)

-------------------

## 特征

* **进行强大的查询**: 内置过滤，分页，模式搜索，批量插入，更新，删除突变
* **即时的**: 通过使用订阅将任何GraphQL查询转换为实时查询
* **合并远程架构**: 通过单个GraphQL Engine端点访问业务逻辑的自定义GraphQL模式. [**阅读更多**](../remote-schemas.md).
* **触发网络钩子或无服务器功能**: 在Postgres插入/更新/删除事件 ([阅读更多](../event-triggers.md))
* **使用现有的实时数据库**: 将其指向现有的Postgres数据库即可立即获得现成的GraphQL API
* **细粒度的访问控制**: 与您的auth系统集成的动态访问控制（例如auth0，firebase-auth）
* **高性能和低占用空间**: 约15MB的docker映像; 〜50MB RAM @ 1000请求/秒; 多核意识
* **管理员界面和迁移**: 管理员界面和受Rails启发的架构迁移
* **Postgres** ❤️: 支持Postgres类型（PostGIS /地理位置等），将视图转换为*图形*，触发具有突变的存储函数或过程

阅读更多 在 [hasura.io](https://hasura.io) 和 [docs](https://docs.hasura.io).

## 目录
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**目录**

- [快速上手](#快速开始)
    - [一键式部署到Heroku](#one-click-deployment-on-heroku)
    - [其他部署方法](#其他部署方法)
- [建筑学](#建筑)
- [客户端工具](#客户端工具)
- [添加业务逻辑](#添加业务逻辑)
    - [远程模式](#远程模式)
    - [通过数据库事件调用Webhook](#在数据库事件上触发Webhooks)
- [演示版](#演示版)
    - [实时应用](#实时应用)
    - [影片](#影片)
- [支持与故障排除](#支持与故障排除)
- [参与发展](#贡献)
- [品牌资产](#品牌资产)
- [执照](#执照)
- [翻译](#翻译)

<!-- markdown-toc end -->

## 快速开始

### One-click deployment on Heroku

尝试Hasura最快的方法是通过Heroku。

1. 单击以下按钮，在带有免费Postgres附加组件的Heroku上部署GraphQL Engine:

    [![部署到Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. 打开Hasura控制台

   访问 `https://<app-name>.herokuapp.com` (*更换 \<应用名称\> 使用您的应用名称*) 打开管理控制台.

3. 进行第一个GraphQL查询

### 其他一键式部署选项

查看有关以下一键式部署选项的说明：

| **基础设施提供商** | **一键式链接** | **附加信息** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![部署到 DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![部署到 Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### 其他部署方法

有关基于Docker的部署和高级配置选项，请参阅[部署
指南](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) 要么
[安装清单](../install-manifests).

## 建筑

Hasura GraphQL引擎位于Postgres数据库实例的前面，并且可以接受来自客户端应用程序的GraphQL请求。 可以将其配置为与现有的身份验证系统一起使用，并可以使用具有来自身份验证系统的动态变量的字段级规则来处理访问控制。

您还可以合并远程GraphQL模式并提供统一的GraphQL API。

![Hasura GraphQL引擎架构](../assets/hasura-arch.svg)

## 客户端工具

Hasura可与任何GraphQL客户端一起使用。 我们建议使用[Apollo Client](https://github.com/apollographql/apollo-client). 见[awesome-graphql](https://github.com/chentsulin/awesome-graphql) 获取客户列表.

## 添加业务逻辑

GraphQL Engine提供了易于使用，可扩展和高性能的方法，用于向您的后端添加自定义业务逻辑:

### 远程模式

除了Hasura基于Postgres的GraphQL模式之外，还可以在远程模式中添加自定义解析器。 适用于实现支付API或查询数据库中不存在的数据等用例 - [阅读更多](../remote-schemas.md).

### 在数据库事件上触发Webhooks

添加基于数据库事件触发的异步业务逻辑。
通知，Postgres或异步数据管道的理想选择
处理中 - [阅读更多](../event-triggers.md).

### 派生数据或数据转换

在Postgres中转换数据或在其上运行业务逻辑以导出可以使用GraphQL Engine查询的另一个数据集 - [阅读更多](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## 演示版

在中查看所有示例应用程序
[community/sample-apps](../community/sample-apps) 目录.

### 实时应用

- 使用React构建的群聊应用程序，包括打字指示器，在线用户和新用户
   消息通知.
  - [试试看](https://realtime-chat.demo.hasura.app/)
  - [讲解](../community/sample-apps/realtime-chat)
  - [浏览API](https://realtime-chat.demo.hasura.app/console)

- 实时位置跟踪应用程序，显示行驶中的车辆正在更改当前GPS
   坐标在地图上移动.
  - [试试看](https://realtime-location-tracking.demo.hasura.app/)
  - [讲解](../community/sample-apps/realtime-location-tracking)
  - [浏览API](https://realtime-location-tracking.demo.hasura.app/console)

- 实时仪表板，用于不断变化的数据上的数据聚合.
  - [试试看](https://realtime-poll.demo.hasura.app/)
  - [讲解](../community/sample-apps/realtime-poll)
  - [浏览API](https://realtime-poll.demo.hasura.app/console)

### 影片

* [将GraphQL添加到自托管的GitLab实例](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [带有Auth0和GraphQL后端的Todo应用](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [与GitLab身份验证集成的GitLab上的GraphQL](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [带有地理位置的仪表板，可进行一千万次骑行（PostGIS，时标）](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## 支持与故障排除

文档和社区将帮助您解决大多数问题。 如果您遇到错误或需要与我们联系，可以使用以下渠道之一与我们联系:
* 支持与反馈: [Discord](https://discord.gg/vBPpJkS)
* 问题与错误追踪: [GitHub 问题](https://github.com/hasura/graphql-engine/issues)
* 关注产品更新: [@HasuraHQ](https://twitter.com/hasurahq)
* 与我们交谈 [网站聊天](https://hasura.io)

我们致力于在社区中营造开放和欢迎的环境。 请参阅[行为准则](../code-of-conduct.md).

如果您要报告安全问题，请[阅读此](../SECURITY.md).

## 贡献

查阅我们的[投稿指南](../CONTRIBUTING.md) 更多细节.

## 品牌资产

可以将Hasura品牌资产（徽标，Hasura吉祥物，由徽章提供动力等）设置为
在[资产/品牌]中找到(assets/brand) 夹。 随时在您的计算机中使用它们
应用程序/网站等。如果您添加“ Powered by Hasura”，我们将非常高兴
使用Hasura构建的应用程序的徽章。 ❤️

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

## 执照

核心GraphQL引擎位于[Apache许可证 2.0](https://www.apache.org/licenses/LICENSE-2.0) (阿帕奇2.0).

所有**其他内容**(以下内容除外 [`服务器`](../server), [`cli`](../cli) 和
[`安慰`](../console) 目录) 可在 [MIT License](../LICENSE-community).
这包括 [`docs`](../docs) 和 [`community`](../community)
目录.

## 翻译

该自述文件可以通过以下翻译获得：

- [Japanese :jp:](../translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](../translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Chinese :cn:](../translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg))


可以找到其他文件的翻译 [这里](../translations).
