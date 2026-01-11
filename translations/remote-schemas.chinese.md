# 远程模式（Remote Schemas）

Hasura 为您提供带有授权和访问控制的 CRUD + 实时 GraphQL API。然而，在许多情况下，您需要编写包含自定义逻辑的 API（查询、变更）。例如，实现支付 API，或查询数据库中不存在的数据。

Hasura 具备合并远程 GraphQL 模式并提供统一 GraphQL API 的能力。可以将其视为自动化的模式合并。您只需构建自己的 GraphQL 服务，并将其 HTTP 端点提供给 Hasura。您的 GraphQL 服务可以使用任何编程语言或框架编写。

远程模式适用于以下场景：

* 自定义变更（*例如：在插入数据前运行验证*）
* 支持支付等功能，并提供一致的接口来访问它们，例如：即通过 GraphQL 引擎的 API 进行
* 从其他来源获取不同的数据（*例如：从天气 API 或另一个数据库*）

为了支持自定义业务逻辑，您需要创建一个自定义的 GraphQL 服务器（请参阅 [boilerplates](community/boilerplates/remote-schemas)）并将其模式与 GraphQL 引擎的模式合并。

![远程模式架构](../assets/remote-schemas-arch.png)

## 演示 (*40 秒*)

[![合并远程模式的视频演示](https://img.youtube.com/vi/eY4n9aPsi0M/0.jpg)](https://www.youtube.com/watch?v=eY4n9aPsi0M)

[合并远程 GraphQL 模式（YouTube 链接）](https://youtu.be/eY4n9aPsi0M)

[添加远程模式](https://youtu.be/01t4t2t4q1c)

## 快速入门

尝试远程模式的最快方式是通过 Heroku。

1. 点击以下按钮，将 GraphQL 引擎与免费的 Postgres 插件一起部署到 Heroku：

   [![部署到 Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. 打开 Hasura 控制台

   访问 `https://<app-name>.herokuapp.com`（*将 \<app-name\> 替换为您的应用名称*）以打开管理控制台。

3. 合并您的第一个远程模式并查询它

   在管理控制台中，打开 ``Remote Schemas`` 标签页并点击 ``Add`` 按钮。填写以下信息：
   * 远程模式名称：``countries`` (*此远程模式的别名*)。
   * GraphQL 服务器 URL：``https://countries.trevorblades.com/`` (*一个公共的 GraphQL API，我们将用它快速查看此功能；由 [@trevorblades](https://github.com/trevorblades) 维护*)。
   * 忽略其余的配置设置并点击 ``Add Remote Schema`` 按钮。

   前往 ``GraphQL`` 标签页并运行以下查询 (*将其粘贴到左侧的查询窗口并点击* ▶️ *(播放) 按钮*)：

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

   您可以使用 ``GraphQL`` 界面右上角的 ``Docs`` 浏览器来探索远程模式中的 GraphQL 类型。

## 模板

适用于流行语言/框架的自定义 GraphQL 服务器的模板可用。

* [常规模板](community/boilerplates/graphql-servers)，可以部署到任何地方。
* [无服务器模板](https://github.com/hasura/graphql-serverless)，可以部署到 AWS Lambda 等无服务器平台。

请注意，更多语言、框架、无服务器平台等的模板正在不断更新，社区贡献非常欢迎。

## 注意事项

**当前的限制**:

* 命名规则：类型名称和节点名称在所有合并的模式中必须是唯一的（区分大小写）。在接下来的几个版本中，将支持合并名称和结构相同的类型。
* 来自不同 GraphQL 服务器的节点无法在同一个查询/变更（query/mutation）中使用。所有顶层节点必须来自同一个 GraphQL 服务器。
* 不支持远程 GraphQL 服务器的订阅（subscription）。

这些限制将在后续版本中內解决

## 📄 文档

阅读完整[文档](https://hasura.io/docs/latest/graphql/core/remote-schemas/index.html)

## 翻译

此文档提供以下翻译版本：

- [French :fr:](translations/remote-schemas.french.md)
- [Hindi  :india:](translations/remote-schemas.hindi.md)
