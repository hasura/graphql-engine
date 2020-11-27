# Hasura GraphQL Engine

[![Bản mới nhất](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Tài liệu](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine là một GraphQL server cực nhanh cung cấp **GraphQL APIs nhanh tức thời trong thời gian thực trên Postgres**, với [**webhook triggers**](event-triggers.md) trên những database event và [**remote schemas**](remote-schemas.md) dành cho business logic.

Hasura giúp bạn xây dựng các ứng dụng [GraphQL](https://hasura.io/graphql/) được hỗ trợ bởi Postgres hoặc dần chuyển sang GraphQL cho những ứng dụng đang có sử dụng Postgres.

Đọc thêm tại [hasura.io](https://hasura.io) và [tài liệu](https://hasura.io/docs/).

------------------

![Hasura GraphQL Engine mẫu Demo](assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime mẫu Demo](assets/realtime.gif)

-------------------

## Tính năng

* **Tạo câu truy vấn mạnh mẽ**: Bộ lọc hỗ trợ được tích hợp sẵn, phân trang, tìm kiếm theo pattern, chèn/cập nhật/xóa nhiều dữ liệu.
* **Thời gian thực**: Chuyển đổi bất kỳ câu truy vấn GraphQL thành live query bằng cách sử dụng subscription.
* **Kết hợp giản đồ từ xa**: Truy cập vào custom GraphQL schemas dành cho business logic qua một GraphQL Engine endpoint. [**Đọc thêm**](remote-schemas.md).
* **Trigger webhooks hoặc những tính năng ngoài server**: Những event trên Postgres thêm/cập nhật/xóa ([đọc thêm](event-triggers.md))
* **Hoạt động tốt với những database đang có**: Thiết lập đến một Postgres database đang có để có được GraphQL API sẵn sàng có thể sử dụng được ngay lập tức.
* **Kiểm soát truy cập cặn kẽ**: Kiểm soát truy cập động (dynamic) giúp kết hợp với hệ thống xác thực của bạn đang có (ví dụ: auth0, firebase-auth)
* **Hiệu năng cao & gọn nhẹ**: chiếm ~15MB trên docker image; ~50MB RAM @ 1000 req/s; đa nhân.
* **Giao diện Admin & và sự chuyển giao**: Giao diện Admin và sự chuyển giao được truyền cảm hứng từ Rails.
* **Postgres** ❤️: Hỗ trợ nhiều Postgres type (PostGIS/geo-location, etc.), chuyển views thành *graphs*, những hàm kích hoạt hoặc những procedure cùng những mutation.

Xem thêm tại [hasura.io](https://hasura.io) và [tài liệu](https://hasura.io/docs/).

## Mục lục
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Mục lục**

- [Bắt đầu nhanh:](#quickstart)
    - [Triển khai bằng một click trên Hasura Cloud](#one-click-deployment-on-hasura-cloud)
    - [Những lựa chọn khác, one-click deployment](#other-one-click-deployment-options)
    - [Những phương thức triển khai khác](#other-deployment-methods)
- [Kiến trúc](#architecture)
- [Công cụ dành cho người dùng khách hàng](#client-side-tooling)
- [Thêm business logic](#add-business-logic)
    - [Giản đồ từ xa](#remote-schemas)
    - [Trigger webhooks trên database events](#trigger-webhooks-on-database-events)
- [Ví dụ thực tế](#demos)
    - [Ứng dụng trong thời gian thực](#realtime-applications)
    - [Videos](#videos)
- [Hỗ trợ & xử lý sự cố](#support--troubleshooting)
- [Đóng góp cho dự án này](#contributing)
- [Tên hiệu, brand](#brand-assets)
- [Bản quyền](#license)
- [Những bản dịch](#translations)

<!-- markdown-toc end -->

## Bắt đầu nhanh:

### Triển khai bằng một click trên Hasura Cloud

Cách nhanh nhất và dễ nhất để thử Hasura là bằng cách [Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html).

1. Bấm vào nút sau đây để triển khai GraphQL engine trên Hasura Cloud bao gồm Postgres add-on hoặc sử dụng một Postgres database đang có sẵn:

    [![Triển khai trên Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. Mở Hasura console

   Bấm vào nút "Launch console" để mở Hasura console.

3. Tạo câu truy vấn GraphQL đầu tiên

   Tạo một bảng (table) và chạy câu truy vấn đầu tiên. Tham khảo [hướng dẫn đơn giản này](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Những lựa chọn khác, one-click deployment

Xem qua những hướng dẫn dành cho những lựa chọn one-click deployment sau:

| **Nhà cung cấp** | **Liên kết** | **Thông tin thêm** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### Những phương thức triển khai khác

Đối với những ứng dụng trên Docker và những lựa chọn cấu hình nâng cao, xem [những hướng dẫn](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) hoặc
[những chi tiết cài đặt](install-manifests).

## Kiến trúc

Hasura GraphQL Engine tạo một đối tượng (instance) Postgres database và có thể chấp nhận những GraphQL request từ ứng dụng client của bạn. Nó có thể được cấu hình để làm việc với hệ thống xác thực mà bạn đang có và có thể kiểm soát sự truy cập bằng cách sử dụng những quy tắc field-level với những dynamic variable trong hệ thống.

Bạn còn có thể gom những GraphQL schema lại với nhau và cung cấp một bộ GraphQL API.

![Kiến trúc của Hasura GraphQL Engine](assets/hasura-arch.svg)

## Công cụ dành cho người dùng khách hàng

Hasura làm việc được với bất kỳ GraphQL client nào. Chúng tôi khuyến nghị sử dụng [Apollo Client](https://github.com/apollographql/apollo-client). Xem thêm danh sách các client [awesome-graphql](https://github.com/chentsulin/awesome-graphql).

## Thêm business logic

GraphQL Engine cung cấp những phương thức có thể sử dụng dễ dàng và có thể nâng cấp được trong tương lai bằng cách thêm vào những business logic tùy chỉnh vào phần backend của bạn:

### Giản đồ từ xa

Thêm những custom resolver vào một giản đồ từ xa (remote schema) cùng với những GraphQL schema trên Hasura Postgres. Áp dụng lý tưởng cho những trường hợp như API trong thanh toán, hoặc truy vấn những data không nằm trong database nội bộ của bạn - [xem thêm](remote-schemas.md).

### Trigger webhooks trên database events

Thêm nhiều những business logic được kích hoạt bởi database event.
Lí tưởng dành cho những ứng dụng cần nhắc thông báo (notifications), dữ liệu từ Postgres hoặc xử lý không đồng bộ - [đọc thêm](event-triggers.md).

### Dữ liệu kế thừa hoặc những sự chuyển đổi dữ liệu

Chuyển đổi dữ liệu trong Postgres hoặc chạy business logic trên nó để kế thừa một dataset khác được truy vấn bởi GraphQL Engine - [đọc thêm](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Ví dụ thực tế

Xem qua những ứng dụng mẫu tại thư mục [community/sample-apps](community/sample-apps).

### Các ứng dụng realtime (thời gian thực)

- Ứng dụng chat nhóm được xây dựng trên React, bao gồm việc lấy trạng thái người dùng đang gõ chữ (typing indicator), người dùng nào đang online, người dùng mới tạo tài khoản.
  tin nhắn thông báo.
  - [Xem thử ngay](https://realtime-chat.demo.hasura.app/)
  - [Hướng dẫn](community/sample-apps/realtime-chat)
  - [Xem các APIs](https://realtime-chat.demo.hasura.app/console)

- Ứng dụng theo dõi vị trí (location tracking) hiển thị một phương tiện giao thông đang di chuyển trên bản đồ thông qua tọa độ.
  - [Xem thử ngay](https://realtime-location-tracking.demo.hasura.app/)
  - [Hướng dẫn](community/sample-apps/realtime-location-tracking)
  - [Xem các APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Một bảng điều khiển (dashboard) dành cho một lượng nhiều dữ liệu (data) liên tục được cập nhật.
  - [Xem thử ngay](https://realtime-poll.demo.hasura.app/)
  - [Hướng dẫn](community/sample-apps/realtime-poll)
  - [Xem các APIs](https://realtime-poll.demo.hasura.app/console)

### Videos

* [Thêm GraphQL vào instance được lưu trữ trên GitLab](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 phút*)
* [Ứng dụng ghi chú Todo với Auth0 và GraphQL backend](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 phút*)
* [GraphQL trên GitLab kết hợp với GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 phút*)
* [Bảng thông tin dành cho 10 triệu phương tiện với geo-location (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 phút*)


## Hỗ trợ & xử lý sự cố

Những tài liệu và cũng như cộng đồng sẽ giúp bạn xử lý khắc phục hầu hết những vấn đề. Nếu bạn gặp bất kỳ lỗi hoặc cần liên lạc với chúng tôi, bạn có thể thông qua một trong những kênh thông tin bên dưới đây:

* Hỗ trợ & phản hồi: [Discord](https://discord.gg/hasura)
* Vấn đề & theo dõi lỗi: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Theo dõi những bản cập nhật: [@HasuraHQ](https://twitter.com/hasurahq)
* Trò chuyện với chúng tôi trên [website chat](https://hasura.io)

Chúng ta cùng nhau xây dựng một cộng đồng mở và một môi trường luôn chào đón những sự đóng góp. Xem qua [Code of Conduct](code-of-conduct.md).

Nếu bạn muốn báo cáo một vấn đề về bảo mật thì vui lòng [đọc tại đây](SECURITY.md).

## Đóng góp

Xem qua [contributing guide](CONTRIBUTING.md) để biết thêm chi tiết.

## Tên hiệu, brand

Thương hiệu Hasura (bao gồm những logo, Hasura mascot, tên hiệu được đánh dấu 'powered by'.) có thể được tìm thấy tại thư mục [assets/brand](assets/brand). Thoải mái sử dụng nó trên ứng dụng/website của bạn. Chúng tôi sẽ rất vui nếu bạn thêm vào nhãn "Powered by Hasura" trong những ứng dụng của bạn mà có sử dụng Hasura. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_white.svg" width="150px"/>
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

## Bản quyền

Lõi (core) của GraphQL Engine được sử dụng dưới quyền [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Tất cả **những nội dung khác** (trừ những nội dung trong các thư mục [`server`](server), [`cli`](cli) và [`console`](console) ) là dưới quyền [MIT License](LICENSE-community).
Bao gồm tất cả những thư mục sau [`docs`](docs) và [`community`](community).
