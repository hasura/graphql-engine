# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine - это сверхбыстрый графический сервер позволяющий создавать **мгновенный GraphQL API работающий в режиме реального времени в базе данных Postgres**, с [**webhook триггерами**](../event-triggers.md) на основе событий в базе данных, и [**remote schemas**](../remote-schemas.md) для бизнес-логики.

Hasura помогает строить GraphQL приложения при поддержке Postgres или постепенно перейти на GraphQL для уже существующих приложений, использующих Postgres.

Подробнее на [hasura.io](https://hasura.io) и [docs](https://hasura.io/docs).

------------------

![Hasura GraphQL Engine Demo](../assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

-------------------

## Характеристики

* **Делать мощные запросы**: встроенная фильтрация, нумерация страниц, поиск шаблонов, объёмная вставка, обновление, удаление мутаций.
* **Режим реального времени**: Конвертируйте любой запрос GraphQL в запрос в режиме реального времени, используя подписку.
* **Слияние удаленных схем**: Доступ к пользовательским схемам GraphQL для бизнес-логики через единую конечную точку GraphQL Engine. [**Подробнее**](../remote-schemas.md).
* **Запуск webhook-ов или serverless функций**: В ответ на события Postgres insert/update/delete.([ подробнее](../event-triggers.md))
* **Работает с существующими базами данных**: Направьте его на существующую базу данных Postgres, чтобы мгновенно получить готовый к использованию GraphQL API.
* **Подробный контроль доступа**: Динамический контроль доступа, интегрируемый с Auth системой (напр: auth0, firebase-auth).
* **Высокая производительность и малое воздействие**: ~15MB изображение docker-a; ~50MB RAM @ 1000 req/s; многоядерный.
* **Интерфейс администрирования и миграции**: Интерфейс администрирования и миграция схемы на Rails
* **Postgres** ❤️: Поддержка Postgres типов (PostGIS/geo-location, и т.д.), преобразуется в *graphs*, запускает процедуры или функции, сохраненные посредством мутаций.

Подробнее на [hasura.io](https://hasura.io) и [docs](https://hasura.io/docs).

## Содержание
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Содержание**

- [Быстрый запуск:](#быстрый-запуск)
    - [Деплоймент одним кликом на Heroku](#деплоймент-одним-кликом-на-Heroku)
    - [Другие методы деплоймента](#другие-варианты-деплоймента-одним-кликом)
- [Архитектура](#архитектура)
- [Инструменты клиентской стороны](#инструменты-клиентской-стороны)
- [Добавление бизнес-логики](#добавить-бизнес-логику)
    - [Схемы удаленного доступа](#схемы-удаленного-доступа)
    - [Запуск webhook по событиям в базе данных](#запуск-webhook-по-событиям-в-базе-данных)
- [Demos](#demos)
    - [Приложения в режиме реального времени](#приложения-в-режиме-реального-времени)
    - [Видео](#видео)
- [Поддержка и устранение неисправностей](#поддержка-и-устранение-неисправностей)
- [Контрибуция](#контрибуция)
- [Элементы бренда](#элементы-бренда)
- [Лицензия](#лицензия)

<!-- markdown-toc end -->

## Быстрый запуск:

### Деплоймент одним кликом на Heroku

Самый быстрый способ попробовать Hasura через Heroku.

1. Нажмите на кнопку ниже, чтобы развернуть GraphQL Engine на Heroku с бесплатным дополнением Postgres:

    [![Деплоймент в Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Откройте консоль Hasura

   Посетите `https://<app-name>.herokuapp.com` (*замените \<app-name\> с именем вашего приложения*) чтобы открыть консоль администрирования.

3. Сделайте свой первый GraphQL-запрос

   Создайте таблицу и сразу же выполните свой первый запрос. Следуйте следующим указаниям [простое руководство](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Другие варианты деплоймента одним кликом

Ознакомьтесь со следующими инструкциями по деплойменте одним кликом:

| **Infra provider** | **One-click link** | **Additional information** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Другие методы деплоймента

Для методов деплоймента на базе Docker и расширенных опций конфигурации, ознакомьтесь с [руководство по деплойменту](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) или
[установока манифестов](../install-manifests).

## Архитектура

Hasura GraphQL Engine расположен перед базой данных Postgres и может принимать запросы GraphQL от ваших клиентских приложений. Он может быть сконфигурирован для работы с вашей существующей системой Auth и может управлять доступом с помощью правил полевого уровня с динамическими переменными из вашей системы auth.

Вы также можете объединить удаленные GraphQL схемы и предоставить единый GraphQL API.

![Архитектура Hasura GraphQL Engine](../assets/hasura-arch.svg)

## Инструменты клиентской стороны

Hasura работает с любым GraphQL клиентом. Мы рекомендуем использовать [Apollo Client](https://github.com/apollographql/apollo-client). Посетите [awesome-graphql](https://github.com/chentsulin/awesome-graphql) для списка клиентов.

## Добавить бизнес-логику

GraphQL Engine предоставляет простые в обосновании, масштабируемые и производительные методы для добавления бизнес-логики в ваш бэкенд:

### Схемы удаленного доступа

Добавьте пользовательские преобразователи в удаленную схему в дополнение к схеме GraphQL на основе Postgres Hasura. Идеально подходит для использования в таких случаях, как имплементация платежного API или запрос данных, отсутствующих в вашей базе данных. - [подробнее](../remote-schemas.md).

### Запуск webhook по событиям в базе данных

Добавьте асинхронную бизнес-логику, которая срабатывает на основе событий в базе данных.
Идеально подходит для уведомлений, каналов передачи данных от Postgres или асинхронной обработки - [подробнее](../event-triggers.md).

### Полученные данные или трансформация данных

Преобразование данных в Postgres или запуск бизнес-логики для получения другого набора данных, который можно запросить с помощью GraphQL Engine. - [подробнее](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## Demos

Ознакомьтесь со всеми примерами приложений в 
[community/examples](../community/sample-apps) папке.

### Приложения в режиме реального времени

- Приложение группового чата, построенное с помощью React, включает в себя индикатор набора текста, онлайн пользователей и уведомления о сообщениях.
  - [Попробуйте это](https://realtime-chat.demo.hasura.app/)
  - [Руководство](../community/sample-apps/realtime-chat)
  - [Просмотреть API](https://realtime-chat.demo.hasura.app/console)

- Приложение для отслеживания местоположения в реальном времени, показывающее движущееся транспортное средство, изменяющее текущие GPS-координаты, движущееся по карте.
  - [Попробуйте это](https://realtime-location-tracking.demo.hasura.app/)
  - [Руководство](../community/sample-apps/realtime-location-tracking)
  - [Просмотреть API](https://realtime-location-tracking.demo.hasura.app/console)

- Информационная панель в режиме реального времени для агрегирования данных по постоянно меняющимся данным.
  - [Попробуйте это](https://realtime-poll.demo.hasura.app/)
  - [Руководство](../community/sample-apps/realtime-poll)
  - [Просмотреть API](https://realtime-poll.demo.hasura.app/console)

### Видео

* [Добавьте GraphQL к экземпляру GitLab, размещенному на вашем сервере](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 мин*)
* [Приложение Todo с Auth0 и GraphQL бэкенд](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 мин*)
* [GraphQL на GitLab интегрированный с GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 мин*)
* [Таблица для 10 миллионов ездок с геолокацией (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 мин*)


## Поддержка и устранение неисправностей

Документация и сообщество поможет вам решить большинство проблем. Если вы столкнулись с ошибкой или вам нужно связаться с нами, вы можете использовать один из следующих каналов связи:

* Поддержка и обратная связь: [Discord](https://discord.gg/vBPpJkS)
* Проблема и отслеживание ошибок: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Следите за обновлениями продукта: [@HasuraHQ](https://twitter.com/hasurahq)
* Поговорите с нами [чат на сайте](https://hasura.io)

Мы стремимся создать открытую и доброжелательную атмосферу в сообществе. Пожалуйста, ознакомьтесь с [Кодекс поведения](../code-of-conduct.md).

Если вы хотите сообщить о проблеме безопасности, пожалуйста, [прочитайте это](../SECURITY.md).

## Контрибуция

Ознакомьтесь с нашим [руководством по контрибуции](../CONTRIBUTING.md) для более подробной информации.

## Элементы бренда

Элементы бренда Hasura (логотипы, Hasura значки и т.д.) можно найти в [assets/brand](../assets/brand) папке.  Можете использовать их в вашем приложении / веб-сайте и т.д. 
Мы были бы рады, если бы вы добавили "Powered by Hasura" бейдж к вашим приложениям, построенным с помощью Hasura.. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Для светлых фонов -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- Для темных фонов -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Лицензия

GraphQL Engine доступно по адресу [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Все **прочие материалы** (за исключением материалов, содержащихся в [`server`](../server), [`cli`](../cli) и
[`console`](../console) папках) доступны под [MIT License](../LICENSE-community).
Сюда входит все, что есть в [`docs`](../docs) и [`community`](../community) папках.
