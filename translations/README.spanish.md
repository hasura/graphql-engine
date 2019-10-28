# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine es un potente servidor GraphQL que provee de **APIs instantaneas y en tiempo real usando Postgres**, con [**disparadores de webhooks**](event-triggers.md) basados en los eventos de la base de datos, y [**esquemas remotos**](remote-schemas.md) para la lógica de negocio.

Hasura te ayuda a construir aplicaciones GraphQL respaldadas por Postgres o permite a aplicaciones construidas con Postgres en moverte gradualmente a GraphQL.

Lee mas en [hasura.io](https://hasura.io) y en [docs](https://docs.hasura.io).

------------------

![Hasura GraphQL Engine Demo](../assets/demo.gif)

------------------

![Hasura GraphQL Engine en tiempo real Demo](../assets/realtime.gif)

-------------------

## Características

* **Crea consultas poderosas**: Soprte para Filtrado, paginación, búsqueda de patrones, inserción masiva, actualización, eliminación de mutaciones.
* **En tiempo real**: Convierta cualquier consulta GraphQL en una consulta de datos en tiempo real mediante suscripciones.
* **Combina esquemas remotos**: Accede a esquemas personalizados de GraphQL para la lógica de negocio a través de un único punto de GraphQL Engine. [**Lee mas**](remote-schemas.md).
* **Desencadena webhooks o funciones serverless**: En Postgres, insertar/actualizar/eliminar eventos ([Lee mas](event-triggers.md))
* **Funciona con base de datos existentes y en uso**: Apúntalo a una base de datos Postgres existente para obtener instantáneamente una API GraphQL lista para usar.
* **Control de acceso Fino**: Control de acceso dinámico que se integra con su sistema de autenticación (por ejemplo: auth0, firebase-auth).
* **High-performance & low-footprint**: Imagen de Docker de ~15MB; ~50MB RAM @ 1000 req/s; trabaja en multinúcleo.
* **UI de administración & Migraciones**: UI de administración & Esquemas de migraciones inspirados en rails.
* **Postgres** ❤️: Soporte para los tipos de Postgres (PostGIS / geo-location, etc.), convierte las vistas en * gráficas *, activa funciones almacenadas o procedimientos con mutaciones

Lee mas en [hasura.io](https://hasura.io) y en [docs](https://docs.hasura.io).

## Tabla de contenido
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Hasura GraphQL Engine](#hasura-graphql-engine)
  - [Características](#caracter%c3%adsticas)
  - [Tabla de contenido](#tabla-de-contenido)
  - [Inicio rápido:](#inicio-r%c3%a1pido)
    - [Implementa con un clic en Heroku](#implementa-con-un-clic-en-heroku)
    - [Otras opciones de implementación a un clic](#otras-opciones-de-implementaci%c3%b3n-a-un-clic)
    - [Otros métodos de implementación](#otros-m%c3%a9todos-de-implementaci%c3%b3n)
  - [Arquitectura](#arquitectura)
  - [Herramientas del lado del cliente](#herramientas-del-lado-del-cliente)
  - [Agregar lógica de negocios](#agregar-l%c3%b3gica-de-negocios)
    - [Esquemas remotos](#esquemas-remotos)
    - [Desencadenar webhooks usando eventos de base de datos](#desencadenar-webhooks-usando-eventos-de-base-de-datos)
    - [Datos derivados o transformaciones de datos](#datos-derivados-o-transformaciones-de-datos)
  - [Demos](#demos)
    - [Aplicaciones en tiempo real](#aplicaciones-en-tiempo-real)
    - [Videos](#videos)
  - [Soporte & Solución de Problemas](#soporte--soluci%c3%b3n-de-problemas)
  - [Contribución](#contribuci%c3%b3n)
  - [Archivos de marca](#archivos-de-marca)
  - [License](#license)
  - [Traducciones](#traducciones)

<!-- markdown-toc end -->

## Inicio rápido:

### Implementa con un clic en Heroku

La forma más rápida de probar Hasura es a través de Heroku.

1. Haga clic en el siguiente botón para implementar GraphQL Engine en Heroku con el complemento gratuito Postgres:

    [![Implementación en Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Abre la consola de Hasura

   Visita `https://<app-name>.herokuapp.com` (*reemplaza \<app-name\> con el nombre de tu aplicación*) para abrir la consola de administración.

3. Haga su primera consulta GraphQL

   Crea una tabla y ejecuta instantáneamente su primera consulta. Sigue esta guía [guía simple](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Otras opciones de implementación a un clic

Consulta las instrucciones para las siguientes opciones de implementación a un clic:

| **Proveedor de infraestructura** | **Enlace de un clic** | **Información Adicional** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Implementación en DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Implementación en Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Otros métodos de implementación

Para la implementación basada en Docker y las opciones de configuración avanzadas, vea [guías de despliegue](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) or
[manifiesto de instalación](install-manifests).

## Arquitectura

El motor GraphQL de Hasura presenta una instancia de base de datos Postgres y puede aceptar solicitudes GraphQL de aplicaciones cliente. Se puede configurar para que funcione con un sistema de autenticación existente y puede manejar el control de acceso a nivel de campo con variables dinámicas del sistema de autenticación implementado.

También puedes fusionar esquemas GraphQL remotos y proporcionar una API GraphQL unificada.

![Arquitectura de Hasura GraphQL Engine](../assets/hasura-arch.svg)

## Herramientas del lado del cliente

Hasura funciona con cualquier cliente GraphQL. Recomendamos usar [Apollo Client](https://github.com/apollographql/apollo-client). Vea [awesome-graphql](https://github.com/chentsulin/awesome-graphql) para una lista de clientes.

## Agregar lógica de negocios

GraphQL Engine proporciona métodos fáciles de razonar, escalables y de alto rendimiento para agregar lógica de negocio personalizada a su servidor:

### Esquemas remotos

Agrega resolvedores personalizados mediante un esquema remoto adicionales al esquema de GraphQL basado en Postgres de Hasura. Ideal para casos de uso como implementar una API de pago o consultar datos que no están en tú base de datos, [Lee mas](remote-schemas.md).

### Desencadenar webhooks usando eventos de base de datos

Agrega la lógica de negocio de forma asíncrona, que es desencadenada por eventos de la base de datos. Ideal para notificaciones, canalizaciones de datos de Postgres o procesamiento asincrónico. - [Lee mas](event-triggers.md).

### Datos derivados o transformaciones de datos

Transforma los datos en Postgres o ejecuta la lógica de negocios sobre los mismos para derivar otro conjunto de datos que puedas consultar con GraphQL Engine - [Lee mas](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demos

Consulta todas las aplicaciones de ejemplo en el directorio
[community/examples](community/examples).

### Aplicaciones en tiempo real

- Aplicación de Chat en Grupo construida en React, incluye indicador de escritura, visualización de usuarios en linea & notificaciones de mensajes nuevos.
  - [Pruébalo](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Examina las APIs](https://realtime-chat.demo.hasura.app/console)

- Aplicación de rastreo en tiempo real, que muestra un vehículo en movimiento cambiando su ubicación GPS actual.
  coordinates moving on a map.
  - [Pruébalo](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-location-tracking)
  - [Examina las APIs](https://realtime-location-tracking.demo.hasura.app/console)

- Panel de control en tiempo real para agregaciones en datos que cambian continuamente.
  - [Pruébalo](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Examina las APIs](https://realtime-poll.demo.hasura.app/console)

### Videos

* [Agrega GraphQL a una instancia de GitLab auto hospedada](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Aplicación Todo con Auth0 y un servidor GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL en GitLab integrado con autenticación de GitLab](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Panel de control para 10 millones de viajes con geolocalización (PostGIS, Escala de tiempo)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## Soporte & Solución de Problemas

La documentación y la comunidad te ayudaran a solucionar la mayoria de los problemas que encuentres. Si has encontrado un error o necesitas ponerte en contacto con nosotros, puedes contactarnos usando uno de los siguientes canales:

* Soporte & retroalimentación: [Discord](https://discord.gg/vBPpJkS)
* Problemas & rastreo de errores: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Sigue las actualizaciones del producto en: [@HasuraHQ](https://twitter.com/hasurahq)
* Habla con nosotros en el [chat de nuestro sitio](https://hasura.io)

Estamos comprometidos en fomentar Un ambiente abierto y acogedor en la comunidad. Por fovor revisa el [Codigo de Conducta](code-of-conduct.md).

Si quieres reportar un fallo de seguridad, por favor [lee esto](SECURITY.md).

## Contribución

Checa nuestra [guía de contribución](CONTRIBUTING.md) para más detalles.

## Archivos de marca

Los archivos de la marca Hasura (logos, la mascota de Hasura, la insignia: desarrollado con, etc.) pueden encontrarse en el archivo [../assets/brand](assets/brand). Sientete libre de usarlos en tu applicación/sitio web etc. Estaremos encantados si agregas el distintivo "Desarrollado con Hasura" en tu aplicación construida usando Hasura. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Para fondos blancos -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- Para fondos negros -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## License

El núcleo de GraphQL Engine esta disponible bajo [Licencia Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Todo **el contenido restante** (exceptuando aquellos en los directorios de: [`server`](server), [`cli`](cli) y
[`console`](console)) están disponibles bajo [Licencia MIT](LICENSE-community).
Esto incluye todo lo que se encuentra en los directorios [`docs`](docs) y [`community`](community).

## Traducciones

Este archivo readme esta disponible en las siguientes traducciones:

- [Japones :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [Frances :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Griego 🇬🇷](translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [Español :es:](translations/README.spanish.md)

Las traducciónes de otros archivos pueden encontrarse: [aquí](translations).
