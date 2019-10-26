# Hasura GraphQL Engine

[![Docs](https://img.shields.io/badge/docs-v1.0-brightgreen.svg?style=flat)](https://docs.hasura.io)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL Engine es un servidor GraphQL ultrarrápido que le ofrece **API GraphQL instantáneas y en tiempo real sobre Postgres**, con [**activadores de webhook**](../event-triggers.md) en eventos de base de datos y [**remoto esquemas**](../remote-schemas.md) para lógica de negocios.

Hasura lo ayuda a construir aplicaciones GraphQL en PostgreSQL o migrar gradualmente a GraphQL para aplicaciones existentes que usan Postgres.

Lea más en [hasura.io](https://hasura.io) y en [docs](https://docs.hasura.io).

------------------

![Demostración del motor Hasura GraphQL](../assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](../assets/realtime.gif)

-------------------

## Caracteristicas

* **Realice consultas potentes**: filtrado incorporado, paginación, búsqueda de patrones, inserción masiva, actualización, eliminación de mutaciones
* **Realtime**: Convierta cualquier consulta GraphQL en una consulta en vivo mediante suscripciones
* **Combinar esquemas remotos**: Acceda a esquemas personalizados de GraphQL para la lógica empresarial a través de un único punto final de GraphQL Engine. [**Leer más**](../remote-schemas.md).
* **Activar webhooks o funciones sin servidor**: En Postgres, insertar/actualizar/eliminar eventos ([leer más](../event-triggers.md))
* **Funciona con bases de datos en vivo existentes**: apúntelo a una base de datos de Postgres existente para obtener instantáneamente una API GraphQL lista para usar
* **Control de acceso de grano fino**: control de acceso dinámico que se integra con su sistema de autenticación (por ejemplo: auth0, firebase-auth)
* **Alto rendimiento y baja huella**: ~ 15 MB de imagen acoplable; ~50MB RAM @ 1000 req/s; multinúcleo consciente
* **UI de administrador y migraciones**: UI de administrador y migraciones de esquemas inspiradas en Rails
* **Postgres** ❤️: admite los tipos de Postgres (PostGIS/geo-location, etc.), convierte las vistas en *gráficos*, activa funciones almacenadas o procedimientos con mutaciones

Lea más en [hasura.io](https://hasura.io) y en [docs](https://docs.hasura.io).

## Tabla de contenido
<!-- markdown-toc start - No edite esta sección. Ejecute M-x markdown-toc-refresh-toc -->
**Tabla de contenido**

- [Inicio rápido:](#inicio-rápido)
    - [Implementación con un clic en Heroku](#despliegue-con-un-clic-en-heroku)
    - [Otros métodos de implementación](#otros-métodos-de-implementación)
- [Arquitectura](#arquitectura)
- [Herramientas del lado del cliente](#herramientas-del-lado-del-cliente)
- [Agregar lógica de negocios](#añadir-lógica-de-negocios)
    - [Esquemas remotos](#esquemas-remotos)
    - [Activar webhooks en eventos de base de datos](#activar-webhooks-en-eventos-de-base-de-datos)
- [Demos](#demos)
    - [Aplicaciones en tiempo real](#aplicaciones-en-tiempo-real)
    - [Videos](#videos)
- [Soporte y solución de problemas](#soporte-y-solución-de-problemas)
- [Contribuyendo](#contribuyendo)
- [Activos de marca](#activos-de-marca)
- [Licencia](#licencia)
- [Traducciones](#traducciones)

<!-- markdown-toc end -->

## Inicio rápido:

### Despliegue con un clic en Heroku

La forma más rápida de probar Hasura es a través de Heroku.

1. Haga clic en el siguiente botón para implementar GraphQL Engine en Heroku con el complemento gratuito Postgres:

    [![Implementar en Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Abra la consola de Hasura

   Visite `https://<app-name>.herokuapp.com` (*reemplace \<app-name\> con el nombre de su aplicación*) para abrir la consola de administración.

3. Haga su primera consulta GraphQL

   Cree una tabla y ejecute instantáneamente su primera consulta. Siga esta [guía simple](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Otras opciones de implementación con un clic

Consulte las instrucciones para las siguientes opciones de implementación con un clic:

| **Proveedor de infraestructura** | **Enlace de un clic** | **Información adicional** |
|:-------------------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| DigitalOcean | [![Implementar en DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Implementar en Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |

### Otros métodos de implementación

Para la implementación basada en Docker y las opciones de configuración avanzadas, consulte [implementación guías](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html) o
[instalar manifiestos](../install-manifests).

## Arquitectura

El motor GraphQL de Hasura presenta una instancia de base de datos Postgres y puede aceptar solicitudes GraphQL de sus aplicaciones cliente. Se puede configurar para que funcione con su sistema de autenticación existente y puede manejar el control de acceso utilizando reglas de nivel de campo con variables dinámicas de su sistema de autenticación.

También puede fusionar esquemas GraphQL remotos y proporcionar una API GraphQL unificada.

![Arquitectura del motor Hasura GraphQL](../assets/hasura-arch.svg)

## Herramientas del lado del cliente

Hasura funciona con cualquier cliente GraphQL. Recomendamos utilizar [Apollo Client](https://github.com/apollographql/apollo-client). Consulte [awesome-graphql](https://github.com/chentsulin/awesome-graphql) para obtener una lista de clientes.

## Añadir lógica de negocios

GraphQL Engine proporciona métodos fáciles de razonar, escalables y de rendimiento para agregar lógica empresarial personalizada a su backend:

### Esquemas remotos

Agregue resolvers personalizados en un esquema remoto además del esquema GraphQL basado en Postgres de Hasura. Ideal para casos de uso como implementar una API de pago o consultar datos que no están en su base de datos - [leer más](../remote-schemas.md).

### Activar webhooks en eventos de base de datos

Agregue lógica de negocios asíncrona que se desencadena en función de los eventos de la base de datos.
Ideal para notificaciones, canalizaciones de datos de Postgres o asíncronas.
procesamiento - [leer más](../event-triggers.md).

### Datos derivados o transformaciones de datos

Transforme los datos en Postgres o ejecute la lógica de negocios en ellos para derivar otro conjunto de datos que se pueda consultar con GraphQL Engine - [leer más](https://docs.hasura.io/1.0/graphql/manual/queries/derived-data.html).

## Demos

Consulte todas las aplicaciones de ejemplo en
Directorio [community/examples](../community/examples).

### Aplicaciones en tiempo real

- La aplicación Group Chat creada con React, incluye un indicador de escritura, usuarios en línea y nuevos
  notificaciones de mensajes.
  - [Pruébalo](https://realtime-chat.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-chat)
  - [Examinar API](https://realtime-chat.demo.hasura.app/console)

- Aplicación de seguimiento de ubicación en vivo que muestra un vehículo en marcha cambiando el GPS actual
  coordenadas moviéndose en un mapa.
  - [Pruébalo](https://realtime-location-tracking.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-location-tracking)
  - [Examinar API](https://realtime-location-tracking.demo.hasura.app/console)
 
- Un panel de control en tiempo real para agregaciones de datos en datos que cambian continuamente.
  - [Pruébalo](https://realtime-poll.demo.hasura.app/)
  - [Tutorial](community/sample-apps/realtime-poll)
  - [Examinar API](https://realtime-poll.demo.hasura.app/console)
    
### Videos

* [Agregue GraphQL a una instancia de GitLab autohospedada](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Aplicación Todo con Auth0 y backend GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GraphQL en GitLab integrado con GitLab auth](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [Panel de control para 10 millones de viajes con ubicación geográfica (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## Soporte y solución de problemas

La documentación y la comunidad lo ayudarán a solucionar la mayoría de los problemas. Si ha encontrado un error o necesita ponerse en contacto con nosotros, puede contactarnos a través de uno de los siguientes canales:

* Soporte y comentarios: [Discordia](https://discord.gg/vBPpJkS)
* Seguimiento de problemas y errores: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* Siga las actualizaciones del producto: [@HasuraHQ](https://twitter.com/hasurahq)
* Hable con nosotros en nuestro [chat del sitio web](https://hasura.io)

Estamos comprometidos a fomentar un ambiente abierto y acogedor en la comunidad. Consulte el [Código de conducta](../code-of-conduct.md).

Si desea informar un problema de seguridad, [lea esto](../SECURITY.md).

## Contribuyendo

Consulte nuestra [guía contribuyente](../CONTRIBUTING.md) para obtener más detalles.

## Activos de marca

Los activos de la marca Hasura (logotipos, la mascota de Hasura, alimentados por insignias, etc.) pueden ser
encontrado en la carpeta [assets/brand](../assets/brand). Siéntase libre de usarlos en su
application/website, etc. Estaríamos encantados si agrega el "Desarrollado por Hasura"
insignia para sus aplicaciones creadas con Hasura. ❤️

<div style="display: flex;">
  <img src="../assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="../assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- Para fondos claros -->
<a href="https://hasura.io">
  <img width = "150px" src = "https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- Para fondos oscuros -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## Licencia

El núcleo GraphQL Engine está disponible bajo la [Licencia Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

Todos los **otros contenidos** (excepto aquellos en [`server`](../server), [`cli`](../cli) y
Los directorios [`consola`](../console) están disponibles bajo la [Licencia MIT](../LICENSE-community).
Esto incluye todo en [`docs`](../docs) y [`community`](../community)
directorios.

## Traducciones

Este archivo Léame está disponible en las siguientes traducciones:

- [Japonés :jp:](../translations/README.japanese.md) (:orar: [@moksahero](https://github.com/moksahero))
- [Francés :fr:](../translations/README.french.md) (:orar: [@l0ck3](https://github.com/l0ck3))
- [Español :es:](../translations/README.spanish.md) (:orar: [@parammittal16](https://github.com/parammittal16))

Se pueden encontrar traducciones para otros archivos [aquí](../translations).
