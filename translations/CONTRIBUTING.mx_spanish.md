# Contribuyendo a Hasura graphql-engine

_Primero_: Si te sientes inseguro acerca de cómo empezar a contribuir, no dudes en preguntarnos en nuestro [Canal de Discord](https://discordapp.com/invite/hasura) en el canal #contrib. También puedes seguir adelante con tu contribución y te daremos retroalimentación. No te preocupes, lo peor que puede suceder es que te pidan amablemente que cambies algo. Agradecemos cualquier contribución y no queremos que un muro de reglas se interponga en ese camino.

Sin embargo, para aquellas personas que deseen un poco más de orientación sobre la mejor manera de contribuir al proyecto, sigan leyendo. Este documento cubrirá lo que estamos buscando. Al abordar los puntos siguientes, las posibilidades de hacer un rapido merge o abordar tus contribuciones aumentará.

## Tabla de contenido

[1. Código de conducta](#code-of-conduct)

[2. Descripción general del repositorio](#overview)

[3. ¡Bienvenidos colaboradores por primera vez! ](#first-timers)

[4. Áreas para contribuir](#areas)

[5. Formas de contribuir](#ways)

[6. Confirmar mensajes](#commit-messages)

[7. Traducciones](#translations)

<a name="code-of-conduct"> </a>

## 1. Código de conducta

Siga nuestro [Código de conducta](code-of-conduct.md) en el contexto de cualquier contribución realizada a Hasura.

<a name="overview"> </a>

## 2. Descripción general del repositorio

[hasura / graphql-engine](https://github.com/hasura/graphql-engine) es un mono-repositorio constistiendo de 3 componentes. Cada uno tiene sus propias guías de contribución:

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

Los tres componentes tienen una única versión, indicada por la etiqueta git o una combinación de nombre de rama y git commit SHA.

Para todas las contribuciones, se debe firmar un CLA (Contributor License Agreement) [aquí](https://cla-assistant.io/hasura/graphql-engine) antes (o después) de que se haya enviado la solicitud de extracción. Un bot solicitará a los contribuyentes que firmen el CLA a través de un comentario de pull request, si es necesario.

<a name="first-timers"> </a>

## 3. ¡Bienvenidos los contribuyentes primerizos!

Agradecemos a contribuyentes primerizos y nos complace ayudarles a comenzar. En caso de preguntas, ¡comuníquese con nosotros!

Encontrará todos los problemas adecuados para quienes contribuyen por primera vez [aquí](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"> </a>

## 4. Áreas para contribuir

Por supuesto, agradecemos las contribuciones a todos los componentes de Hasura. Sin embargo, hemos identificado tres áreas que son particularmente adecuadas para las contribuciones de código abierto.

### Documentación

Nuestro objetivo es mantener nuestra documentación completa y actualizada. Si deseas ayudar, te agradecemos cualquier tipo de contribución:

- Reportar contenido faltante

- Corregir errores en documentación existente

- Ayúdanos a agregar documentación

La guía de contribución para la documentación se puede encontrar en [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

### Contenido de la comunidad

Desde que lanzamos nuestra [página de aprendizaje](https://hasura.io/learn/), estamos felices por las contribuciones:

- Corregir errores en tutoriales de aprendizaje existentes

- Agregue nuevos tutoriales (comuníquese con nosotros si tiene ideas para evitar palabras duplicadas)

El archivo README del repositorio de aprendizaje se puede encontrar [aquí](https://github.com/hasura/learn-graphql).

Además del contenido de aprendizaje, hemos identificado otras tres formas de contribuir con el contenido de la comunidad técnica:

- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Aplicaciones de muestra](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Herramientas](community/tools)

Si deseas contribuir a la comunidad

- escribiendo una entrada de blog técnica

- hablando en un evento

- organizando un taller

echa un vistazo a nuestra [wiki de la comunidad](https://github.com/hasura/graphql-engine/wiki/Community-Wiki).

No dude en enviar un pull request si tiene algo que agregar, incluso si no está relacionado con nada de lo mencionado anteriormente.

### Hasura CLI

Tenemos algunos problemas en la CLI que son adecuados para contribuciones de código abierto. Si conoce Go o si le gustaría aprenderlo con la práctica, consulte los siguientes [problemas](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

El archivo README del repositorio CLI se puede encontrar [aquí](https://github.com/hasura/graphql-engine/tree/master/cli).

<a name="ways"> </a>

## 5. Formas de contribuir

### Informar un problema

- Asegúrese de hacer pruebas con la última versión publicada. Es posible que ya hayamos solucionado el error que está experimentando.

- Proporcione los pasos para reproducir el problema, incluyendo la versión de Postgres,
  graphql-engine y el proveedor en el que se está ejecutando (Heroku, Docker, etc.).

- Incluya los registros del servidor, si procede.

### Trabajando en un problema

- Usamos el flujo de trabajo: [fork-and-branch git workflow.](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Asegúrese de que haya un problema asociado con el trabajo que está realizando.

- Si está trabajando en un problema, comente que lo está haciendo para evitar que otros también dupliquen el trabajo.

- Haga squash a sus commits y consulte el issue usando `fix # <issue-no>` o `close # <issue-no>` en el mensaje de confirmación, al final.
  Por ejemplo: `resolve answers to everything (fix #42)` o `resolve answers to everything, fix #42`

- Rebase master con su rama antes de enviar una solicitud de extracción.

<a name="commit-messages"> </a>

## 6. Confirmar mensajes

- La primera línea debe ser un resumen de los cambios, no más de 50
  caracteres, seguido de un cuerpo opcional que tiene más detalles sobre el
  cambios. Consulte [este enlace](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  para obtener más información sobre cómo escribir buenos mensajes de confirmación.

- Use el imperativo presente: "agregar / arreglar / cambiar", no "agregar / arreglar / cambiar" ni "agregar / arreglar / cambiar".

- No escriba en mayúscula la primera letra de la línea de resumen.

- No agregue un punto / punto (.) Al final de la línea de resumen.

<a name="translations"> </a>

## 7. Traducciones

Este documento está disponible en las siguientes traducciones:

- [Ingles](../CONTRIBUTING.md)
- [Francés](./CONTRIBUTING.french.md)
- [Español](./CONTRIBUTING.mx_spanish.md)

(Créditos: algunas secciones están adaptadas de https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
