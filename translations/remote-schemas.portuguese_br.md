# Esquemas remotos

Hasura oferece CRUD + APIs de GraphQL em tempo real com autorização e controle de acesso. No entanto, em muitos casos, você precisará escrever APIs (consultas, mutações) que contenham lógica personalizada. Por exemplo, implementar uma API de pagamento ou consultar dados que não estão em seu banco de dados.

Hasura tem a capacidade de mesclar esquemas GraphQL remotos e fornecer uma API GraphQL unificada. Pense nisso como uma fusão de esquema automatizada. Tudo o que você precisa fazer é construir seu próprio serviço GraphQL e fornecer o endpoint HTTP para Hasura. Seu serviço GraphQL pode ser escrito em qualquer linguagem ou estrutura.

Os esquemas remotos são ideais para casos de uso como:

* Personalização de mutações (*por exemplo, validações em execução antes de inserções*)
* Suporte a recursos como pagamentos, etc. e fornece uma interface consistente para acessá-los, por exemplo, por trás da API do GraphQL Engine
* Buscar dados díspares de outras fontes (*por exemplo, de uma API de clima ou outro banco de dados*)

Para oferecer suporte à lógica de negócios personalizada, você precisará criar um servidor GraphQL personalizado (consulte [boilerplates](community / boilerplates / remote-schemas)) e mesclar seu esquema com o GraphQL Engine.

![Arquitetura de esquemas remotos](assets / remote-schemas-arch.png)

## Demo (*40 segundos*)

[![Demonstração em vídeo da fusão de esquemas remotos](https://img.youtube.com/vi/eY4n9aPsi0M/0.jpg)](https://www.youtube.com/watch?v=eY4n9aPsi0M)

[Mesclar esquemas GraphQL remotos (link do YouTube)](https://youtu.be/eY4n9aPsi0M)

[Adicionando um esquema remoto](https://youtu.be/01t4t2t4q1c)

## Começo rápido

A maneira mais rápida de testar o esquema remoto é via Heroku.

1. Clique no botão a seguir para implantar o GraphQL Engine no Heroku com o add-on gratuito do Postgres:

    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql- motor-heroku)

2. Abra o console Hasura

   Visite `https://<app-name>.herokuapp.com` (*substitua \<app-name\> pelo nome do seu aplicativo*) para abrir o console de administração.

3. Combine seu primeiro esquema remoto e consulte-o

   No console de administração, abra a guia ``Esquemas remotos`` e clique no botão ``Adicionar``. Preencha os seguintes detalhes:
   * Nome do esquema remoto: ``países`` (*um apelido para este esquema remoto*).
   * URL do servidor GraphQL: ``https://countries.trevorblades.com/``(*uma API GraphQL pública que usaremos para verificar rapidamente este recurso; mantida por [@trevorblades](https://github.com/trevorblades)*.
   * Ignore as configurações restantes e clique no botão ``Add Remote Schema``.

   Vá para a guia ``GraphiQL` e execute a seguinte consulta (*cole-o na janela de consulta à esquerda e clique no botão* ▶️ * (reproduzir)*):

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

   Você pode explorar os tipos GraphQL do esquema remoto usando o explorador ``Docs`` no canto superior direito da interface ``GraphiQL``.

## Boilerplates

Boilerplates para servidores GraphQL personalizados em linguagens/estruturas populares estão disponíveis.

* [Boilerplates comuns](community/boilerplates/graphql-servers) que podem ser implantados em qualquer lugar.
* [Boilerplates sem servidor](https://github.com/hasura/graphql-serverless) que podem ser implantados em plataformas sem servidor como AWS Lambda, etc.

Observe que padrões para mais linguagens, frameworks, plataformas sem servidor, etc. estão sendo iterados e as contribuições da comunidade são muito bem-vindas.


## Ressalvas

**Limitações atuais**:

* Nomenclatura: os nomes dos tipos e dos nós precisam ser exclusivos em todos os esquemas mesclados (correspondência com distinção entre maiúsculas e minúsculas). Nas próximas iterações, o suporte para mesclar tipos com o mesmo nome e estrutura estará disponível.
* Nós de diferentes servidores GraphQL não podem ser usados ​​na mesma consulta/mutação. Todos os nós de nível superior devem ser do mesmo servidor GraphQL.
* As assinaturas no servidor GraphQL remoto não são suportadas.

Essas limitações serão abordadas nas próximas versões.

## Documentação

Leia a [documentação] completa (https://hasura.io/docs/1.0/graphql/manual/remote-schemas/index.html).
