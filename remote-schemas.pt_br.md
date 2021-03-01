# Esquemas remotos

Faça um _merge_ remoto de esquemas GraphQL com o esquema GraphQL Engine's baseado no Postgres para uma _query_ de todos os 
seus tipos GraphQL do mesmo _endpoint_. Esquemas remotos são ideais para os seguintes casos:

* Customizando mutações (ex: rodar validações antes de _inserts*_)
* Suporta funcionalidade como pagamentos e etc. providionando uma consistente interface para acessá-los
por trás da API GraphQL Engine
* Buscando dados diferentes de outras fontes (ex: de uma API climática ou outra base de dados*)

Para suportar lógica de negócio customizada você precisa criar um servidor GraphQL personalizado
(veja [boilerplates](community/boilerplates/remote-schemas)) e dar um _merge_ do seu esquema com o GraphQL Engine.

![Arquitetura de esquemas remotos](assets/remote-schemas-arch.png)

## Demo (*40 segundos*)

[![video de demonstração de _merge_ de esquemas remotos](https://img.youtube.com/vi/eY4n9aPsi0M/0.jpg)](https://www.youtube.com/watch?v=eY4n9aPsi0M)

[_Merge_ remoto de esquemas GraphQL (YouTube link)](https://youtu.be/eY4n9aPsi0M)

## Início rápido

A maneira mais rápida de tentar utilizar um esquema remoto é via Heroku.

1. Clique no botão a seguir para fazer um deploy do GraphQL Engine no Heroku com o add-on gratuito do Postgres:

    [![Deploy no Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Abra o console Hasura

   Visite `https://<nome-do-app>.herokuapp.com` (*substitua \<nome-do-app\> pelo nome de sua aplicação*) 
   para abrir o console _admin_.

3. Faça um _merge_ do seu primeiro esquema remoto e faça uma _query_ nele

   No console _admin_, abra a guia do ``Remote Schemas`` e  clique no botão ``Add``. Preencha com os detalhes a seguir:
   * Nome do _Remote Schema_: ``countries`` (*um alias para esse esquema remoto*).
   * URL do servidor GraphQL: ``https://countries.trevorblades.com/`` (*uma API pública do GraphQL que nós usaremos para conferir rapidamente essa funcionalidade; mantido por [@trevorblades](https://github.com/trevorblades)*. 
   * Ignore configurações restantes e clique no botão ``Add Remote Schema``.

   Navegue até a guia ``GraphiQL`` e rode a _query_ a seguir (cole-a na janela da _query_ à esqueda e clique no botão _play_ ▶️):

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

  Você pode explorar os tipos GraphQL do esquema remoto utilizando o explorador ``Docs`` no canto superior direito da 
  interface ``GraphiQL``.

## Boilerplates

_Boilerplates_ para servidores GraphQL em linguagens/_frameworks_  populares estão disponíveis.

* [_Boilerplates_ regulares](community/boilerplates/graphql-servers) que podem ser implementados em qualquer lugar.
* [_Serverless boilerplates_](https://github.com/hasura/graphql-serverless) que podem ser implementados em plataformas _serverless_ como AWS Lambda, etc.

Por favor note que os _boilerplates_ para mais linguagens, _frameworks_, plataformas _serverless_, etc. estão sendo complementadas e as contribuições da comunidade são muito bem vindas. 


## Ressalvas

**Limitações atuais**:

* Nomenclatura: nomes _type_ e nomes _node_ precisam ser únicos dentre todos os esquemas "_mergeados_" (combinação sensível à maiúsculas e minúsculasas). Nas próximas iterações, o suporte aos _merging types_ com o mesmo nome e estrutura estarão disponíveis.
* Nós de diferentes servidores GraphQL não podem ser utilizados com a mesma _query_/mutação. Todos os nós de alto nível devem ser do mesmo servidor GraphQL.
* Assinaturas nos servidores GraphQL remotos não são suportadas.

Essas limitações serão revistas em versões posteriores.

## Documentação

Leia a [documentação](https://docs.hasura.io/1.0/graphql/manual/remote-schemas/index.html) completa.

## Translations

Esse documento está disponível nas seguintes traduções:

- [Francês :fr:](translations/remote-schemas.french.md)
- [Hindu  :india:](translations/remote-schemas.hindi.md)
