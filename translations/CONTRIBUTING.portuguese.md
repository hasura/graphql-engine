# Contributing to Hasura graphql-engine

*Antes de mais nada*: se você sente insegurança sobre como começar a contribuir, fique à vontade de nos perguntar em nosso canal do [Discord channel](https://discordapp.com/invite/hasura), no canal #contrib ou mandando uma DM para Marion (@marion#9554). Você também pode contribuir diretamente e nós iremos te dar um  _feedback_. Não se preocupe, o máximo que pode acontecer é você ser gentilmente solicitado a mudar alguma coisa. Nós agradecemos quaisquer contribuições, e nós não queremos um monte de regras atrapalhando o caminho delas.

No entando, para aqueles que gostariam de um pouco mais de detalhes quanto à melhor forma de contribuir para o projeto, leia a seguir. Esse ocumento irá cobrir os tópicos que buscamos. Sendo fiel aos tópicos abaixo, as chances de nós 
podermos rapidamente dar um _merge_ ou endereçar suas contribuições, vão aumentar.

## Overview

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) é um _mono-repositório- composto de 
3 componentes. Cada um tem seus respectivos guias de contribuição:

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

Todos o três componentes possuem uma versão _single_, demarcados ou pelo _git tag_ ou pela combinação entre o nome da _branch_ e o _git commit SHA_.

Para todas as contribuições um _CLA (Contributor License Agreement)_ precisa ser assinado [aqui](https://cla-assistant.io/hasura/graphql-engine) antes (ou depois) do _pull request_ ser feito. Um _bot_ irá solicitar aos contribuidores(as) para assinar o CLA via um comentário no _pull request_, se necessário.

## Bem vindos contribuidores(as) de primeira viagem!

Nós agradecemos aos contribuidores(as) de primeira viagem e ficamos felizes em ajudá-los(as) a começar. Em caso de dúvidas, basta nos contactar!

Você emontrará todas as _issues_ adequadas à contribuidores(as) de primeira viagem [aqui](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

## Áreas onde contribuir

É claro que agradecemos às contribuiçõs a todos os componentes do Hasura. No entanto, identificamos três areas que são particularmente de acordo para contribuições _open source_.

### Documentação

Nosso objetivo é manter nossa documentação atualizada e compreensível. Se deseja nos ajudar com isso, somos gratos em qualquer tipo de contribuição:

- Aviso de conteúdo faltando

- Consertar erros na documentação existente

- Ajudar-nos à complementar a documentação

O guia para contribuir com documentação está em [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).


### Conteúdo da comunidade

Encontramos outras três formas de contribuir com o conteúdo da comunidade:

- [boilerplates](community/boilerplates)

- [sample-apps](community/sample-apps) 

- [tools](community/tools) 

Fique à vontade de mandar um _pull request_ se tiver algo para adicionar, mesmo que não esteja relacionado com os três tópicos acima.

### Hasura CLI

Temos algumas _issues_ no CLI que se encaixam em contribuições _open source_. se você souber Go ou se quiser aprender pela prática, confira essas [issues](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

O README do repositório CLI pode ser encontrado [aqui](https://github.com/hasura/graphql-engine/tree/master/cli).

## Formas de contribuir

### Reportando uma _issue_

- Tenha certeza que seu teste está rodando na versão atual. É possível que já tenhamos consertado o erro que está relatando.

- Nos forneça os passos necessários para reproduzir o problema, incluindo a versão do Postgres,
  versão do _graphql-engine_ e seu provedor de infraestrutura (Heroku, Docker, etc.).

- Por favor inclua os logs do servidor, se forem relevantes.

### Trabalhano em uma _issue_

- Por favor esteja certo de que existe um problema associado ao seu trabalho.

- Se estiver trabalhando em uma _issue_, por favor escreva um comentário para que saibamos e evitemos duplicidade de trabalho para outras pessoas.

- Dê um _squash_ em seus _commits_ e refira-se ao problema usando `fix #<issue-no>` ou `close
  #<issue-no>` na mensagem do _commit_, ao final.
  Por exemplo: `resolve answers to everything (fix #42)` ou `resolve answers to everything, fix #42`

- Dê um _Rebase master_ com sua branch antes de mandar um _pull request_.

## Mensagens do Commit 

- A primeira linha deve ser um sumário das mudanças, sem ultrapassar 50 caracteres, 
  seguidos por um texto adicional contendo maiores detalhes acerca das mudanças. 
  Acesse [aqui](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  para maiores informações em como escrever boas mensagens de _commit_.

- Use o presente do imperativo: "add/fix/change", e não "added/fixed/changed" e nem "adds/fixes/changes".

- Não use maiúscula na primeira letra na linha do sumário.

- Não coloque ponto (.) ao final da linha do sumário.

## Traduções

Esse documento está disponível em:

- [French :fr:](translations/CONTRIBUTING.french.md)


(Creditos: Algumas seções estão adaptadas do https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
