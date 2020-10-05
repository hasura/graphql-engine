# Contribuindo com Hasura graphql-engine

_Primeiramente_: se você se sente inseguro em começar a contribuir, fique à vontade para nos perguntar no nosso [Discord](https://discordapp.com/invite/hasura) no canal #contrib. Você também pode seguir adiante com a sua contribuição e nós daremos nosso feedback. Não se preocupe - a pior coisa que pode acontecer é você ser educadamente pedido para mudar alguma coisa. Nós agradecemos qualquer contribuição, e não queremos uma muralha de regras impedindo o caminho.

Porém, para as pessoas que querem mais detalhes na melhor maneira de contribuir com o projeto, continue lendo. Esse documento cobre o que estamos procurando. Lendo os pontos abaixo, as chances de aprovar ou discutir sobre suas contribuições aumentarão.

## Sumário

[1. Código de conduta](#code-of-conduct)

[2. Visão geral do repositório](#overview)

[3. Contribuidores de primeira viagem são bem vindos!](#first-timers)

[4. Áreas para contribuições](#areas)

[5. Maneiras de contribuir](#ways)

[6. Mensagens de commit](#commit-messages)

[7. Traduções](#translations)

<a name="code-of-conduct"></a>

## 1. Código de conduta

Por favor, siga nosso [Código de conduta](./code-of-conduct.portuguese_br.md) no que se tratar de contribuições feitas ao Hasura.

<a name="overview"></a>

## 2. Visão geral do repositório

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) é um mono-repo que consiste de 3 componentes. Cada um deles tem seu próprio guia de contribuição:

1. [Server (Haskell)](../server/CONTRIBUTING.md)
2. [CLI (Go)](../cli/CONTRIBUTING.md)
3. [Console (JavaScript)](../console/README.md#contributing-to-hasura-console)

Todos os 3 componentes tem uma única versão, indicada ou pela tag do Git ou pela combinação do nome da branch e do SHA do commit do Git.

Para todas as contribuições, um TLC (Termo de Licença de Contribuidor) precisa ser assinado [aqui](https://cla-assistant.io/hasura/graphql-engine) antes (ou depois) que o pull request seja enviado. Um bot irá solicitar aos contribuidores que assinem o TLC através de um pull request, se for necessário.

<a name="first-timers"></a>

## 3. Contribuidores de primeira viagem são bem vindos!

Nós gostamos de contribuidores de primeira viagem e ficamos felizes em ajudar a começar. Em caso de dúvidas, nos pergunte!

Você encontrará todas as issues adequadas aos contribuidores de primeira viagem [aqui](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. Áreas para contribuições

Obviamente, aceitamos contribuições em todos os componentes do Hasura. Porém, identificamos três áreas que são particularmente adequadas a contribuições open source.

### Documentação

Nosso objetivo é manter nossa documentação atualizada e fácil de entender. Se você quiser nos ajudar, ficaremos gratos com qualquer tipo de contribuição:

- Sinalizar conteúdo faltando

- Corrigir erros na documentação existente

- Ajudar a adicionar conteúdo na documentação

O guia de contribuição para documentação pode ser lido em [docs/CONTRIBUTING.md](../docs/CONTRIBUTING.md).

### Conteúdo comunitário

Agora que lançamos nossa [página de aprendizado](https://hasura.io/learn/), ficamos felizes em receber contribuições sobre:

- Correções de erros em tutoriais de aprendizado existentes
  
- Novos tutoriais (por favor, nos procure se você tiver ideias para evitar duplicação de palavras)
  
O arquivo README do repositório de aprendizado pode ser lido [aqui](https://github.com/hasura/learn-graphql).

Além do conteúdo de aprendizado, nós identificamos outras três maneiras de contribuir com conteúdo técnico comunitário:

- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Aplicações de exemplo](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Ferramentas](community/tools) 

Se você gostaria de contribuir com a comunidade

- escrevendo um post de blog técnico

- palestrando em um evento
  
- organizando um workshop

confira nossa [wiki da comunidade](https://github.com/hasura/graphql-engine/wiki/Community-Wiki).

Sinta-se livre para enviar um pull request se você tiver algo a contribuir, mesmo que não seja relacionado aos pontos citados acima.

### Hasura CLI

Nós temos algumas questões relacionadas ao CLI que são adequadas para contribuição open source. Se você sabe Go ou gostaria de aprender fazendo, confira essas [issues](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

O arquivo README do repositório do CLI pode ser lido [aqui](https://github.com/hasura/graphql-engine/tree/master/cli).

<a name="ways"></a>

## 5. Maneiras de contribuir

### Relatando um problema

- Tenha certeza de que está testando na última versão lançada. É possível que nós já tenhamos corrigido o problema que você encontrou.

- Forneça um passo a passo para reproduzir o erro, incluindo versão do Postgres, versão do graphql-engine e o servidor em que você está rodando a aplicação (Heroku, Docker, etc.).

- Inclua os logs do servidor, caso eles sejam relevantes.

### Trabalhando em uma issue

- Nós trabalhamos com [fork e branches do Git](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Tenha certeza de que existe uma issue associada com o que você está fazendo.

- Se você estiver trabalhando em uma issue, não esqueça de avisar, para evitar trabalho duplicado.

- Faça squash dos seus commits e se refira à issue usando `fix #<número da issue>` ou `close #<número da issue>` no final da mensagem de commit. As mensagens de commit devem ser em inglês. 
  Por exemplo: `resolve answers to everything (fix #42)` ou `resolve answers to everything, fix #42`. 

- Faça rebase da sua branch com a master antes de enviar seu pull request.

<a name="commit-messages"></a>

## 6. Mensagens de commit

- A primeira linha da mensagem de commit deve ser um resumo das mudanças feitas, sem exceder 50 caracteres, seguido por um detalhamento opcional das mudanças. Use como referência [este link](https://github.com/erlang/otp/wiki/writing-good-commit-messages) para saber mais sobre boas mensagens de commit.

- Utilize, em inglês, palavras imperativas: "add/fix/change" (adiciona/corrige/muda), e não "added/fixed/changed" (adicionado/corrigido/mudado) ou "adds/fixes/changes" (adicionando/corrigindo/mudando).

- Não utilize letra maiúscula na primeira letra do resumo na mensagem de commit.

- Não utilize ponto final no resumo na mensagem de commit.

<a name="translations"></a>

## 7. Traduções

Este documento está disponível nas seguintes traduções:
 - [Inglês 🇺🇸](../CONTRIBUTING.md)
 - [Francês 🇫🇷](./CONTRIBUTING.french.md)

(Créditos: Alguns trechos foram adaptados de https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
