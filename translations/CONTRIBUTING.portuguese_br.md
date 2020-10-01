# Contribuindo para Hasura graphql-engine

_Primeiramente_: se você se sentir inseguro sobre como começar contribuindo, sinta-se á vontade de nos perguntar no nosso [Canal no Discord](https://discordapp.com/invite/hasura), tanto no canal #contrib quanto como uma DM (mensagem direta) para Marion (@marion#9554) (em inglês). Você também pode simplesmente seguir com sua contribuição, e lhe daremos um feedback. Não se preocupe - o pior que pode acontecer é que você seja educadamente solicitado para mudar alguma coisa. Nós adoramos qualquer contribuição, e não queremos uma parede de regras no caminho.

De qualquer forma, para aqueles que queiram uma melhor orientação na melhor forma de contribuir com o projeto, continue lendo. Esse documento irá cobrir o que nós estamos buscando. Levando em consideração os pontos abaixo, as chances de que nós rapidamente mergeemos ou levemos suas contribuições em consideração irá aumentar.

## Tabela de conteúdos

[1. Código de conduta ](#code-of-conduct)

[2. Visão Geral ](#overview)

[3. Boas-vindas aos contribuidores de primeira viagem! ](#first-timers)

[4. Áreas para Contribuição ](#areas)

[5. Maneiras de contribuir ](#ways)

[6. Mensagens de commit ](#commit-messages)

[7. Traduções ](#translations)

<a name="code-of-conduct"></a>

## 1. Código de conduta

Por favor, siga nosso [Código de conduta](translations/code-of-conduct.portuguese_br.md) no contexto de qualquer contribuição para Hasura.

<a name="overview"></a>

## 2. Visão Geral

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) é um monorepo consistindo de 3 componentes. Cada um tem seus próprios guias de contribuição (em inglês):

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

Todos os três componentes tem uma única versão, simbolizada pela git tag ou pela combinação do nome da branch e do SHA do commit.

Para todos os contribuidores, o CLA (Contributor License Agreement - Acordo de Licença do Contribuidor) precisa ser assinado [aqui](https://cla-assistant.io/hasura/graphql-engine) antes (ou depois) do pull request ter sido submetido. Um bot irá solicitar que o contribuidor assine o CLA através de um comentário no pull request, caso seja necessário.

<a name="first-timers"></a>

## 3. Boas-vindas aos contribuidores de primeira viagem!

Nós apreciamos contribuidores de primeira viagem e ficaremos feliz de ajudá-lo na sua iniciação. Em caso de dúvidas, contate-nos!

Você encontrará issues (problemas) apropriadas para novos contribuidores [aqui](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. Áreas para Contribuição

É claro, adoramos contribuições para todos os componentes do Hasura. Porém, identificamos 3 áreas que se encaixam perfeitamente para contribuições open source.

### Documentação

Nossa meta é manter nossa documentação compreensiva e atualizada. Se você gostaria de nos ajudar com isso, somos gratos por qualquer tipo de contribuição:

- Reportar conteúdo faltante

- Corrigir erros em documentação já existente

- Nos ajudar adicionando nova documentação

O guia de contribuição para documentação pode ser encontrado em [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md) (em inglês).

### Conteúdo da comunidade

Desde que lançamos nossa [página de aprendizado](https://hasura.io/learn/), nós estamos felizes com contribuições que:

- Corrigem erros em tutoriais de aprendizado já existentes

- Adicionam novos tutoriais (por favor, nos contate se você tem ideias de como evitar duplicação de palavras)

O README do repositório de aprendizado pode ser encontrado [aqui](https://github.com/hasura/learn-graphql) (em inglês).

Além do conteúdo de aprendizado, identificamos três outras formas de contribuir com conteúdo técnico para a comunidade:

- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Exemplos de apps](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Ferramentas](community/tools)

Se você gostaria de contribuir para a comunidade:

- escrevendo um post técnico para um blog;

- palestrar em um evento;

- organizar um workshop;

cheque nossa [wiki da comunidade](https://github.com/hasura/graphql-engine/wiki/Community-Wiki) (em inglês).

Sinta-se à vontade para criar um pull request se você possui algo para adicionar, mesmo que não seja relacionado a algo mencionado acima.

### CLI Hasura

Nós temos algumas issues (problemas) na nossa CLI (Interface de Linha de Comando) que se encaixam em contribuições open source. Se você sabe Go, ou gostaria de aprender na prática, cheque as seguintes [issues](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

O README do repositório da CLI pode ser encontrado [aqui](https://github.com/hasura/graphql-engine/tree/master/cli) (em inglês).

<a name="ways"></a>

## 5. Maneiras de contribuir

### Criando uma issue

- Tenha certeza de que você testou com a última versão lançado. É possível que já tenhamos corrigido o bug que você está enfrentando.

- Forneça um passo-a-passo de como reproduzir o problema, incluindo a versão do Postgres, a versão do graphql-engine e o provedor que você está usando (Heroku, Docker, etc.).

- Por favor, inclua os logs do servidor, se relevante.

### Trabalhando com uma issue

- Utilizamos o [workflow do git fork-and-branch](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Tenha certeza de que existe uma issue associada ao trabalho que você está fazendo.

- Se você estiver trabalhando em uma issue, por favor comente que está o fazendo, para evitar trabalho duplicado por outras pessoas.

- Faça squash nos seus commits e referencie a issue usando `fix #<issue-no>` ou `close #<issue-no>` no final da sua mensagem de commit.
  Por exemplo: `resolve answers to everything (fix #42)` or `resolve answers to everything, fix #42`, lembrando de manter os commits em inglês.

- Faça rebase da master com sua branch, antes de enviar o pull request.

<a name="commit-messages"></a>

## 6. Mensagens de commit

- A primeira linha deve ser um sumário das mudanças, não excedendo 50 caracteres, seguido de um corpo opcional com mais detalhes sobre as mudanças. Confira [esse link](https://github.com/erlang/otp/wiki/writing-good-commit-messages) (em inglês) para mais informações sobre como escrever boas mensagens de commit.

- Use o tempo presente imperativo, e em inglês: "add/fix/change", e não "added/fixed/changed" nem "adds/fixes/changes".

- Não capitalize a primeira letra da linha de sumário.

- Não adicione um ponto final (.) no final da linha de sumário.
- <a name="translations"></a>

## 7. Traduções

Esse documento está disponível nas seguintes traduções:

- [Inglês](../CONTRIBUTING.md)
- [Francês 🇫🇷](./CONTRIBUTING.french.md)
- [Português Brasileiro :brazil:](./CONTRIBUTING.portuguese_br.md)

(Créditos: Algumas seções foram adaptadas a partir de https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
