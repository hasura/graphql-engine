# Contribuindo para o Hasura graphql-engine

_Primeiramente_: se você se sente inseguro sobre como começar a contribuir, fique à vontade para nos perguntar em nosso [Canal no Discord](https://discordapp.com/invite/hasura) no canal __#contrib channel__. Você também pode seguir em frente e contribuir, e lhe daremos um _feedback_ posteriormente. Não se preocupe, o pior que pode acontecer é te pedirmos a gentileza de realizar alguma alteração. Nós gostamos de contribuições, e não queremos um monte de regras complexas atrapalhando tudo.

No entanto, para as pessoas que desejam um pouco mais de direcionamento na melhor maneira para contribuir com o projeto, continue a leitura. Esse documento irá cobrir que você está procurando. Ao adotar as orientações abaixo, as chances de nós rapidamente incorporarmos suas alterações ou encaminhar suas contribuuições vão aumentar. 

## Índice

[1. Código de Conduta ](#codigo-de-conduta)

[2. Visão geral do repositório ](#visao-geral)

[3. Boas vindas aos novos colaboradores! ](#boas-vindas)

[4. Áreas para contribuir ](#areas)

[5. Maneiras de contribuir ](#maneiras-de-contribuir)

[6. Mensagens nos Commits ](#mensagens-commit)

[7. Traduções ](#traduçoes)

<a name="codigo-de-conduta"></a>

## 1. Código de Conduta

Por favor, siga nosso [Código de conduta](code-of-conduct.md) no que diz respeito a quaisquer contribuições feitas para o Hasura.

<a name="visao-geral"></a>

## 2. Visão geral do repositório

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) é um mono-repositório, que consiste em 3 componentes. Cada um deles possui seus respectivos guias para contribuições:

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

Todos os três componentes possuem uma única versão, determinadas ou pela _git tag_ ou uma combinação do nome da _branch_ e o SHA do _commit_.

Para todas as contribuições, um CLA (_Contributor License Agreement_), ou Acordo de Licença para Contribuição, precisa ser assinado [aqui](https://cla-assistant.io/hasura/graphql-engine) antes (ou depois) do _pull request_ ser feito. Um _bot_ pedirá que contribuidores assinem o Acordo de Licença para Contribuição via comentário no _pull request_, se necessário.

<a name="boas-vindas"></a>

## 3. Boas vindas aos primeiros colaboradores!

Nós gostamos dos primeiros contribuidores e estamos felizes em te ajudar a começar. Em caso de dúvidas, fale com a gente!

Você encontrará todas as dúvidas cabíveis aos primeiros contribuidores [aqui](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. Áreas para contribuir

É claro que gostamos de contribuições para todos os componentes do Hasura. No entanto, identificamos três áreas que são particularmente adequadas para contribuições de código aberto (_open source_).

### Documentação

Nosso objetivo é manter nossa documentação fácil de entender e atualizada. Se você gostaria de nos ajudar em fazer isso, somos gratos por quaisquer tipos de contribuições:

- Reportar conteúdo inexistente ou faltando;

- Consertar erros na documentação existente;

- Nos ajudar a preencher a documentação.

O guia para contribuição na documentação se encontra em [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

### Conteúdo da comunidade

Desde que nós lançamos a nossa [página de aprendizado](https://hasura.io/learn/), estamos felizes com contribuições:

- Para consertar erros nos tutoriais existentes;

- Que acrescentam novos tutoriais (por favor nos avise caso tenha ideias quanto a evitar duplicação e palavras).

O LEIA-ME do repositório de aprendizagem se encontra [aqui](https://github.com/hasura/learn-graphql).

Separado do conteúdo para aprendizagem, nós identificamos três outras maneiras de se contribuir com o conteúdo técnico da comunidade:

- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Sample apps](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Tools](community/tools)

Se gostaria de contribuir para a comunidade:

- escrevendo uma postagem com conteúdo técnico;

- palestrando em algum evento;

- organizando um _workshop_.

Acesse nossa [wiki pública](https://github.com/hasura/graphql-engine/wiki/Community-Wiki).

Fique à vontade de mandar um _pull request_ se tiver algo para acrescentar, mesmo se não tiver relação com nada que foi mencionado acima.

### Hasura CLI

Nós temos alguns problemas no CLI que são adequados para contribuições de código aberto (_open source_). Se você souber __Go__ ou se gostaria de aprender na prática, confira esses [problemas](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

O LEIA-ME do repositório CLI pode ser encontrado [aqui](https://github.com/hasura/graphql-engine/tree/master/cli).

<a name="maneiras-de-contribuir"></a>

## 5. Maneiras de contribuir

### Reportando um problema

- Certifique-se que seus testes estejam utilizando a última versão disponível. É possível que já tenhamos corrigido o _bug_ que você está enfrentando.

- Forneça os passos para que o problema seja reproduzido, incluindo a versão do Postgres, a versão do _graphql-engine_ e onde você está realizando a implementação (Heroku, Docker, etc.).

- Por favor, inclua os _logs_ do servidor, caso seja relevante.

### Trabalhando em um problema

- Nós usamos o [fork-and-branch git workflow](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Por favor, certifique-se de que existe um problema (_issue_) associado com o seu trabalho;

- Se estiver trabalhando em um problema (_issue_) por favor comente o que está fazendo para evitar retrabalho por parte de outras pessoas.

- Dê um _Squash_ em seus _commits_ e refira à _issue_ utilizando `fix #<issue-no>` ou `close #<issue-no>` na mensagem do _commit_, no final.
  Por exemplo: `resolve answers to everything (fix #42)` ou `resolve answers to everything, fix #42`

- Faça um _Rebase_  na _master_ com sua _branch_ antes de submeter um _pull request_.

<a name="mensagens-commit"></a>

## 6. Mensagens nos _commits_

- A primeira linha deve ser um resumo das mudanças, não ultrapassando 50 caractéres, seguidos por um corpo de texto adicional (opcional), contendo maiores detalhes acerca das mudanças. Consulte [esse link](https://github.com/erlang/otp/wiki/writing-good-commit-messages) para mais informações de como escrever boas mensagens em seus _commits_;

- Use frases em inglês do tipo: _"add/fix/change"_, e não _"added/fixed/changed"_ e nem _"adds/fixes/changes"_;

- Não use maiúscula na primeira letra na linha do sumário;

- Não use ponto final (.) ao final da linha do sumário.

<a name="traduçoes"></a>

## 7. Traduções

Esse documento está disponível em:

- [Francês 🇫🇷](translations/CONTRIBUTING.french.md)
- [Português Brasileiro :brazil:](translations/CONTRIBUTING.pt_br.md)

(Créditos: Algumas seções foram adaptadas de https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
