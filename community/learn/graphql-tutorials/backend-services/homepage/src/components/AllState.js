const angularIcon = require('../images/angular-icon.svg');
const vueIcon = require('../images/Vue-icon.svg');
const reactIcon = require('../images/React-icon.svg');
const elm = require('../images/elm.svg');
const re = require('../images/re.svg');
const hasura = require('../images/hasura-icon.svg');
const postgres = require('../images/postgres.svg');

const reactBlue = require('../images/react-blue.svg');
const ios = require('../images/ios.svg');
const android = require('../images/android-icon.svg');
const arrow = require('../images/arrow.svg');

const frontendTutorial = [
  {
    imgSrc:reactIcon,
    imgAlt:'React icon',
    url: 'https://www.google.com/',
    comingSoon: '',
  },
  {
    imgSrc:vueIcon,
    imgAlt:'Vue icon',
    url: 'https://www.google.com/',
    comingSoon: '',
  },
  {
    imgSrc:angularIcon,
    imgAlt:'Angular icon',
    url: 'https://www.google.com/',
    comingSoon: '',
  },
  {
    imgSrc:elm,
    imgAlt:'elm icon',
    url: 'https://www.google.com/',
    comingSoon: 'Coming soon',
  },
  {
    imgSrc:re,
    imgAlt:'re icon',
    url: 'https://www.google.com/',
    comingSoon: 'Coming soon',
  },
]
const backendTutorial = [
  {
    imgSrc:hasura,
    imgAlt:'hasura icon',
    url: 'https://www.google.com/',
    comingSoon: '',
  },
  {
    imgSrc:postgres,
    imgAlt:'postgres icon',
    url: 'https://www.google.com/',
    comingSoon: 'Coming soon',
  },
]
const mobileTutorial = [
  {
    imgSrc:reactBlue,
    imgAlt:'reactBlue icon',
    url: 'https://www.google.com/',
    comingSoon: '',
  },
  {
    imgSrc:ios,
    imgAlt:'ios icon',
    url: 'https://www.google.com/',
    comingSoon: '',
  },
  {
    imgSrc:android,
    imgAlt:'android icon',
    url: 'https://www.google.com/',
    comingSoon: 'Coming soon',
  },
  {
    imgSrc:arrow,
    imgAlt:'arrow icon',
    url: 'https://www.google.com/',
    comingSoon: 'Coming soon',
  },
]
const learnFrontend = [
  {
    list: 'GraphQL queries, mutations and subscriptions.',
  },
  {
    list: 'GraphQL queries, mutations and subscriptions.',
  },
  {
    list: 'Using an existing auth system with your GraphQL APIs.',
  },
  {
    list: 'Using an existing auth system with your GraphQL APIs.',
  },
  {
    list: 'GraphQL query variables and fragments.',
  },
  {
    list: 'GraphQL query variables and fragments.',
  },
  {
    list: 'Building a realtime feed with your GraphQL client.',
  },
  {
    list: 'Building a realtime feed with your GraphQL client.',
  },
  {
    list: 'GraphQL & UI modularisation techniques.',
  },
  {
    list: 'GraphQL & UI modularisation techniques.',
  },
]
const learnBackend = [
  {
    list: 'GraphQL queries, mutations and subscriptions.',
  },
  {
    list: 'Using an existing auth system with your GraphQL APIs.',
  },
  {
    list: 'GraphQL query variables and fragments.',
  },
  {
    list: 'Building a realtime feed with your GraphQL client.',
  },
  {
    list: 'GraphQL & UI modularisation techniques.',
  },
]
export {frontendTutorial, backendTutorial, mobileTutorial, learnFrontend, learnBackend}
