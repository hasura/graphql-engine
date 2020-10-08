# हसुरा GraphQL इंजिन

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

हासुरा ग्राफिक्अल इंजिन एक झगमगणारा-वेगवान ग्राफिकल सर्व्हर आहे जो आपल्याला **झटपट, realtime GraphQL APIs प्रती पोस्टग्रेस** देतो, [**webhook triggers**](event-triggers.md)सोबत डेटाबेस घटनांवर, आणि [**remote schemas**](remote-schemas.md) व्यवसायाच्या युक्तिवाद साठी.

हसुरा पोस्टग्रेसच्या सहाय्याने [GraphQL](https://hasura.io/graphql/) apps तयार करण्यास किंवा पोस्टग्रेस वापरुन विद्यमान अनुप्रयोगांसाठी क्रमिकपणे ग्राफिकवर जाण्यास मदत करते.

[Hasura.io](https://hasura.io) आणि [दस्तऐवज](https://hasura.io/docs/) वर अधिक वाचा.

------------------

![Hasura GraphQL Engine Demo](assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](assets/realtime.gif)

-------------------

## वैशिष्ट्ये

* **प्रभावी क्वेरी बनवा**: अंगभूत filtering, पृष्ठांकन, नमुना शोध, मोठ्या प्रमाणात insert, update, delete mutations
* **Realtime**: कोणतीही GraphQL क्वेरी subscriptions द्वारे live क्वेरीमध्ये रूपांतरित करा.
* **रिमोट स्कीमा समाकन**: एकाच ग्राफिक इंजिन अंत्यबिंदू मार्गे व्यवसाय तर्कसाठी सानुकूल GraphQL स्कीमामध्ये प्रवेश करा. [**अधिक वाचा**](remote-schemas.md).
* **Trigger webhooks किंवा server विहीन कार्ये**: पोस्टग्रेसवर insert/update/delete events ([अधिक वाचा](event-triggers.md)).
* **विद्यमान, live डेटाबेससह कार्य करते**: वापरण्यास तयार ग्राफिक API त्वरित मिळविण्यासाठी विद्यमान पोस्टग्रेस डेटाबेसकडे निर्देश करा.
* **Fine-grained प्रवेश नियंत्रण**: dynamic access control जो आपके auth सिस्टमसोबत इंटीग्रेट करा (eg: auth0, firebase-auth).
* **High-performance आणि low-footprint**: ~15MB docker image; ~50MB RAM @ 1000 req/s; multi-core जाणीव.
* **Admin UI आणि स्थलांतर**: Admin UI आणि Rails-inspired स्कीमा स्थलांतर.
* **पोस्टग्रेस** ❤️: पोस्टग्रेस प्रकारांना समर्थन देते (पोस्टजीआयएस / भौगोलिक स्थान इ.), दृश्ये *आलेखांकडे* वळवतात, संचयित कार्ये किंवा उत्परिवर्तनांसह कार्यपद्धती ट्रिगर करतात.

अधिक येथे वाचा [hasura.io](https://hasura.io) आणि [docs](https://hasura.io/docs/).

## सामग्री सारणी
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**सामग्री सारणी**

- [द्रुत प्रारंभ:](#quickstart)
    - [हसुरा क्लाऊडवर एक क्लिकमधे उपयोजन](#one-click-deployment-on-hasura-cloud)
    - [अन्य एक-क्लिक उपयोजन पर्याय](#other-one-click-deployment-options)
    - [इतर उपयोजन पद्धती](#other-deployment-methods)
- [वास्तुकला](#architecture)
- [Client-side साधन](#client-side-tooling)
- [व्यवसाय तर्क जोडा](#add-business-logic)
    - [रिमोट स्कीमा](#remote-schemas)
    - [Trigger webhooks on डेटाबेस इव्हेंट](#trigger-webhooks-on-database-events)
- [डेमो](#demos)
    - [Realtime अनुप्रयोगs](#realtime-applications)
    - [व्हिडिओ](#videos)
- [समर्थन आणि समस्यानिवारण](#support--troubleshooting)
- [देणगी](#contributing)
- [ब्रँड मालमत्ता](#brand-assets)
- [परवाना](#license)
- [भाषांतर](#translations)

<!-- markdown-toc end -->

## द्रुत प्रारंभ:

### हसुरा क्लाऊडवर एक क्लिकमधे उपयोजन

हसुराचा प्रयत्न करण्याचा सर्वात वेगवान आणि सोपा मार्ग [Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html)द्वारे आहे.

1. पोस्टग्रेस अ‍ॅड-ऑन किंवा विद्यमान पोस्टग्रेस डेटाबेस वापरुन हसुरा क्लाऊडवर ग्राफिक इंजिन उपयोजित करण्यासाठी खालील बटणावर क्लिक करा:

    [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. हसुरा console उघडा
  
   हसुरा console उघडण्यासाठी "Launch console" बटणावर क्लिक करा.

3. आपली प्रथम ग्राफिक क्वेरी बनवा

   एक टेबल तयार करा आणि त्वरित आपली प्रथम क्वेरी चालवा. हे [सोपा मार्गदर्शक](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html)अनुसरण करा.

### अन्य एक-क्लिक उपयोजन पर्याय

खालील एक-क्लिक उपयोजन पर्यायांसाठी सूचना पहा:

| **Infra provider** | **One-click link** | **Additional information** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### इतर उपयोजन पद्धती

For Docker-based deployment and advanced configuration options, see [deployment
guides](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) or
[install manifests](install-manifests).

## वास्तुकला

हसुरा ग्राफिक इंजिन पोस्टग्रेस डेटाबेस घटना समोर ठेवते आणि आपल्या client अ‍ॅप्सवरील ग्राफिकल requests स्वीकारू शकते. आपल्या विद्यमान ऑथ सिस्टमसह कार्य करण्यासाठी हे कॉन्फिगर केले जाऊ शकते आणि आपल्या ऑथ सिस्टममधील डायनॅमिक व्हेरिएबल्ससह फील्ड-स्तरीय नियमांचा वापर करुन प्रवेश नियंत्रण हाताळू शकते.

आपण रिमोट ग्राफिक स्कीमा देखील विलीन आणि युनिफाइड ग्राफिक एपीआय प्रदान करू शकता.

![Hasura GraphQL Engine architecture](../assets/hasura-arch.svg)

## Client-side साधन

हसुरा कोणत्याही ग्राफिकल client बरोबर काम करते. आम्ही [अपोलो Client](https://github.com/apollographql/apollo-client) वापरण्याची शिफारस करतो. ग्राहकांच्या सूचीसाठी [awesome-graphql](https://github.com/chentsulin/awesome-graphql) पहा.

## व्यवसाय तर्क जोडा

ग्राफिकल इंजिन आपल्या बॅकएंडवर सानुकूल व्यवसाय लॉजिक जोडण्यासाठी सुलभतेने, स्केलेबल आणि परफॉरमिंग पद्धती प्रदान करते:

### रिमोट स्कीमा

हसुराच्या पोस्टग्रेस-आधारित ग्राफिक स्कीमा व्यतिरिक्त दूरस्थ स्कीमामध्ये सानुकूल निराकरणकर्ता जोडा. पेमेंट एपीआय लागू करणे किंवा आपल्या डेटाबेसमध्ये नसलेला डेटा क्वेरी करणे यासारख्या वापरासाठी-उदाहरणार्थ - [अधिक वाचा](remote-schemas.md).

### Trigger webhooks on डेटाबेस इव्हेंट

डेटाबेस इव्हेंटच्या आधारावर trigger केलेला अतुल्यकालिक व्यवसाय तर्क जोडा. सूचनांसाठी उपयुक्त, पोस्टग्रेस किंवा asynchronous processing कडील डेटा-पाइपलाइन - [अधिक वाचा](event-triggers.md).

### व्युत्पन्न डेटा किंवा डेटा ट्रान्सफॉर्मेशन

पोस्टग्रेसमध्ये डेटा रूपांतरित करा किंवा ग्राफिक इंजिन वापरून क्वेरी केली जाऊ शकते असा दुसरा डेटासेट मिळविण्यासाठी त्यावर व्यवसाय तर्क जोडा - [अधिक वाचा](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## डेमो

[community/sample-apps](community/sample-apps) मधील सर्व उदाहरण अनुप्रयोग पहा.

### Realtime अनुप्रयोग

- Reactसह बनविलेले गट चॅट प्लिकेशनमध्ये एक टाइपिंग सूचक, ऑनलाइन वापरकर्ते आणि नवीन संदेश सूचना         समाविष्ट आहेत.
  - [प्रयत्न करा](https://realtime-chat.demo.hasura.app/)
  - [प्रशिक्षण](community/sample-apps/realtime-chat)
  - [Browse APIs](https://realtime-chat.demo.hasura.app/console)

- Live location tracking app that shows a running vehicle changing current GPS
  coordinates moving on a map.
  - [प्रयत्न करा](https://realtime-location-tracking.demo.hasura.app/)
  - [प्रशिक्षण](community/sample-apps/realtime-location-tracking)
  - [Browse APIs](https://realtime-location-tracking.demo.hasura.app/console)

- सतत बदलत्या डेटा वर डेटा एकत्रीकरणसाठी एक रीयलटाइम डैशबोर्ड
  - [प्रयत्न करा](https://realtime-poll.demo.hasura.app/)
  - [प्रशिक्षण](community/sample-apps/realtime-poll)
  - [Browse APIs](https://realtime-poll.demo.hasura.app/console)

### व्हिडिओ

* [self-host केलेल्या GitLabच्या उदाहरणामध्ये ग्राफिक जोडा](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [Auth0 आणि ग्राफ्यूएल बॅकएंडसह टोडो app](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [GitLabवरील ग्राफ्यूएल गिटलाब ऑथसह समाकलित करा](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [भौगोलिक स्थानासह 10 मिलियन राइडसाठी डॅशबोर्ड (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## समर्थन आणि समस्यानिवारण

दस्तऐवज आणि समुदाय आपल्याला बर्‍याच समस्यांचे निराकरण करण्यात मदत करेल. आपल्याला बग आला असल्यास किंवा आमच्याशी संपर्क साधण्याची आवश्यकता असल्यास आपण खालीलपैकी एक चॅनेल वापरुन आमच्याशी संपर्क साधू शकता:

* समर्थन आणि अभिप्राय: [Discord](https://discord.gg/hasura)
* मुद्दा आणि बग-ट्रॅकिंग: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* उत्पादन अद्यतनांचे अनुसरण: [@HasuraHQ](https://twitter.com/hasurahq)
* आमच्या [website chat](https://hasura.io)वर आमच्याशी बोला 

आम्ही समाजात खुल्या आणि स्वागतार्ह वातावरणाला चालना देण्यासाठी वचनबद्ध आहोत. कृपया [Code of Conduct](code-of-conduct.md) पहा.

आपण एखाद्या सुरक्षा समस्येचा अहवाल देऊ इच्छित असल्यास, कृपया [हे वाचा](SECURITY.md).

## देणगी

अधिक माहितीसाठी आमची [contributing guide](CONTRIBUTING.md) पहा.

## ब्रँड मालमत्ता

[../assets/brand](../assets/brand) फोल्डरमध्ये हसुरा ब्रँड मालमत्ता (लोगो, हसुरा मॅस्कॉट, बॅज इत्यादिद्वारे समर्थित) आढळू शकतात. आपल्या अनुप्रयोग / वेबसाइट इत्यादी मध्ये त्यांचा वापर मोकळ्या मनाने करा जर आपण हसुरा वापरुन तयार केलेल्या आपल्या अनुप्रयोगांमध्ये "पॉवर्ड बाय हसुरा" बॅज जोडला तर आम्हाला आनंद होईल. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- For light backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- For dark backgrounds -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## परवाना

कोर ग्राफिक इंजिन [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0) अंतर्गत उपलब्ध आहे.

सर्व **इतर सामग्री** ([`सर्व्हर`](server)) मधील, [`cli`](cli) आणि त्याशिवाय [`console`](console) निर्देशिका [एमआयटी परवाना](LICENSE-community) अंतर्गत उपलब्ध आहेत. यात [`डॉक्स`](docs) आणि [`समुदाय`](community) निर्देशिकांमधील प्रत्येक गोष्टीचा समावेश आहे.

## भाषांतर

पुढील भाषांतरांमध्ये ही README उपलब्ध आहेः

- [Japanese :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [French :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [Bosnian :bosnia_herzegovina:](translations/README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [Russian :ru:](translations/README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [Greek 🇬🇷](translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [Spanish 🇲🇽](/translations/README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [Indonesian :indonesia:](translations/README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [Brazilian Portuguese :brazil:](translations/README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [German 🇩🇪](translations/README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [Chinese :cn:](translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [Turkish :tr:](translations/README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [Korean :kr:](translations/README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))

इतर फायलींसाठी भाषांतरे [येथे](translations) आढळू शकतात.
