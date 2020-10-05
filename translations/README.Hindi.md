# Hasura GraphQL Engine

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL इंजन एक धधकते-तेज़ GraphQL सर्वर है जो आपको  **तुरंत, Postgres पर रियल टाइम GraphQL एपीआई** के साथ [**webhook ट्रिगर**](event-triggers.md), डेटाबेस घटनाओं और [**Remote स्कीमास**](remote-schemas.md)
व्यापार तर्क के लिए देता है।

Hasura आपको Postgres द्वारा समर्थित GraphQL एप्स बनाने में मदद करता है या Postgres का उपयोग करते हुए मौजूदा अनुप्रयोगों के लिए GraphQL की ओर बढ़ें।

[hasura.io](https://hasura.io) और [docs](https://hasura.io/docs/) पर अधिक पढ़ें।

------------------

![Hasura GraphQL इंजन डेमो](assets/demo.gif)

------------------

![Hasura GraphQL इंजन रीयलटाइम डेमो](assets/realtime.gif)

-------------------

## विशेषताएं
* **शक्तिशाली क्वेरी करें**: अंतर्निहित फ़िल्टरिंग, पृष्ठांकन, पैटर्न खोज, बल्क इंसर्ट, अपडेट, म्यूटेशन हटाएं।
* **रियल टाइम**: सब्सक्रिप्शन का उपयोग करके किसी भी GraphQL क्वेरी को लाइव क्वेरी में कनवर्ट करें।
* **दूरस्थ स्कीमा मर्ज करें**: सिंगल GraphQL इंजन एंडपॉइंट के माध्यम से व्यावसायिक तर्क के लिए कस्टम GraphQL स्कीमाओं तक पहुँच। [**अधिक पढ़ें**](remote-schemas.md).
* **ट्रिगर वेबहूक या सर्वर रहित फ़ंक्शंस**: Postgres पर ईवेंट डालें / अपडेट करें / हटाएं। ([**अधिक पढ़ें**](event-triggers.md))
* **मौजूदा, लाइव डेटाबेस के साथ काम करता है**: इसे एक मौजूदा Postgres डेटाबेस पर तुरंत तैयार रेडी-टू-यूज़ GraphQL एपीआई प्राप्त करें।
* **बारीक-बारीक पहुंच नियंत्रण**: डायनेमिक एक्सेस कंट्रोल जो आपके प्रमाणीकरण प्रणाली के साथ इंटीग्रेट होता है (उदाहरण के लिए: auth0, फायरबेस-auth)
* **उच्च प्रदर्शन और कम पदचिह्न**: ~ 15 एमबी डॉकटर छवि; ~ 50MB RAM @ 1000 req / s; मल्टी-कोर जागरूक
* **व्यवस्थापक UI और माइग्रेशन**: व्यवस्थापक UI और रेल-प्रेरित स्कीमा माइग्रेशन 
* **Postgres :heart:**: Postgres के प्रकारों का समर्थन करता है (पोस्टजीआईएस / भू-स्थान आदि), रेखांकन पर विचार बदल देता है, उत्परिवर्तन के साथ संग्रहीत कार्यों या प्रक्रियाओं को ट्रिगर करना

[hasura.io](https://hasura.io) और [डॉक्स](https://hasura.io/docs/) पर अधिक पढ़ें।

## विषय - सूची
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**विषय-सूची**
- [जल्दी शुरू](#जल्दी-शुरू)
  - [हसुरा क्लाउड पर एक-क्लिक पर तैनाती](#हसुरा-क्लाउड-पर-एक-क्लिक-पर-तैनाती)
  - [अन्य एक क्लिक तैनाती विकल्प](#अन्य-एक-क्लिक-तैनाती-विकल्प)
  - [अन्य तैनाती के तरीके](#अन्य-तैनाती-के-तरीके)
- [वास्तुकला](#वास्तुकला)
- [क्लाइंट-साइड टूलिंग](#क्लाइंट-साइड-टूलिंग)
- [व्यापार तर्क जोड़ें](#व्यापार-तर्क-जोड़ें)
  - [दूरस्थ स्कीमा](#दूरस्थ-स्कीमा)
  - [डेटाबेस की घटनाओं पर ट्रिगर वेबहूक](#डेटाबेस-की-घटनाओं-पर-ट्रिगर-वेबहूक)
- [प्रदर्शन](#प्रदर्शन)
  - [रीयलटाइम अनुप्रयोग](#रीयलटाइम-अनुप्रयोग)
  - [वीडियो](#वीडियो)
- [सहायता और समस्या निवारण](#सहायता-और-समस्या-निवारण)
- [योगदान](#योगदान)
- [ब्रांड संपत्ति](#ब्रांड-संपत्ति)
- [लाइसेंस](#लाइसेंस)
- [अनुवाद](#अनुवाद)

<!-- markdown-toc end -->

## जल्दी शुरू:

### हसुरा क्लाउड पर एक-क्लिक पर तैनाती

हसुरा को आज़माने का सबसे तेज़ और आसान तरीका है [हसुरा क्लाउड](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html)।

1. हसुरा क्लाउड पर ग्राफक्यूएल इंजन को तैनात करने के लिए निम्नलिखित बटन पर क्लिक करें, जिसमें पोस्टग्रैस ऐड-ऑन या मौजूदा पोस्टग्रेज डेटाबेस का उपयोग करना शामिल है:

  [![हसुरा क्लाउड पर नियत करें](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. हसुरा कंसोल खोलें

  हसुरा कंसोल को खोलने के लिए "लॉन्च कंसोल" बटन पर क्लिक करें।

3. अपनी पहली GraphQL क्वेरी बनाओ

   एक तालिका बनाएं और तुरंत अपनी पहली क्वेरी चलाएँ। इस [सरल गाइड](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html) का पालन करें।

### अन्य एक क्लिक तैनाती विकल्प

निम्नलिखित एक-क्लिक परिनियोजन विकल्पों के लिए निर्देश देखें:

| **इन्फ्रा प्रोवाइडर** | **एक-क्लिक लिंक** | **अतिरिक्त जानकारी** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [डॉक्स](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [डॉक्स](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [डॉक्स](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [डॉक्स](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### अन्य तैनाती के तरीके

डॉकर-आधारित परिनियोजन और उन्नत कॉन्फ़िगरेशन विकल्पों के लिए, [तैनाती गाइड](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) या
[स्थापित मैनिफ़ेस्ट](install-manifests) देखें।

## वास्तुकला

Hasura GraphQL इंजन एक Postgres डेटाबेस उदाहरण से सामना करता है और अपने क्लाइंट ऐप्स से GraphQL अनुरोध स्वीकार कर सकते हैं। इसे आपके मौजूदा सिस्टम के साथ काम करने के लिए कॉन्फ़िगर किया जा सकता है और उनके प्रमाणीकरण प्रणालियों से गतिशील चर के साथ क्षेत्र-स्तरीय नियमों का उपयोग करके अभिगम नियंत्रण को संभाल सकते हैं।

आप रिमोट GraphQL स्कीमा को भी मर्ज कर सकते हैं और एक एकीकृत GraphQL एपीआई प्रदान कर सकते हैं।

![Hasura GraphQL इंजन आर्किटेक्चर](assets/hasura-arch.svg)

## क्लाइंट-साइड टूलिंग

Hasura किसी भी GraphQL क्लाइंट के साथ काम करता है। हम [अपोलो क्लाइंट](https://github.com/apollographql/apollo-client) का उपयोग करने की सलाह देते हैं। क्लाइंट्स की सूची के लिए [भययोग्य-ग्राफकल](https://github.com/chentsulin/awesome-graphql) देखें।

## व्यापार तर्क जोड़ें

GraphQL इंजन आपके बैकएंड में कस्टम व्यावसायिक तर्क जोड़ने के लिए आसान-से-उचित, मापनीय और प्रदर्शन करने के तरीके प्रदान करता है:

### दूरस्थ स्कीमा

एक दूरस्थ स्कीमा में कस्टम रिज़ॉल्वर को Hasura के Postgres-आधारित GraphQL स्कीमा के अलावा जोड़ें। उपयोग के मामलों के लिए आदर्श जैसे भुगतान एपीआई को लागू करना, या डेटा को क्वेरी करना जो आपके डेटाबेस में नहीं है - [अधिक पढ़ें](remote-schemas.md).

### डेटाबेस की घटनाओं पर ट्रिगर वेबहूक

डेटाबेस घटनाओं के आधार पर ट्रिगर किए गए अतुल्यकालिक व्यावसायिक तर्क जोड़ें।
सूचनाओं के लिए आदर्श,
सूचनाओं के लिए आदर्श, Postgres से डेटा-पाइपलाइन या अतुल्यकालिक
प्रसंस्करण - [अधिक पढ़ें](event-triggers.md).

### व्युत्पन्न डेटा या डेटा परिवर्तन

Postgres में डेटा ट्रांसफ़ॉर्म करें या किसी अन्य डेटासेट को प्राप्त करने के लिए उस पर व्यावसायिक लॉजिक चलाएं, जिसे GraphQL इंजन का उपयोग करके क्वेर किया जा सकता है - [अधिक पढ़ें](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## प्रदर्शन

[community/sample-apps](community/sample-apps) निर्देशिका में सभी उदाहरण एप्लिकेशन देखें।

### रीयलटाइम अनुप्रयोग

- रिएक्ट के साथ बनाया गया ग्रुप चैट एप्लिकेशन में एक टाइपिंग इंडिकेटर, ऑनलाइन उपयोगकर्ता और नया शामिल हैं
  संदेश सूचनाएं।
  - [कोशिश करके देखो](https://realtime-chat.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-chat)
  - [एपीआई ब्राउज़ करें](https://realtime-chat.demo.hasura.app/console)

- लाइव लोकेशन ट्रैकिंग ऐप जो एक चालू वाहन को वर्तमान जीपीएस निर्देशांक को बदलते हुए दिखाता है जो एक नक्शे पर आगे बढ़ता है।
  - [कोशिश करके देखो](https://realtime-location-tracking.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-location-tracking)
  - [एपीआई ब्राउज़ करें](https://realtime-location-tracking.demo.hasura.app/console)

- लगातार बदलते डेटा पर डेटा एकत्रीकरण के लिए एक रीयलटाइम डैशबोर्ड।
  - [कोशिश करके देखो](https://realtime-poll.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-poll)
  - [एपीआई ब्राउज़ करें](https://realtime-poll.demo.hasura.app/console)

### वीडियो

* [एक स्व-होस्टित GitLab उदाहरण के लिए GraphQL जोड़ें](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 मिनट*)
* [Auth0 और GraphQL बैकएंड के साथ टोडो ऐप](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 मिनट*)
* [GitLab पर GraphQL GitLab प्रमाणीकरण के साथ एकीकृत है](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 मिनट*)
* [10million के लिए डैशबोर्ड भू-स्थान के साथ सवारी करता है (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 मिनट*)


## सहायता और समस्या निवारण

प्रलेखन और समुदाय आपको अधिकांश मुद्दों का निवारण करने में मदद करेंगे। यदि आपको बग का सामना करना पड़ा है या हमारे साथ संपर्क करने की आवश्यकता है, आप निम्नलिखित में से किसी एक चैनल का उपयोग करके हमसे संपर्क कर सकते हैं:

* समर्थन और प्रतिक्रिया: [Discord](https://discord.gg/hasura)
* समस्या और बग ट्रैकिंग: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* उत्पाद अपडेट का पालन करें: [@HasuraHQ](https://twitter.com/hasurahq)
* हमसे बात करो हमारी [वेबसाइट चैट](https://hasura.io)

हम समुदाय में एक खुले और स्वागत वाले माहौल को बढ़ावा देने के लिए प्रतिबद्ध हैं। कृपया देखें [आचार संहिता](code-of-conduct.md)

यदि आप किसी सुरक्षा समस्या की रिपोर्ट करना चाहते हैं, तो कृपया [इसे पढ़ें](SECURITY.md)

## योगदान

अधिक जानकारी के लिए हमारी [योगदान करने वाली मार्गदर्शिका](CONTRIBUTING.md) देखें।

## ब्रांड संपत्ति

हसुरा ब्रांड की संपत्ति (लोगो, हसुरा शुभंकर, बैज आदि द्वारा संचालित) [आस्तियों / ब्रांड](assets/brand) फ़ोल्डर में हो सकती है। 

बेझिझक उन्हें अपने एप्लिकेशन / वेबसाइट आदि में उपयोग करें। "Hasura द्वारा संचालित ❤️" को जोड़ने पर हम रोमांचित हो जाएंगे
Hasura के उपयोग से निर्मित आपके अनुप्रयोगों के लिए बिल्ला। 

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

## लाइसेंस

कोर GraphQL इंजन [Apache लाइसेंस 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0) के तहत उपलब्ध है। 

सभी **अन्य सामग्री** (इन्हें छोड़कर [`server`](server), [`cli`](cli) और
[`console`](console) निर्देशिका) [एमआईटी लाइसेंस](LICENSE-community) के तहत उपलब्ध है।
इसमें [`डॉक्स`](docs) और [`समुदाय`](community) निर्देशिका सब कुछ शामिल है।

## अनुवाद

यह रीडमी निम्नलिखित अनुवादों में उपलब्ध है:

- [जापानी :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [फ्रेंच :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [बोस्नियाई :bosnia_herzegovina:](translations/README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [रूसी:ru:](translations/README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [यूनानी 🇬🇷](translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [स्पेनिश 🇲🇽](/translations/README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [इन्डोनेशियाई :indonesia:](translations/README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [ब्राजिलियन पुर्तगाली :brazil:](translations/README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [जर्मन 🇩🇪](translations/README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [चीनी :cn:](translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [तुर्की :tr:](translations/README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [कोरियाई :kr:](translations/README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))

अन्य फ़ाइलों के लिए अनुवाद [यहाँ](translations) मिल सकते हैं



# Hasura GraphQL Engine

[![Latest release](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![Docs](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

Hasura GraphQL इंजन एक धधकते-तेज़ GraphQL सर्वर है जो आपको  **तुरंत, Postgres पर रियल टाइम GraphQL एपीआई** के साथ [**webhook ट्रिगर**](event-triggers.md), डेटाबेस घटनाओं और [**Remote स्कीमास**](remote-schemas.md)
व्यापार तर्क के लिए देता है।

Hasura आपको Postgres द्वारा समर्थित GraphQL एप्स बनाने में मदद करता है या Postgres का उपयोग करते हुए मौजूदा अनुप्रयोगों के लिए GraphQL की ओर बढ़ें।

[hasura.io](https://hasura.io) और [docs](https://hasura.io/docs/) पर अधिक पढ़ें।

------------------

![Hasura GraphQL इंजन डेमो](assets/demo.gif)

------------------

![Hasura GraphQL इंजन रीयलटाइम डेमो](assets/realtime.gif)

-------------------

## विशेषताएं
* **शक्तिशाली क्वेरी करें**: अंतर्निहित फ़िल्टरिंग, पृष्ठांकन, पैटर्न खोज, बल्क इंसर्ट, अपडेट, म्यूटेशन हटाएं।
* **रियल टाइम**: सब्सक्रिप्शन का उपयोग करके किसी भी GraphQL क्वेरी को लाइव क्वेरी में कनवर्ट करें।
* **दूरस्थ स्कीमा मर्ज करें**: सिंगल GraphQL इंजन एंडपॉइंट के माध्यम से व्यावसायिक तर्क के लिए कस्टम GraphQL स्कीमाओं तक पहुँच। [**अधिक पढ़ें**](remote-schemas.md).
* **ट्रिगर वेबहूक या सर्वर रहित फ़ंक्शंस**: Postgres पर ईवेंट डालें / अपडेट करें / हटाएं। ([**अधिक पढ़ें**](event-triggers.md))
* **मौजूदा, लाइव डेटाबेस के साथ काम करता है**: इसे एक मौजूदा Postgres डेटाबेस पर तुरंत तैयार रेडी-टू-यूज़ GraphQL एपीआई प्राप्त करें।
* **बारीक-बारीक पहुंच नियंत्रण**: डायनेमिक एक्सेस कंट्रोल जो आपके प्रमाणीकरण प्रणाली के साथ इंटीग्रेट होता है (उदाहरण के लिए: auth0, फायरबेस-auth)
* **उच्च प्रदर्शन और कम पदचिह्न**: ~ 15 एमबी डॉकटर छवि; ~ 50MB RAM @ 1000 req / s; मल्टी-कोर जागरूक
* **व्यवस्थापक UI और माइग्रेशन**: व्यवस्थापक UI और रेल-प्रेरित स्कीमा माइग्रेशन 
* **Postgres :heart:**: Postgres के प्रकारों का समर्थन करता है (पोस्टजीआईएस / भू-स्थान आदि), रेखांकन पर विचार बदल देता है, उत्परिवर्तन के साथ संग्रहीत कार्यों या प्रक्रियाओं को ट्रिगर करना

[hasura.io](https://hasura.io) और [डॉक्स](https://hasura.io/docs/) पर अधिक पढ़ें।

## विषय - सूची
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**विषय-सूची**
- [जल्दी शुरू](#जल्दी-शुरू)
  - [हसुरा क्लाउड पर एक-क्लिक पर तैनाती](#हसुरा-क्लाउड-पर-एक-क्लिक-पर-तैनाती)
  - [अन्य एक क्लिक तैनाती विकल्प](#अन्य-एक-क्लिक-तैनाती-विकल्प)
  - [अन्य तैनाती के तरीके](#अन्य-तैनाती-के-तरीके)
- [वास्तुकला](#वास्तुकला)
- [क्लाइंट-साइड टूलिंग](#क्लाइंट-साइड-टूलिंग)
- [व्यापार तर्क जोड़ें](#व्यापार-तर्क-जोड़ें)
  - [दूरस्थ स्कीमा](#दूरस्थ-स्कीमा)
  - [डेटाबेस की घटनाओं पर ट्रिगर वेबहूक](#डेटाबेस-की-घटनाओं-पर-ट्रिगर-वेबहूक)
- [प्रदर्शन](#प्रदर्शन)
  - [रीयलटाइम अनुप्रयोग](#रीयलटाइम-अनुप्रयोग)
  - [वीडियो](#वीडियो)
- [सहायता और समस्या निवारण](#सहायता-और-समस्या-निवारण)
- [योगदान](#योगदान)
- [ब्रांड संपत्ति](#ब्रांड-संपत्ति)
- [लाइसेंस](#लाइसेंस)
- [अनुवाद](#अनुवाद)

<!-- markdown-toc end -->

## जल्दी शुरू:

### हसुरा क्लाउड पर एक-क्लिक पर तैनाती

हसुरा को आज़माने का सबसे तेज़ और आसान तरीका है [हसुरा क्लाउड](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html)।

1. हसुरा क्लाउड पर ग्राफक्यूएल इंजन को तैनात करने के लिए निम्नलिखित बटन पर क्लिक करें, जिसमें पोस्टग्रैस ऐड-ऑन या मौजूदा पोस्टग्रेज डेटाबेस का उपयोग करना शामिल है:

  [![हसुरा क्लाउड पर नियत करें](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. हसुरा कंसोल खोलें

  हसुरा कंसोल को खोलने के लिए "लॉन्च कंसोल" बटन पर क्लिक करें।

3. अपनी पहली GraphQL क्वेरी बनाओ

   एक तालिका बनाएं और तुरंत अपनी पहली क्वेरी चलाएँ। इस [सरल गाइड](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html) का पालन करें।

### अन्य एक क्लिक तैनाती विकल्प

निम्नलिखित एक-क्लिक परिनियोजन विकल्पों के लिए निर्देश देखें:

| **इन्फ्रा प्रोवाइडर** | **एक-क्लिक लिंक** | **अतिरिक्त जानकारी** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [डॉक्स](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [डॉक्स](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [डॉक्स](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [डॉक्स](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### अन्य तैनाती के तरीके

डॉकर-आधारित परिनियोजन और उन्नत कॉन्फ़िगरेशन विकल्पों के लिए, [तैनाती गाइड](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) या
[स्थापित मैनिफ़ेस्ट](install-manifests) देखें।

## वास्तुकला

Hasura GraphQL इंजन एक Postgres डेटाबेस उदाहरण से सामना करता है और अपने क्लाइंट ऐप्स से GraphQL अनुरोध स्वीकार कर सकते हैं। इसे आपके मौजूदा सिस्टम के साथ काम करने के लिए कॉन्फ़िगर किया जा सकता है और उनके प्रमाणीकरण प्रणालियों से गतिशील चर के साथ क्षेत्र-स्तरीय नियमों का उपयोग करके अभिगम नियंत्रण को संभाल सकते हैं।

आप रिमोट GraphQL स्कीमा को भी मर्ज कर सकते हैं और एक एकीकृत GraphQL एपीआई प्रदान कर सकते हैं।

![Hasura GraphQL इंजन आर्किटेक्चर](assets/hasura-arch.svg)

## क्लाइंट-साइड टूलिंग

Hasura किसी भी GraphQL क्लाइंट के साथ काम करता है। हम [अपोलो क्लाइंट](https://github.com/apollographql/apollo-client) का उपयोग करने की सलाह देते हैं। क्लाइंट्स की सूची के लिए [भययोग्य-ग्राफकल](https://github.com/chentsulin/awesome-graphql) देखें।

## व्यापार तर्क जोड़ें

GraphQL इंजन आपके बैकएंड में कस्टम व्यावसायिक तर्क जोड़ने के लिए आसान-से-उचित, मापनीय और प्रदर्शन करने के तरीके प्रदान करता है:

### दूरस्थ स्कीमा

एक दूरस्थ स्कीमा में कस्टम रिज़ॉल्वर को Hasura के Postgres-आधारित GraphQL स्कीमा के अलावा जोड़ें। उपयोग के मामलों के लिए आदर्श जैसे भुगतान एपीआई को लागू करना, या डेटा को क्वेरी करना जो आपके डेटाबेस में नहीं है - [अधिक पढ़ें](remote-schemas.md).

### डेटाबेस की घटनाओं पर ट्रिगर वेबहूक

डेटाबेस घटनाओं के आधार पर ट्रिगर किए गए अतुल्यकालिक व्यावसायिक तर्क जोड़ें।
सूचनाओं के लिए आदर्श,
सूचनाओं के लिए आदर्श, Postgres से डेटा-पाइपलाइन या अतुल्यकालिक
प्रसंस्करण - [अधिक पढ़ें](event-triggers.md).

### व्युत्पन्न डेटा या डेटा परिवर्तन

Postgres में डेटा ट्रांसफ़ॉर्म करें या किसी अन्य डेटासेट को प्राप्त करने के लिए उस पर व्यावसायिक लॉजिक चलाएं, जिसे GraphQL इंजन का उपयोग करके क्वेर किया जा सकता है - [अधिक पढ़ें](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## प्रदर्शन

[community/sample-apps](community/sample-apps) निर्देशिका में सभी उदाहरण एप्लिकेशन देखें।

### रीयलटाइम अनुप्रयोग

- रिएक्ट के साथ बनाया गया ग्रुप चैट एप्लिकेशन में एक टाइपिंग इंडिकेटर, ऑनलाइन उपयोगकर्ता और नया शामिल हैं
  संदेश सूचनाएं।
  - [कोशिश करके देखो](https://realtime-chat.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-chat)
  - [एपीआई ब्राउज़ करें](https://realtime-chat.demo.hasura.app/console)

- लाइव लोकेशन ट्रैकिंग ऐप जो एक चालू वाहन को वर्तमान जीपीएस निर्देशांक को बदलते हुए दिखाता है जो एक नक्शे पर आगे बढ़ता है।
  - [कोशिश करके देखो](https://realtime-location-tracking.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-location-tracking)
  - [एपीआई ब्राउज़ करें](https://realtime-location-tracking.demo.hasura.app/console)

- लगातार बदलते डेटा पर डेटा एकत्रीकरण के लिए एक रीयलटाइम डैशबोर्ड।
  - [कोशिश करके देखो](https://realtime-poll.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-poll)
  - [एपीआई ब्राउज़ करें](https://realtime-poll.demo.hasura.app/console)

### वीडियो

* [एक स्व-होस्टित GitLab उदाहरण के लिए GraphQL जोड़ें](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 मिनट*)
* [Auth0 और GraphQL बैकएंड के साथ टोडो ऐप](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 मिनट*)
* [GitLab पर GraphQL GitLab प्रमाणीकरण के साथ एकीकृत है](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 मिनट*)
* [10million के लिए डैशबोर्ड भू-स्थान के साथ सवारी करता है (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 मिनट*)


## सहायता और समस्या निवारण

प्रलेखन और समुदाय आपको अधिकांश मुद्दों का निवारण करने में मदद करेंगे। यदि आपको बग का सामना करना पड़ा है या हमारे साथ संपर्क करने की आवश्यकता है, आप निम्नलिखित में से किसी एक चैनल का उपयोग करके हमसे संपर्क कर सकते हैं:

* समर्थन और प्रतिक्रिया: [Discord](https://discord.gg/hasura)
* समस्या और बग ट्रैकिंग: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* उत्पाद अपडेट का पालन करें: [@HasuraHQ](https://twitter.com/hasurahq)
* हमसे बात करो हमारी [वेबसाइट चैट](https://hasura.io)

हम समुदाय में एक खुले और स्वागत वाले माहौल को बढ़ावा देने के लिए प्रतिबद्ध हैं। कृपया देखें [आचार संहिता](code-of-conduct.md)

यदि आप किसी सुरक्षा समस्या की रिपोर्ट करना चाहते हैं, तो कृपया [इसे पढ़ें](SECURITY.md)

## योगदान

अधिक जानकारी के लिए हमारी [योगदान करने वाली मार्गदर्शिका](CONTRIBUTING.md) देखें।

## ब्रांड संपत्ति

हसुरा ब्रांड की संपत्ति (लोगो, हसुरा शुभंकर, बैज आदि द्वारा संचालित) [आस्तियों / ब्रांड](assets/brand) फ़ोल्डर में हो सकती है। 

बेझिझक उन्हें अपने एप्लिकेशन / वेबसाइट आदि में उपयोग करें। "Hasura द्वारा संचालित ❤️" को जोड़ने पर हम रोमांचित हो जाएंगे
Hasura के उपयोग से निर्मित आपके अनुप्रयोगों के लिए बिल्ला। 

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

## लाइसेंस

कोर GraphQL इंजन [Apache लाइसेंस 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0) के तहत उपलब्ध है। 

सभी **अन्य सामग्री** (इन्हें छोड़कर [`server`](server), [`cli`](cli) और
[`console`](console) निर्देशिका) [एमआईटी लाइसेंस](LICENSE-community) के तहत उपलब्ध है।
इसमें [`डॉक्स`](docs) और [`समुदाय`](community) निर्देशिका सब कुछ शामिल है।

## अनुवाद

यह रीडमी निम्नलिखित अनुवादों में उपलब्ध है:

- [जापानी :jp:](translations/README.japanese.md) (:pray: [@moksahero](https://github.com/moksahero))
- [फ्रेंच :fr:](translations/README.french.md) (:pray: [@l0ck3](https://github.com/l0ck3))
- [बोस्नियाई :bosnia_herzegovina:](translations/README.bosnian.md) (:pray: [@hajro92](https://github.com/hajro92))
- [रूसी:ru:](translations/README.russian.md) (:pray: [@highflyer910](https://github.com/highflyer910))
- [यूनानी 🇬🇷](translations/README.greek.md) (:pray: [@MIP2000](https://github.com/MIP2000))
- [स्पेनिश 🇲🇽](/translations/README.mx_spanish.md)(:pray: [@ferdox2](https://github.com/ferdox2))
- [इन्डोनेशियाई :indonesia:](translations/README.indonesian.md) (:pray: [@anwari666](https://github.com/anwari666))
- [ब्राजिलियन पुर्तगाली :brazil:](translations/README.portuguese_br.md) (:pray: [@rubensmp](https://github.com/rubensmp))
- [जर्मन 🇩🇪](translations/README.german.md) (:pray: [@FynnGrandke](https://github.com/FynnGrandke))
- [चीनी :cn:](translations/README.chinese.md) (:pray: [@jagreetdg](https://github.com/jagreetdg) & [@johnbanq](https://github.com/johnbanq))
- [तुर्की :tr:](translations/README.turkish.md) (:pray: [@berat](https://github.com/berat))
- [कोरियाई :kr:](translations/README.korean.md) (:pray: [@라스크](https://github.com/laskdjlaskdj12))

अन्य फ़ाइलों के लिए अनुवाद [यहाँ](translations) मिल सकते हैं



