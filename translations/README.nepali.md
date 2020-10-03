# हसुरा ग्राफक्युएल ईन्जिन

[![पछिल्लो विमोचन](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
[![कागजात](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
[![सर्कलसीआई](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

हसुरा ग्राफक्युएल ईन्जिन एक ब्लाजिंग-फास्ट ग्राफिक सर्भर हो जुन तपाईलाई **पोस्टग्रेसमा तत्काल, रियलटाइम ग्राफिक एपीआईहरू** दिन्छ, साथै व्यापार तर्कको लागि डाटाबेस घटनाहरूमा [**वेबहुक ट्रिगर**](event-triggers.md), र [**रिमोट स्कीमा**](remote-schemas.md) पनि दिन्छ।

हसुराले तपाईंलाई पोष्टग्रेस द्वारा समर्थित [ग्राफक्युएल](https://hasura.io/graphql/) सिर्जना गर्न मद्दत गर्दछ वा पोष्टग्रेस प्रयोग गरेर अवस्थित अनुप्रयोगहरूको लागि ग्राफक्युएलमा क्रमिक रूपमा सार्न मदत गर्छ।

[हसुरा.आइ ओ](https://hasura.io) र [कागजात](https://hasura.io/docs/) मा थप पढ्नुहोस्।

------------------

![हसुरा ग्राफक्युएल ईन्जिन डेमो](../assets/demo.gif)

------------------

![हसुरा ग्राफक्युएल ईन्जिन रियलटाइम डेमो](../assets/realtime.gif)

-------------------

## विशेषताहरु

* **शक्तिशाली प्रश्नहरू बनाउनुहोस्**: निर्मित फिल्टरिंग, पेजिनेसन, प्याटर्न खोजी, बल्क सम्मिलित, अपडेट गर्नुहोस्, म्यूटेशनहरू मेटाउनुहोस्
* **रियलटाइम**: कुनै पनि ग्राफक्युएल क्वेरीलाई सदस्यता प्रयोग गरेर प्रत्यक्ष प्रश्नमा रूपान्तरण गर्नुहोस्
* **रिमोट स्किमाहरू मर्ज गर्नुहोस्**: एकल ग्राफक्युएल इञ्जिनको अन्तिम पोइन्ट मार्फत व्यवसाय तर्कको लागि कस्टम ग्राफक्युएल स्किमाहरू पहुँच गर्नुहोस्। [**थप पढ्नुहोस्**](remote-schemas.nepali.md).
* **ट्रिगर वेबबुक वा सर्वर रहित कार्यहरू**: पोस्टग्रेसमा घटनाहरू इन्सेर्ट/अपडेट/डिलित गर्नुहोस्([थप पढ्नुहोस्](event-triggers.md))
* **अवस्थित, प्रत्यक्ष डाटाबेसको साथ काम गर्दछ**: ग्राफक्युएल तत्काल प्रयोग गर्नका लागि तुरून्त यसलाई अवस्थित पोस्टग्रेस डाटाबेसमा देखाउनुहोस्।
* **फाइन-ग्रेन्ड पहुँच नियन्त्रण**: डाईनामिक एक्सेस कन्ट्रोल जुन तपाईंको प्रमाणिकरण प्रणालीसँग समाहित हुन्छ (उदाहरण: अथजेरो, फायरबेस-अथ)
* **उच्च प्रदर्शन र कम-पदचिन्ह**: ~१५ एमबि डकर इमेज; ~५० एमबि RAM @ १००० req/s; मल्ति-कोर अवेर 
* **प्रशासन युआइ  र माइग्रेसनहरू**: प्रशासन युआइ र रेलस-प्रेरित स्कीमा माइग्रेसनहरू
* **पोस्टग्रेस** ❤️: पोस्टग्रेस प्रकार (PostGIS / भू-स्थान, आदि) समर्थन गर्दछ, दृश्यलाई *ग्राफ्स* मा परिवर्तन गर्दछ, भण्डार गरिएको कार्यहरू वा प्रक्रियाहरू उत्परिवर्तनको साथ ट्रिगर गर्दछ

[हसुरा.आइ ओ](https://hasura.io) र [कागजात](https://hasura.io/docs/) मा थप पढ्नुहोस्।

## सामग्री को तालिका
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**सामग्री को तालिका**

- [द्रुतसुरु:](#quickstart)
    - [हसुरा क्लाउडमा एक क्लिक डिप्लोयमेन्ट](#one-click-deployment-on-hasura-cloud)
    - [अन्य एक क्लिक डिप्लोयमेन्ट विकल्पहरू](#other-one-click-deployment-options)
    - [अन्य डिप्लोयमेन्ट विधिहरू](#other-deployment-methods)
- [वास्तुकला](#architecture)
- [ग्राहक साइड टूलिंग](#client-side-tooling)
- [व्यापार तर्क थप्नुहोस्](#add-business-logic)
    - [रिमोट स्किमा](#remote-schemas)
    - [डाटाबेस घटनाहरूमा वेबहूक्स ट्रिगर गर्नुहोस्](#trigger-webhooks-on-database-events)
- [डेमो](#demos)
    - [रियलटाइम अनुप्रयोगहरू](#realtime-applications)
    - [भिडियोहरू](#videos)
- [समर्थन र समस्या निवारण](#support--troubleshooting)
- [योगदान गर्दै](#contributing)
- [ब्राण्ड सम्पत्ति](#brand-assets)
- [लाइसेन्स](#license)
- [अनुवाद](#translations)

<!-- markdown-toc end -->

## द्रुतसुरु:

### हसुरा क्लाउडमा एक क्लिक डिप्लोयमेन्ट

हसुरालाई चलाउन चाँडो र सजिलो तरीका भनेको [हासुरा क्लाउड](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html) मार्फत हो।

1. हसुरा क्लाउडमा ग्राफक्युएल इन्जिनलाई पोष्ट गर्न पोस्टग्रेस एड-अन सहित वा अवस्थित पोष्टग्रेस डाटाबेसको लागि निम्न बटनमा क्लिक गर्नुहोस् :

    [![हसुरा क्लाउडमा दिप्लोय गर्नुहोस](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. हसुरा कन्सोल खोल्नुहोस्

   हसुरा कन्सोल खोल्न "लन्च कन्सोल" बटन मा क्लिक गर्नुहोस्।

3. तपाईंको पहिलो ग्राफक्युएल क्वेरी बनाउनुहोस्

   एउटा तालिका सिर्जना गर्नुहोस् र तुरुन्त तपाईंको पहिलो क्वेरी चलाउनुहोस्। यो [साधारण गाइड](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html) अनुसरण गर्नुहोस्।

### अन्य एक क्लिक डिप्लोयमेन्ट विकल्पहरू

निम्नलिखित एक क्लिक डिप्लोयमेन्ट विकल्पहरूको लागि निर्देशन जाँच गर्नुहोस्:

| **इन्फ्रा प्रदायक** | **एक क्लिक लिंक** | **थप जानकारी** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| हिरोकु | [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| डिजिटलओसन | [![Deploy to DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| एजुर | [![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| रेन्डर | [![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### अन्य डिप्लोयमेन्ट विधिहरू

डकर-आधारित डिप्लोयमेन्ट र उन्नत कन्फिगरेसन विकल्पहरूको लागि, हेर्नुहोस् [डिप्लोयमेन्ट गाइड](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) वा
[इन्स्टल म्यनिफेस्ट](install-manifests)

## वास्तुकला

हसुरा ग्राफक्युएल इञ्जिनले एक पोस्टग्रेस डाटाबेस दृष्टान्तलाई फ्रन्ट गर्दछ र तपाइँको ग्राहक अनुप्रयोगहरूबाट ग्राफक्युएल अनुरोधहरू स्वीकार गर्न सक्दछ। यो तपाईंको अवस्थित अथ प्रणालीको साथ काम गर्नको लागि कन्फिगर गर्न सकिन्छ र तपाईंको अधिकार प्रणालीबाट डाईनामिक भ्यारीएबलको साथ क्षेत्र-स्तर नियमहरू प्रयोग गरेर पहुँच नियन्त्रण ह्यान्डल गर्न सक्दछ।

तपाईं टाढाको ग्राफक्युएल स्किमाहरू मर्ज गर्न र एकीकृत ग्राफक्युएल एपीआई प्रदान गर्न सक्नुहुनेछ।

![हसुरा ग्राफक्युएल इन्जिन वास्तुकला](../assets/hasura-arch.svg)

## ग्राहक साइड टूलिंग

हसुरा कुनै पनि ग्राफक्युएल ग्राहकको साथ काम गर्दछ। हामी [अपोलो ग्राहक](https://github.com/apollographql/apollo-client) को सिफारिश गर्दछौं। ग्राहकहरूको सूचीको लागि [असम -ग्राफक्युएल](https://github.com/chentsulin/awesome-graphql) हेर्नुहोस्।

## व्यापार तर्क थप्नुहोस्

ग्राफक्युएल इञ्जिनले तपाईको ब्याकइन्डमा अनुकूल व्यापार तर्क थप्नका लागि सजिलो-कारण, स्केलेबल र प्रदर्शन विधिहरू प्रदान गर्दछ।

### रिमोट स्किमा

हसुराको पोस्टग्रेस-आधारित ग्राफक्युएल स्किमाको अतिरिक्त दूरस्थ स्कीमामा अनुकूलन समाधान गर्नेहरू थप्नुहोस्। भुक्तानी एपीआई कार्यान्वयन, वा तपाईंको डाटाबेसमा नभएको डाटा क्वेरी गर्ने जस्ता प्रयोगका केसहरूका लागि आदर्श - [थप पढ्नुहोस्](remote-schemas.md).

### डाटाबेस घटनाहरूमा वेबहूक्स ट्रिगर गर्नुहोस्

एसिन्क्रोनस व्यापार तर्क जोड जो डेटाबेस घटनाहरूमा आधारित ट्रिगर भयो।
सूचनाहरूको लागि आदर्श, पोष्टग्रेस वा एसिन्क्रोनसबाट डाटा-पाइपलाइनहरू प्रशोधन गर्दै - [थप पढ्नुहोस्](event-triggers.md).

### व्युत्पन्न डाटा वा डाटा रूपान्तरण

डाटालाई पोस्टग्रेसमा रूपान्तरण गर्नुहोस् वा त्यसमा व्यापार तर्क चलाउनुहोस् अर्को डाटासेट निकाल्नको लागि जुन ग्राफक्युएल इन्जिन प्रयोग गरेर क्वेरी गर्न सकिन्छ - [थप पढ्नुहोस्](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## डेमो

[community/sample-apps](community/sample-apps) डाइरेक्टरीमा सबै उदाहरण अनुप्रयोगहरू हेर्नुहोस।

### रियलटाइम अनुप्रयोगहरू

- प्रतिक्रियासँग बनाइएको समूह कुराकानी अनुप्रयोगमा एक टाइपिंग सूचक, अनलाइन प्रयोगकर्ताहरू र नयाँ सन्देश सूचनाहरू सामेल छन्।
  - [प्रयोग गर्नुहोस्](https://realtime-chat.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-chat)
  - [एपीआईहरू ब्राउज गर्नुहोस](https://realtime-chat.demo.hasura.app/console)

- प्रत्यक्ष स्थान ट्र्याकिंग एप जुन चलिरहेको सवारीसाधन नक्सामा सर्दै हालको जीपीएस निर्देशांक बदल्दै देखाउँदछ।
  - [प्रयोग गर्नुहोस्](https://realtime-location-tracking.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-location-tracking)
  - [एपीआईहरू ब्राउज गर्नुहोस](https://realtime-location-tracking.demo.hasura.app/console)

- डाटा परिवर्तन गर्न डाटामा एकत्रीकरणको लागि एक रियलटाइम ड्यासबोर्ड।
  - [प्रयोग गर्नुहोस्](https://realtime-poll.demo.hasura.app/)
  - [ट्यूटोरियल](community/sample-apps/realtime-poll)
  - [एपीआईहरू ब्राउज गर्नुहोस](https://realtime-poll.demo.hasura.app/console)

### भिडियोहरू

* [एउटा स्व-होस्ट गरिएको गितलाब दृष्टान्तमा ग्राफक्युएल जोड्नुहोस्](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 mins*)
* [अथजेरो र ग्राफक्युएल ब्याकइन्डको साथ टुडु एप](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 mins*)
* [गितलाब मा ग्राफक्युएल गितलाब अथको साथ एकीकृत](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 mins*)
* [भू-स्थान(पोस्ट-जीआइएस, टाइमस्केल) को साथ दश मिलियन सवारीको लागि ड्यासबोर्ड](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 mins*)


## समर्थन र समस्या निवारण

कागजात र समुदायले तपाईंलाई अधिक समस्याहरूको निवारणमा मद्दत गर्दछ। यदि तपाईंले बग सामना गर्नु भएको छ वा हामीसँग सम्पर्कमा बस्नु आवश्यक छ भने, तपाईंले तलका मध्ये एक प्रयोग गरी हामीलाई सम्पर्क गर्न सक्नुहुन्छ:

* समर्थन र प्रतिक्रिया: [Discord](https://discord.gg/hasura)
* मुद्दा र बग ट्र्याकिंग: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* उत्पादन अपडेट अनुसरण गर्नुहोस्s: [@HasuraHQ](https://twitter.com/hasurahq)
* हाम्रो [वेबसाइट च्याट](https://hasura.io) मा हामीसँग कुरा गर्नुहोस्।

हामी समुदायमा खुला र स्वागत योग्य वातावरण विकास गर्न प्रतिबद्ध छौं। कृपया [आचार संहिता](code-of-conduct.nepali.md) हेर्नुहोस्।

यदि तपाईं सुरक्षा मुद्दा रिपोर्ट गर्न चाहानुहुन्छ भने, कृपया [यसलाई पढ्नुहोस्](SECURITY.nepali.md)।

## योगदान गर्दै

थप विवरणहरूको लागि हाम्रो [योगदान दिने गाइड](CONTRIBUTING.nepali.md) जाँच गर्नुहोस्।.

## ब्राण्ड सम्पत्ति

हसुरा ब्रान्ड सम्पत्ति (लोगोहरू, हसुरा मस्कट, ब्याज आदि) [assets/brand](../assets/brand) फोल्डरमा फेला पार्नसक्नुहुन्छ। यसलाई तपाईको एप/वेबसाइटमा प्रयोग गर्न सक्नुहुन्छ। यदि तपाईले "Powered by Hasura" ब्याज थप्नुभयो भने हामी अत्यन्तै खुसी हुनेछौं। ❤️

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

## लाइसेन्स

मुख्य ग्राफक्युएल इन्जिन [अपाचे लाइसेन्स २.०](https://www.apache.org/licenses/LICENSE-2.0)(Apache-2.0) अन्तर्गत उपलब्ध छ।

सबै **अन्य सामग्री** ([[सर्वर`](../server), [`क्लाइट](../cli) ,
[`console`](../console) निर्देशिकाहरू मा बाहेक) [MIT लाइसेन्स](../LICENSE-community) अन्तर्गत उपलब्ध छन्।
यसले निर्देशिका [`कागजात](../docs) र [` समुदाय`](../community) मा सबै समावेश गर्दछ।

## अनुवाद

यो रीडमी निम्न अनुवादहरूमा उपलब्ध छ:

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

अन्य फाईलहरूको लागि अनुवादहरू [यहाँ](translations) फेला पार्न सकिन्छ।
