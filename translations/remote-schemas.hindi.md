# Remote schemas

अगर आप एक ही जगह से सभी प्रकार की GraphQL की क्वेरी करना चाहते हैं तो रिमोट GraphQL स्कीमा को GraphQL इंजन के Postgres आधारित स्कीमा के साथ विलय करे। रिमोट स्कीमा कुछ उदाहरण के लिए आदर्श होते हैं जैसे:

* म्यूटेशन का अनुकूलन (जैसे इन्सर्ट से पहले वेलिडेशन करना)
* भुगतान जैसी सुविधाओं का समर्थन करना, आदि और उन्हें एक्सेस करने के लिए एक सुसंगत इंटरफ़ेस प्रदान करना यानि GraphQL Engine के API के पीछे
* अन्य सूत्रों से डेटा अलग करना (*जैसे मौसम के एपीआई या किसी अन्य डेटाबेस से*)

कस्टम व्यवसाय तर्क का समर्थन करने के लिए, आपको एक कस्टम GraphQL सर्वर बनाने की आवश्यकता होगी (देखे [boilerplates](../community/boilerplates/remote-schemas)) और उसके स्कीमा को GraphQL Engine के साथ विलय करे।

![remote schems architecture](../assets/remote-schemas-arch.png)

## डेमो (*40 सेकंड*)

[![video demo of merging remote schemas](https://img.youtube.com/vi/eY4n9aPsi0M/0.jpg)](https://www.youtube.com/watch?v=eY4n9aPsi0M)

[रिमोट GraphQL स्कीमा को मर्ज करें (YouTube link)](https://youtu.be/eY4n9aPsi0M)

## शुरू करे 

रिमोट स्कीमा को आज़माने का सबसे तेज़ तरीका Heroku है।

1. मुफ्त Postgres के साथ Heroku पर GraphQL Engine को डेप्लॉय करने के लिए निम्न बटन पर क्लिक करें:

    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Hasura कंसोल खोलें

   एडमिन कंसोल को खोलने के लिए `https: // <app-name> .herokuapp.com` (* अपने ऐप के नाम के साथ * <app-name \> बदलें) पर जाएँ।

3. अपना पहला रिमोट स्कीमा मर्ज करें और इसे क्वेरी करें

   In the admin console, open the ``Remote Schemas`` tab and click on the ``Add`` button.एडमिन कंसोल में ``Remote Schemas`` टैब खोलें और `` Add`` बटन पर क्लिक करें। निम्नलिखित विवरण भरें:
   * Remote Schema नाम: ``countries`` (*इस remote schema के लिए एक उपनाम *).
   * GraphQL server URL: ``https://countries.trevorblades.com/`` (*एक सार्वजनिक GraphQL API जिसे हम इस सुविधा की शीघ्रता से जांच करने के लिए उपयोग करेंगे;[@trevorblades](https://github.com/trevorblades) द्वारा बनाए रखा गया।*
   * शेष कॉन्फ़िगरेशन सेटिंग्स पर ध्यान न दें और ``Add Remote Schema`` बटन पर क्लिक करें।

   ``GraphiQL` टैब पर जाएं और निम्नलिखित क्वेरी चलाएँ (*बाईं ओर क्वेरी विंडो में पेस्ट करें और * ▶️ *(play) बटन पर क्लिक करें *):

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

   आप ``GraphiQL`` इंटरफ़ेस के शीर्ष दाएं कोने में ``Docs`` एक्सप्लोरर का उपयोग करके रिमोट स्कीमा से GraphQL के प्रकारों का पता लगा सकते हैं।

## Boilerplates

लोकप्रिय भाषाओं / फ़्रेमवर्क्स में कस्टम GraphQL सर्वर के लिए Boilerplates उपलब्ध हैं।

* [नियमित boilerplates](../community/boilerplates) जिसे कहीं भी deploy किया जा सकता है।
* [सर्वर रहित boilerplates](https://github.com/hasura/graphql-serverless) जिसे सर्वर रहित प्लेटफार्मों जैसे AWS Lambda, आदि पर deploy किया जा सकता है।

कृपया ध्यान दें कि अधिक भाषाओं, फ्रेमवर्क, सर्वर रहित प्लेटफ़ॉर्म आदि के लिए boilerplates को पुनरावृत्त किया जा रहा है और सामुदायिक योगदान बहुत स्वागत योग्य है।


## Caveats

**वर्तमान सीमाएँ**:

* सभी मर्ज किए गए स्कीमा में टाइप नाम और नोड नाम अद्वितीय होने चाहिए (केस-सेंसिटिव मिलान)। अगले कुछ पुनरावृत्तियों में, सटीक नाम और संरचना के साथ मर्ज करने के तरीको का समर्थन उपलब्ध होगा।
* एक ही क्वेरी / म्यूटेशन में विभिन्न GraphQL सर्वर से नोड्स का उपयोग नहीं किया जा सकता है। सभी टॉप-लेवल नोड्स को एक ही GraphQL सर्वर से होना चाहिए।
* Subscriptions on remote GraphQL server are not supported. रिमोट GraphQL सर्वर पर सदस्यता समर्थित नहीं हैं।

इन सीमाओं को आने वाले वर्जन्स में संबोधित किया जाएगा।

## प्रलेखन

पूरा पढ़ें [प्रलेखन](https://hasura.io/docs/1.0/graphql/manual/remote-schemas/index.html).

## अनुवाद

यह दस्तावेज़ निम्नलिखित अनुवादों में उपलब्ध है:

- [French :fr:](translations/remote-schemas.french.md)
