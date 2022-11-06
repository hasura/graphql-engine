# हसुरा ग्राफिक-इंजिनमध्ये सहयोग देणे

_प्रथमतः_: आपण योगदान कसे सुरू करावे याबद्दल असुरक्षित वाटत असल्यास, # नियंत्रण चॅनेलमधील आमच्या [Discord Channel](https://discordapp.com/invite/hasura) वर मोकळ्या मनाने विचारा. आपण आपल्या योगदानासह पुढे जाऊ शकता आणि आम्ही आपला अभिप्राय देऊ. काळजी करू नका - सर्वात वाईट घडू शकते ते म्हणजे आपल्याला काहीतरी बदलण्यास नम्रपणे निवेदन केला जाईल. आम्ही कोणत्याही योगदानाचे कौतुक करतो आणि त्या मार्गावर उभे राहण्यासाठी आम्हाला नियमांची भिंत नको आहे.

तथापि, ज्यांना या प्रकल्पात योगदान देण्याच्या चांगल्या मार्गावर थोडे अधिक मार्गदर्शन हवे आहे त्यांच्यासाठी वाचा. ही लेखी माहिती आम्ही ज्या गोष्टी शोधत आहोत त्याचा आच्छादन करेल. खाली दिलेल्या मुद्द्यांकडे लक्ष वेधून, आम्ही शक्यताद्रुतपणे विलीन होऊ शकतात किंवा आपले योगदान वाढेल यावर संबोधू शकता.

## सामग्री सारणी

[1. आचारसंहिता ](#code-of-conduct)

[2. Repo सर्वेक्षण ](#overview)

[3. प्रथमच योगदानाचे हार्दिक स्वागत! ](#first-timers)

[4. मदतीची क्षेत्रे ](#areas)

[5. योगदान देण्याचे मार्ग ](#ways)

[6. Commit संदेश ](#commit-messages)

[7. भाषांतर ](#translations)

<a name="code-of-conduct"></a>

## 1. आचारसंहिता

कृपया हसुराला दिलेल्या कोणत्याही योगदानाच्या संदर्भात आमच्या [आचारसंहिता](code-of-conduct.md) चे अनुसरण करा.

<a name="overview"></a>

## 2. Repo सर्वेक्षण

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) एक mono-repo आहे ज्यामध्ये 3 भाग आहेत. प्रत्येकाचे स्वत: चे योगदान देणारे मार्गदर्शक आहेत:

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

तिन्ही घटकांमधे एक म्हणणे आहे, एकतर git tag किंवा शाखेचे नाव आणि git commit SHAच्या मिश्रणाद्वारे दर्शविली जाते.

For all contributions, a CLA (Contributor License Agreement) needs to be signed [here](https://cla-assistant.io/hasura/graphql-engine) before (or after) the pull request has been submitted. A bot will prompt contributors to sign the CLA via a pull request comment, if necessary.

<a name="first-timers"></a>

## 3. प्रथमच योगदानाचे हार्दिक स्वागत!

We appreciate first time contributors and we are happy to assist you in getting started. In case of questions, just reach out to us!

You find all issues suitable for first time contributors [here](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. मदतीची क्षेत्रे

नक्कीच, आम्ही हसुराच्या सर्व भागांच्या योगदानाचे कौतुक करतो. तथापि, आम्ही तीन क्षेत्रे शोधली आहेत जी open source योगदानासाठी विशेषतः योग्य आहेत.

### दस्तऐवज

आमचे ध्येय आमचे दस्तऐवज व्यापक आणि अद्ययावत ठेवणे आहे. आपण असे करण्यास आम्हाला मदत करू इच्छित असल्यास आम्ही कोणत्याही प्रकारच्या योगदानाबद्दल आभारी आहोत:

- गहाळ सामग्रीचा अहवाल द्या

- विद्यमान डॉक्समध्ये त्रुटी दूर करा

- दस्तऐवज जोडण्यात आम्हाला मदत करा

दस्तऐवजांसाठी योगदान देणारी मार्गदर्शक व्यक्ती [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md)वर आढळू शकते.

### समुदाय सामग्री

आम्ही आमचे [शिक्षण पृष्ठ](https://hasura.io/learn/) सुरू केल्यापासून, योगदानाबद्दल आम्हाला आनंद आहे:, 

- विद्यमान शिकवण्या शिकवण्यांमध्ये त्रुटी दूर करा

- नवीन ट्यूटोरियल जोडा (डुप्लिकेट शब्द टाळण्यासाठी आपल्याकडे कल्पना असल्यास कृपया आमच्यापर्यंत संपर्क साधा)

लर्न रिपॉझिटरीचे README [येथे](https://github.com/hasura/learn-graphql) आढळू शकते.

शिकण्याच्या सामग्री व्यतिरिक्त, आम्ही तांत्रिक समुदाय सामग्रीसह सहयोग करण्याचे आणखी तीन मार्ग ओळखले आहेत:


- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [नमुना अनुप्रयोग](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [साधने](community/tools)

आपण समुदायामध्ये योगदान देऊ इच्छित असल्यास

- तांत्रिक ब्लॉग पोस्ट लिहिणे

- एका कार्यक्रमात बोलणे

- एक कार्यशाळेचे आयोजन

आमचे [community wiki](https://github.com/hasura/graphql-engine/wiki/Community-Wiki) पहा.

वर नमूद केलेल्या कोणत्याही गोष्टींशी संबंधित नसले तरीही आपल्याकडे जोडण्यासाठी काही असल्यास pull request सबमिट करा.

### हसुरा CLI

आमच्याकडे CLI वर काही समस्या आहेत जे ओपन सोर्स योगदानासाठी योग्य आहेत. आपल्याला गो माहित असल्यास किंवा आपण हे करुन ते जाणून घेऊ इच्छित असल्यास, पुढील [समस्या](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22) पहा.

CLI repositoryचे README [येथे](https://github.com/hasura/graphql-engine/tree/master/cli) आढळू शकते.

<a name="ways"></a>

## 5. योगदान देण्याचे मार्ग

### कमतरता तक्रार करण्यासाठी

- नवीनतम आवृत्तीवर आपली चाचणी असल्याचे सुनिश्चित करा. आपण कदाचित अनुभवत असलेले बग आम्ही आधीच निश्चित केले असेल.

- पोस्टग्रेस आवृत्तीसह, समस्येचे पुनरुत्पादित करण्यासाठी चरण प्रदान करा, ग्राफिक-इंजिन आवृत्ती आणि आपण चालवित असलेले प्रदाता (हीरोकू, डॉकर, इ.).

- कृपया संबंधित असल्यास सर्व्हरचे लॉग समाविष्ट करा.

### एखाद्या समस्येवर काम करत असताना

- आम्ही [fork-and-branch git workflow](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/) वापरतो.

- कृपया आपण करीत असलेल्या कार्याशी संबंधित काही समस्या असल्याचे सुनिश्चित करा.

- आपण एखाद्या समस्येवर काम करत असल्यास, कृपया टिप्पणी द्या की आपण इतरांकडून डुप्लिकेट काम रोखण्यासाठी असे केले जात आहे.

- आपली कमिटी स्क्वॅश करा आणि शेवटी कमिट संदेशामध्ये `fix #<issue-no>` किंवा `close #<issue-no>` वापरून समस्येचा संदर्भ घ्या.
  उदाहरणार्थ: `resolve answers to everything (fix #42)` किंवा `resolve answers to everything, fix #42`

- Pull request सबमिट करण्यापूर्वी आपल्या शाखेसह masterला rebase करावे.

<a name="commit-messages"></a>

## 6. Commit संदेश

- प्रथम ओळ केलेल्या बदलांचा सारांश असावा, 50 वर्णांपेक्षा जास्त नसावा, त्यानंतर पर्यायी संस्था ज्यामध्ये बदलांविषयी अधिक माहिती असेल. चांगले commit संदेश लिहिण्यासाठी अधिक माहितीसाठी [या दुव्याचा](https://github.com/erlang/otp/wiki/writing-good-commit-messages) संदर्भ घ्या.

- आज्ञावाचक वर्तमान काळ वापरा: "add/fix/change", "added/fixed/changed" किंवा "adds/fixes/changes" नाही.

- सारांश ओळीचे पहिले अक्षर capital करू नये.

- सारांश ओळीच्या शेवटी कालावधी / बिंदू (.) जोडू नये.

<a name="translations"></a>

## 7. भाषांतर

हे दस्तऐवज पुढील अनुवादांमध्ये उपलब्ध आहेः

- [French 🇫🇷](translations/CONTRIBUTING.french.md)

(क्रेडिट्स: काही विभाग https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md तून रुपांतरित झाले आहेत)
