#  &#x202b;  מנוע GraphQL של Hasura

&#x202b; [![גרסא אחרונה](https://img.shields.io/github/v/release/hasura/graphql-engine)](https://github.com/hasura/graphql-engine/releases/latest)
&#x202b; [![מסמכים](https://img.shields.io/badge/docs-v1.x-brightgreen.svg?style=flat)](https://hasura.io/docs)
&#x202b; [![CircleCI](https://circleci.com/gh/hasura/graphql-engine.svg?style=shield)](https://circleci.com/gh/hasura/graphql-engine)


<a href="https://discord.gg/vBPpJkS"><img src="https://img.shields.io/badge/chat-discord-brightgreen.svg?logo=discord&style=flat"></a>
<a href="https://twitter.com/intent/follow?screen_name=HasuraHQ"><img src="https://img.shields.io/badge/Follow-HasuraHQ-blue.svg?style=flat&logo=twitter"></a>
<a href="https://eepurl.com/dBUfJ5"><img src="https://img.shields.io/badge/newsletter-subscribe-yellow.svg?style=flat"></a>

&#x202b; מנוע GraphQL של Hasura הוא שרת GraphQL מהיר שמאפשר **פניות API של GraphQL בצורה מיידית, בזמן אמת מעל Postgres** עם [**טריגרי וובהוק**](event-triggers.md) על ארועי דאטהבייס ו [**סכמות מרוחקות**](remote-schemas.md) עבור לוגיקה עסקית.
&#x202b; Hasura עוזרת לך לבנות יישומי [GraphQL](https://hasura.io/graphql/) מעל Postgress או לעבור בצורה הדרגתית לGraphQL עבור יישומים קיימים המשתמשים ב-Postgress.

&#x202b; קרא עוד ב [hasura.io](https://hasura.io) ואת ה [תיעוד](https://hasura.io/docs).

------------------

![Hasura GraphQL Engine Demo](assets/demo.gif)

------------------

![Hasura GraphQL Engine Realtime Demo](assets/realtime.gif)

-------------------

## &#x202b; תכונות

* &#x202b;  **בנה שאילתות עוצמתיות**: מאפשר סינון מובנה, חלוקה לדפים, חיפוש לפי תבנית, הכנסה של קבוצה, עדכון, מחיקה של מוטציות
* &#x202b;  **זמן אמת**: מאפשר המרת כל שאילתת GraphQL לשאילתה בזמן אמת על ידי שימוש במינויים (subscriptions).
* &#x202b;  **מזג סכמות מרוחקות**: מאפשר גישה לסכמות GraphQL מותאמות אישית למידע עסקי על ידי נק׳ קצה יחידה של מנוע Graph QL.  [**קרא עוד**](remote-schemas.md).
* &#x202b;  **הפעלת וובהוקס או פונקציות serverless**: לפי ארועי הכנסה/עדכון/מחיקה של Postgres.   ([קרא עוד](event-triggers.md))
* &#x202b;  **עובד עם דאטהבייסים קיימים וחיים**: כוון את המערכת לדאטהבייס Postgres קיים בשביל לקבל ממשק GraphQL מוכן לעבודה.
* &#x202b;  **בקרת גישה מצומצמת ומדוייקת**: בקרת גישה דינמית המתממשקת עם מערכת ההרשאות שלך (לדוג׳: auth0, firebase-auth).
* &#x202b;  **ביצועים גבוהים וחתימת זכרון נמוכה**:  ~15MB אימג׳ דוקר; זכרון ~50MB RAM @ 1000 req/s; תמיכה בריבוי ליבות.
* &#x202b;  ** ממשק ניהול וניוד**: ממשק ניהול ומערכת ניוד סכמות בהשראת Rails
* &#x202b;  **Postgres** ❤️: תמיכה בקבצי Postgres (PostGIS/geo-location, etc.), הפיכת תצוגות ל*גרפים*, הפעלת פונקציות שמורות או פרוצדורות עם מוטציות.

&#x202b; קרא עוד ב-[hasura.io](https://hasura.io) וגם ב- [תיעוד](https://hasura.io/docs/).

## &#x202b;  תוכן עניינים
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Quickstart:](#quickstart)
    - [One-click deployment on Hasura Cloud](#one-click-deployment-on-hasura-cloud)
    - [Other one-click deployment options](#other-one-click-deployment-options)
    - [Other deployment methods](#other-deployment-methods)
- [Architecture](#architecture)
- [Client-side tooling](#client-side-tooling)
- [Add business logic](#add-business-logic)
    - [Remote schemas](#remote-schemas)
    - [Trigger webhooks on database events](#trigger-webhooks-on-database-events)
- [Demos](#demos)
    - [Realtime applications](#realtime-applications)
    - [Videos](#videos)
- [Support & Troubleshooting](#support--troubleshooting)
- [Contributing](#contributing)
- [Brand assets](#brand-assets)
- [License](#license)
- [Translations](#translations)

<!-- markdown-toc end -->

## &#x202b; התחלה מהירה:

### &#x202b;  פריסה בקליק אחד עם Hasura Cloud

&#x202b; הדרך המהירה והקלה ביותר לנסות את Hasura היא באמצעות  [Hasura Cloud](https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html).


1. &#x202b;  לחץ על הכפתור הבא כדי לפרוס את מנוע GraphQL על Hasura Cloud - כולל תוסף Postgres עבור שימוש בדאטהבייס Postgres קיים:

    [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

2. &#x202b;  פתח את הקונסולה של Hausra

   &#x202b; לחץ על הכפתור "Launch console" כדי לפתוח את הקונסולה של Hasura.

3. &#x202b;  צור את שאילתת GraphQL הראשונה שלך

   &#x202b; צור טבלה ומיד הרץ את השאילתא שלך. עקוב אחרי [המדריך הפשוט הזה](https://hasura.io/docs/1.0/graphql/manual/getting-started/first-graphql-query.html).

### &#x202b;   אפשרויות פריסה בקליק אחד אחרות

&#x202b; בדוק את ההוראות עבור אפשרויות הפריסה בקליק אחד הבאות:

| **ספק התשתית** | **לינק קליק-אחד** | **מידע נוסף** |
|:------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------:|
| Heroku | [![פרוס ל-Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) | [מסמכים](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/heroku-one-click.html) |
| DigitalOcean | [![פרוס ל-DigitalOcean](https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet_200px.png)](https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=readme) | [מסמכים](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/digital-ocean-one-click.html#hasura-graphql-engine-digitalocean-one-click-app) |
| Azure | [![פרוס ל-Azure](http://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json) | [מסמכים](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html) |
| Render | [![פרוס ל-Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/render-examples/hasura-graphql) | [docs](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/render-one-click.html) |

### &#x202b;  שיטות פריסה נוספות

&#x202b; עבור פריסה מבוססת דוקר ועוד תצורות מתקדמות ראה  [מדריכי פריסה](https://hasura.io/docs/1.0/graphql/manual/getting-started/index.html) או
&#x202b; [מניפסטי התקנה](install-manifests).

##  &#x202b; ארכיטקטורה

&#x202b; מנוע GraphQL של Hasura חושף מופע דאטהבייס Postgress ויודע לקבל בקשות GraphQL מיישומי הלקוח שלך. ניתן להגדיר אותו לעבוד עם מערכת ההרשאות הקיימת שלך והוא יכול לנהל הרשאות גישה באמצעות חוקים ברמת שדות עם משתנים דינאמיים מתוך מערכת ההרשאות שלך.

&#x202b; אתה יכול גם למזג סכמות GraphQL מרוקות ולפסק ממשק GraphQL אחיד.

![Hasura GraphQL Engine architecture](assets/hasura-arch.svg)

## &#x202b; כלים לצד לקוח
&#x202b; Hasura עובד עם כל כלי לקוח של GraphQL. אנחנו ממליצים להשתמש ב- [Apollo Client](https://github.com/apollographql/apollo-client). ראה [awesome-graphql](https://github.com/chentsulin/awesome-graphql) עבור רשימת כלי לקוח.

## &#x202b;  הוסף לוגיקה עסקית

&#x202b; מנוע GraphQL מציע מתודות פשוטות לתפישה, סקלאביליות ובעלות ביצועים גבוהים עבור הוספת של לוגיקה עסקית מותאמת אישית עבור צד השרת שלך:

### &#x202b;  סכמות מרוחקות

&#x202b; הוסף שירותי תרגום מותאמים אישית על סכמות מרוחקות בנוסף לסכמת GraphQL המבוססת על Postgres של Hasura. אידאלי לפתרונות כמו מימוש ממשק תשלום, או תשאול מידע שנמצא בדאטהבייס שאינו שלך - [קרא עוד](remote-schemas.md).

### &#x202b;  הפעל וובהוקים על ארועי דאטהבייס.

&#x202b; הוסף לוגיקה עסקית אסינכרונית המופעלת על בסיס ארועי דאטהבייס.
&#x202b; אידאלי עבור התראות, צינורות מידע מתוך Postgress או עיבוד אסינכרוני
 - &#x202b;  [קרא עוד](event-triggers.md).

### &#x202b;  גזירת מידע והמרות מידע נוספות

&#x202b; המר מידע ב-Postgres או הרץ לוגיקה עסקית על המידע כדי לגזור אוסף נתונים אחר שניתן לתשאול באמצעות מנוע GraphQL - [קרא עוד](https://hasura.io/docs/1.0/graphql/manual/queries/derived-data.html).

## &#x202b;  הדגמות

&#x202b; בדוק את כל הדוגמאות בספריית [community/sample-apps](community/sample-apps).


### &#x202b;  יישומי זמן אמת

- &#x202b;  יישום צ׳אט קבוצתי בנוי עם React, עם מכווני ״משתמש מקליד״, משתמשים מחוברים והתראות הודעה חדשה.
  - &#x202b; [נסה את זה](https://realtime-chat.demo.hasura.app/)
  - &#x202b;  [מדריך](community/sample-apps/realtime-chat)
  - &#x202b;  [עיין ב-APIs](https://realtime-chat.demo.hasura.app/console)


- &#x202b; יישום מידע מיקום בזמן אמת אשר מראה שינויי קואורדינטות GPS של כלי הרכב הזזים על המפה.
  - &#x202b;  [נסה את זה](https://realtime-location-tracking.demo.hasura.app/)
  - &#x202b;  [מדריך](community/sample-apps/realtime-location-tracking)
  - &#x202b;  [עיין ב-APIs](https://realtime-location-tracking.demo.hasura.app/console)

- &#x202b;  לוח מידע עם סיכום נתונים עבור מידע המשתנה באופן מתמשך.
  - &#x202b;  [נסה את זה](https://realtime-poll.demo.hasura.app/)
  - &#x202b;  [מדריך](community/sample-apps/realtime-poll)
  - &#x202b;  [עיין ב-APIs](https://realtime-poll.demo.hasura.app/console)

### &#x202b; סרטונים

* &#x202b;  [הוספת GraphQL למופע GitLab באירוח עצמי](https://www.youtube.com/watch?v=a2AhxKqd82Q) (*3:44 דק׳*)
* &#x202b;  [יישומון רשימת משימות עם Auth0 ו-GraphQL](https://www.youtube.com/watch?v=15ITBYnccgc) (*4:00 דק׳*)
* &#x202b;  [GraphQL על GitLab עם התממשקות למערכת ההרשאות של GitLab](https://www.youtube.com/watch?v=m1ChRhRLq7o) (*4:05 דק׳*)
* &#x202b;  [לוח מידע עבור 10 מיליון נסיעות עם מידע גיאוגרפי (PostGIS, Timescale)](https://www.youtube.com/watch?v=tsY573yyGWA) (*3:06 דק׳*)


## &#x202b;  תמיכה ותפעול תקלות
&#x202b; התיעוד והקהילה ישמחו לעזור לנסות ולפתור את מירב הבעיות. אם נתקלת בבאג או צריך ליצור איתנו קשר, אתה יכול ליצור איתנו קשר באמצעות אחד הערוצים הבאים:

* &#x202b;  תמיכה ופידבק: [Discord](https://discord.gg/hasura)
* &#x202b;  מעקב תקלות ובאגים: [GitHub issues](https://github.com/hasura/graphql-engine/issues)
* &#x202b;  מעקב על עדכוני מוצר: [@HasuraHQ](https://twitter.com/hasurahq)
* &#x202b;  דבר איתנו [בצ׳אט באתר](https://hasura.io)

&#x202b; אנו מחוייבים לסביבה מכילה ומזמינה בקיהלה שלנו. אנא עבור על  [קוד ההתנהגות](code-of-conduct.md) שלנו 

&#x202b; אם תרצה לדווח על בעיית אבטחה - בבקשה קרא [קרא את זה](SECURITY.md).

## &#x202b;  תרומה
&#x202b; בדוק ב- [מדריך התרומות](CONTRIBUTING.md) עבור עוד מידע.

## &#x202b;  נכסי המותג
&#x202b; נכסי המותג Hasura (לוגואים, הקמיע של Hasura, תג ה-powered by ועוד) יכולים להמצא בתיקיית [assets/brand](assets/brand). תרגיש חופשי להשתמש בהם ביישום/אתר שלך וכו׳. נשמח אם תוסיף את התג ״Powered by Hasura״ ליישומים שנבנו באמצעות Hasua. ❤️

<div style="display: flex;">
  <img src="assets/brand/powered_by_hasura_blue.svg" width="150px"/>
  <img src="assets/brand/powered_by_hasura_white.svg" width="150px"/>
</div>

```html
<!-- עבור רקע בהיר -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_blue.svg" />
</a>

<!-- עבור רקע כהה -->
<a href="https://hasura.io">
  <img width="150px" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_white.svg" />
</a>
```

## &#x202b;  רישיון

&#x202b; מנוע הGraphQL זמין תחת רשיון [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (Apache-2.0).

&#x202b; כל **שאר התכנים** (מלבד אלה בספריות [`server`](server), [`cli`](cli) ו-[`console`](console)) זמינים תחת רשיון [MIT License](LICENSE-community).
&#x202b; הדבר כולל גם את הכל ב [`docs`](docs) וב- [`community`](community).
