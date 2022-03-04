import React, { useState, useEffect, useRef } from "react";
import useIsBrowser from '@docusaurus/useIsBrowser';
import BrowserOnly from "@docusaurus/BrowserOnly";
import styles from "./styles.module.scss";

interface CustomInjectedScript extends HTMLScriptElement {
  onreadystatechange?: () => void;
}

export default function Paperform({ formId, styleClassName="" }) {
  const isBrowser = useIsBrowser();
  const [isLoaded, setIsLoaded] = useState(false);
  const embedDivRef = useRef(null);

  useEffect(() => {
    const existingEmbed = document.getElementById("paperform_embed");

    if (existingEmbed) {
      setIsLoaded(true);
      return;
    }

    const script:CustomInjectedScript = document.createElement("script");
    script.id = "paperform_embed";
    script.src = "https://forms.hasura.io/__embed.min.js";
    script.onreadystatechange = () => {
      if (this.readyState === "complete" || this.readyState === "loaded") {
        setIsLoaded(true);
      }
    };
    script.onload = () => setIsLoaded(true);
    document.body.prepend(script);

    return () => script.remove();
  }, []);

  // function handleFormSubmit({ detail }) {
  //   const { form_id, data } = detail;
  //   const email = data.find(d => d.type === "email")?.value;

  //   if (!!email) {
  //     // let nameTraits = {};
  //     // if (!!vals.FirstName) nameTraits.firstName = vals.FirstName;
  //     // if (!!vals.LastName) nameTraits.lastName = vals.LastName;

  //     window.analytics.identify(email, {
  //       email,
  //       identifiedBy: `Paperform ${form_id} Submitted`,
  //       // ...nameTraits,
  //     });
  //   }

  //   window.analytics.track("form submit", {
  //     data,
  //     category: "website",
  //     label: `Paperform ${form_id} Submitted`,
  //     action: "form submit",
  //   });

  //   typeof onSubmitCB === "function" && onSubmitCB(detail);
  // }

  useEffect(() => {
    const refCurrValue = embedDivRef.current;

    // isBrowser &&
    //   refCurrValue?.setAttribute(
    //     "data-prefill",
    //     `utm_landing-page=${window.location.pathname}&utm_search=${window.location.search}`
    //   );

    // refCurrValue?.addEventListener("PaperformSubmission", handleFormSubmit);
    // return () => refCurrValue?.removeEventListener("PaperformSubmission", handleFormSubmit);
  }, [isBrowser]);

  return (
    <div className={`${styles["paperform-embed-wrapper"]} ${styleClassName ? styleClassName : ''}`}>
      {!isLoaded && <span className={styles.loadingText}>Loading...</span>}
      <BrowserOnly>
        {() => (
          <div
            data-prefill-inherit="1"
            data-prefill={`utm_landing-page=${window.location.pathname}`}
            data-no-scroll="1"
            ref={embedDivRef}
            id={formId}
            data-paperform-id={formId}
            data-spinner="1"
          />
        )}
      </BrowserOnly>
    </div>
  );
}
