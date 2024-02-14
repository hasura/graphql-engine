import React from 'react';

// import hasuraConDark from '@site/static/img/hasura-con-dark.png';
// import hasuraConLight from '@site/static/img/hasura-con-light.png';
import ArrowRight from '@site/static/icons/arrow_right.svg';

import styles from './styles.module.scss';

const HasuraConBanner = props => {
  // const isSnowFlakeSection = props.location.pathname.startsWith(`/docs/latest/databases/snowflake`);

  // const isObservabilitySection = props.location.pathname.startsWith(`/docs/latest/observability`);

  // const isSecuritySection = props.location.pathname.startsWith(`/docs/latest/security`);

  // const isMySQLSection = props.location.pathname.startsWith(`/docs/latest/databases/mysql`);

  // const isOracleSection = props.location.pathname.startsWith(`/docs/latest/databases/oracle`);

  // const isMariaDBSection = props.location.pathname.startsWith(`/docs/latest/databases/mariadb`);

  // Banner for - New product launch webinar */
  // if (isMySQLSection || isOracleSection || isMariaDBSection) {
  //   return (
  //     <div className={styles['product-launch-webinar-bg']}>
  //       <a className={styles['webinar-banner']} href="https://hasura.io/events/webinar/product-launch/">
  //         <div className={styles['hasura-con-brand']}>
  //           <img
  //             className={styles['brand-light']}
  //             src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1683628053/main-web/Group_11457_vceb9f.png"
  //             alt="hasura-webinar"
  //           />
  //         </div>
  //         <div className={styles['content-div']}>
  //           <h3>Ship faster with low-code APIs on MySQL, MariaDB, and Oracle</h3>
  //           <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
  //             View Recording
  //             <ArrowRight />
  //           </div>
  //         </div>
  //       </a>
  //     </div>
  //   );
  // }

  // if (isSnowFlakeSection) {
  //   return (
  //     <div className={styles['snowflake-bg']}>
  //       <a className={styles['webinar-banner']} href="https://hasura.io/events/webinar/snowflake-and-postgresql/">
  //         <div className={styles['hasura-con-brand']}>
  //           <img
  //             className={styles['brand-light']}
  //             src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1677756408/main-web/Group_11455_1_ziz1fz.png"
  //             alt="Hasura Con"
  //           />
  //         </div>
  //         <div className={styles['content-div']}>
  //           <h3>Combining Snowflake and PostgreSQL to build low-latency apps on historical data insights</h3>
  //           <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
  //             View Recording
  //             <ArrowRight />
  //           </div>
  //         </div>
  //       </a>
  //     </div>
  //   );
  // }

  // if (isSnowFlakeSection) {
  //   return (
  //     <div className={styles['snowflake-bg']}>
  //       <a className={styles['webinar-banner']} href="https://hasura.io/events/webinar/snowflake-and-postgresql/">
  //         <div className={styles['hasura-con-brand']}>
  //           <img
  //             className={styles['brand-light']}
  //             src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1677756408/main-web/Group_11455_1_ziz1fz.png"
  //             alt="Hasura Con"
  //           />
  //         </div>
  //         <div className={styles['content-div']}>
  //           <h3>Combining Snowflake and PostgreSQL to build low-latency apps on historical data insights</h3>
  //           <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
  //             View Recording
  //             <ArrowRight />
  //           </div>
  //         </div>
  //       </a>
  //     </div>
  //   );
  // }

  // if (isObservabilitySection) {
  //   return (
  //     <div className={styles['observe-bg']}>
  //       <a
  //         className={styles['webinar-banner']}
  //         href="https://hasura.io/events/webinar/best-practices-for-api-observability-with-hasura/"
  //       >
  //         <div className={styles['hasura-con-brand']}>
  //           <img
  //             className={styles['brand-light']}
  //             src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1677759444/main-web/Group_11455_2_rdpykm.png"
  //             alt="Hasura Con"
  //           />
  //         </div>
  //         <div className={styles['content-div']}>
  //           <h3>Best Practices for API Observability with Hasura</h3>
  //           <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
  //             View Recording
  //             <ArrowRight />
  //           </div>
  //         </div>
  //       </a>
  //     </div>
  //   );
  // }

  // if (isSecuritySection) {
  //   return (
  //     <div className={styles['security-bg']}>
  //       <a className={styles['webinar-banner']} href="https://hasura.io/events/webinar/securing-your-api-with-hasura/">
  //         <div className={styles['hasura-con-brand']}>
  //           <img
  //             className={styles['brand-light']}
  //             src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1677759811/main-web/Group_11455_3_azgk7w.png"
  //             alt="Hasura Con"
  //           />
  //         </div>
  //         <div className={styles['content-div']}>
  //           <h3>Securing your API with Hasura</h3>
  //           <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
  //             View Recording
  //             <ArrowRight />
  //           </div>
  //         </div>
  //       </a>
  //     </div>
  //   );
  // }

  return (
    <a className={styles['hasura-con-banner']} href="https://hasura.io/events/hasura-con-2024">
      <div className={styles['hasura-con-brand']}>
        <svg
          fill="none"
          height="42"
          viewBox="0 0 239 42"
          width="239"
          xmlns="http://www.w3.org/2000/svg"
        >
          <path
            d="m38.0802 14.8938c1.1907-3.5976.5857-10.81721-1.6165-13.50688-.2856-.35146-.8325-.30753-1.0793.07322l-2.7976 4.31519c-.6921.85913-1.9166 1.0495-2.8265.42956-2.9572-2.00138-6.505-3.18757-10.3334-3.23639-3.8284-.04881-7.4052 1.05927-10.40597 2.98744-.92444.59553-2.14896.38075-2.81687-.49791l-2.69588-4.38352c-.23716-.385636-.78408-.439332-1.07932-.097632-2.265111 2.635972-3.03467 9.840922-1.931152 13.467822.367839 1.2009.459799 2.4749.2178 3.7099-.23716 1.2204-.479159 2.6995-.493679 3.7295-.121 10.5731 8.276381 19.2426 18.754971 19.3646 10.4834.122 19.0792-8.3473 19.2002-18.9156.0097-1.0299-.1936-2.5139-.4065-3.7391-.213-1.2399-.092-2.5091.3049-3.7002z"
            fill="#3970fd"
          />
          <g fill="#fff">
            <path d="m20.1496 13.6664 1.6087 4.6515c.0826.2432.3146.4088.5707.403l4.4542-.02c.589-.0015.8323.7504.3531 1.0931l-3.6412 2.5884c-.2098.1493-.303.414-.2266.6653l1.4019 4.6901c.1627.5462-.4578.9959-.9219.6646l-3.8586-2.7106c-.2079-.147-.4819-.1446-.6899-.0042l-3.8737 2.687c-.4712.3256-1.0886-.1234-.9175-.6697l1.4334-4.6816c.0754-.245-.0162-.5131-.2232-.6646l-3.6225-2.6137c-.4758-.3428-.2273-1.0965.3599-1.089l4.4545.0496c.2589.004.49-.1597.5743-.4029l1.6381-4.6373c.1881-.5383.952-.5335 1.1352.0027z" />
            <path d="m57.4343 9.99387h4.0106v22.00613h-4.0106v-9.3814h-4.5338v9.3814h-4.0106v-22.00613h4.0106v9.55573h4.5338zm16.0444 22.00613-.837-4.5686h-4.8128l-.7672 4.5686h-4.0107l4.4292-22.00613h5.4056l4.6384 22.00613zm-5.1267-7.6028h3.7317l-1.9182-10.5322zm17.7397 3.5922v-4.7082c0-.372-.0698-.6161-.2093-.7323-.1395-.1395-.3952-.2093-.7672-.2093h-2.8249c-2.3947 0-3.5921-1.1625-3.5921-3.4875v-5.4056c0-2.3018 1.2555-3.45263 3.7665-3.45263h3.8362c2.511 0 3.7665 1.15083 3.7665 3.45263v3.069h-4.0455v-2.511c0-.372-.0697-.6161-.2092-.7324-.1395-.1395-.3953-.2092-.7673-.2092h-1.3252c-.3953 0-.6626.0697-.8021.2092-.1395.1163-.2093.3604-.2093.7324v4.4291c0 .372.0698.6278.2093.7673.1395.1162.4068.1743.8021.1743h2.7551c2.4413 0 3.6619 1.1393 3.6619 3.4178v5.7544c0 2.3017-1.2671 3.4526-3.8014 3.4526h-3.7665c-2.5342 0-3.8014-1.1509-3.8014-3.4526v-3.0342h4.0107v2.4762c0 .372.0697.6277.2092.7672.1395.1163.4069.1744.8021.1744h1.3253c.372 0 .6277-.0581.7672-.1744.1395-.1395.2093-.3952.2093-.7672zm14.4183-17.99553h4.01v18.55353c0 2.3017-1.267 3.4526-3.801 3.4526h-4.255c-2.5342 0-3.8014-1.1509-3.8014-3.4526v-18.55353h4.0107v17.99553c0 .372.0697.6277.2092.7672.1395.1163.3953.1744.7673.1744h1.8483c.3953 0 .6629-.0581.8019-.1744.14-.1395.21-.3952.21-.7672zm10.971 13.42683v8.5793h-4.011v-22.00613h8.091c2.535 0 3.802 1.15083 3.802 3.45263v6.5216c0 1.9065-.849 3.0225-2.546 3.348l3.662 8.6839h-4.325l-3.348-8.5793zm0-10.3578v7.3935h2.895c.372 0 .627-.0582.767-.1744.139-.1395.209-.3953.209-.7673v-5.5102c0-.372-.07-.6161-.209-.7324-.14-.1395-.395-.2092-.767-.2092zm19.659 18.9371-.837-4.5686h-4.813l-.767 4.5686h-4.011l4.429-22.00613h5.406l4.638 22.00613zm-5.127-7.6028h3.732l-1.919-10.5322zm22.273-7.1145h-4.045v-3.4177c0-.372-.07-.6161-.209-.7324-.14-.1395-.396-.2092-.768-.2092h-1.639c-.372 0-.628.0697-.767.2092-.14.1163-.209.3604-.209.7324v14.2987c0 .372.069.6278.209.7673.139.1162.395.1744.767.1744h1.639c.372 0 .628-.0582.768-.1744.139-.1395.209-.3953.209-.7673v-3.348h4.045v3.7665c0 2.3018-1.267 3.4527-3.801 3.4527h-4.08c-2.535 0-3.802-1.1509-3.802-3.4527v-15.1357c0-2.3018 1.267-3.45263 3.802-3.45263h4.08c2.534 0 3.801 1.15083 3.801 3.45263zm6.143-7.28883h4.255c2.534 0 3.801 1.15083 3.801 3.45263v15.1009c0 2.3017-1.267 3.4526-3.801 3.4526h-4.255c-2.534 0-3.801-1.1509-3.801-3.4526v-15.1009c0-2.3018 1.267-3.45263 3.801-3.45263zm4.045 17.99553v-13.9849c0-.372-.069-.6161-.209-.7324-.139-.1395-.395-.2092-.767-.2092h-1.848c-.396 0-.663.0697-.803.2092-.139.1163-.209.3604-.209.7324v13.9849c0 .372.07.6277.209.7672.14.1163.407.1744.803.1744h1.848c.372 0 .628-.0581.767-.1744.14-.1395.209-.3952.209-.7672zm15.405-17.99553h3.662v22.00613h-3.766l-4.709-14.5778v14.5778h-3.696v-22.00613h3.871l4.638 14.36853zm15.151 4.01063v3.2782h-4.011v-3.8362c0-2.3018 1.267-3.45263 3.801-3.45263h3.348c2.558 0 3.837 1.15083 3.837 3.45263v2.4761c0 1.7205-.524 3.3945-1.57 5.022l-4.568 7.9864h6.242v3.069h-10.985v-2.8946l5.684-9.207c.814-1.2323 1.221-2.5808 1.221-4.0455v-1.8484c0-.372-.07-.6161-.209-.7324-.14-.1395-.396-.2092-.768-.2092h-1.011c-.395 0-.663.0697-.802.2092-.14.1163-.209.3604-.209.7324zm16.849 13.9849v-13.9849c0-.372-.07-.6161-.209-.7324-.14-.1395-.407-.2092-.802-.2092h-1.604c-.372 0-.628.0697-.768.2092-.139.1163-.209.3604-.209.7324v13.9849c0 .372.07.6277.209.7672.14.1163.396.1744.768.1744h1.604c.395 0 .662-.0581.802-.1744.139-.1395.209-.3952.209-.7672zm4.011-14.5429v15.1009c0 2.3017-1.267 3.4526-3.802 3.4526h-4.045c-2.534 0-3.801-1.1509-3.801-3.4526v-15.1009c0-2.3018 1.267-3.45263 3.801-3.45263h4.045c2.535 0 3.802 1.15083 3.802 3.45263zm6.063.558v3.2782h-4.011v-3.8362c0-2.3018 1.267-3.45263 3.802-3.45263h3.348c2.557 0 3.836 1.15083 3.836 3.45263v2.4761c0 1.7205-.523 3.3945-1.57 5.022l-4.568 7.9864h6.242v3.069h-10.985v-2.8946l5.684-9.207c.814-1.2323 1.221-2.5808 1.221-4.0455v-1.8484c0-.372-.07-.6161-.209-.7324-.14-.1395-.395-.2092-.767-.2092h-1.012c-.395 0-.662.0697-.802.2092-.139.1163-.209.3604-.209.7324zm21.802 10.6369v3.1038h-2.093v4.2548h-3.801v-4.2548h-7.987v-3.4177l6.906-14.33363h4.01l-7.254 14.64753h4.325v-4.2548h3.801v4.2548z" />
          </g>
        </svg>
      </div>
      <div className={styles['hasura-con-space-between']}>
        <div>
          <div className={styles['hasura-con-23-title']}>The HasuraCon 2024 CFP is open!</div>
        </div>
        <div className={styles['hasura-con-register-button'] + ' ' + styles['hasura-con-register-mobile-hide']}>
          Read more
          <img
            src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1683723549/main-web/chevron-right_ldbi7d.png"
            alt="arrow-icon"
          />
        </div>
      </div>
    </a>
  );
};

export default HasuraConBanner;
