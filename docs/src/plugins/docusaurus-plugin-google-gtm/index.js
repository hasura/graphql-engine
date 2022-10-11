 const path = require('path');
 const { Joi } = require('@docusaurus/utils-validation');
//  import type { LoadContext, Plugin } from '@docusaurus/types';
//  import type { PluginOptions } from './plugin-google-gtm';
 
 function pluginGoogleGTM(
   context,
   options,
 ) {
   const { trackingID } = options;
   const isProd = process.env.NODE_ENV === 'production';
 
   return {
     name: 'docusaurus-plugin-google-gtm',
 
     async contentLoaded({actions}) {
       actions.setGlobalData(options);
     },
 
     injectHtmlTags() {
       if (!isProd) return {};

       return {
         headTags: [
           {
             tagName: 'link',
             attributes: {
               rel: 'preconnect',
               href: 'https://www.googletagmanager.com',
             },
           },
           {
             tagName: 'script',
             innerHTML: `(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
                new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
                j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
                'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
                })(window,document,'script','dataLayer', '${trackingID}');`,
           },
         ],
         preBodyTags: [
          {
            tagName: 'noscript',
            innerHTML: `<iframe src="https://www.googletagmanager.com/ns.html?id=${trackingID}" height="0" width="0" style="display:none;visibility:hidden"></iframe>`,
          },
         ],
       };
     },
   };
 }

 const pluginOptionsSchema = Joi.object({
  trackingID: Joi.string().required(),
});

pluginGoogleGTM.validateOptions = function ({
  validate,
  options,
}) {
  return validate(pluginOptionsSchema, options);
}
 
 module.exports = pluginGoogleGTM