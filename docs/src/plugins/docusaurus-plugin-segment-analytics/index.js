 const path = require('path');
 const { Joi } = require('@docusaurus/utils-validation');
//  import type { LoadContext, Plugin } from '@docusaurus/types';
//  import type { PluginOptions } from './plugin-segment-analytics';
 
 function pluginSegmentAnalitics(
   context,
   options,
 ) {
   const { prodKey, devKey, trackPage } = options;
   const isProd = process.env.NODE_ENV === 'production';
   const writeKey = isProd ? prodKey : devKey;
 
   return {
     name: 'docusaurus-plugin-segment-analytics',
 
     async contentLoaded({actions}) {
       actions.setGlobalData(options);
     },
 
     getClientModules() {
       return [path.resolve(__dirname, './segmentAnalytics')];
     },
 
     injectHtmlTags() {
       if (!isProd) return {};

       return {
         headTags: [
           {
             tagName: 'link',
             attributes: {
               rel: 'preconnect',
               href: 'https://cdn.segment.com',
             },
           },
           {
             tagName: 'script',
             innerHTML: `
               !function(){var analytics=window.analytics=window.analytics||[];if(!analytics.initialize)if(analytics.invoked)window.console&&console.error&&console.error("Segment snippet included twice.");else{analytics.invoked=!0;analytics.methods=["trackSubmit","trackClick","trackLink","trackForm","pageview","identify","reset","group","track","ready","alias","debug","page","once","off","on","addSourceMiddleware","addIntegrationMiddleware","setAnonymousId","addDestinationMiddleware"];analytics.factory=function(e){return function(){var t=Array.prototype.slice.call(arguments);t.unshift(e);analytics.push(t);return analytics}};for(var e=0;e<analytics.methods.length;e++){var key=analytics.methods[e];analytics[key]=analytics.factory(key)}analytics.load=function(key,e){var t=document.createElement("script");t.type="text/javascript";t.async=!0;t.src="https://cdn.segment.com/analytics.js/v1/" + key + "/analytics.min.js";var n=document.getElementsByTagName("script")[0];n.parentNode.insertBefore(t,n);analytics._loadOptions=e};analytics._writeKey="${writeKey}";;analytics.SNIPPET_VERSION="4.15.3";
               analytics.load("${writeKey}");
               ${trackPage ? "analytics.page();" : ""}
               }}();`,
           },
         ],
       };
     },
   };
 }

 const pluginOptionsSchema = Joi.object({
  prodKey: Joi.string().required(),
  devKey: Joi.string().required(),
  trackPage: Joi.boolean().default(false),
  trackPageDelay: Joi.number().default(50),
});

pluginSegmentAnalitics.validateOptions = function ({
  validate,
  options,
}) {
  return validate(pluginOptionsSchema, options);
}
 
 module.exports = pluginSegmentAnalitics