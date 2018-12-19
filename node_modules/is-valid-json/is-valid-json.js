/*
 * is-valid-json.js : this file contains main function logic
 */

// @param <obj> : The object which is to be checked
module.exports = function( obj ){

   // check "obj" for string
   if( typeof obj === "string" ){
     // obj is string, parse it using JSON.parse()
     try{
       var isJSON = JSON.parse( obj );
       // this is for null, as "null" is a valid JSON and type of null is object
       // but "null" is itself falsey
       // if isJSON is null then if( isJSON && ... ) will be false
       if( isJSON && typeof isJSON === "object"){
         // everything goes fine, return true
         return true;
       }
       else{
         return false;
       }
     }catch(e){
       // cannot parse, invalid JSON
       return false;
     }
   }
   else{
     // "obj" is not string, so first,
     // JSON.stringify() it, then parse
     jsonString = JSON.stringify( obj );
     // now parse this json string
     try{
       var checkJSON = JSON.parse( jsonString );
       // this is for null, as "null" is a valid JSON and type of null is object
       // but "null" is itself falsey
       // if isJSON is null then if( isJSON && ... ) will be false
       if( checkJSON && typeof checkJSON === "object"){
         // everything goes fine, return true
         return true;
       }
       else{
         return false;
       }
     }catch(e){
       // cannot parse, invalid JSON
       return false;
     }

   }

};
