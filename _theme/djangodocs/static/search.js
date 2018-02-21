var promise_ajax = function(url, data, type) {
  return new Promise(function(resolve, reject) {
    var req = new XMLHttpRequest();
    req.open(type, url, true);
    req.setRequestHeader("Content-type", "application/json");
    req.onload = function() {
      if (req.status === 200) {
        resolve(req.response);
      } else {
        reject(req.response);
      }
    };
    req.onerror = function() {
      reject(Error("Network Error"));
    };
    // Make the request
    req.send(JSON.stringify(data));
  });
};

var searchFunc = function( query, callback, page, restrictAttributes, attributesToRetrieve ) {
  var APPLICATION_ID = "{{ APPLICATION_ID }}";
  var SEARCH_ONLY_KEY = "{{ APPLICATION_SEARCH_KEY }}";

  var hitsPerPage = 100;
  var page_no = parseInt(page) - 1;
  var offset = (hitsPerPage * (page - 1));

  var indexName = "docs_search";

  if ( query === "tutorials" || query === "guides" ) {
    indexName = "docs_by_date_desc";
  }

  //var client = algoliasearch(APPLICATION_ID, SEARCH_ONLY_KEY);
  var client = algoliasearch(APPLICATION_ID, SEARCH_ONLY_KEY, {"protocol":"https:"}); // localhost
  var index = client.initIndex(indexName);
  
  var queries = {
    query: query,
    hitsPerPage: hitsPerPage,
    page: page_no,
    offset: offset,
    restrictSearchableAttributes : restrictAttributes
  };

  if ( attributesToRetrieve ) {
    queries["attributesToRetrieve"] = attributesToRetrieve;
  }

  index.search(queries, callback );
};

/* Track event function */
const trackga = function ( category, action, label, value ) {
  // If ga is available
  if ( ga ) {
    ga('send', {
        hitType: 'event',
        eventCategory: category,
        eventAction: action,
        eventLabel: label,
        eventValue: value
    });
  }
};
