var promise_ajax = function(url, data, type) {
  return new Promise(function(resolve, reject) {
    var req = new XMLHttpRequest();
    req.open(type, url, true);
    req.setRequestHeader("Content-type", "application/json");
    req.onload = function() {
      if (req.status == 200) {
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

var searchFunc = function( query, callback ) {
  var APPLICATION_ID = '2R09CYX6BF';                                                                                                                              
  var SEARCH_ONLY_KEY = '1361d3d9973e36ed78e29c3018f74aa6';
  var hitsPerPage = 10;
  var pageNo = 1;
  var offset = 0;

  var queries = [];

  var client = algoliasearch(APPLICATION_ID, SEARCH_ONLY_KEY, {"protocol":"https:"}); // localhost
  //var client = algoliasearch(APPLICATION_ID, SEARCH_ONLY_KEY);
  
  queries.push(
    {
      indexName: 'docs_search',
      query: query,
      params: {
        hitsPerPage: hitsPerPage,
        page: pageNo,
        offset: offset
      }
    }
  );

  client.search(queries, callback );
};
