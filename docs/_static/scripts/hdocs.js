window.hdocs = (function () {
  const DB_URL = HDOCS_BASE_DOMAIN ? 'https://data.' + HDOCS_BASE_DOMAIN + '/v1/query' : 'https://data.hasura-stg.hasura-app.io/v1/query';

  return {
    setup: function () {
      Array.from(document.getElementsByClassName('menuLink')).forEach(function (el) { el.addEventListener('click', hdocs.toggleMenu) });

      Array.from(document.getElementsByClassName('tracked')).forEach(function (el) { el.addEventListener('click', function () {
        const dataLabel = el.getAttribute('data-label');
        hdocs.trackGA(dataLabel);
        hdocs.saTrack(dataLabel);
      }) });

      document.getElementById('nav_tree_icon').addEventListener('click', hdocs.handleNavClick);
      document.getElementById('sidebar-close').addEventListener('click', hdocs.handleNavClick);

      // document.getElementById('close-banner').addEventListener('click', hdocs.closeBanner);

      // document.getElementById('thumb_up_button').addEventListener('click', function () { hdocs.sendFeedback('positive', 'Great to hear that! If you have any other feedback, please share here:') });
      // document.getElementById('thumb_down_button').addEventListener('click', function () { hdocs.sendFeedback('negative', 'Sorry to hear that. Please tell us what you were looking for:') });
      // document.getElementById('feedback_btn').addEventListener('click', hdocs.handleSubmitFeedback);

      docsearch({
        appId: 'WCBB1VVLRC',
        apiKey: HDOCS_ALGOLIA_API_KEY,
        indexName: HDOCS_ALGOLIA_INDEX,
        inputSelector: '#search_element',
        transformData: hdocs.transformSearchData,
        debug: false
      });

      hdocs.getGithubStarCount();
      hdocs.setReleaseTags();
      hdocs.setExternalLinks();
      hdocs.setupGraphiQL();
      hdocs.newsletterForm();
    },
    newsletterForm: function () {
      const searchParams = new URLSearchParams(window.location.search);
      const searchAliId = searchParams.get("aliId");
      var marketoForm = document.getElementById("mktoForm_1011");
      var marketoSuccess = document.getElementById("marketo-success");
      if (searchAliId || searchAliId === '') {
        marketoForm.classList.add('hide');
        marketoSuccess.classList.remove('hide');
        marketoSuccess.scrollIntoView({ behavior: "smooth", block: "start", inline: "start" });
      } else {
        marketoForm.classList.remove('hide');
        marketoSuccess.classList.add('hide');
      }
    },
    trackGA: function (label, action) {
      window.dataLayer = window.dataLayer || [];
      window.dataLayer.push({
        event: 'Click Events',
        category: 'Docs Custom',
        action: action || 'Link Click',
        label: label
      })
    },
    saTrack: function (label) {
      window.analytics = window.analytics || [];
      window.analytics.track("click", {
        category: 'docs',
        action: 'click',
        label: 'Clicked ' + label,
        placement: 'header',
      })
    },
    toggleMenu: function () {
      var x = document.getElementById("navbar")
      if (x.className === "topnav") {
        x.className += " responsive"
      } else {
        x.className = "topnav"
      }
    },
    closeBanner: function() {
      var banner = document.getElementById("banner-stripe");
      banner.className += " hide";
    },
    request: function (url, data, type) {
      return new Promise(function (resolve, reject) {
        var req = new XMLHttpRequest();
        req.open(type, url, true);
        req.setRequestHeader("Content-type", "application/json");
        req.onload = function () {
          if (req.status === 200) {
            resolve(req.response);
          } else {
            reject(req.response);
          }
        };
        req.onerror = function () {
          reject(Error("Network Error"));
        };
        // Make the request
        req.send(JSON.stringify(data));
      });
    },
    getGithubStarCount: function () {
      const bodyObj = {
        type: "select",
        args: {
          table: "github_repos",
          columns: ["star_count"],
          where: { $or: [{ name: "graphql-engine" }] },
        }
      }
      const options = {
        method: "POST",
        credentials: "include",
        body: JSON.stringify(bodyObj),
        headers: {
          "Content-Type": "application/json",
        },
      }
      fetch(DB_URL, options)
        .then(function (response) {
          return response.json();
        })
        .then(function (data) {
          const githubStar = data[0].star_count;
          document.getElementById("gitHubCount").innerHTML = hdocs.formatNumber(githubStar);
          document.getElementById("gitHubBtn").classList.remove("hide");
        })
        .catch(function (e) { {console.error(e);} })
    },
    setReleaseTags: function () {
      if (document.getElementsByClassName('latest-release-tag').length > 0) {
        hdocs.request('https://releases.hasura.io/graphql-engine?agent=docs.hasura.io', null, 'GET')
          .then(function (response) {
            const data = JSON.parse(response);
            Array.from(document.getElementsByClassName('latest-release-tag')).forEach(function (el) { el.innerHTML = data.latest });
            Array.from(document.getElementsByClassName('latest-prerelease-tag')).forEach(function (el) { el.innerHTML = data.prerelease });
          })
          .catch(function (err) {
            console.log(err);
          });
      }
    },
    setExternalLinks: function () {
      Array.from(document.getElementsByClassName('external')).forEach(function (el) { el.setAttribute('target', '_blank') });
    },
    transformSearchData: function (suggestions) {
      if (window.location.origin !== 'https://hasura.io') {
        suggestions.forEach(function (suggestion) {
          suggestion.url = suggestion.url.replace(/https:\/\/hasura.io\/docs\/[^\/]*/, window.location.origin);
        });
      }

      const searchField = document.getElementById('search_element');
      const searchHelp = document.getElementById('search_help');
      const hideHelp = function () { searchHelp.classList.add('hide'); }

      if (suggestions.length === 0) {
        setTimeout(function () { searchHelp.classList.remove('hide'); }, 100);
        searchField.addEventListener('blur', function () {
          setTimeout(hideHelp, 100);
        }, { once: true });
        searchField.addEventListener('input', function () {
          if (searchField.value === '') {
            setTimeout(hideHelp, 100);
          }
        }, { once: true });
      } else if (!searchHelp.classList.contains('hide')) {
        hideHelp();
      }

      return suggestions;
    },
    handleNavClick: function (e) {
      const sidebar = document.getElementById('sidebar');
      const background = document.getElementById('content_inner_wrapper');
      const body = document.querySelector('body');

      if (sidebar.classList.contains('mobile-hide')) {
        sidebar.classList.remove('mobile-hide');
        background.classList.add('no_scroll');
        body.classList.add('no_scroll');
      } else {
        sidebar.classList.add('mobile-hide');
        background.classList.remove('no_scroll');
        body.classList.remove('no_scroll');
      }
    },
    // sendFeedback: function (feedback, feedbackDetailMsg) {
    //   const insertQuery = {
    //     'type': 'insert',
    //     'args': {
    //       'table': 'docs_feedback',
    //       'objects': [
    //         {
    //           'page': window.location.pathname,
    //           'feedback': feedback
    //         }
    //       ],
    //       'returning': ['id']
    //     }
    //   };
    //
    //   hdocs.request(DB_URL, insertQuery, 'POST')
    //     .then(function (response) {
    //       const data = JSON.parse(response);
    //
    //       if (feedback == 'positive' || feedback == 'negative') {
    //         document.getElementById('feedback_box').setAttribute('data-id', data['returning'][0]['id']);
    //         document.getElementById('feedback_detail_msg').innerHTML = feedbackDetailMsg;
    //         document.getElementById('feedback').classList.add('hide');
    //         document.getElementById('detailed_feedback').classList.remove('hide');
    //       }
    //     })
    //     .catch(function (err) {
    //       alert('Error capturing feedback');
    //       console.log(err);
    //     });
    // },
    // handleSubmitFeedback: function () {
    //   const feedbackBox = document.getElementById('feedback_box');
    //   const feedbackId = feedbackBox.getAttribute('data-id');
    //   const feedbackContent = feedbackBox.value;
    //
    //   const updateQuery = {
    //     'type': 'update',
    //     'args': {
    //       'table': 'docs_feedback',
    //       '$set': {
    //         'feedback_content': feedbackContent
    //       },
    //       'where': { 'id': feedbackId }
    //     }
    //   };
    //
    //   hdocs.request(DB_URL, updateQuery, 'POST')
    //     .then(function (response) {
    //       document.getElementById('detailed_feedback').classList.add('hide');
    //       document.getElementById('thank_you').classList.remove('hide');
    //     })
    //     .catch(function (err) {
    //       alert('Error capturing feedback');
    //       console.log(err);
    //     });
    // },
    graphQLFetcher: function (endpoint) {
      endpoint = endpoint || HDOCS_GRAPHIQL_DEFAULT_ENDPOINT;

      return function (graphQLParams) {
        const params = {
          method: 'post',
          headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(graphQLParams),
          credentials: 'include'
        };

        return fetch(endpoint, params)
          .then(function (response) {
            return response.text();
          })
          .then(function (responseBody) {
            try {
              return JSON.parse(responseBody);
            } catch (error) {
              return responseBody;
            }
          });
      }
    },
    setupGraphiQL: function () {
      if (typeof (React) === 'undefined' || typeof (ReactDOM) === 'undefined' || typeof (GraphiQL) === 'undefined') {
        return;
      }

      const targets = document.getElementsByClassName('graphiql');

      for (var i = 0; i < targets.length; i++) {
        const target = targets[i];

        const endpoint = target.getElementsByClassName("endpoint")[0].innerHTML.trim();
        const query = target.getElementsByClassName("query")[0].innerHTML.trim();
        const response = target.getElementsByClassName("response")[0].innerHTML.trim();
        const variables = target.getElementsByClassName("variables")[0].innerHTML.trim();

        const graphiQLElement = React.createElement(GraphiQL, {
          fetcher: hdocs.graphQLFetcher(endpoint),
          schema: null, // TODO: pass undefined if introspection supported
          query: query,
          response: response,
          variables: variables
        });

        ReactDOM.render(graphiQLElement, target);
      }

      Array.from(document.getElementsByClassName('variable-editor')).forEach(function (el) { el.style.height = '120px' });
    },
    formatNumber: function (number) {
      if (typeof number !== "number") return number;

      const SIsymbol = ["", "k", "M", "G", "T", "P", "E"];
      const absNumber = Math.abs(number);
      const sign = Math.sign(number);

      // what tier? (determines SI symbol)
      const tier = Math.log10(absNumber) / 3 | 0;

      // if zero, we don't need a suffix
      if (tier === 0) return sign * absNumber;

      // get suffix and determine scale
      const suffix = SIsymbol[tier];
      const scale = Math.pow(10, tier * 3);

      // scale the number
      const scaled = absNumber / scale;

      // format number and add suffix
      return sign * scaled.toFixed(1) + suffix;
    }
  }
})();
