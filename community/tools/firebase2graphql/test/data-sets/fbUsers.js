module.exports = {
  f2g_test_scores: {
    Rishi: 24,
    Rikin: 26,
    Tanmai: 27,
  },
  f2g_test_author: {
    someone: {
      one: {
        name: 'Rishi',
        age: 24,
        articles: {
          first: {
            title: 'Rishis article',
            body: "Rishi's article's body",
            comments: {
              'Comment 1': true,
              'Comment 2': true,
            },
          },
          second: {
            title: 'Rishis another article',
            body: "Rishi's another article's body",
            comments: {
              'Comment 3': true,
            },
          },
        },
        friends: {
          Rikin: true,
        },
      },
      two: {
        name: 'Rikin',
        age: 30,
        articles: {
          third: {
            title: "Rikin's article",
            body: "Rikin's article's body",
            comments: {
              'Comment 4': true,
              'Comment 5': true,
            },
          },
          fourth: {
            title: 'Rikins another article',
            body: "Rikin's another article's body",
            comments: {
              'Comment 6': true,
              'Comment df': true,
            },
          },
        },
        friends: {
          Rishi: true,
          Tanmai: true,
        },
      },
      three: {
        name: 'Tanmai',
        age: 30,
        articles: {
          fifth: {
            title: "Tanmai's article",
            body: "Tanmai's article's body",
            comments: {
              'Comment asdjf': true,
              'Comment dsiafjijf': true,
            },
          },
          sixth: {
            title: "Tanmai's another article",
            body: "Tanmai's another article's body",
            comments: {
              'Coafsdfment asdjf': true,
              'Commenasdft dsiafjijf': true,
            },
          },
        },
        friends: {
          Rikin: true,
        },
      },
    },
  },
  f2g_test_articles: {
    first: {
      title: 'Rishis article',
      body: "Rishi's article's body",
      author: {
        name: 'Rishi',
        age: 24,
      },
    },
    second: {
      title: 'Rishis another article',
      body: "Rishi's another article's body",
      author: {
        name: 'Rishi',
        age: 24,
      },
    },
    third: {
      title: "Rikin's article",
      body: "Rikin's article's body",
      author: {
        name: 'Rikin',
        age: 30,
      },
    },
    fourth: {
      title: 'Rikins another article',
      body: "Rikin's another article's body",
      author: {
        name: 'Rikin',
        age: 30,
      },
    },
    fifth: {
      title: "Tanmai's article",
      body: "Tanmai's article's body",
      author: {
        name: 'Tanmai',
        age: 30,
      },
    },
    sixth: {
      title: "Tanmai's another article",
      body: "Tanmai's another article's body",
      author: {
        name: 'Tanmai',
        age: 30,
      },
    },
  },
};
