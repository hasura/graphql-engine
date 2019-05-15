const db = {
  j2g_test_users: [
    {id: 1, name: 'Fredi Bach', country: 'CH', birthday: '1975-09-03', sex: 'm', email: 'osxcode@gmail.com', j2g_test_userStatus_id: 2, date: new Date(), object: {hey: 'there', whats: 'up'}},
    {id: 2, name: 'Samuel Patzen', country: 'CH', birthday: '1978-02-01', sex: 'm', email: 'patzen@bluewin.ch', j2g_test_userStatus_id: 2, date: new Date()},
    {id: 3, name: 'Hans Muster', country: 'CH', birthday: '1978-02-01', sex: 'm', email: 'hans.muster@domain.ch', j2g_test_userStatus_id: 1, date: new Date()},
  ],
  j2g_test_userStatus: [
    {id: 1, key: 'inactive'},
    {id: 2, key: 'active'},
    {id: 3, key: 'blocked'},
  ],
  j2g_test_userConfigs: [
    {id: 1, j2g_test_users_id: 1},
  ],
  j2g_test_leagues: [
    {id: 1, name: 'Switzerland', yearly: true, description: 'Waypoint are all placed in Switzerland by local instructors and top pilots.', created: '2018-05-01', seasonStart: '10-01', seasonEnd: '09-31'},
    {id: 2, name: 'Austria', yearly: true, description: 'Waypoint are all placed in Austria by local instructors and top pilots.', created: '2018-05-02', seasonStart: '10-01', seasonEnd: '09-31'},
    {id: 3, name: 'Vol Liber Grischun Clubmeisterschaft', yearly: false, created: '2018-05-02', seasonStart: '2018-10-01', seasonEnd: '2048-10-01'},
  ],
  j2g_test_userLeagues: [
    {id: 1, j2g_test_users_id: 1, j2g_test_leagues_id: 1, isAdmin: true},
    {id: 2, j2g_test_users_id: 1, j2g_test_leagues_id: 2, isAdmin: true},
    {id: 3, j2g_test_users_id: 2, j2g_test_leagues_id: 1},
    {id: 4, j2g_test_users_id: 1, j2g_test_leagues_id: 3},
    {id: 5, j2g_test_users_id: 2, j2g_test_leagues_id: 3, isAdmin: true},
  ],
  j2g_test_files: [
    {id: 1, j2g_test_mimetypes_id: 1, width: 250, height: 250, url: 'https://imgplaceholder.com/250x250/cccccc/757575/ion-happy-outline'},
    {id: 2, j2g_test_mimetypes_id: 1, width: 800, height: 400, url: 'https://imgplaceholder.com/800x400/cccccc/757575/fa-image'},
    {id: 3, j2g_test_mimetypes_id: 1, width: 300, height: 200, url: 'https://imgplaceholder.com/300x200/cccccc/757575/fa-map-marker'},
    {id: 4, j2g_test_mimetypes_id: 3, url: 'https://mycdn.com/fredi-bach/2018-07-02-001.igc'},
    {id: 5, j2g_test_mimetypes_id: 3, url: 'https://mycdn.com/fredi-bach/2018-07-03-001.igc'},
  ],
  j2g_test_mimetypes: [
    {id: 1, mime: 'image/png', description: 'Portable Network Graphics'},
    {id: 2, mime: 'image/jpeg', description: 'JPEG images'},
    {id: 3, mime: 'application/vnd.fai.igc', description: 'Flight track file'},
  ],
  j2g_test_types: [
    {id: 1, name: 'Challenge', description: 'A challenging waypoint, only for the best', points: 200},
    {id: 2, name: 'Altitude', description: 'A big mountain, that needs altitude to reach', points: 150},
    {id: 3, name: 'Beauty', description: 'Just a nice view', points: 100},
    {id: 4, name: 'Takeoff', description: 'Official takoeff', points: 10},
    {id: 5, name: 'Landing', description: 'Official landing', points: 10},
  ],
  j2g_test_waypoints: [
    {id: 1, j2g_test_leagues_id: 1, j2g_test_types_id: 1, lat: 3.789, lng: 41.987, radius: 400, points: 100, minAltitude: 3500, name: 'Oberalp Pass', description: 'From Andermatt to Disentis', j2g_test_files_id: 3},
    {id: 2, j2g_test_leagues_id: 1, j2g_test_types_id: 2, lat: 3.589, lng: 41.787, radius: 400, points: 100, minAltitude: 3500, name: 'Furka Pass', description: 'From the Goms to Andermatt', j2g_test_files_id: 3},
    {id: 3, j2g_test_leagues_id: 1, j2g_test_types_id: 4, lat: 3.889, lng: 40.787, radius: 400, points: 10, name: 'Fiesch'},
  ],
  j2g_test_waypointNotes: [
    {id: 1, j2g_test_waypoints_id: 1, j2g_test_noteTypes_id: 1, title: 'Föhn', text: 'Bei Föhn sehr gefährlich!'},
    {id: 2, j2g_test_waypoints_id: 1, j2g_test_noteTypes_id: 2, title: 'Basis', text: 'Braucht mindestens 3000 Meter Basis, besser mehr.'},
  ],
  j2g_test_waypointPhotos: [
    {id: 1, j2g_test_users_id: 1, official: true, j2g_test_waypoints_id: 1, j2g_test_mimetypes_id: 2, width: 1080, height: 960, url: 'https://mycdn.com/fredi-bach/oberalp-2018-1.jpeg'},
    {id: 2, j2g_test_users_id: 1, official: true, j2g_test_waypoints_id: 1, j2g_test_mimetypes_id: 2, width: 1080, height: 960, url: 'https://mycdn.com/fredi-bach/oberalp-2018-2.jpeg'},
    {id: 3, j2g_test_users_id: 2, official: false, j2g_test_waypoints_id: 1, j2g_test_mimetypes_id: 2, width: 1080, height: 960, url: 'https://mycdn.com/fredi-bach/oberalp-2018-3.jpeg'},
  ],
  j2g_test_waypointSuggestions: [
    {id: 1, j2g_test_users_id: 2, j2g_test_leagues_id: 1, j2g_test_types_id: 1, lat: 11.789, lng: 33.987, radius: 800, points: 100, minAltitude: 3500, name: 'Limmeren Stausee', description: 'Auf dem Weg von der Surselva ins Glaernerland', files_id: 3},
  ],
  j2g_test_noteTypes: [
    {id: 1, name: 'Wind', icon: 'wind', class: 'waypoint-note-wind'},
    {id: 2, name: 'Altitude', icon: 'altitude', class: 'waypoint-note-altitude'},
  ],
  j2g_test_sponsors: [
    {id: 1, waypoints_id: 1, j2g_test_users_id: 1, name: 'Flugschule Appenzell', url: 'http://www.gleitschirm.ch', slogan: 'Die Flugschule im Alpstein.'},
    {id: 2, waypoints_id: 2, name: 'Ozone', url: 'http://www.flyozone.ch', slogan: 'Real world performance.'},
  ],
  j2g_test_waypointChats: [
    {id: 1, j2g_test_waypoints_id: 1, j2g_test_users_id: 1, message: 'Can be quite hard with low base!', datetime: '2018-07-02 12:48:45'},
    {id: 2, j2g_test_waypoints_id: 1, j2g_test_users_id: 2, message: 'Oh yes, it can!', datetime: '2018-07-02 12:52:11'},
  ],
  j2g_test_wings: [
    {id: 1, model: 'Zeno', brand: 'Ozone', certification: 'D'},
    {id: 2, model: 'Mentor 3', brand: 'Nova', certification: 'B'},
  ],
  j2g_test_flights: [
    {id: 1, j2g_test_users_id: 1, j2g_test_leagues_id: 1, j2g_test_wings_id: 1, date: '2018-07-02', score: 200, j2g_test_files_id: 4, comment: 'Bockig!'},
    {id: 2, j2g_test_users_id: 2, j2g_test_leagues_id: 1, j2g_test_wings_id: 2, date: '2018-07-03', score: 100, j2g_test_files_id: 5},
  ],
  j2g_test_favoriteFlights: [
    {id: 1, j2g_test_users_id: 1, j2g_test_flights_id: 2, datetime: '2018-07-02 12:48:45'},
  ],
  j2g_test_flightWaypoints: [
    {id: 1, j2g_test_flights_id: 1, j2g_test_waypoints_id: 1, datetime: '2018-07-02 12:48:45', score: 100},
    {id: 2, j2g_test_flights_id: 1, j2g_test_waypoints_id: 2, datetime: '2018-07-02 13:11:59', score: 100},
    {id: 3, j2g_test_flights_id: 2, j2g_test_waypoints_id: 2, datetime: '2018-08-02 14:06:11', score: 100},
  ],
  j2g_test_flightComments: [
    {id: 1, j2g_test_flights_id: 1, j2g_test_users_id: 2, datetime: '2018-08-02 14:06:11', text: 'Ok, that was nice!'},
    {id: 2, j2g_test_flights_id: 1, j2g_test_users_id: 1, datetime: '2018-08-02 14:09:11', text: 'Thanks'},
  ],
  j2g_test_leagueSeasonUserScores: [
    {id: 1, j2g_test_users_id: 1, j2g_test_leagues_id: 1, season: '2018', score: 200, flightCount: 1},
    {id: 2, j2g_test_users_id: 1, j2g_test_leagues_id: 2, season: '2018', score: 0, flightCount: 0},
    {id: 3, j2g_test_users_id: 2, j2g_test_leagues_id: 1, season: '2018', score: 100, flightCount: 1},
  ],
  j2g_test_routes: [
    {id: 1, j2g_test_users_id: 1, j2g_test_leagues_id: 1, name: 'Wallis Sightseeing', description: 'A great route for a low wind high cloudbase day.'},
    {id: 2, j2g_test_users_id: 1, j2g_test_leagues_id: 1, name: 'Surselva Adventure'},
  ],
  j2g_test_routeWaypoints: [
    {id: 1, j2g_test_routes_id: 1, j2g_test_waypoints_id: 1},
    {id: 2, j2g_test_routes_id: 1, j2g_test_waypoints_id: 2, j2g_test_routeWaypoints_id: 1},
    {id: 3, j2g_test_routes_id: 1, j2g_test_waypoints_id: 3, j2g_test_routeWaypoints_id: 2},
  ],
  j2g_test_favoriteRoutes: [
    {id: 1, j2g_test_users_id: 1, j2g_test_routes_id: 1, datetime: '2018-07-01 15:48:45'},
  ],
};

module.exports = db;
