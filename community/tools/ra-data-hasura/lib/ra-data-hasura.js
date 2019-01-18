(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory();
	else if(typeof define === 'function' && define.amd)
		define("ra-data-hasura", [], factory);
	else if(typeof exports === 'object')
		exports["ra-data-hasura"] = factory();
	else
		root["ra-data-hasura"] = factory();
})(typeof self !== 'undefined' ? self : this, function() {
return /******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = "./src/index.js");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./src/index.js":
/*!**********************!*\
  !*** ./src/index.js ***!
  \**********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _queries = __webpack_require__(/*! ./queries */ "./src/queries.js");

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

var cloneQuery = function cloneQuery(query) {
  return JSON.parse(JSON.stringify(query));
};

var _default = function _default(serverEndpoint, headers) {
  var convertDataRequestToHTTP = function convertDataRequestToHTTP(type, resource, params) {
    var options = {};
    var finalQuery = {};

    switch (type) {
      case 'GET_LIST':
        // select multiple
        var finalSelectQuery = cloneQuery(_queries.selectQuery);
        var finalCountQuery = cloneQuery(_queries.countQuery);
        finalSelectQuery.args.table = resource;
        finalSelectQuery.args.limit = params.pagination.perPage;
        finalSelectQuery.args.offset = params.pagination.page * params.pagination.perPage - params.pagination.perPage;
        finalSelectQuery.args.where = params.filter;
        finalSelectQuery.args.order_by = {
          column: params.sort.field,
          type: params.sort.order.toLowerCase()
        };
        finalCountQuery.args.table = resource;
        finalQuery = cloneQuery(_queries.bulkQuery);
        finalQuery.args.push(finalSelectQuery);
        finalQuery.args.push(finalCountQuery);
        break;

      case 'GET_ONE':
        // select one
        finalQuery = cloneQuery(_queries.selectQuery);
        finalQuery.args.table = resource;
        finalQuery.args.where = {
          id: {
            '$eq': params.id
          }
        };
        break;

      case 'CREATE':
        // create one
        var createFields = Object.keys(params.data);
        finalQuery = cloneQuery(_queries.insertQuery);
        finalQuery.args.table = resource;
        finalQuery.args.objects.push(params.data); // id is mandatory

        createFields.push('id');
        finalQuery.args.returning = createFields;
        break;

      case 'UPDATE':
        // update one
        var updateFields = Object.keys(params.data);
        finalQuery = cloneQuery(_queries.updateQuery);
        finalQuery.args.table = resource;
        finalQuery.args['$set'] = params.data;
        finalQuery.args.where = {
          id: {
            '$eq': params.id
          }
        }; // id is mandatory

        updateFields.push('id');
        finalQuery.args.returning = updateFields;
        break;

      case 'UPDATE_MANY':
        // update multiple ids with given data
        var updateManyFields = Object.keys(params.data);
        finalQuery = cloneQuery(_queries.updateQuery);
        finalQuery.args.table = resource;
        finalQuery.args['$set'] = params.data;
        finalQuery.args.where = {
          'id': {
            '$in': params.ids
          }
        }; // id is mandatory

        updateManyFields.push('id');
        finalQuery.args.returning = updateManyFields;
        break;

      case 'DELETE':
        // delete one
        var deleteFields = Object.keys(params.previousData);
        finalQuery = cloneQuery(_queries.deleteQuery);
        finalQuery.args.table = resource;
        finalQuery.args.where = {
          id: {
            '$eq': params.id
          }
        }; // id is mandatory

        deleteFields.push('id');
        finalQuery.args.returning = deleteFields;
        break;

      case 'DELETE_MANY':
        // delete multiple
        finalQuery = cloneQuery(_queries.deleteQuery);
        finalQuery.args.table = resource;
        finalQuery.args.where = {
          'id': {
            '$in': params.ids
          }
        }; // id is mandatory

        finalQuery.args.returning = ['id'];
        break;

      case 'GET_MANY':
        // select multiple within where clause
        finalQuery = cloneQuery(_queries.selectQuery);
        finalQuery.args.table = resource;
        finalQuery.args.where = {
          'id': {
            '$in': params.ids
          }
        };
        break;

      case 'GET_MANY_REFERENCE':
        // select multiple with relations
        var finalManyQuery = cloneQuery(_queries.selectQuery);
        var finalManyCountQuery = cloneQuery(_queries.countQuery);
        finalSelectQuery.args.table = resource;
        finalSelectQuery.args.limit = params.pagination.perPage;
        finalSelectQuery.args.offset = params.pagination.page * params.pagination.perPage - params.pagination.perPage;
        finalSelectQuery.args.where = _defineProperty({}, params.target, params.id);
        finalSelectQuery.args.order_by = {
          column: params.sort.field,
          type: params.sort.order.toLowerCase()
        };
        finalCountQuery.args.table = resource;
        finalQuery = cloneQuery(_queries.bulkQuery);
        finalQuery.args.push(finalManyQuery);
        finalQuery.args.push(finalManyCountQuery);
        break;

      default:
        throw new Error("Unsupported type ".concat(type));
    }

    ;
    options.body = JSON.stringify(finalQuery);
    return {
      options: options
    };
  };

  var convertHTTPResponse = function convertHTTPResponse(response, type, resource, params) {
    // handle errors and throw with the message
    if ('error' in response || 'code' in response) {
      throw new Error(JSON.stringify(response));
    }

    switch (type) {
      case 'GET_LIST':
        return {
          data: response[0],
          total: response[1]['count']
        };

      case 'GET_ONE':
        return {
          data: response[0]
        };

      case 'CREATE':
        return {
          data: response.returning[0]
        };

      case 'UPDATE':
        return {
          data: response.returning[0]
        };

      case 'UPDATE_MANY':
        var updatedIds = response.returning.map(function (item) {
          return item.id;
        });
        return {
          data: updatedIds
        };

      case 'DELETE':
        return {
          data: response.returning[0]
        };

      case 'DELETE_MANY':
        var deletedIds = response.returning.map(function (item) {
          return item.id;
        });
        return {
          data: deletedIds
        };

      case 'GET_MANY':
        return {
          data: response
        };

      case 'GET_MANY_REFERENCE':
        return {
          data: response[0],
          total: response[1].count
        };

      default:
        return {
          data: response
        };
    }
  };

  return function (type, resource, params) {
    var _convertDataRequestTo = convertDataRequestToHTTP(type, resource, params),
        options = _convertDataRequestTo.options;

    options.method = 'POST';
    options.headers = headers;
    return fetch(serverEndpoint + '/v1/query', options).then(function (response) {
      return response.json().then(function (data) {
        return convertHTTPResponse(data, type, resource, params);
      });
    });
  };
};

exports.default = _default;
module.exports = exports["default"];

/***/ }),

/***/ "./src/queries.js":
/*!************************!*\
  !*** ./src/queries.js ***!
  \************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.deleteQuery = exports.updateQuery = exports.insertQuery = exports.countQuery = exports.selectQuery = exports.bulkQuery = void 0;
// define hasura json api queries
var bulkQuery = {
  type: 'bulk',
  args: []
};
exports.bulkQuery = bulkQuery;
var selectQuery = {
  type: 'select',
  args: {
    table: '',
    columns: ['*']
  }
};
exports.selectQuery = selectQuery;
var countQuery = {
  type: 'count',
  args: {
    table: '',
    where: {
      id: {
        '$gt': 0
      }
    }
  }
};
exports.countQuery = countQuery;
var insertQuery = {
  type: 'insert',
  args: {
    table: '',
    objects: [],
    returning: []
  }
};
exports.insertQuery = insertQuery;
var updateQuery = {
  type: 'update',
  args: {
    table: '',
    $set: {},
    where: {},
    returning: []
  }
};
exports.updateQuery = updateQuery;
var deleteQuery = {
  type: 'delete',
  args: {
    table: '',
    where: {},
    returning: []
  }
};
exports.deleteQuery = deleteQuery;

/***/ })

/******/ });
});
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly9yYS1kYXRhLWhhc3VyYS93ZWJwYWNrL3VuaXZlcnNhbE1vZHVsZURlZmluaXRpb24iLCJ3ZWJwYWNrOi8vcmEtZGF0YS1oYXN1cmEvd2VicGFjay9ib290c3RyYXAiLCJ3ZWJwYWNrOi8vcmEtZGF0YS1oYXN1cmEvLi9zcmMvaW5kZXguanMiLCJ3ZWJwYWNrOi8vcmEtZGF0YS1oYXN1cmEvLi9zcmMvcXVlcmllcy5qcyJdLCJuYW1lcyI6WyJjbG9uZVF1ZXJ5IiwicXVlcnkiLCJKU09OIiwicGFyc2UiLCJzdHJpbmdpZnkiLCJzZXJ2ZXJFbmRwb2ludCIsImhlYWRlcnMiLCJjb252ZXJ0RGF0YVJlcXVlc3RUb0hUVFAiLCJ0eXBlIiwicmVzb3VyY2UiLCJwYXJhbXMiLCJvcHRpb25zIiwiZmluYWxRdWVyeSIsImZpbmFsU2VsZWN0UXVlcnkiLCJmaW5hbENvdW50UXVlcnkiLCJhcmdzIiwidGFibGUiLCJsaW1pdCIsInBhZ2luYXRpb24iLCJwZXJQYWdlIiwib2Zmc2V0IiwicGFnZSIsIndoZXJlIiwiZmlsdGVyIiwib3JkZXJfYnkiLCJjb2x1bW4iLCJzb3J0IiwiZmllbGQiLCJvcmRlciIsInRvTG93ZXJDYXNlIiwicHVzaCIsImlkIiwiY3JlYXRlRmllbGRzIiwiT2JqZWN0Iiwia2V5cyIsImRhdGEiLCJvYmplY3RzIiwicmV0dXJuaW5nIiwidXBkYXRlRmllbGRzIiwidXBkYXRlTWFueUZpZWxkcyIsImlkcyIsImRlbGV0ZUZpZWxkcyIsInByZXZpb3VzRGF0YSIsImZpbmFsTWFueVF1ZXJ5IiwiZmluYWxNYW55Q291bnRRdWVyeSIsInRhcmdldCIsIkVycm9yIiwiYm9keSIsImNvbnZlcnRIVFRQUmVzcG9uc2UiLCJyZXNwb25zZSIsInRvdGFsIiwidXBkYXRlZElkcyIsIm1hcCIsIml0ZW0iLCJkZWxldGVkSWRzIiwiY291bnQiLCJtZXRob2QiLCJmZXRjaCIsInRoZW4iLCJqc29uIiwiYnVsa1F1ZXJ5Iiwic2VsZWN0UXVlcnkiLCJjb2x1bW5zIiwiY291bnRRdWVyeSIsImluc2VydFF1ZXJ5IiwidXBkYXRlUXVlcnkiLCIkc2V0IiwiZGVsZXRlUXVlcnkiXSwibWFwcGluZ3MiOiJBQUFBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBLENBQUM7QUFDRCxPO0FDVkE7QUFDQTs7QUFFQTtBQUNBOztBQUVBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOztBQUVBO0FBQ0E7O0FBRUE7QUFDQTs7QUFFQTtBQUNBO0FBQ0E7OztBQUdBO0FBQ0E7O0FBRUE7QUFDQTs7QUFFQTtBQUNBO0FBQ0E7QUFDQSxrREFBMEMsZ0NBQWdDO0FBQzFFO0FBQ0E7O0FBRUE7QUFDQTtBQUNBO0FBQ0EsZ0VBQXdELGtCQUFrQjtBQUMxRTtBQUNBLHlEQUFpRCxjQUFjO0FBQy9EOztBQUVBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQSxpREFBeUMsaUNBQWlDO0FBQzFFLHdIQUFnSCxtQkFBbUIsRUFBRTtBQUNySTtBQUNBOztBQUVBO0FBQ0E7QUFDQTtBQUNBLG1DQUEyQiwwQkFBMEIsRUFBRTtBQUN2RCx5Q0FBaUMsZUFBZTtBQUNoRDtBQUNBO0FBQ0E7O0FBRUE7QUFDQSw4REFBc0QsK0RBQStEOztBQUVySDtBQUNBOzs7QUFHQTtBQUNBOzs7Ozs7Ozs7Ozs7Ozs7Ozs7OztBQ2xGQTs7OztBQVNBLElBQU1BLFVBQVUsR0FBRyxTQUFiQSxVQUFhLENBQUNDLEtBQUQsRUFBVztBQUM1QixTQUFPQyxJQUFJLENBQUNDLEtBQUwsQ0FBV0QsSUFBSSxDQUFDRSxTQUFMLENBQWVILEtBQWYsQ0FBWCxDQUFQO0FBQ0QsQ0FGRDs7ZUFJZSxrQkFBQ0ksY0FBRCxFQUFpQkMsT0FBakIsRUFBNkI7QUFDMUMsTUFBTUMsd0JBQXdCLEdBQUcsU0FBM0JBLHdCQUEyQixDQUFDQyxJQUFELEVBQU9DLFFBQVAsRUFBaUJDLE1BQWpCLEVBQTRCO0FB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TyxFQUFFLEVBRkw7QUFHSkMsYUFBUyxFQUFFO0FBSFA7QUFGWSxDQUFwQjs7QUFTQSxJQUFNNEIsV0FBVyxHQUFHO0FBQ2xCekQsTUFBSSxFQUFFLFFBRFk7QUFFbEJPLE1BQUksRUFBRTtBQUNKQyxTQUFLLEVBQUUsRUFESDtBQUVKa0QsUUFBSSxFQUFFLEVBRkY7QUFHSjVDLFNBQUssRUFBRSxFQUhIO0FBSUplLGFBQVMsRUFBRTtBQUpQO0FBRlksQ0FBcEI7O0FBVUEsSUFBTThCLFdBQVcsR0FBRztBQUNsQjNELE1BQUksRUFBRSxRQURZO0FBRWxCTyxNQUFJLEVBQUU7QUFDSkMsU0FBSyxFQUFFLEVBREg7QUFFSk0sU0FBSyxFQUFFLEVBRkg7QUFHSmUsYUFBUyxFQUFFO0FBSFA7QUFGWSxDQUFwQiIsImZpbGUiOiJyYS1kYXRhLWhhc3VyYS5qcyIsInNvdXJjZXNDb250ZW50IjpbIihmdW5jdGlvbiB3ZWJwYWNrVW5pdmVyc2FsTW9kdWxlRGVmaW5pdGlvbihyb290LCBmYWN0b3J5KSB7XG5cdGlmKHR5cGVvZiBleHBvcnRzID09PSAnb2JqZWN0JyAmJiB0eXBlb2YgbW9kdWxlID09PSAnb2JqZWN0Jylcblx0XHRtb2R1bGUuZXhwb3J0cyA9IGZhY3RvcnkoKTtcblx0ZWxzZSBpZih0eXBlb2YgZGVmaW5lID09PSAnZnVuY3Rpb24nICYmIGRlZmluZS5hbWQpXG5cdFx0ZGVmaW5lKFwicmEtZGF0YS1oYXN1cmFcIiwgW10sIGZhY3RvcnkpO1xuXHRlbHNlIGlmKHR5cGVvZiBleHBvcnRzID09PSAnb2JqZWN0Jylcblx0XHRleHBvcnRzW1wicmEtZGF0YS1oYXN1cmFcIl0gPSBmYWN0b3J5KCk7XG5cdGVsc2Vcblx0XHRyb290W1wicmEtZGF0YS1oYXN1cmFcIl0gPSBmYWN0b3J5KCk7XG59KSh0eXBlb2Ygc2VsZiAhPT0gJ3VuZGVmaW5lZCcgPyBzZWxmIDogdGhpcywgZnVuY3Rpb24oKSB7XG5yZXR1cm4gIiwiIFx0Ly8gVGhlIG1vZHVsZSBjYWNoZVxuIFx0dmFyIGluc3RhbGxlZE1vZHVsZXMgPSB7fTtcblxuIFx0Ly8gVGhlIHJlcXVpcmUgZnVuY3Rpb25cbiBcdGZ1bmN0aW9uIF9fd2VicGFja19yZXF1aXJlX18obW9kdWxlSWQpIHtcblxuIFx0XHQvLyBDaGVjayBpZiBtb2R1bGUgaXMgaW4gY2FjaGVcbiBcdFx0aWYoaW5zdGFsbGVkTW9kdWxlc1ttb2R1bGVJZF0pIHtcbiBcdFx0XHRyZXR1cm4gaW5zdGFsbGVkTW9kdWxlc1ttb2R1bGVJZF0uZXhwb3J0cztcbiBcdFx0fVxuIFx0XHQvLyBDcmVhdGUgYSBuZXcgbW9kdWxlIChhbmQgcHV0IGl0IGludG8gdGhlIGNhY2hlKVxuIFx0XHR2YXIgbW9kdWxlID0gaW5zdGFsbGVkTW9kdWxlc1ttb2R1bGVJZF0gPSB7XG4gXHRcdFx0aTogbW9kdWxlSWQsXG4gXHRcdFx0bDogZmFsc2UsXG4gXHRcdFx0ZXhwb3J0czoge31cbiBcdFx0fTtcblxuIFx0XHQvLyBFeGVjdXRlIHRoZSBtb2R1bGUgZnVuY3Rpb25cbiBcdFx0bW9kdWxlc1ttb2R1bGVJZF0uY2FsbChtb2R1bGUuZXhwb3J0cywgbW9kdWxlLCBtb2R1bGUuZXhwb3J0cywgX193ZWJwYWNrX3JlcXVpcmVfXyk7XG5cbiBcdFx0Ly8gRmxhZyB0aGUgbW9kdWxlIGFzIGxvYWRlZFxuIFx0XHRtb2R1bGUubCA9IHRydWU7XG5cbiBcdFx0Ly8gUmV0dXJuIHRoZSBleHBvcnRzIG9mIHRoZSBtb2R1bGVcbiBcdFx0cmV0dXJuIG1vZHVsZS5leHBvcnRzO1xuIFx0fVxuXG5cbiBcdC8vIGV4cG9zZSB0aGUgbW9kdWxlcyBvYmplY3QgKF9fd2VicGFja19tb2R1bGVzX18pXG4gXHRfX3dlYnBhY2tfcmVxdWlyZV9fLm0gPSBtb2R1bGVzO1xuXG4gXHQvLyBleHBvc2UgdGhlIG1vZHVsZSBjYWNoZVxuIFx0X193ZWJwYWNrX3JlcXVpcmVfXy5jID0gaW5zdGFsbGVkTW9kdWxlcztcblxuIFx0Ly8gZGVmaW5lIGdldHRlciBmdW5jdGlvbiBmb3IgaGFybW9ueSBleHBvcnRzXG4gXHRfX3dlYnBhY2tfcmVxdWlyZV9fLmQgPSBmdW5jdGlvbihleHBvcnRzLCBuYW1lLCBnZXR0ZXIpIHtcbiBcdFx0aWYoIV9fd2VicGFja19yZXF1aXJlX18ubyhleHBvcnRzLCBuYW1lKSkge1xuIFx0XHRcdE9iamVjdC5kZWZpbmVQcm9wZXJ0eShleHBvcnRzLCBuYW1lLCB7IGVudW1lcmFibGU6IHRydWUsIGdldDogZ2V0dGVyIH0pO1xuIFx0XHR9XG4gXHR9O1xuXG4gXHQvLyBkZWZpbmUgX19lc01vZHVsZSBvbiBleHBvcnRzXG4gXHRfX3dlYnBhY2tfcmVxdWlyZV9fLnIgPSBmdW5jdGlvbihleHBvcnRzKSB7XG4gXHRcdGlmKHR5cGVvZiBTeW1ib2wgIT09ICd1bmRlZmluZWQnICYmIFN5bWJvbC50b1N0cmluZ1RhZykge1xuIFx0XHRcdE9iamVjdC5kZWZpbmVQcm9wZXJ0eShleHBvcnRzLCBTeW1ib2wudG9TdHJpbmdUYWcsIHsgdmFsdWU6ICdNb2R1bGUnIH0pO1xuIFx0XHR9XG4gXHRcdE9iamVjdC5kZWZpbmVQcm9wZXJ0eShleHBvcnRzLCAnX19lc01vZHVsZScsIHsgdmFsdWU6IHRydWUgfSk7XG4gXHR9O1xuXG4gXHQvLyBjcmVhdGUgYSBmYWtlIG5hbWVzcGFjZSBvYmplY3RcbiBcdC8vIG1vZGUgJiAxOiB2YWx1ZSBpcyBhIG1vZHVsZSBpZCwgcmVxdWlyZSBpdFxuIFx0Ly8gbW9kZSAmIDI6IG1lcmdlIGFsbCBwcm9wZXJ0aWVzIG9mIHZhbHVlIGludG8gdGhlIG5zXG4gXHQvLyBtb2RlICYgNDogcmV0dXJuIHZhbHVlIHdoZW4gYWxyZWFkeSBucyBvYmplY3RcbiBcdC8vIG1vZGUgJiA4fDE6IGJlaGF2ZSBsaWtlIHJlcXVpcmVcbiBcdF9fd2VicGFja19yZXF1aXJlX18udCA9IGZ1bmN0aW9uKHZhbHVlLCBtb2RlKSB7XG4gXHRcdGlmKG1vZGUgJiAxKSB2YWx1ZSA9IF9fd2VicGFja19yZXF1aXJlX18odmFsdWUpO1xuIFx0XHRpZihtb2RlICYgOCkgcmV0dXJuIHZhbHVlO1xuIFx0XHRpZigobW9kZSAmIDQpICYmIHR5cGVvZiB2YWx1ZSA9PT0gJ29iamVjdCcgJiYgdmFsdWUgJiYgdmFsdWUuX19lc01vZHVsZSkgcmV0dXJuIHZhbHVlO1xuIFx0XHR2YXIgbnMgPSBPYmplY3QuY3JlYXRlKG51bGwpO1xuIFx0XHRfX3dlYnBhY2tfcmVxdWlyZV9fLnIobnMpO1xuIFx0XHRPYmplY3QuZGVmaW5lUHJvcGVydHkobnMsICdkZWZhdWx0JywgeyBlbnVtZXJhYmxlOiB0cnVlLCB2YWx1ZTogdmFsdWUgfSk7XG4gXHRcdGlmKG1vZGUgJiAyICYmIHR5cGVvZiB2YWx1ZSAhPSAnc3RyaW5nJykgZm9yKHZhciBrZXkgaW4gdmFsdWUpIF9fd2VicGFja19yZXF1aXJlX18uZChucywga2V5LCBmdW5jdGlvbihrZXkpIHsgcmV0dXJuIHZhbHVlW2tleV07IH0uYmluZChudWxsLCBrZXkpKTtcbiBcdFx0cmV0dXJuIG5zO1xuIFx0fTtcblxuIFx0Ly8gZ2V0RGVmYXVsdEV4cG9ydCBmdW5jdGlvbiBmb3IgY29tcGF0aWJpbGl0eSB3aXRoIG5vbi1oYXJtb255IG1vZHVsZXNcbiBcdF9fd2VicGFja19yZXF1aXJlX18ubiA9IGZ1bmN0aW9uKG1vZHVsZSkge1xuIFx0XHR2YXIgZ2V0dGVyID0gbW9kdWxlICYmIG1vZHVsZS5fX2VzTW9kdWxlID9cbiBcdFx0XHRmdW5jdGlvbiBnZXREZWZhdWx0KCkgeyByZXR1cm4gbW9kdWxlWydkZWZhdWx0J107IH0gOlxuIFx0XHRcdGZ1bmN0aW9uIGdldE1vZHVsZUV4cG9ydHMoKSB7IHJldHVybiBtb2R1bGU7IH07XG4gXHRcdF9fd2VicGFja19yZXF1aXJlX18uZChnZXR0ZXIsICdhJywgZ2V0dGVyKTtcbiBcdFx0cmV0dXJuIGdldHRlcjtcbiBcdH07XG5cbiBcdC8vIE9iamVjdC5wcm90b3R5cGUuaGFzT3duUHJvcGVydHkuY2FsbFxuIFx0X193ZWJwYWNrX3JlcXVpcmVfXy5vID0gZnVuY3Rpb24ob2JqZWN0LCBwcm9wZXJ0eSkgeyByZXR1cm4gT2JqZWN0LnByb3RvdHlwZS5oYXNPd25Qcm9wZXJ0eS5jYWxsKG9iamVjdCwgcHJvcGVydHkpOyB9O1xuXG4gXHQvLyBfX3dlYnBhY2tfcHVibGljX3BhdGhfX1xuIFx0X193ZWJwYWNrX3JlcXVpcmVfXy5wID0gXCJcIjtcblxuXG4gXHQvLyBMb2FkIGVudHJ5IG1vZHVsZSBhbmQgcmV0dXJuIGV4cG9ydHNcbiBcdHJldHVybiBfX3dlYnBhY2tfcmVxdWlyZV9fKF9fd2VicGFja19yZXF1aXJlX18ucyA9IFwiLi9zcmMvaW5kZXguanNcIik7XG4iLCJpbXBvcnQge1xuICBidWxrUXVlcnksXG4gIHNlbGVjdFF1ZXJ5LFxuICBjb3VudFF1ZXJ5LFxuICBpbnNlcnRRdWVyeSxcbiAgdXBkYXRlUXVlcnksXG4gIGRlbGV0ZVF1ZXJ5XG59IGZyb20gJy4vcXVlcmllcyc7XG5cbmNvbnN0IGNsb25lUXVlcnkgPSAocXVlcnkpID0+IHtcbiAgcmV0dXJuIEpTT04ucGFyc2UoSlNPTi5zdHJpbmdpZnkocXVlcnkpKTtcbn07XG5cbmV4cG9ydCBkZWZhdWx0IChzZXJ2ZXJFbmRwb2ludCwgaGVhZGVycykgPT4ge1xuICBjb25zdCBjb252ZXJ0RGF0YVJlcXVlc3RUb0hUVFAgPSAodHlwZSwgcmVzb3VyY2UsIHBhcmFtcykgPT4ge1xuICAgIGNvbnN0IG9wdGlvbnMgPSB7fTtcbiAgICBsZXQgZmluYWxRdWVyeSA9IHt9O1xuXG4gICAgc3dpdGNoICh0eXBlKSB7XG4gICAgICBjYXNlICdHRVRfTElTVCc6XG4gICAgICAgIC8vIHNlbGVjdCBtdWx0aXBsZVxuICAgICAgICBjb25zdCBmaW5hbFNlbGVjdFF1ZXJ5ID0gY2xvbmVRdWVyeShzZWxlY3RRdWVyeSk7XG4gICAgICAgIGNvbnN0IGZpbmFsQ291bnRRdWVyeSA9IGNsb25lUXVlcnkoY291bnRRdWVyeSk7XG5cbiAgICAgICAgZmluYWxTZWxlY3RRdWVyeS5hcmdzLnRhYmxlID0gcmVzb3VyY2U7XG4gICAgICAgIGZpbmFsU2VsZWN0UXVlcnkuYXJncy5saW1pdCA9IHBhcmFtcy5wYWdpbmF0aW9uLnBlclBhZ2U7XG4gICAgICAgIGZpbmFsU2VsZWN0UXVlcnkuYXJncy5vZmZzZXQgPSAocGFyYW1zLnBhZ2luYXRpb24ucGFnZSAqIHBhcmFtcy5wYWdpbmF0aW9uLnBlclBhZ2UpIC0gcGFyYW1zLnBhZ2luYXRpb24ucGVyUGFnZTtcbiAgICAgICAgZmluYWxTZWxlY3RRdWVyeS5hcmdzLndoZXJlID0gcGFyYW1zLmZpbHRlcjtcbiAgICAgICAgZmluYWxTZWxlY3RRdWVyeS5hcmdzLm9yZGVyX2J5ID0ge2NvbHVtbjogcGFyYW1zLnNvcnQuZmllbGQsIHR5cGU6IHBhcmFtcy5zb3J0Lm9yZGVyLnRvTG93ZXJDYXNlKCl9O1xuICAgICAgICBmaW5hbENvdW50UXVlcnkuYXJncy50YWJsZSA9IHJlc291cmNlO1xuICAgICAgICBmaW5hbFF1ZXJ5ID0gY2xvbmVRdWVyeShidWxrUXVlcnkpO1xuICAgICAgICBmaW5hbFF1ZXJ5LmFyZ3MucHVzaChmaW5hbFNlbGVjdFF1ZXJ5KTtcbiAgICAgICAgZmluYWxRdWVyeS5hcmdzLnB1c2goZmluYWxDb3VudFF1ZXJ5KTtcbiAgICAgICAgYnJlYWs7XG4gICAgICBjYXNlICdHRVRfT05FJzpcbiAgICAgICAgLy8gc2VsZWN0IG9uZVxuICAgICAgICBmaW5hbFF1ZXJ5ID0gY2xvbmVRdWVyeShzZWxlY3RRdWVyeSk7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy50YWJsZSA9IHJlc291cmNlO1xuICAgICAgICBmaW5hbFF1ZXJ5LmFyZ3Mud2hlcmUgPSB7IGlkOiB7ICckZXEnOiBwYXJhbXMuaWQgfSB9O1xuICAgICAgICBicmVhaztcbiAgICAgIGNhc2UgJ0NSRUFURSc6XG4gICAgICAgIC8vIGNyZWF0ZSBvbmVcbiAgICAgICAgY29uc3QgY3JlYXRlRmllbGRzID0gT2JqZWN0LmtleXMocGFyYW1zLmRhdGEpO1xuXG4gICAgICAgIGZpbmFsUXVlcnkgPSBjbG9uZVF1ZXJ5KGluc2VydFF1ZXJ5KTtcbiAgICAgICAgZmluYWxRdWVyeS5hcmdzLnRhYmxlID0gcmVzb3VyY2U7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy5vYmplY3RzLnB1c2gocGFyYW1zLmRhdGEpO1xuICAgICAgICAvLyBpZCBpcyBtYW5kYXRvcnlcbiAgICAgICAgY3JlYXRlRmllbGRzLnB1c2goJ2lkJyk7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy5yZXR1cm5pbmcgPSBjcmVhdGVGaWVsZHM7XG4gICAgICAgIGJyZWFrO1xuICAgICAgY2FzZSAnVVBEQVRFJzpcbiAgICAgICAgLy8gdXBkYXRlIG9uZVxuICAgICAgICBjb25zdCB1cGRhdGVGaWVsZHMgPSBPYmplY3Qua2V5cyhwYXJhbXMuZGF0YSk7XG5cbiAgICAgICAgZmluYWxRdWVyeSA9IGNsb25lUXVlcnkodXBkYXRlUXVlcnkpO1xuICAgICAgICBmaW5hbFF1ZXJ5LmFyZ3MudGFibGUgPSByZXNvdXJjZTtcbiAgICAgICAgZmluYWxRdWVyeS5hcmdzWyckc2V0J10gPSBwYXJhbXMuZGF0YTtcbiAgICAgICAgZmluYWxRdWVyeS5hcmdzLndoZXJlID0geyBpZDogeyAnJGVxJzogcGFyYW1zLmlkIH19O1xuICAgICAgICAvLyBpZCBpcyBtYW5kYXRvcnlcbiAgICAgICAgdXBkYXRlRmllbGRzLnB1c2goJ2lkJyk7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy5yZXR1cm5pbmcgPSB1cGRhdGVGaWVsZHM7XG4gICAgICAgIGJyZWFrO1xuICAgICAgY2FzZSAnVVBEQVRFX01BTlknOlxuICAgICAgICAvLyB1cGRhdGUgbXVsdGlwbGUgaWRzIHdpdGggZ2l2ZW4gZGF0YVxuICAgICAgICBjb25zdCB1cGRhdGVNYW55RmllbGRzID0gT2JqZWN0LmtleXMocGFyYW1zLmRhdGEpO1xuXG4gICAgICAgIGZpbmFsUXVlcnkgPSBjbG9uZVF1ZXJ5KHVwZGF0ZVF1ZXJ5KTtcbiAgICAgICAgZmluYWxRdWVyeS5hcmdzLnRhYmxlID0gcmVzb3VyY2U7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJnc1snJHNldCddID0gcGFyYW1zLmRhdGE7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy53aGVyZSA9IHsgJ2lkJzogeyAnJGluJzogcGFyYW1zLmlkcyB9IH07XG4gICAgICAgIC8vIGlkIGlzIG1hbmRhdG9yeVxuICAgICAgICB1cGRhdGVNYW55RmllbGRzLnB1c2goJ2lkJyk7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy5yZXR1cm5pbmcgPSB1cGRhdGVNYW55RmllbGRzO1xuICAgICAgICBicmVhaztcbiAgICAgIGNhc2UgJ0RFTEVURSc6XG4gICAgICAgIC8vIGRlbGV0ZSBvbmVcbiAgICAgICAgY29uc3QgZGVsZXRlRmllbGRzID0gT2JqZWN0LmtleXMocGFyYW1zLnByZXZpb3VzRGF0YSk7XG5cbiAgICAgICAgZmluYWxRdWVyeSA9IGNsb25lUXVlcnkoZGVsZXRlUXVlcnkpO1xuICAgICAgICBmaW5hbFF1ZXJ5LmFyZ3MudGFibGUgPSByZXNvdXJjZTtcbiAgICAgICAgZmluYWxRdWVyeS5hcmdzLndoZXJlID0geyBpZDogeyAnJGVxJzogcGFyYW1zLmlkIH19O1xuICAgICAgICAvLyBpZCBpcyBtYW5kYXRvcnlcbiAgICAgICAgZGVsZXRlRmllbGRzLnB1c2goJ2lkJyk7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy5yZXR1cm5pbmcgPSBkZWxldGVGaWVsZHM7XG4gICAgICAgIGJyZWFrO1xuICAgICAgY2FzZSAnREVMRVRFX01BTlknOlxuICAgICAgICAvLyBkZWxldGUgbXVsdGlwbGVcbiAgICAgICAgZmluYWxRdWVyeSA9IGNsb25lUXVlcnkoZGVsZXRlUXVlcnkpO1xuICAgICAgICBmaW5hbFF1ZXJ5LmFyZ3MudGFibGUgPSByZXNvdXJjZTtcbiAgICAgICAgZmluYWxRdWVyeS5hcmdzLndoZXJlID0geyAnaWQnOiB7ICckaW4nOiBwYXJhbXMuaWRzIH0gfTtcbiAgICAgICAgLy8gaWQgaXMgbWFuZGF0b3J5XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy5yZXR1cm5pbmcgPSBbJ2lkJ107XG4gICAgICAgIGJyZWFrO1xuICAgICAgY2FzZSAnR0VUX01BTlknOlxuICAgICAgICAvLyBzZWxlY3QgbXVsdGlwbGUgd2l0aGluIHdoZXJlIGNsYXVzZVxuICAgICAgICBmaW5hbFF1ZXJ5ID0gY2xvbmVRdWVyeShzZWxlY3RRdWVyeSk7XG4gICAgICAgIGZpbmFsUXVlcnkuYXJncy50YWJsZSA9IHJlc291cmNlO1xuICAgICAgICBmaW5hbFF1ZXJ5LmFyZ3Mud2hlcmUgPSB7ICdpZCc6IHsgJyRpbic6IHBhcmFtcy5pZHMgfSB9O1xuICAgICAgICBicmVhaztcbiAgICAgIGNhc2UgJ0dFVF9NQU5ZX1JFRkVSRU5DRSc6XG4gICAgICAgIC8vIHNlbGVjdCBtdWx0aXBsZSB3aXRoIHJlbGF0aW9uc1xuICAgICAgICBjb25zdCBmaW5hbE1hbnlRdWVyeSA9IGNsb25lUXVlcnkoc2VsZWN0UXVlcnkpO1xuICAgICAgICBjb25zdCBmaW5hbE1hbnlDb3VudFF1ZXJ5ID0gY2xvbmVRdWVyeShjb3VudFF1ZXJ5KTtcblxuICAgICAgICBmaW5hbFNlbGVjdFF1ZXJ5LmFyZ3MudGFibGUgPSByZXNvdXJjZTtcbiAgICAgICAgZmluYWxTZWxlY3RRdWVyeS5hcmdzLmxpbWl0ID0gcGFyYW1zLnBhZ2luYXRpb24ucGVyUGFnZTtcbiAgICAgICAgZmluYWxTZWxlY3RRdWVyeS5hcmdzLm9mZnNldCA9IChwYXJhbXMucGFnaW5hdGlvbi5wYWdlICogcGFyYW1zLnBhZ2luYXRpb24ucGVyUGFnZSkgLSBwYXJhbXMucGFnaW5hdGlvbi5wZXJQYWdlO1xuICAgICAgICBmaW5hbFNlbGVjdFF1ZXJ5LmFyZ3Mud2hlcmUgPSB7IFtwYXJhbXMudGFyZ2V0XTogcGFyYW1zLmlkIH07XG4gICAgICAgIGZpbmFsU2VsZWN0UXVlcnkuYXJncy5vcmRlcl9ieSA9IHtjb2x1bW46IHBhcmFtcy5zb3J0LmZpZWxkLCB0eXBlOiBwYXJhbXMuc29ydC5vcmRlci50b0xvd2VyQ2FzZSgpfTtcbiAgICAgICAgZmluYWxDb3VudFF1ZXJ5LmFyZ3MudGFibGUgPSByZXNvdXJjZTtcbiAgICAgICAgZmluYWxRdWVyeSA9IGNsb25lUXVlcnkoYnVsa1F1ZXJ5KTtcbiAgICAgICAgZmluYWxRdWVyeS5hcmdzLnB1c2goZmluYWxNYW55UXVlcnkpO1xuICAgICAgICBmaW5hbFF1ZXJ5LmFyZ3MucHVzaChmaW5hbE1hbnlDb3VudFF1ZXJ5KTtcbiAgICAgICAgYnJlYWs7XG4gICAgICBkZWZhdWx0OlxuICAgICAgICB0aHJvdyBuZXcgRXJyb3IoYFVuc3VwcG9ydGVkIHR5cGUgJHt0eXBlfWApO1xuICAgIH07XG4gICAgb3B0aW9ucy5ib2R5ID0gSlNPTi5zdHJpbmdpZnkoZmluYWxRdWVyeSk7XG4gICAgcmV0dXJuIHsgb3B0aW9ucyB9O1xuICB9O1xuXG4gIGNvbnN0IGNvbnZlcnRIVFRQUmVzcG9uc2UgPSAocmVzcG9uc2UsIHR5cGUsIHJlc291cmNlLCBwYXJhbXMpID0+IHtcbiAgICAvLyBoYW5kbGUgZXJyb3JzIGFuZCB0aHJvdyB3aXRoIHRoZSBtZXNzYWdlXG4gICAgaWYgKCdlcnJvcicgaW4gcmVzcG9uc2UgfHwgJ2NvZGUnIGluIHJlc3BvbnNlKSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoSlNPTi5zdHJpbmdpZnkocmVzcG9uc2UpKTtcbiAgICB9XG4gICAgc3dpdGNoICh0eXBlKSB7XG4gICAgICBjYXNlICdHRVRfTElTVCc6XG4gICAgICAgIHJldHVybiB7XG4gICAgICAgICAgZGF0YTogcmVzcG9uc2VbMF0sXG4gICAgICAgICAgdG90YWw6IHJlc3BvbnNlWzFdWydjb3VudCddXG4gICAgICAgIH07XG4gICAgICBjYXNlICdHRVRfT05FJzpcbiAgICAgICAgcmV0dXJuIHtcbiAgICAgICAgICBkYXRhOiByZXNwb25zZVswXVxuICAgICAgICB9O1xuICAgICAgY2FzZSAnQ1JFQVRFJzpcbiAgICAgICAgcmV0dXJuIHtcbiAgICAgICAgICBkYXRhOiByZXNwb25zZS5yZXR1cm5pbmdbMF1cbiAgICAgICAgfTtcbiAgICAgIGNhc2UgJ1VQREFURSc6XG4gICAgICAgIHJldHVybiB7XG4gICAgICAgICAgZGF0YTogcmVzcG9uc2UucmV0dXJuaW5nWzBdXG4gICAgICAgIH07XG4gICAgICBjYXNlICdVUERBVEVfTUFOWSc6XG4gICAgICAgIGNvbnN0IHVwZGF0ZWRJZHMgPSByZXNwb25zZS5yZXR1cm5pbmcubWFwKChpdGVtKSA9PiB7XG4gICAgICAgICAgcmV0dXJuIGl0ZW0uaWQ7XG4gICAgICAgIH0pO1xuXG4gICAgICAgIHJldHVybiB7XG4gICAgICAgICAgZGF0YTogdXBkYXRlZElkc1xuICAgICAgICB9O1xuICAgICAgY2FzZSAnREVMRVRFJzpcbiAgICAgICAgcmV0dXJuIHtcbiAgICAgICAgICBkYXRhOiByZXNwb25zZS5yZXR1cm5pbmdbMF1cbiAgICAgICAgfTtcbiAgICAgIGNhc2UgJ0RFTEVURV9NQU5ZJzpcbiAgICAgICAgY29uc3QgZGVsZXRlZElkcyA9IHJlc3BvbnNlLnJldHVybmluZy5tYXAoKGl0ZW0pID0+IHtcbiAgICAgICAgICByZXR1cm4gaXRlbS5pZDtcbiAgICAgICAgfSk7XG5cbiAgICAgICAgcmV0dXJuIHtcbiAgICAgICAgICBkYXRhOiBkZWxldGVkSWRzXG4gICAgICAgIH07XG4gICAgICBjYXNlICdHRVRfTUFOWSc6XG4gICAgICAgIHJldHVybiB7XG4gICAgICAgICAgZGF0YTogcmVzcG9uc2VcbiAgICAgICAgfTtcbiAgICAgIGNhc2UgJ0dFVF9NQU5ZX1JFRkVSRU5DRSc6XG4gICAgICAgIHJldHVybiB7XG4gICAgICAgICAgZGF0YTogcmVzcG9uc2VbMF0sXG4gICAgICAgICAgdG90YWw6IHJlc3BvbnNlWzFdLmNvdW50XG4gICAgICAgIH07XG4gICAgICBkZWZhdWx0OlxuICAgICAgICByZXR1cm4geyBkYXRhOiByZXNwb25zZSB9O1xuICAgIH1cbiAgfTtcblxuICByZXR1cm4gKHR5cGUsIHJlc291cmNlLCBwYXJhbXMpID0+IHtcbiAgICBjb25zdCB7IG9wdGlvbnMgfSA9IGNvbnZlcnREYXRhUmVxdWVzdFRvSFRUUChcbiAgICAgIHR5cGUsXG4gICAgICByZXNvdXJjZSxcbiAgICAgIHBhcmFtc1xuICAgICk7XG5cbiAgICBvcHRpb25zLm1ldGhvZCA9ICdQT1NUJztcbiAgICBvcHRpb25zLmhlYWRlcnMgPSBoZWFkZXJzO1xuICAgIHJldHVybiBmZXRjaChzZXJ2ZXJFbmRwb2ludCArICcvdjEvcXVlcnknLCBvcHRpb25zKS50aGVuKGZ1bmN0aW9uIChyZXNwb25zZSkge1xuICAgICAgcmV0dXJuIHJlc3BvbnNlLmpzb24oKS50aGVuKChkYXRhKSA9PiB7XG4gICAgICAgIHJldHVybiBjb252ZXJ0SFRUUFJlc3BvbnNlKGRhdGEsIHR5cGUsIHJlc291cmNlLCBwYXJhbXMpO1xuICAgICAgfSk7XG4gICAgfSk7XG4gIH07XG59O1xuIiwiLy8gZGVmaW5lIGhhc3VyYSBqc29uIGFwaSBxdWVyaWVzXG5jb25zdCBidWxrUXVlcnkgPSB7XG4gIHR5cGU6ICdidWxrJyxcbiAgYXJnczogW11cbn07XG5cbmNvbnN0IHNlbGVjdFF1ZXJ5ID0ge1xuICB0eXBlOiAnc2VsZWN0JyxcbiAgYXJnczoge1xuICAgIHRhYmxlOiAnJyxcbiAgICBjb2x1bW5zOiBbJyonXVxuICB9XG59O1xuXG5jb25zdCBjb3VudFF1ZXJ5ID0ge1xuICB0eXBlOiAnY291bnQnLFxuICBhcmdzOiB7XG4gICAgdGFibGU6ICcnLFxuICAgIHdoZXJlOiB7IGlkOiB7ICckZ3QnOiAwIH19XG4gIH1cbn07XG5cbmNvbnN0IGluc2VydFF1ZXJ5ID0ge1xuICB0eXBlOiAnaW5zZXJ0JyxcbiAgYXJnczoge1xuICAgIHRhYmxlOiAnJyxcbiAgICBvYmplY3RzOiBbXSxcbiAgICByZXR1cm5pbmc6IFtdXG4gIH1cbn07XG5cbmNvbnN0IHVwZGF0ZVF1ZXJ5ID0ge1xuICB0eXBlOiAndXBkYXRlJyxcbiAgYXJnczoge1xuICAgIHRhYmxlOiAnJyxcbiAgICAkc2V0OiB7fSxcbiAgICB3aGVyZToge30sXG4gICAgcmV0dXJuaW5nOiBbXVxuICB9XG59O1xuXG5jb25zdCBkZWxldGVRdWVyeSA9IHtcbiAgdHlwZTogJ2RlbGV0ZScsXG4gIGFyZ3M6IHtcbiAgICB0YWJsZTogJycsXG4gICAgd2hlcmU6IHt9LFxuICAgIHJldHVybmluZzogW11cbiAgfVxufTtcblxuZXhwb3J0IHsgYnVsa1F1ZXJ5LCBzZWxlY3RRdWVyeSwgY291bnRRdWVyeSwgaW5zZXJ0UXVlcnksIHVwZGF0ZVF1ZXJ5LCBkZWxldGVRdWVyeSB9O1xuXG4iXSwic291cmNlUm9vdCI6IiJ9