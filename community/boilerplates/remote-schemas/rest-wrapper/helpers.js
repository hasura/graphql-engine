const { ApolloError } = require('apollo-server');
const fetch = require('node-fetch');

const getData = async url => {
    try {
        const res = await fetch(url);
        const json = await res.json();
        if(isHTTPError(res.status)) {
            throw new ApolloError(json, "http-status-error", {statusCode: res.status, error: json});
        }
        console.log(json);
        return json;
    } catch (error) {
        console.log(JSON.stringify(error));
        throw error;
    }
};

const postData = async (url, body) => {
    try {
        const res = await fetch(url, {
            method: 'post',
            body:    JSON.stringify(body),
            headers: { 'Content-Type': 'application/json' },
        });
        const json = await res.json();
        if(isHTTPError(res.status)) {
            throw new ApolloError(json, "http-status-error", {statusCode: res.status, error: json});
        }
        console.log(json);
        return json;
    } catch (error) {
        console.log(JSON.stringify(error));
        throw error;
    }
};

const isHTTPError = status => {
    return !((status >= 200) && (status < 300));
};

exports.getData = getData;
exports.postData = postData;

