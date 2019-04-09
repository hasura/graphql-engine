package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"regexp"
	"strings"
)

var frontMatter = []string{
	"SET statement_timeout = 0;",
	"SET lock_timeout = 0;",
	"SET idle_in_transaction_session_timeout = 0;",
	"SET client_encoding = 'UTF8';",
	"SET standard_conforming_strings = on;",
	"SELECT pg_catalog.set_config('search_path', '', false);",
	"SET check_function_bodies = false;",
	"SET client_min_messages = warning;",
	"SET row_security = off;",
	"SET default_tablespace = '';",
	"SET default_with_oids = false;",
	"CREATE SCHEMA public;",
}

const helpStr = `POST the SQL file contents to this URL:

curl --data-binary @filename.sql https://hasura-edit-pg-dump.now.sh > newfile.sql

Source code can be found at https://github.com/hasura/graphql-engine/tree/master/scripts/edit-pg-dump`

// Handler is the now.sh serverless handler
func Handler(w http.ResponseWriter, r *http.Request) {

	// if method is not POST, respond with help message
	if r.Method != "POST" {
		fmt.Fprintf(w, helpStr)
		return
	}

	// build the regular expression for matching empty newlines
	emptyNewlineRe, err := regexp.Compile(`(?m)^\s*$[\r\n]*`)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	// build the regular expression for matching SQL comments
	commentsRe, err := regexp.Compile(`(?m)^--.*$`)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	// build the regular expression for matching triggers created by Hasura
	triggerRe, err := regexp.Compile(`(?m)^CREATE TRIGGER "?notify_hasura_.+"? AFTER \w+ ON .+ FOR EACH ROW EXECUTE PROCEDURE "?hdb_views"?\."?notify_hasura_.+"?\(\);$`)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	// read the POST body
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	// convert bytes to string
	bodyStr := string(body)

	// remove the SQL front matter
	for _, l := range frontMatter {
		bodyStr = strings.Replace(bodyStr, l, "", 1)
	}

	// replace the regex matches
	bodyStr = commentsRe.ReplaceAllLiteralString(bodyStr, "")
	bodyStr = triggerRe.ReplaceAllLiteralString(bodyStr, "")
	bodyStr = emptyNewlineRe.ReplaceAllLiteralString(bodyStr, "\n")

	// prefix a message
	bodyStr = "--\r\n-- SQL edited by https://hasura-edit-pg-dump.now.sh\r\n--\r\n" + bodyStr

	// respond with the edited string
	fmt.Fprintf(w, bodyStr)
}
