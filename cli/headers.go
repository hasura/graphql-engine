package cli

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

// ParseHGEHeaders parses the repeatable `--header "Name: value"` flag inputs into
// a map of header name to value, suitable for ExecutionContext.HGEHeaders.
//
// These headers are transported, as-is, on every HTTP request the CLI makes to the
// Hasura GraphQL Engine (including the migrate and metadata operations and the
// preflight version/status/config probes). A common use case is supplying a custom
// authentication header, e.g. `--header "Authorization: Bearer <JWT>"`, instead of
// an admin secret.
//
// Parsing rules:
//
//   - The name and value are separated by the first ":" only. Everything after it
//     (minus optional surrounding whitespace) is the value, so values may themselves
//     contain ":" and "," characters (for example Bearer tokens or URLs).
//   - Only optional leading/trailing spaces and horizontal tabs (the "OWS" rule in
//     RFC 7230 §3.2) are trimmed from the name and value. Carriage returns, line
//     feeds and other control characters are NOT treated as trimmable whitespace, so
//     they survive trimming and are then rejected by validation (preventing a trailing
//     "\r\n" from being silently stripped). The value itself may not be empty.
//   - Header names are validated as RFC 7230 "tokens". Names or values containing
//     CR, LF or other control characters are rejected to prevent header (CRLF)
//     injection.
//   - Duplicate header names (compared case-insensitively, as HTTP header names are
//     case-insensitive) are rejected rather than silently collapsed, so the request
//     the server receives is unambiguous.
//
// Error messages intentionally never include header values so that secrets such as
// JWTs are not leaked into terminal output, logs or telemetry.
func ParseHGEHeaders(rawHeaders []string) (map[string]string, error) {
	var op errors.Op = "cli.ParseHGEHeaders"

	if len(rawHeaders) == 0 {
		return nil, nil
	}

	headers := make(map[string]string, len(rawHeaders))
	// seen maps the lower-cased header name to the canonical spelling first seen, so
	// that case-insensitive conflicts can be reported with both spellings.
	seen := make(map[string]string, len(rawHeaders))

	for i, raw := range rawHeaders {
		rawName, rawValue, found := strings.Cut(raw, ":")
		if !found {
			// Reference the input by position, never by content: a malformed value
			// without a colon could itself be a pasted secret.
			return nil, errors.E(op, fmt.Errorf(
				`invalid --header at position %d: expected format "Name: value"`, i+1))
		}

		// Trim only RFC 7230 OWS (space and horizontal tab). Using strings.TrimSpace
		// here would also strip CR/LF (they are Unicode whitespace), silently accepting
		// an injected trailing "\r\n"; by trimming only " \t" any CR/LF or other control
		// character survives and is rejected by the validation below.
		const ows = " \t"

		name := strings.Trim(rawName, ows)
		value := strings.Trim(rawValue, ows)

		if name == "" {
			return nil, errors.E(op, fmt.Errorf(
				"invalid --header at position %d: header name cannot be empty", i+1))
		}

		if !isValidHeaderName(name) {
			return nil, errors.E(op, fmt.Errorf(
				"invalid --header at position %d: header name contains disallowed characters", i+1))
		}

		if value == "" {
			// The header name is safe to surface; it is not a secret.
			return nil, errors.E(op, fmt.Errorf(
				"invalid value for --header %q: value cannot be empty", name))
		}

		if !isValidHeaderValue(value) {
			return nil, errors.E(op, fmt.Errorf(
				"invalid value for --header %q: value contains disallowed characters", name))
		}

		key := strings.ToLower(name)
		if existing, ok := seen[key]; ok {
			return nil, errors.E(op, fmt.Errorf(
				"duplicate --header %q (conflicts with %q): each header may only be specified once",
				name, existing))
		}

		seen[key] = name
		headers[name] = value
	}

	return headers, nil
}

// isValidHeaderName reports whether name is a valid HTTP header field name, i.e. an
// RFC 7230 "token" (1*tchar).
func isValidHeaderName(name string) bool {
	for i := range len(name) {
		if !isTokenChar(name[i]) {
			return false
		}
	}

	return true
}

// isTokenChar reports whether c is a valid RFC 7230 "tchar".
func isTokenChar(c byte) bool {
	switch c {
	case '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~':
		return true
	}

	return (c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		(c >= '0' && c <= '9')
}

// isValidHeaderValue reports whether value is a valid HTTP header field value:
// horizontal tabs and any byte >= 0x20 except the DEL character (0x7f). This rejects
// CR, LF and other control characters, preventing header/CRLF injection.
func isValidHeaderValue(value string) bool {
	for i := range len(value) {
		c := value[i]
		if c == '\t' {
			continue
		}

		if c < 0x20 || c == 0x7f {
			return false
		}
	}

	return true
}
