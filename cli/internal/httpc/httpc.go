package httpc

import (
	"bytes"
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"path/filepath"
	"strings"
	"sync"

	"github.com/pkg/errors"
)

type Client struct {
	client *http.Client

	Mutex     sync.Mutex
	BaseURL   *url.URL
	UserAgent string
	headers   map[string]string
}

func New(httpClient *http.Client, baseUrl string, headers map[string]string) (*Client, error) {
	u, err := url.ParseRequestURI(baseUrl)
	if err != nil {
		return nil, err
	}
	if httpClient == nil {
		httpClient = new(http.Client)
	}
	client := &Client{
		client:    httpClient,
		BaseURL:   u,
		UserAgent: "hasura-cli",
		headers:   headers,
	}
	return client, nil
}

func (c *Client) SetHeaders(headers map[string]string) {
	c.headers = headers
}

func (c *Client) NewRequest(method, urlStr string, body interface{}) (*http.Request, error) {
	u, err := c.BaseURL.Parse(urlStr)
	if err != nil {
		return nil, err
	}

	var buf io.ReadWriter
	if body != nil {
		buf = &bytes.Buffer{}
		enc := json.NewEncoder(buf)
		enc.SetEscapeHTML(false)
		err := enc.Encode(body)
		if err != nil {
			return nil, err
		}
	}

	req, err := http.NewRequest(method, u.String(), buf)
	if err != nil {
		return nil, err
	}

	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	for k, v := range c.headers {
		req.Header.Set(k, v)
	}
	return req, nil
}

func (c *Client) BareDo(ctx context.Context, req *http.Request) (*Response, error) {
	if ctx == nil {
		return nil, errors.New("context must be non-nil")
	}
	req = req.WithContext(ctx)

	resp, err := c.client.Do(req)
	if err != nil {
		// If we got an error, and the context has been canceled,
		// the context's error is probably more useful.
		select {
		case <-ctx.Done():
			return nil, ctx.Err()
		default:
		}
		return nil, err
	}

	response := &Response{resp}

	return response, err
}

type Response struct {
	*http.Response
}

func (c *Client) LockAndDo(ctx context.Context, req *http.Request, v interface{}) (*Response, error) {
	c.Mutex.Lock()
	defer c.Mutex.Unlock()
	return c.Do(ctx, req, v)
}

func hasJSONContentType(headers http.Header) bool {
	const jsonHeaderName = "application/json"
	if strings.Contains(headers.Get("Content-Type"), jsonHeaderName) || strings.Contains(headers.Get("content-type"), jsonHeaderName) {
		return true
	}
	return false
}

func (c *Client) Do(ctx context.Context, req *http.Request, v interface{}) (*Response, error) {
	resp, err := c.BareDo(ctx, req)
	if err != nil {
		return resp, err
	}
	defer resp.Body.Close()
	switch v := v.(type) {
	case nil:
	case io.Writer:
		if hasJSONContentType(resp.Header) {
			// indent json response
			var respBodyBytes []byte
			respBodyBytes, err = ioutil.ReadAll(resp.Body)
			if err != nil {
				return resp, err
			}
			var buf bytes.Buffer
			err = json.Indent(&buf, respBodyBytes, "", "  ")
			if err != nil {
				return resp, err
			}
			// copy it to writer
			_, err = io.Copy(v, &buf)
		} else {
			_, err = io.Copy(v, resp.Body)
		}
	default:
		decErr := json.NewDecoder(resp.Body).Decode(v)
		if decErr == io.EOF {
			decErr = nil // ignore EOF errors caused by empty response body
		}
		if decErr != nil {
			err = decErr
		}
	}
	return resp, err
}

func GenerateTLSConfig(caPath string, insecureSkipTLSVerify bool) (*tls.Config, error) {
	tlsConfig := &tls.Config{InsecureSkipVerify: insecureSkipTLSVerify}
	if caPath != "" {
		// Get the SystemCertPool, continue with an empty pool on error
		rootCAs, _ := x509.SystemCertPool()
		if rootCAs == nil {
			rootCAs = x509.NewCertPool()
		}
		// read cert
		certPath, _ := filepath.Abs(caPath)
		cert, err := ioutil.ReadFile(certPath)
		if err != nil {
			return nil, fmt.Errorf("error reading CA %s: %w", caPath, err)
		}
		if ok := rootCAs.AppendCertsFromPEM(cert); !ok {
			return nil, fmt.Errorf("unable to append given CA cert")
		}
		tlsConfig.RootCAs = rootCAs
	}
	return tlsConfig, nil
}

func NewHttpClientWithTLSConfig(tlsConfig *tls.Config) (*http.Client, error) {
	tr := &http.Transport{TLSClientConfig: tlsConfig}
	tr.Proxy = http.ProxyFromEnvironment
	httpClient := &http.Client{Transport: tr}
	return httpClient, nil
}
