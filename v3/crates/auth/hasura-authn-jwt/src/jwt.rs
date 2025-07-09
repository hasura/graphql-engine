use std::collections::HashMap;
use std::time::Duration;

use axum::http::{HeaderMap, HeaderValue};
use cookie::{self, Cookie};
use hasura_authn_core::{JsonSessionVariableValue, Role};
use jsonptr::Pointer;
use jsonwebtoken::{self as jwt, DecodingKey, Validation, decode};
use jwt::decode_header;
use open_dds::session_variables::SessionVariableName;
use reqwest::StatusCode;
use reqwest::header::{AUTHORIZATION, COOKIE};
use schemars::r#gen::SchemaGenerator;
use schemars::schema::{Schema, SchemaObject};

use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Error as SerdeDeError};
use serde_json::{Value, json};
use std::collections::HashSet;
use tracing_util::{ErrorVisibility, SpanVisibility, TraceableError};
use url::Url;

/// Name of the key, which is by default used to lookup the Hasura claims
/// in the claims obtained after decoding the JWT.
pub(crate) const DEFAULT_HASURA_CLAIMS_NAMESPACE: &str = "claims.jwt.hasura.io";

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error decoding the `Authorization` header - {0}")]
    ErrorDecodingAuthorizationHeader(jwt::errors::Error),
    #[error("`kid` (Key ID) header claim not found in the header")]
    KidHeaderNotFound,
    #[error("Expected the Hasura claims to be a String when `claimsFormat` is `stringifiedJson`")]
    ExpectedStringifiedJson,
    #[error("The default role is not present in the allowed roles")]
    DisallowedDefaultRole,
    #[error("Error while parsing the claims map entry: {claim_name} - {err}")]
    ParseClaimsMapEntryError {
        claim_name: String,
        err: serde_json::Error,
    },
    #[error("Expected string value for claim {claim_name}")]
    ClaimMustBeAString { claim_name: String },
    #[error("Required claim {claim_name} not found")]
    RequiredClaimNotFound { claim_name: String },
    #[error("JWT Authorization token source: Header name {header_name} not found.")]
    AuthorizationHeaderSourceNotFound { header_name: String },
    #[error("JWT Authorization token source: Cookie header not found")]
    CookieNotFound,
    #[error(
        "JWT Authorization token source: cookie name {cookie_name} not found in the Cookie header"
    )]
    CookieNameNotFound { cookie_name: String },
    #[error("Error in parsing the {header_name} header: {err}")]
    AuthorizationHeaderParseError { err: String, header_name: String },
    #[error("Error in parsing the Cookie header value: {err}")]
    CookieParseError { err: cookie::ParseError },
    #[error("Missing corresponding value for the cookie with cookie name: {cookie_name}")]
    MissingCookieValue { cookie_name: String },
    #[error("JWT validation error: {0}")]
    JWTValidationError(jwt::errors::Error),
    #[error("Internal Error - {0}")]
    Internal(#[from] InternalError),
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        // For the purpose of traces, all JWT errors should be developer facing.
        ErrorVisibility::User
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InternalError {
    #[error("Error while constructing the JWT decoding key: {0}")]
    JWTDecodingKeyError(jwt::errors::Error),
    #[error("Error while decoding the JWT: {0}")]
    JWTDecodingError(jwt::errors::Error),
    #[error("Hasura claims not found in the JWT at {path:}")]
    HasuraClaimsNotFound { path: String },
    #[error("Hasura claims is expected to be a JSON object value.")]
    HasuraClaimsShouldBeAnObject,
    #[error("Error in parsing the Hasura claims map: {0}")]
    JSONParseError(serde_json::Error),
    #[error("Duplicate allowed role found in the JWT claims")]
    DuplicateAllowedRoleFound,
    #[error("Internal request error: {0}")]
    ReqwestError(reqwest::Error),
    #[error("Error while fetching the JWKSet: {0}")]
    ErrorFetchingJWKSet(reqwest::Error),
    #[error("No matching JWK found for the given kid: {kid}")]
    NoMatchingJWKFound { kid: String },
    #[error("Received unsuccessful response {0} status while fetching JWK ")]
    UnsuccessfulJWKFetch(StatusCode),
    #[error("Algorithm not found in the JWK")]
    AlgorithmNotFoundInJWK,
}

impl Error {
    pub fn to_status_code(&self) -> StatusCode {
        match self {
            Error::Internal(_e) => StatusCode::INTERNAL_SERVER_ERROR,
            Error::ErrorDecodingAuthorizationHeader(_)
            | Error::KidHeaderNotFound
            | Error::ExpectedStringifiedJson
            | Error::DisallowedDefaultRole
            | Error::ParseClaimsMapEntryError {
                claim_name: _,
                err: _,
            }
            | Error::RequiredClaimNotFound { claim_name: _ }
            | Error::AuthorizationHeaderSourceNotFound { header_name: _ }
            | Error::CookieNotFound
            | Error::CookieNameNotFound { cookie_name: _ }
            | Error::AuthorizationHeaderParseError {
                err: _,
                header_name: _,
            }
            | Error::CookieParseError { err: _ }
            | Error::MissingCookieValue { cookie_name: _ }
            | Error::ClaimMustBeAString { claim_name: _ }
            | Error::JWTValidationError(_) => StatusCode::BAD_REQUEST,
        }
    }

    pub fn into_middleware_error(self) -> engine_types::MiddlewareError {
        let is_internal = match self {
            Error::Internal(_) => true,
            Error::ErrorDecodingAuthorizationHeader(_)
            | Error::KidHeaderNotFound
            | Error::ExpectedStringifiedJson
            | Error::DisallowedDefaultRole
            | Error::ParseClaimsMapEntryError {
                claim_name: _,
                err: _,
            }
            | Error::RequiredClaimNotFound { claim_name: _ }
            | Error::AuthorizationHeaderSourceNotFound { header_name: _ }
            | Error::CookieNotFound
            | Error::CookieNameNotFound { cookie_name: _ }
            | Error::AuthorizationHeaderParseError {
                err: _,
                header_name: _,
            }
            | Error::CookieParseError { err: _ }
            | Error::MissingCookieValue { cookie_name: _ }
            | Error::ClaimMustBeAString { claim_name: _ }
            | Error::JWTValidationError(_) => false,
        };
        engine_types::MiddlewareError {
            status: self.to_status_code(),
            message: self.to_string(),
            is_internal,
        }
    }
}

// Note: This enum is defined only for the `JsonSchema` implementation.
// Because the `jsonwebtoken::Algorithm` type doesn't have a `JsonSchema`
// implementation.
#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(remote = "jwt::Algorithm")]
#[schemars(title = "JWTAlgorithm")]
/// The algorithm used to decode the JWT.
enum JWTAlgorithm {
    /// HMAC using SHA-256
    HS256,
    /// HMAC using SHA-384
    HS384,
    /// HMAC using SHA-512
    HS512,

    /// ECDSA using SHA-256
    ES256,
    /// ECDSA using SHA-384
    ES384,

    /// RSASSA-PKCS1-v1_5 using SHA-256
    RS256,
    /// RSASSA-PKCS1-v1_5 using SHA-384
    RS384,
    /// RSASSA-PKCS1-v1_5 using SHA-512
    RS512,

    /// RSASSA-PSS using SHA-256
    PS256,
    /// RSASSA-PSS using SHA-384
    PS384,
    /// RSASSA-PSS using SHA-512
    PS512,

    /// Edwards-curve Digital Signature Algorithm (EdDSA)
    EdDSA,
}

fn serialize_url<S>(url: &Url, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.serialize_str(url.as_str())
}

fn deserialize_url<'de, D>(deserializer: D) -> Result<Url, D::Error>
where
    D: Deserializer<'de>,
{
    let buf = String::deserialize(deserializer)?;

    Url::parse(&buf).map_err(SerdeDeError::custom)
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "JWTKey")]
/// JWT key configuration according to which the incoming JWT will be decoded.
pub enum JWTKey {
    /// JWT mode when the algorithm `type` and `key`
    /// is known
    Fixed(JWTKeyConfig),
    /// JWT mode where the `type` and `key` parameters
    /// are obtained dynamically through JWK.
    #[serde(serialize_with = "serialize_url", deserialize_with = "deserialize_url")]
    JwkFromUrl(Url),
}

#[derive(Serialize, Deserialize, Default, PartialEq, Clone, JsonSchema, Debug)]
#[schemars(title = "JWTClaimsFormat")]
pub enum JWTClaimsFormat {
    /// Claims will be in the JSON format.
    #[default]
    Json,
    /// Claims will be in the Stringified JSON format.
    StringifiedJson,
}

fn json_pointer_schema(generator: &mut SchemaGenerator) -> Schema {
    let mut schema: SchemaObject = <String>::json_schema(generator).into();
    schema.format = Some("JSON pointer".to_string());
    schema.into()
}

#[derive(Serialize, Deserialize, PartialEq, Clone, JsonSchema, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "JWTClaimsMappingPathEntry")]
/// Entry to lookup the Hasura claims at the specified JSON Pointer
pub struct JWTClaimsMappingPathEntry<T> {
    /// JSON pointer to find the particular claim in the decoded
    /// JWT token.
    #[schemars(schema_with = "json_pointer_schema")]
    path: Pointer,
    /// Default value to be used when no value is found when
    /// looking up the value using the `path`.
    default: Option<T>,
}

#[derive(Serialize, Deserialize, PartialEq, Clone, JsonSchema, Debug)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "JWTClaimsMappingEntry")]
pub enum JWTClaimsMappingEntry<T> {
    /// Literal value of the claims mapping
    Literal(T),
    /// Look up the Hasura claims at the specified JSON Pointer
    /// and provide a default value if the lookup fails.
    Path(JWTClaimsMappingPathEntry<T>),
}

#[derive(Serialize, Deserialize, PartialEq, Clone, JsonSchema, Debug)]
#[schemars(title = "JWTClaimsMap")]
// This causes the flattened custom_claims to be included in the JSON schema (see https://github.com/GREsau/schemars/issues/259)
// It is only required in v0.8 and is apparently fixed in v1.0 (which is in alpha at time of writing)
#[schemars(deny_unknown_fields)]
/// Can be used when Hasura claims are not all present in the single object, but individual
/// claims are provided a JSON pointer within the decoded JWT and optionally a default value.
pub struct JWTClaimsMap {
    #[serde(rename = "x-hasura-default-role")]
    /// JSON pointer to lookup the default role within the decoded JWT.
    pub default_role: JWTClaimsMappingEntry<Role>,
    #[serde(rename = "x-hasura-allowed-roles")]
    /// JSON pointer to lookup the allowed roles within the decoded JWT.
    pub allowed_roles: JWTClaimsMappingEntry<Vec<Role>>,
    #[serde(flatten)]
    /// A dictionary of the custom claims, where the key is the name of the claim and the value
    /// is the JSON pointer to lookup the custom claims within the decoded JWT.
    pub custom_claims:
        HashMap<SessionVariableName, JWTClaimsMappingEntry<JsonSessionVariableValue>>,
}

#[derive(Serialize, Deserialize, PartialEq, Clone, JsonSchema, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "JWTClaimsNamespace")]
/// Used when all of the Hasura claims are present in a single object within the decoded JWT.
pub struct JWTClaimsNamespace {
    /// Format in which the Hasura claims will be present.
    pub claims_format: JWTClaimsFormat,
    /// Pointer to lookup the Hasura claims within the decoded claims.
    #[schemars(schema_with = "json_pointer_schema")]
    pub location: Pointer,
}

#[derive(Serialize, Deserialize, PartialEq, Clone, JsonSchema, Debug)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "JWTClaimsConfig")]
/// Config to describe how/where the engine should look for the claims
/// within the decoded token.
pub enum JWTClaimsConfig {
    Locations(JWTClaimsMap),
    Namespace(JWTClaimsNamespace),
}

#[derive(Serialize, Deserialize, PartialEq, Clone, JsonSchema, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "JWTCookieLocation")]
pub struct JWTCookieLocation {
    name: String,
}

#[derive(Serialize, Deserialize, PartialEq, Clone, JsonSchema, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "JWTHeaderLocation")]
pub struct JWTHeaderLocation {
    name: String,
}

/// Source of the Authorization token
#[derive(Serialize, Deserialize, PartialEq, Clone, JsonSchema, Debug)]
#[serde(tag = "type")]
#[serde(deny_unknown_fields)]
#[schemars(title = "JWTTokenLocation")]
pub enum JWTTokenLocation {
    /// Get the bearer token from the `Authorization` header.
    #[schemars(title = "JWTBearerAuthorizationLocation")]
    BearerAuthorization,
    /// Get the token from the Cookie header under the specified cookie name.
    Cookie(JWTCookieLocation),
    /// Custom header from where the header should be parsed from.
    Header(JWTHeaderLocation),
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "JWTConfig")]
#[schemars(example = "JWTConfig::example")]
/// JWT config according to which the incoming JWT will be verified and decoded
/// to extract the session variable claims.
pub struct JWTConfig {
    /// Optional validation to check that the `aud` field is a member
    /// of the `audience` received, otherwise will throw error.
    pub audience: Option<HashSet<String>>,
    /// Optional validation to check that the `iss` field is
    /// a member of the `iss` received, otherwise will throw error.
    pub issuer: Option<String>,
    /// Allowed leeway (in seconds) to the `exp` validation
    /// to account for clock skew.
    pub allowed_skew: Option<u64>,
    /// Claims config. Either specified via `claims_mappings` or `claims_namespace_path`
    pub claims_config: JWTClaimsConfig,
    /// Source of the JWT authentication token.
    pub token_location: JWTTokenLocation,
    /// Mode according to which the JWT auth is configured.
    pub key: JWTKey,
}

impl JWTConfig {
    fn example() -> Self {
        let value = json!(
            {
                "key": {
                    "fixed": {
                        "algorithm": "HS256",
                        "key": {
                            "value": "token"
                        }
                    }
                },
                "tokenLocation": {
                    "type": "BearerAuthorization"
                },
                "claimsConfig": {
                    "namespace": {
                        "claimsFormat": "Json",
                        "location": jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]),
                    }
                }
            }
        );
        serde_json::from_value(value).unwrap()
    }
}

/// JWT Secret config according to which the
/// incoming JWT will be decoded.
#[derive(Debug, Serialize, Deserialize, PartialEq, Clone, JsonSchema)]
#[schemars(title = "JWTKeyConfig")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
pub struct JWTKeyConfig {
    #[serde(with = "JWTAlgorithm")]
    /// The algorithm used to decode the JWT.
    pub algorithm: jwt::Algorithm,
    /// The key to use for decoding the JWT.
    pub key: open_dds::EnvironmentValue,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct HasuraClaims {
    /// The role that will be used when the `x-hasura-role`
    /// is not passed by the user.
    #[serde(rename = "x-hasura-default-role")]
    pub default_role: Role,
    /// List of the acceptable values of the `x-hasura-role`
    /// value.
    #[serde(rename = "x-hasura-allowed-roles")]
    pub allowed_roles: Vec<Role>,
    /// Any other session variables, that are required
    /// as per the user's defined permissions.
    /// For example, things like `x-hasura-user-id` can go here.
    #[serde(flatten)]
    pub custom_claims: HashMap<SessionVariableName, JsonSessionVariableValue>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Claims {
    /// Subject of the JWT.
    pub sub: Option<String>,
    /// Issue of the JWT.
    pub iss: Option<String>,
    /// Audience, recipient for which the JWT is intended.
    pub aud: Option<String>,
    /// Expiration time, time (in seconds) after which the JWT expires.
    pub exp: u64,
    /// Issued at time, time at which the JWT was issued;
    /// can be used to determine age of the JWT.
    pub iat: u64,
    /// Parse unrecognized fields in this HashMap. Primarily, we
    /// have to parse the `HasuraClaims` from this.
    #[serde(flatten)]
    pub unrecognized_claims: HashMap<String, Value>,
}

/// Gets the decoding key according to the decrypting algorithm
fn get_decoding_key(secret_config: &JWTKeyConfig) -> Result<DecodingKey, Error> {
    let secret_key = secret_config.key.value.as_bytes();
    match secret_config.algorithm {
        jwt::Algorithm::HS256 | jwt::Algorithm::HS384 | jwt::Algorithm::HS512 => {
            Ok(jwt::DecodingKey::from_secret(secret_key))
        }
        jwt::Algorithm::ES256 | jwt::Algorithm::ES384 => jwt::DecodingKey::from_ec_pem(secret_key),
        jwt::Algorithm::RS256
        | jwt::Algorithm::RS384
        | jwt::Algorithm::RS512
        | jwt::Algorithm::PS256
        | jwt::Algorithm::PS384
        | jwt::Algorithm::PS512 => jwt::DecodingKey::from_rsa_pem(secret_key),
        jwt::Algorithm::EdDSA => jwt::DecodingKey::from_ed_pem(secret_key),
    }
    .map_err(|e| Error::Internal(InternalError::JWTDecodingKeyError(e)))
}

async fn get_decoding_key_from_jwk_url(
    http_client: &reqwest::Client,
    jwk_url: &Url,
    jwt_authorization_header: &str,
) -> Result<(Vec<jwt::Algorithm>, jwt::DecodingKey), Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async("fetch_jwk", "Fetch JWK", SpanVisibility::Internal, || {
            Box::pin(async {
                let jwk_request = http_client
                    .get(jwk_url.clone())
                    .headers(tracing_util::get_trace_headers())
                    .timeout(Duration::from_secs(60))
                    .build()
                    .map_err(InternalError::ReqwestError)?;

                let jwk_response = http_client
                    .execute(jwk_request)
                    .await
                    .map_err(InternalError::ErrorFetchingJWKSet)?;
                if jwk_response.status().is_success() {
                    let jwk_set: jwt::jwk::JwkSet = jwk_response
                        .json()
                        .await
                        .map_err(InternalError::ReqwestError)?;
                    let decoded_header = decode_header(jwt_authorization_header)
                        .map_err(Error::ErrorDecodingAuthorizationHeader)?;
                    let kid = decoded_header.kid.ok_or(Error::KidHeaderNotFound)?;
                    let jwk = jwk_set
                        .find(kid.as_str())
                        .ok_or(InternalError::NoMatchingJWKFound { kid })?;
                    let decoding_key = jwt::DecodingKey::from_jwk(jwk)
                        .map_err(InternalError::JWTDecodingKeyError)?;
                    let acceptable_algorithms = get_acceptable_algorithms_for_key(jwk);
                    Ok((acceptable_algorithms, decoding_key))
                } else {
                    Err(InternalError::UnsuccessfulJWKFetch(jwk_response.status()))?
                }
            })
        })
        .await
}

fn get_acceptable_algorithms_for_key(jwk: &jwt::jwk::Jwk) -> Vec<jwt::Algorithm> {
    match &jwk.algorithm {
        // Elliptic curve family algorithms
        jsonwebtoken::jwk::AlgorithmParameters::EllipticCurve(_) => {
            vec![jwt::Algorithm::ES256, jwt::Algorithm::ES384]
        }
        // RSA family algorithms
        jsonwebtoken::jwk::AlgorithmParameters::RSA(_) => vec![
            jwt::Algorithm::RS256,
            jwt::Algorithm::RS384,
            jwt::Algorithm::RS512,
            jwt::Algorithm::PS256,
            jwt::Algorithm::PS384,
            jwt::Algorithm::PS512,
        ],
        // HMAC family algorithms
        jsonwebtoken::jwk::AlgorithmParameters::OctetKey(_) => vec![
            jwt::Algorithm::HS256,
            jwt::Algorithm::HS384,
            jwt::Algorithm::HS512,
        ],
        // Edwards-curve algorithms
        jsonwebtoken::jwk::AlgorithmParameters::OctetKeyPair(_) => vec![jwt::Algorithm::EdDSA],
    }
}

fn get_claims_mapping_entry_value<T: for<'de> serde::Deserialize<'de> + Clone>(
    claim_name: String,
    claims_mapping_entry: &JWTClaimsMappingEntry<T>,
    json_value: &serde_json::Value,
) -> Result<Option<T>, Error> {
    match claims_mapping_entry {
        JWTClaimsMappingEntry::Literal(literal_value) => Ok(Some(literal_value.clone())),
        JWTClaimsMappingEntry::Path(JWTClaimsMappingPathEntry { path, default }) => {
            Ok(match json_value.pointer(path) {
                Some(v) => Some(
                    serde_json::from_value(v.clone())
                        .map_err(|e| Error::ParseClaimsMapEntryError { claim_name, err: e })?,
                ),
                None => default.clone(),
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AudienceValidationMode {
    /// If the JWT contains an aud claim, it must be validated against the audience configured in the AuthConfig.
    /// If one is not configured, the JWT will be rejected.
    Required,

    /// If the JWT contains an aud claim, it will be validated against the audience configured in the AuthConfig,
    /// but only if one is configured. This is a serious security issue, but this behaviour is retained for
    /// backwards compatibility and is disabled via the OpenDDFlag `RequireJwtAudienceValidationIfAudClaimPresent`
    Optional,
}

/// Determines whether a JWT error should be treated as a client error (4xx) or server error (5xx)
#[allow(clippy::match_same_arms)]
fn categorize_jwt_error(jwt_error: jwt::errors::Error) -> Error {
    use jwt::errors::ErrorKind;

    match jwt_error.kind() {
        // Client errors - issues with the token provided by the client These are clearly client-side validation
        // failures
        ErrorKind::ExpiredSignature
        | ErrorKind::InvalidToken
        | ErrorKind::InvalidSignature
        | ErrorKind::InvalidIssuer
        | ErrorKind::InvalidAudience
        | ErrorKind::InvalidSubject
        | ErrorKind::ImmatureSignature
        | ErrorKind::MissingRequiredClaim(_) => Error::JWTValidationError(jwt_error),

        // Server errors - issues with server configuration or key management These indicate problems with the server
        // setup, not the client's token
        ErrorKind::InvalidEcdsaKey
        | ErrorKind::InvalidRsaKey(_)
        | ErrorKind::RsaFailedSigning
        | ErrorKind::InvalidAlgorithmName
        | ErrorKind::InvalidKeyFormat
        | ErrorKind::InvalidAlgorithm
        | ErrorKind::MissingAlgorithm
        | ErrorKind::Crypto(_) => Error::Internal(InternalError::JWTDecodingError(jwt_error)),

        // Ambiguous errors - could be either client or server issues We treat these as server errors to maintain
        // backward compatibility These could be caused by malformed tokens (client) or server parsing issues
        ErrorKind::Base64(_) | ErrorKind::Json(_) | ErrorKind::Utf8(_) => {
            Error::Internal(InternalError::JWTDecodingError(jwt_error))
        }

        // Catch-all for any future error variants added to the jsonwebtoken library Default to internal error for now,
        // but when new variants are added, they should be explicitly categorized above
        _ => Error::Internal(InternalError::JWTDecodingError(jwt_error)),
    }
}

pub(crate) async fn decode_and_parse_hasura_claims(
    http_client: &reqwest::Client,
    jwt_config: &JWTConfig,
    jwt: String,
    audience_validation_mode: AudienceValidationMode,
) -> Result<HasuraClaims, Error> {
    let (acceptable_algorithms, decoding_key) = match &jwt_config.key {
        JWTKey::Fixed(conf) => (vec![conf.algorithm], get_decoding_key(conf)?),
        JWTKey::JwkFromUrl(jwk_url) => {
            get_decoding_key_from_jwk_url(http_client, jwk_url, &jwt).await?
        }
    };

    let mut validation = Validation::default();
    validation.algorithms = acceptable_algorithms;
    validation.validate_aud = match audience_validation_mode {
        AudienceValidationMode::Required => true,
        AudienceValidationMode::Optional => false,
    };

    // Additional validations according to the `jwt_config`.
    if let Some(aud) = &jwt_config.audience {
        validation.set_audience(&aud.iter().collect::<Vec<_>>());
    }

    if let Some(issuer) = &jwt_config.issuer {
        validation.set_issuer(&[issuer]);
    }

    if let Some(leeway) = jwt_config.allowed_skew {
        validation.leeway = leeway;
    }

    let claims: serde_json::Value = decode(&jwt, &decoding_key, &validation)
        .map_err(categorize_jwt_error)?
        .claims;

    let hasura_claims = match &jwt_config.claims_config {
        // This case can be avoided, if we can use serde's `Default` and `Flatten`
        // together, but unfortunately that is not possible at the moment.
        // https://github.com/serde-rs/serde/issues/1626
        JWTClaimsConfig::Namespace(JWTClaimsNamespace {
            claims_format,
            location: claims_namespace_path,
        }) => {
            let unprocessed_hasura_claims = claims.pointer(claims_namespace_path).ok_or(
                InternalError::HasuraClaimsNotFound {
                    path: claims_namespace_path.to_string(),
                },
            )?;

            let hasura_claims: HasuraClaims = match claims_format {
                JWTClaimsFormat::Json => serde_json::from_value(unprocessed_hasura_claims.clone())
                    .map_err(InternalError::JSONParseError),
                JWTClaimsFormat::StringifiedJson => {
                    let stringified_hasura_claims = unprocessed_hasura_claims
                        .as_str()
                        .ok_or(Error::ExpectedStringifiedJson)?;
                    serde_json::from_str(stringified_hasura_claims)
                        .map_err(InternalError::JSONParseError)
                }
            }?;
            hasura_claims
        }
        JWTClaimsConfig::Locations(claims_mappings) => {
            let default_role = get_claims_mapping_entry_value(
                "x-hasura-default-role".to_string(),
                &claims_mappings.default_role,
                &claims,
            )?
            .ok_or(Error::RequiredClaimNotFound {
                claim_name: "x-hasura-default-role".to_string(),
            })?;

            let allowed_roles = get_claims_mapping_entry_value(
                "x-hasura-allowed-roles".to_string(),
                &claims_mappings.allowed_roles,
                &claims,
            )?
            .ok_or(Error::RequiredClaimNotFound {
                claim_name: "x-hasura-allowed-roles".to_string(),
            })?;
            let mut custom_claims = HashMap::new();

            for (claim_name, claims_mapping_entry) in &claims_mappings.custom_claims {
                let claim_value = get_claims_mapping_entry_value(
                    claim_name.to_string(),
                    claims_mapping_entry,
                    &claims,
                )?;
                claim_value.map(|claim_val| custom_claims.insert(claim_name.clone(), claim_val));
            }

            HasuraClaims {
                default_role,
                allowed_roles,
                custom_claims,
            }
        }
    };
    if !(hasura_claims.allowed_roles).contains(&hasura_claims.default_role) {
        return Err(Error::DisallowedDefaultRole);
    }
    Ok(hasura_claims)
}

pub(crate) fn get_authorization_token(
    token_location: &JWTTokenLocation,
    headers: &HeaderMap,
) -> Result<String, Error> {
    Ok(match token_location {
        JWTTokenLocation::BearerAuthorization => {
            let authorization_header: &HeaderValue = {
                headers
                    .get(AUTHORIZATION)
                    .ok_or(Error::AuthorizationHeaderSourceNotFound {
                        header_name: "Authorization".to_string(),
                    })?
            };
            let authorization_header: String = authorization_header
                .to_str()
                .map_err(|e| Error::AuthorizationHeaderParseError {
                    err: e.to_string(),
                    header_name: "Authorization".to_string(),
                })?
                .to_string();
            let bearer_token = match authorization_header
                .split_whitespace()
                .collect::<Vec<&str>>()
                .as_slice()
            {
                ["Bearer", auth_token] => Ok(*auth_token),
                _ => Err(Error::AuthorizationHeaderParseError {
                    err:
                    "The `Authorization` header is expected to be in the format of `Bearer <JWT>`"
                            .to_string(),
                    header_name: "Authorization".to_string(),
                }),
            }?;
            bearer_token.to_string()
        }
        JWTTokenLocation::Header(JWTHeaderLocation { name }) => headers
            .get(name)
            .ok_or(Error::AuthorizationHeaderSourceNotFound {
                header_name: name.to_string(),
            })?
            .to_str()
            .map_err(|e| Error::AuthorizationHeaderParseError {
                err: e.to_string(),
                header_name: name.to_string(),
            })?
            .to_string(),

        JWTTokenLocation::Cookie(JWTCookieLocation { name }) => {
            let cookie_header = headers
                .get(COOKIE)
                .ok_or(Error::CookieNotFound)?
                .to_str()
                .map_err(|e| Error::AuthorizationHeaderParseError {
                    err: e.to_string(),
                    header_name: "Cookie".to_string(),
                })?;

            let (cookie_name, cookie_value) = {
                let cookie_value =
                    Cookie::parse(cookie_header).map_err(|e| Error::CookieParseError { err: e })?;
                (cookie_value.name_raw(), cookie_value.value_raw())
            };
            if cookie_name == Some(name) {
                cookie_value
                    .ok_or(Error::MissingCookieValue {
                        cookie_name: name.to_string(),
                    })?
                    .to_string()
            } else {
                return Err(Error::CookieNameNotFound {
                    cookie_name: name.to_string(),
                });
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use jsonwebkey as jwk;
    use jwk::{Algorithm::ES256, JsonWebKey};
    use jwt::{EncodingKey, encode};
    use openssl::pkey::PKey;
    use openssl::rsa::Rsa;
    use tokio;

    use super::*;

    fn get_claims(
        hasura_claims: &serde_json::Value,
        insert_hasura_claims_at: &str,
    ) -> anyhow::Result<Claims> {
        let mut claims_json = json!(
            {
                "sub": "1234567890",
                "name": "John Doe",
                "iat": 1693439022,
                "exp": 1916239022,
                "claims.jwt.hasura.io": {}
            }
        );
        *claims_json
            .pointer_mut(insert_hasura_claims_at)
            .ok_or(anyhow::anyhow!(
                "Could not find {insert_hasura_claims_at:?}"
            ))? = hasura_claims.clone();
        let claims = serde_json::from_value(claims_json)?;
        Ok(claims)
    }

    fn get_encoded_claims(alg: jwt::Algorithm) -> anyhow::Result<String> {
        let hasura_claims = get_default_hasura_claims();
        let claims: Claims = get_claims(
            &serde_json::to_value(hasura_claims)?,
            jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]).as_str(),
        )?;
        let jwt_header = jwt::Header {
            alg,
            ..Default::default()
        };

        let encoded_claims = encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_secret("token".as_ref()),
        )?;
        Ok(encoded_claims)
    }

    fn get_default_hasura_claims() -> HasuraClaims {
        let mut hasura_custom_claims = HashMap::new();
        hasura_custom_claims.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            JsonSessionVariableValue(json!("1")),
        );
        HasuraClaims {
            default_role: Role::new("user"),
            allowed_roles: vec![Role::new("foo"), Role::new("bar"), Role::new("user")],
            custom_claims: hasura_custom_claims,
        }
    }

    fn get_all_jwt_algorithms() -> Vec<jwt::Algorithm> {
        vec![
            jwt::Algorithm::HS256,
            jwt::Algorithm::HS384,
            jwt::Algorithm::HS512,
            jwt::Algorithm::ES256,
            jwt::Algorithm::ES384,
            jwt::Algorithm::RS256,
            jwt::Algorithm::RS384,
            jwt::Algorithm::RS512,
            jwt::Algorithm::PS256,
            jwt::Algorithm::PS384,
            jwt::Algorithm::PS512,
            jwt::Algorithm::EdDSA,
        ]
    }

    #[test]
    // This test emulates scenarios where JWT location is supplied as different sources - Bearer token, Custom header and Cookie
    fn test_jwt_header_source() -> anyhow::Result<()> {
        let mut header_map = HeaderMap::new();

        // Authorization header tests

        header_map.insert(AUTHORIZATION, "Bearer my_authorization_token".parse()?);

        assert_eq!(
            get_authorization_token(&JWTTokenLocation::BearerAuthorization, &header_map)?,
            "my_authorization_token"
        );

        header_map.insert(&AUTHORIZATION, "Bearer_my_authorization_token".parse()?);
        assert_eq!(
            get_authorization_token(&JWTTokenLocation::BearerAuthorization, &header_map)
                .unwrap_err()
                .to_string(),
            "Error in parsing the Authorization header: The `Authorization` header is expected to be in the format of `Bearer <JWT>`"
        );

        // Custom header tests

        header_map.insert("custom-jwt-header", "my_authorization_token".parse()?);
        assert_eq!(
            get_authorization_token(
                &JWTTokenLocation::Header(JWTHeaderLocation {
                    name: "custom-jwt-header".to_string()
                }),
                &header_map
            )?,
            "my_authorization_token"
        );

        // Cookie tests
        header_map.insert("Cookie", "my_cookie=my_authorization_token".parse()?);
        assert_eq!(
            get_authorization_token(
                &JWTTokenLocation::Cookie(JWTCookieLocation {
                    name: "my_cookie".to_string()
                }),
                &header_map
            )?,
            "my_authorization_token"
        );

        assert_eq!(
            get_authorization_token(
                &JWTTokenLocation::Cookie(JWTCookieLocation {
                    name: "non_existent_cookie".to_string()
                }),
                &header_map
            )
            .unwrap_err()
            .to_string(),
            "JWT Authorization token source: cookie name non_existent_cookie not found in the Cookie header"
        );
        Ok(())
    }

    #[tokio::test]
    // This test checks if the JSON claims are decoded correctly from the encoded JWT using HS256 algorithm
    async fn test_jwt_encode_and_decode() -> anyhow::Result<()> {
        let hasura_claims = get_default_hasura_claims();
        let encoded_claims = get_encoded_claims(jwt::Algorithm::HS256)?;

        let jwt_secret_config_json = json!(
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                  "namespace": {
                     "claimsFormat": "Json",
                     "location": jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]),
                  }
               }
            }
        );

        let jwt_config: JWTConfig = serde_json::from_value(jwt_secret_config_json)?;

        let http_client = reqwest::Client::new();

        let decoded_claims = decode_and_parse_hasura_claims(
            &http_client,
            &jwt_config,
            encoded_claims,
            AudienceValidationMode::Required,
        )
        .await?;
        assert_eq!(hasura_claims, decoded_claims);
        Ok(())
    }

    #[tokio::test]
    // This test emulates if the stringified JSON claims are decoded correctly from the encoded JWT using HS256 algorithm
    async fn test_jwt_stringified_hasura_claims() -> anyhow::Result<()> {
        let jwt_secret_config_value = json!(
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                  "namespace": {
                     "claimsFormat": "StringifiedJson",
                     "location": jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]),
                  }
               }
            }
        );

        let jwt_config = serde_json::from_value(jwt_secret_config_value)?;

        let hasura_claims = get_default_hasura_claims();
        let stringified_hasura_claims = serde_json::to_string(&hasura_claims)?;
        let claims = get_claims(
            &serde_json::to_value(stringified_hasura_claims)?,
            jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]).as_str(),
        )?;
        let alg = jwt::Algorithm::HS256;
        let jwt_header = jwt::Header {
            alg,
            ..Default::default()
        };

        let encoded_claims = encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_secret("token".as_ref()),
        )?;

        let http_client = reqwest::Client::new();

        let decoded_claims = decode_and_parse_hasura_claims(
            &http_client,
            &jwt_config,
            encoded_claims,
            AudienceValidationMode::Required,
        )
        .await?;
        assert_eq!(hasura_claims, decoded_claims);
        Ok(())
    }

    #[tokio::test]
    // This test emulates a scenario where Hasura claims are present in a different namespace path
    async fn test_jwt_claims_namespace_path() -> anyhow::Result<()> {
        let alg = jwt::Algorithm::HS256;

        let jwt_secret_config_json = json!(
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                  "namespace": {
                     "claimsFormat": "Json",
                     "location": "/foo/hasuraClaims"
                  }
               }
            }
        );

        let jwt_config = serde_json::from_value(jwt_secret_config_json)?;

        let hasura_claims = get_default_hasura_claims();
        let claims_json = json!(
            {
                "sub": "1234567890",
                "name": "John Doe",
                "iat": 1693439022,
                "exp": 1916239022,
                "foo": {
                    "hasuraClaims": {
                        "x-hasura-default-role": "user",
                        "x-hasura-allowed-roles": ["foo", "bar", "user"],
                        "x-hasura-user-id": "1"
                    }
                }
            }
        );
        let claims: Claims = serde_json::from_value(claims_json)?;

        let jwt_header = jwt::Header {
            alg,
            ..Default::default()
        };
        let encoded_claims = encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_secret("token".as_ref()),
        )?;

        let http_client = reqwest::Client::new();

        let decoded_claims = decode_and_parse_hasura_claims(
            &http_client,
            &jwt_config,
            encoded_claims,
            AudienceValidationMode::Required,
        )
        .await?;
        assert_eq!(hasura_claims, decoded_claims);
        Ok(())
    }

    #[tokio::test]
    // This test emulates a scenario where each location of the Hasura claims is specified explicitly
    async fn test_jwt_claims_mapping() -> anyhow::Result<()> {
        let alg = jwt::Algorithm::HS256;

        let jwt_secret_config_json = json!(
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                 "locations": {
                     "x-hasura-default-role": {
                        "path": {
                           "path": "/roles/2",
                           "default": "user"
                         }
                     },
                     "x-hasura-allowed-roles": {
                        "path": {
                          "path": "/roles"
                        }
                     },
                     "x-hasura-user-id": {
                        "literal": "1"
                     }
                  }
               }
            }
        );

        let jwt_config = serde_json::from_value(jwt_secret_config_json)?;

        let hasura_claims = get_default_hasura_claims();
        let claims_json = json!(
            {
                "sub": "1234567890",
                "name": "John Doe",
                "iat": 1693439022,
                "exp": 1916239022,
                "roles": [
                     "foo",
                     "bar",
                     "user"
                ]
            }
        );
        let claims: Claims = serde_json::from_value(claims_json)?;
        let jwt_header = jwt::Header {
            alg,
            ..Default::default()
        };
        let encoded_claims = encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_secret("token".as_ref()),
        )?;

        let http_client = reqwest::Client::new();

        let decoded_claims = decode_and_parse_hasura_claims(
            &http_client,
            &jwt_config,
            encoded_claims,
            AudienceValidationMode::Required,
        )
        .await?;
        assert_eq!(hasura_claims, decoded_claims);
        Ok(())
    }

    #[tokio::test]
    // This test emulates a scenario where each location of the Hasura claims is specified explicitly, but no custom claims are present
    async fn test_jwt_claims_mapping_with_no_custom_claims() -> anyhow::Result<()> {
        let alg = jwt::Algorithm::HS256;

        let jwt_secret_config_json = json!(
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                 "locations": {
                     "x-hasura-default-role": {
                        "path": {
                           "path": "/roles/2",
                           "default": "user"
                         }
                     },
                     "x-hasura-allowed-roles": {
                        "path": {
                          "path": "/roles"
                        }
                     }
                  }
               }
            }
        );

        let jwt_config = serde_json::from_value(jwt_secret_config_json)?;

        let mut hasura_claims = get_default_hasura_claims();
        hasura_claims.custom_claims.clear();

        let claims_json = json!(
            {
                "sub": "1234567890",
                "name": "John Doe",
                "iat": 1693439022,
                "exp": 1916239022,
                "roles": [
                     "foo",
                     "bar",
                     "user"
                ]
            }
        );
        let claims: Claims = serde_json::from_value(claims_json)?;
        let jwt_header = jwt::Header {
            alg,
            ..Default::default()
        };
        let encoded_claims = encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_secret("token".as_ref()),
        )?;

        let http_client = reqwest::Client::new();

        let decoded_claims = decode_and_parse_hasura_claims(
            &http_client,
            &jwt_config,
            encoded_claims,
            AudienceValidationMode::Required,
        )
        .await?;
        assert_eq!(hasura_claims, decoded_claims);
        Ok(())
    }

    #[tokio::test]
    // This test emulates a scenario where the location of the claim is a malformed json pointer
    async fn test_jwt_claims_mapping_with_malformed_json_pointer() -> anyhow::Result<()> {
        let jwt_secret_config_json = json!(
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                 "locations": {
                     "x-hasura-default-role": {
                        "path": {
                           "path": "/roles/2",
                           "default": "user"
                         }
                     },
                     "x-hasura-allowed-roles": {
                        "path": {
                          "path": "/roles"
                        }
                     },
                     "x-hasura-user-id": {
                        "path": {
                            "path": "custom_claims/user_id" // This is missing a leading slash
                        }
                     }
                  }
               }
            }
        );

        let jwt_config_result = serde_json::from_value::<JWTConfig>(jwt_secret_config_json);
        assert!(
            jwt_config_result.is_err(),
            "jwt_config_result should have been an error: {jwt_config_result:?}"
        );
        assert_eq!(
            jwt_config_result.unwrap_err().to_string(),
            "json pointer \"custom_claims/user_id\" is malformed due to missing starting slash"
        );
        Ok(())
    }

    #[derive(Serialize)]
    struct JWKWithKeys {
        keys: Vec<JsonWebKey>,
    }

    #[tokio::test]
    // This test emulates scenarios where multiple JWKs are present and only the correct encoded JWT is used to decode the Hasura claims
    async fn test_jwk() -> anyhow::Result<()> {
        tracing_util::initialize_tracing(
            None,
            "test_jwk".to_string(),
            None,
            tracing_util::PropagateBaggage::Disable,
            tracing_util::ExportTracesStdout::Disable,
        )?;
        let mut server = mockito::Server::new_async().await;

        let url = server.url();

        let mut test_jwk_1 = jwk::JsonWebKey::new(jwk::Key::generate_p256());
        test_jwk_1.set_algorithm(ES256).unwrap();
        test_jwk_1.key_id = Some("random_kid".to_string());

        let mut test_jwk_2 = jwk::JsonWebKey::new(jwk::Key::generate_p256());
        test_jwk_2.set_algorithm(ES256).unwrap();
        test_jwk_2.key_id = Some("random_kid_2".to_string());

        let jwk_with_keys = JWKWithKeys {
            keys: vec![test_jwk_1.clone(), test_jwk_2],
        };

        // Create a mock
        let non_empty = mockito::Matcher::Regex(".+".to_string());
        let mock = server
            .mock("GET", "/jwk")
            // check W3C trace headers are present
            .match_header("traceparent", non_empty.clone())
            .match_header("tracestate", mockito::Matcher::Any)
            // check B3 trace headers are present
            .match_header("x-b3-traceid", non_empty.clone())
            .match_header("x-b3-spanid", non_empty)
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_header("x-api-key", "1234")
            .with_body(serde_json::to_string(&jwk_with_keys).unwrap())
            .create();

        let http_client = reqwest::Client::new();
        let hasura_claims = get_default_hasura_claims();

        let claims: Claims = get_claims(
            &serde_json::to_value(&hasura_claims)?,
            jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]).as_str(),
        )?;

        let mut jwt_header = jwt::Header::new(jwt::Algorithm::ES256);
        jwt_header.kid = Some("random_kid".to_string());

        let authorization_token_1 = jwt::encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_ec_pem(test_jwk_1.key.to_pem().as_bytes())?,
        )?;

        let jwk_url = url + "/jwk";

        let jwt_config_json = json!({
            "key": {
               "jwkFromUrl": jwk_url,
            },
            "tokenLocation": {
               "type": "BearerAuthorization",
            },
            "claimsConfig": {
                "namespace": {
                    "claimsFormat": "Json",
                    "location": jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]),
                },
            },
        }
        );

        let jwt_config: JWTConfig = serde_json::from_value(jwt_config_json)?;

        let decoded_hasura_claims = decode_and_parse_hasura_claims(
            &http_client,
            &jwt_config,
            authorization_token_1,
            AudienceValidationMode::Required,
        )
        .await?;

        mock.assert();

        assert_eq!(hasura_claims, decoded_hasura_claims);

        let mut test_jwk_3 = jwk::JsonWebKey::new(jwk::Key::generate_p256());
        test_jwk_3.set_algorithm(ES256)?;
        test_jwk_3.key_id = Some("random_kid_3".to_string());

        let mut jwt_header_2 = jwt::Header::new(jwt::Algorithm::ES256);
        jwt_header_2.kid = Some("random_kid_3".to_string());

        let authorization_token_2 = jwt::encode(
            &jwt_header_2,
            &claims,
            &EncodingKey::from_ec_pem(test_jwk_3.key.to_pem().as_bytes())?,
        )?;

        assert_eq!(
            decode_and_parse_hasura_claims(
                &http_client,
                &jwt_config,
                authorization_token_2,
                AudienceValidationMode::Required
            )
            .await
            .unwrap_err()
            .to_string(),
            "Internal Error - No matching JWK found for the given kid: random_kid_3"
        );
        Ok(())
    }

    #[tokio::test]
    // This test verifies that expired JWT tokens return a client error (400) instead of server error (500)
    // while other JWT errors maintain backward compatibility and still return 500
    async fn test_expired_jwt_returns_client_error() -> anyhow::Result<()> {
        let hasura_claims = get_default_hasura_claims();

        // Create a JWT with an expired timestamp
        let expired_claims = json!(
            {
                "sub": "1234567890",
                "name": "John Doe",
                "iat": 1693439022,
                "exp": 1693439022, // Same as iat, so it's immediately expired
                "claims.jwt.hasura.io": hasura_claims
            }
        );
        let claims: Claims = serde_json::from_value(expired_claims)?;

        let jwt_header = jwt::Header {
            alg: jwt::Algorithm::HS256,
            ..Default::default()
        };

        let encoded_claims = encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_secret("token".as_ref()),
        )?;

        let jwt_secret_config_json = json!(
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                  "namespace": {
                     "claimsFormat": "Json",
                     "location": jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]),
                  }
               }
            }
        );

        let jwt_config: JWTConfig = serde_json::from_value(jwt_secret_config_json)?;
        let http_client = reqwest::Client::new();

        let result = decode_and_parse_hasura_claims(
            &http_client,
            &jwt_config,
            encoded_claims,
            AudienceValidationMode::Required,
        )
        .await;

        // Verify that we get a JWTValidationError (client error) not an Internal error
        assert!(result.is_err());
        let error = result.unwrap_err();
        match &error {
            Error::JWTValidationError(jwt_error) => {
                assert_eq!(jwt_error.kind(), &jwt::errors::ErrorKind::ExpiredSignature);
            }
            _ => panic!("Expected JWTValidationError but got: {error:?}"),
        }

        // Verify the status code is 400 (Bad Request)
        assert_eq!(error.to_status_code(), StatusCode::BAD_REQUEST);

        Ok(())
    }

    #[tokio::test]
    // This test emulates encoding and decoding of JWTs with all the JWT algorithms supported by Hasura
    async fn test_jwt_encode_and_decode_for_all_algorithms() -> anyhow::Result<()> {
        for alg in get_all_jwt_algorithms() {
            match alg {
                jwt::Algorithm::HS256 => {
                    let jwt_key_config = json!(
                        {
                            "fixed": {
                               "algorithm": "HS256",
                               "key": {
                                  "value": "token"
                               }
                            }
                        }
                    );
                    helper_test_jwt_encode_decode(
                        &jwt_key_config,
                        jwt::Header::new(jwt::Algorithm::HS256),
                        EncodingKey::from_secret("token".as_ref()),
                    )
                    .await?;
                }
                jwt::Algorithm::HS384 => {
                    let jwt_key_config = json!(
                        {
                            "fixed": {
                               "algorithm": "HS384",
                               "key": {
                                  "value": "token"
                               }
                            }
                        }
                    );
                    helper_test_jwt_encode_decode(
                        &jwt_key_config,
                        jwt::Header::new(jwt::Algorithm::HS384),
                        EncodingKey::from_secret("token".as_ref()),
                    )
                    .await?;
                }
                jwt::Algorithm::HS512 => {
                    let jwt_key_config = json!(
                        {
                            "fixed": {
                               "algorithm": "HS512",
                               "key": {
                                  "value": "token"
                               }
                            }
                        }
                    );
                    helper_test_jwt_encode_decode(
                        &jwt_key_config,
                        jwt::Header::new(jwt::Algorithm::HS512),
                        EncodingKey::from_secret("token".as_ref()),
                    )
                    .await?;
                }
                jwt::Algorithm::ES256 => {
                    let mut test_jwk = jwk::JsonWebKey::new(jwk::Key::generate_p256());
                    test_jwk.set_algorithm(ES256)?;
                    test_jwk.key_id = Some("random_kid".to_string());
                    let test_jwk_public_key = test_jwk
                        .key
                        .to_public()
                        .ok_or(anyhow::anyhow!("not a public key"))?
                        .as_ref()
                        .clone()
                        .to_pem()
                        .replace('\n', "");
                    let jwt_key_config = json!(
                        {
                            "fixed": {
                                "algorithm": "ES256",
                                "key": { "value": test_jwk_public_key }
                            }
                        }
                    );

                    helper_test_jwt_encode_decode(
                        &jwt_key_config,
                        jwt::Header::new(jwt::Algorithm::ES256),
                        EncodingKey::from_ec_pem(test_jwk.key.to_pem().as_bytes())?,
                    )
                    .await?;
                }
                jwt::Algorithm::RS256 => {
                    let rsa = Rsa::generate(2048)?;
                    let pkey = PKey::from_rsa(rsa)?;
                    let pub_key = String::from_utf8(pkey.public_key_to_pem()?)?.replace('\n', "");
                    let priv_key = pkey.private_key_to_pem_pkcs8()?;
                    let jwt_key_config = json!(
                        {
                            "fixed": {
                                "algorithm": "RS256",
                                "key": { "value": pub_key }
                            }
                        }
                    );

                    helper_test_jwt_encode_decode(
                        &jwt_key_config,
                        jwt::Header::new(jwt::Algorithm::RS256),
                        EncodingKey::from_rsa_pem(&priv_key)?,
                    )
                    .await?;
                }
                jwt::Algorithm::RS384 => {
                    let rsa = Rsa::generate(3072)?;
                    let pkey = PKey::from_rsa(rsa)?;
                    let pub_key = String::from_utf8(pkey.public_key_to_pem()?)?.replace('\n', "");
                    let priv_key = pkey.private_key_to_pem_pkcs8()?;
                    let jwt_key_config = json!(
                        {
                            "fixed": {
                                "algorithm": "RS384",
                                "key": { "value": pub_key }
                            }
                        }
                    );

                    helper_test_jwt_encode_decode(
                        &jwt_key_config,
                        jwt::Header::new(jwt::Algorithm::RS384),
                        EncodingKey::from_rsa_pem(&priv_key)?,
                    )
                    .await?;
                }
                jwt::Algorithm::RS512 => {
                    let rsa = Rsa::generate(4096)?;
                    let pkey = PKey::from_rsa(rsa)?;
                    let pub_key = String::from_utf8(pkey.public_key_to_pem()?)?.replace('\n', "");
                    let priv_key = pkey.private_key_to_pem_pkcs8()?;
                    let jwt_key_config = json!(
                        {
                            "fixed": {
                                "algorithm": "RS512",
                                "key": { "value": pub_key }
                            }
                        }
                    );

                    helper_test_jwt_encode_decode(
                        &jwt_key_config,
                        jwt::Header::new(jwt::Algorithm::RS512),
                        EncodingKey::from_rsa_pem(&priv_key)?,
                    )
                    .await?;
                }
                // TODO: Add tests for ES384, PS256, PS384, PS512, EdDSA
                _ => {}
            }
        }
        Ok(())
    }

    async fn helper_test_jwt_encode_decode(
        jwt_key_config: &serde_json::Value,
        jwt_header: jwt::Header,
        encoding_key: EncodingKey,
    ) -> anyhow::Result<()> {
        let hasura_claims = get_default_hasura_claims();
        let claims: Claims = get_claims(
            &serde_json::to_value(hasura_claims.clone())?,
            jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]).as_str(),
        )?;
        let encoded_claims = encode(&jwt_header, &claims, &encoding_key)?;
        let jwt_secret_config_json = json!({
            "key": jwt_key_config,
            "tokenLocation": {
                "type": "BearerAuthorization"
            },
            "claimsConfig": {
                "namespace": {
                    "claimsFormat": "Json",
                    "location": jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]),
                },
            },
        });
        let jwt_config: JWTConfig = serde_json::from_value(jwt_secret_config_json)?;
        let http_client = reqwest::Client::new();
        let decoded_claims = decode_and_parse_hasura_claims(
            &http_client,
            &jwt_config,
            encoded_claims,
            AudienceValidationMode::Required,
        )
        .await
        .unwrap();
        assert_eq!(hasura_claims, decoded_claims);
        Ok(())
    }
}
