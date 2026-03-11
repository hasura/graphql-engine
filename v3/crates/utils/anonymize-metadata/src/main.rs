/// Anonymizes a customer metadata build JSON file for use in benchmarks.
///
/// Replaces:
/// - descriptions/comments with lorem ipsum (preserving length)
/// - table/collection names with table_1, table_2, etc.
/// - column/field names with column_1, column_2, etc.
/// - object type / model / boolean expression / order-by expression names (derived)
/// - GraphQL type names and query root fields (derived)
/// - auth tokens, bearer tokens, URLs
/// - data connector names
/// - UUIDs embedded in strings
use serde_json::Value;
use std::collections::BTreeMap;
use std::io::{self, Read};

// ---------------------------------------------------------------------------
// Lorem ipsum generator (cycles to fill any length)
// ---------------------------------------------------------------------------

const LOREM: &str = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad \
minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit \
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat \
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. ";

fn lorem_ipsum(len: usize) -> String {
    LOREM.chars().cycle().take(len).collect()
}

// ---------------------------------------------------------------------------
// Mapping state
// ---------------------------------------------------------------------------

struct Anonymizer {
    /// "ADS.ADS_DWH.SOME_TABLE" -> "schema_1.schema_2.table_3"
    collection_map: BTreeMap<String, String>,
    /// "COLUMN_NAME" -> "column_1"
    column_map: BTreeMap<String, String>,
    /// "snowflake" -> "connector_1"
    connector_map: BTreeMap<String, String>,
    /// Scalar types we should NOT rename (DATE, String, Float, etc.)
    scalar_types: std::collections::BTreeSet<String>,

    collection_counter: usize,
    column_counter: usize,
    connector_counter: usize,
    schema_segment_map: BTreeMap<String, String>,
    schema_segment_counter: usize,
}

impl Anonymizer {
    fn new() -> Self {
        Self {
            collection_map: BTreeMap::new(),
            column_map: BTreeMap::new(),
            connector_map: BTreeMap::new(),
            scalar_types: std::collections::BTreeSet::new(),
            collection_counter: 0,
            column_counter: 0,
            connector_counter: 0,
            schema_segment_map: BTreeMap::new(),
            schema_segment_counter: 0,
        }
    }

    // -- First pass: collect scalar types so we don't rename them --

    fn collect_scalar_types(&mut self, root: &Value) {
        let subgraphs = root.get("subgraphs").and_then(Value::as_array);
        for sg in subgraphs.into_iter().flatten() {
            for obj in sg
                .get("objects")
                .and_then(Value::as_array)
                .into_iter()
                .flatten()
            {
                if obj.get("kind").and_then(Value::as_str) == Some("ScalarType")
                    && let Some(name) = obj
                        .get("definition")
                        .and_then(|d| d.get("name"))
                        .and_then(Value::as_str)
                {
                    self.scalar_types.insert(name.to_string());
                }
            }
        }
        // Also add some well-known scalars
        for s in ["String", "Int", "Float", "Boolean", "ID"] {
            self.scalar_types.insert(s.to_string());
        }
    }

    // -- Name mapping helpers --

    fn anonymize_schema_segment(&mut self, segment: &str) -> String {
        self.schema_segment_map
            .get(segment)
            .cloned()
            .unwrap_or_else(|| {
                self.schema_segment_counter += 1;
                let anon = format!("schema_{}", self.schema_segment_counter);
                self.schema_segment_map
                    .insert(segment.to_string(), anon.clone());
                anon
            })
    }

    /// Map a fully-qualified collection name like "ADS.ADS_DWH.TABLE_NAME"
    fn anonymize_collection(&mut self, name: &str) -> String {
        if let Some(mapped) = self.collection_map.get(name) {
            return mapped.clone();
        }
        let parts: Vec<&str> = name.split('.').collect();
        let anon_parts: Vec<String> = if parts.len() > 1 {
            let schema_parts: Vec<String> = parts[..parts.len() - 1]
                .iter()
                .map(|s| self.anonymize_schema_segment(s))
                .collect();
            self.collection_counter += 1;
            let table_part = format!("table_{}", self.collection_counter);
            let mut all = schema_parts;
            all.push(table_part);
            all
        } else {
            self.collection_counter += 1;
            vec![format!("table_{}", self.collection_counter)]
        };
        let result = anon_parts.join(".");
        self.collection_map.insert(name.to_string(), result.clone());
        result
    }

    /// Map an object type name like "ADS_ADS_DWH_FOO" by figuring out which
    /// collection it came from (the pattern is underscores replacing dots).
    fn anonymize_type_name(&mut self, name: &str) -> String {
        // If it's a scalar, don't touch it
        if self.scalar_types.contains(name) {
            return name.to_string();
        }

        // Check if it starts with a known scalar (e.g. "DATE_BoolExp", "String_OrderBy")
        for scalar in &self.scalar_types.clone() {
            if name.starts_with(scalar) {
                return name.to_string();
            }
        }

        // Try to find the LONGEST matching collection prefix
        // Collections have dots; the type name replaces dots with underscores
        let mut best_match: Option<(usize, String)> = None;
        for (orig_coll, _) in self.collection_map.clone() {
            let type_form = orig_coll.replace('.', "_");
            if name.starts_with(&type_form)
                && best_match
                    .as_ref()
                    .is_none_or(|(len, _)| type_form.len() > *len)
            {
                best_match = Some((type_form.len(), orig_coll.clone()));
            }
        }

        if let Some((match_len, orig_coll)) = best_match {
            let anon_coll = self.anonymize_collection(&orig_coll);
            let anon_type = anon_coll.replace('.', "_");
            let suffix = &name[match_len..];
            return format!("{anon_type}{suffix}");
        }

        name.to_string()
    }

    fn anonymize_column(&mut self, name: &str) -> String {
        self.column_map.get(name).cloned().unwrap_or_else(|| {
            self.column_counter += 1;
            let anon = format!("column_{}", self.column_counter);
            self.column_map.insert(name.to_string(), anon.clone());
            anon
        })
    }

    fn anonymize_connector(&mut self, name: &str) -> String {
        self.connector_map.get(name).cloned().unwrap_or_else(|| {
            self.connector_counter += 1;
            let anon = format!("connector_{}", self.connector_counter);
            self.connector_map.insert(name.to_string(), anon.clone());
            anon
        })
    }

    fn anonymize_graphql_root_field(&self, name: &str) -> String {
        // Convert camelCase query root field using collection map
        // The pattern: "adsAdsDwhFoo" for collection "ADS.ADS_DWH.FOO"
        // We just derive from the anonymized type name in camelCase
        // Simple approach: use a generic name
        format!("queryField_{}", name.len() % 1000)
    }

    // -- Process the whole document --

    fn process(&mut self, root: &mut Value) {
        // First pass: collect all collection names from Model sources and
        // DataConnectorLink so we have the mapping ready
        self.collect_scalar_types(root);
        self.first_pass(root);
        // Second pass: do the actual anonymization
        self.second_pass(root);
        // Final pass: replace ALL remaining description fields with lorem ipsum
        Self::anonymize_all_descriptions(root);
    }

    fn first_pass(&mut self, root: &Value) {
        let subgraphs = root.get("subgraphs").and_then(Value::as_array);
        for sg in subgraphs.into_iter().flatten() {
            for obj in sg
                .get("objects")
                .and_then(Value::as_array)
                .into_iter()
                .flatten()
            {
                let kind = obj.get("kind").and_then(Value::as_str).unwrap_or("");
                let def = obj.get("definition");

                match kind {
                    "DataConnectorLink" => {
                        if let Some(name) = def.and_then(|d| d.get("name")).and_then(Value::as_str)
                        {
                            self.anonymize_connector(name);
                        }
                        // Register all collections from schema
                        if let Some(colls) = def
                            .and_then(|d| d.get("schema"))
                            .and_then(|s| s.get("schema"))
                            .and_then(|s| s.get("collections"))
                            .and_then(Value::as_array)
                        {
                            for coll in colls {
                                if let Some(n) = coll.get("name").and_then(Value::as_str) {
                                    self.anonymize_collection(n);
                                }
                            }
                        }
                        // Register all columns from object_types in schema
                        if let Some(obj_types) = def
                            .and_then(|d| d.get("schema"))
                            .and_then(|s| s.get("schema"))
                            .and_then(|s| s.get("object_types"))
                            .and_then(Value::as_object)
                        {
                            for (_type_name, type_def) in obj_types {
                                if let Some(fields) =
                                    type_def.get("fields").and_then(Value::as_object)
                                {
                                    for (field_name, _) in fields {
                                        self.anonymize_column(field_name);
                                    }
                                }
                            }
                        }
                    }
                    "Model" => {
                        if let Some(src) = def.and_then(|d| d.get("source"))
                            && let Some(coll) = src.get("collection").and_then(Value::as_str)
                        {
                            self.anonymize_collection(coll);
                        }
                    }
                    "ObjectType" => {
                        // Register columns from field definitions
                        if let Some(fields) =
                            def.and_then(|d| d.get("fields")).and_then(Value::as_array)
                        {
                            for field in fields {
                                if let Some(n) = field.get("name").and_then(Value::as_str) {
                                    self.anonymize_column(n);
                                }
                            }
                        }
                        // Register columns from data connector field mappings
                        if let Some(mappings) = def
                            .and_then(|d| d.get("dataConnectorTypeMapping"))
                            .and_then(Value::as_array)
                        {
                            for mapping in mappings {
                                if let Some(fm) =
                                    mapping.get("fieldMapping").and_then(Value::as_object)
                                {
                                    for (col_name, col_def) in fm {
                                        self.anonymize_column(col_name);
                                        // Also the "column.name" inside
                                        if let Some(inner) = col_def
                                            .get("column")
                                            .and_then(|c| c.get("name"))
                                            .and_then(Value::as_str)
                                        {
                                            self.anonymize_column(inner);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// Recursively replace all "description" string values with lorem ipsum of the same length.
    fn anonymize_all_descriptions(val: &mut Value) {
        match val {
            Value::Object(map) => {
                if let Some(desc) = map.get("description").and_then(Value::as_str) {
                    let len = desc.len();
                    if len > 0 {
                        map.insert("description".to_string(), Value::String(lorem_ipsum(len)));
                    }
                }
                for v in map.values_mut() {
                    Self::anonymize_all_descriptions(v);
                }
            }
            Value::Array(arr) => {
                for v in arr {
                    Self::anonymize_all_descriptions(v);
                }
            }
            _ => {}
        }
    }

    fn second_pass(&mut self, root: &mut Value) {
        let subgraphs = root
            .get_mut("subgraphs")
            .and_then(Value::as_array_mut)
            .map(std::mem::take);
        let Some(mut subgraphs) = subgraphs else {
            return;
        };

        for sg in &mut subgraphs {
            let objects = sg
                .get_mut("objects")
                .and_then(Value::as_array_mut)
                .map(std::mem::take);
            let Some(mut objects) = objects else { continue };

            for obj in &mut objects {
                let kind = obj
                    .get("kind")
                    .and_then(Value::as_str)
                    .unwrap_or("")
                    .to_string();
                let def = obj.get_mut("definition");
                let Some(def) = def else { continue };

                match kind.as_str() {
                    "DataConnectorLink" => self.anonymize_data_connector_link(def),
                    "ObjectType" => self.anonymize_object_type(def),
                    "ScalarType" => {} // leave scalar types alone
                    "DataConnectorScalarRepresentation" => {
                        self.anonymize_dc_scalar_repr(def);
                    }
                    "BooleanExpressionType" => self.anonymize_boolean_expression(def),
                    "OrderByExpression" => self.anonymize_order_by_expression(def),
                    "Model" => self.anonymize_model(def),
                    "ModelPermissions" => self.anonymize_model_permissions(def),
                    "TypePermissions" => self.anonymize_type_permissions(def),
                    "GraphqlConfig" => {} // leave as-is
                    "Command" => self.anonymize_command(def),
                    "CommandPermissions" => self.anonymize_command_permissions(def),
                    _ => {}
                }
            }

            sg.as_object_mut()
                .unwrap()
                .insert("objects".to_string(), Value::Array(objects));
        }

        root.as_object_mut()
            .unwrap()
            .insert("subgraphs".to_string(), Value::Array(subgraphs));
    }

    // -- Per-kind anonymizers --

    fn anonymize_data_connector_link(&mut self, def: &mut Value) {
        // Connector name
        if let Some(name) = def.get("name").and_then(Value::as_str).map(String::from) {
            let anon = self.anonymize_connector(&name);
            def["name"] = Value::String(anon);
        }

        // Headers: redact values
        if let Some(headers) = def.get_mut("headers").and_then(Value::as_object_mut) {
            for (_header_name, header_val) in headers.iter_mut() {
                if let Some(obj) = header_val.as_object_mut()
                    && let Some(v) = obj.get_mut("value")
                {
                    *v = Value::String("Bearer REDACTED".to_string());
                }
            }
        }

        // URLs
        self.anonymize_urls(def);

        // Schema internals
        if let Some(schema) = def.get_mut("schema").and_then(|s| s.get_mut("schema")) {
            self.anonymize_connector_schema(schema);
        }

        // Capabilities: leave alone (structural, not sensitive)
    }

    fn anonymize_urls(&self, val: &mut Value) {
        match val {
            Value::String(s) => {
                if s.starts_with("http://") || s.starts_with("https://") {
                    *s = "http://connector.local".to_string();
                }
            }
            Value::Object(map) => {
                for v in map.values_mut() {
                    self.anonymize_urls(v);
                }
            }
            Value::Array(arr) => {
                for v in arr {
                    self.anonymize_urls(v);
                }
            }
            _ => {}
        }
    }

    fn anonymize_connector_schema(&mut self, schema: &mut Value) {
        // Collections
        if let Some(colls) = schema.get_mut("collections").and_then(Value::as_array_mut) {
            for coll in colls.iter_mut() {
                // Name
                if let Some(name) = coll.get("name").and_then(Value::as_str).map(String::from) {
                    let anon = self.anonymize_collection(&name);
                    coll["name"] = Value::String(anon.clone());
                    // Type usually matches collection name
                    if coll.get("type").and_then(Value::as_str) == Some(&name) {
                        coll["type"] = Value::String(anon);
                    }
                }
                // Description
                if let Some(desc) = coll.get("description").and_then(Value::as_str) {
                    let len = desc.len();
                    coll.as_object_mut()
                        .unwrap()
                        .insert("description".to_string(), Value::String(lorem_ipsum(len)));
                }
            }
        }

        // Object types (keyed by collection-like names)
        if let Some(obj_types) = schema
            .get_mut("object_types")
            .and_then(Value::as_object_mut)
        {
            let entries: Vec<(String, Value)> = obj_types
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            obj_types.clear();

            for (orig_name, mut type_def) in entries {
                let anon_name = self.anonymize_collection(&orig_name);

                // Anonymize field names and descriptions within
                if let Some(fields) = type_def.get_mut("fields").and_then(Value::as_object_mut) {
                    let field_entries: Vec<(String, Value)> =
                        fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    fields.clear();

                    for (orig_field, mut field_def) in field_entries {
                        let anon_field = self.anonymize_column(&orig_field);

                        // Anonymize field description
                        if let Some(desc) = field_def.get("description").and_then(Value::as_str) {
                            let len = desc.len();
                            field_def
                                .as_object_mut()
                                .unwrap()
                                .insert("description".to_string(), Value::String(lorem_ipsum(len)));
                        }

                        // Anonymize field type if it references a collection type
                        if let Some(ftype) =
                            field_def.get_mut("type").and_then(Value::as_object_mut)
                        {
                            self.anonymize_ndc_type(ftype);
                        }

                        fields.insert(anon_field, field_def);
                    }
                }

                // Anonymize description on the type itself
                if let Some(desc) = type_def.get("description").and_then(Value::as_str) {
                    let len = desc.len();
                    type_def
                        .as_object_mut()
                        .unwrap()
                        .insert("description".to_string(), Value::String(lorem_ipsum(len)));
                }

                obj_types.insert(anon_name, type_def);
            }
        }
    }

    fn anonymize_ndc_type(&self, _type_obj: &mut serde_json::Map<String, Value>) {
        // NDC types can reference object types; for now we leave scalar types alone
        // and this is mainly structural
    }

    fn anonymize_object_type(&mut self, def: &mut Value) {
        // Name
        if let Some(name) = def.get("name").and_then(Value::as_str).map(String::from) {
            let anon = self.anonymize_type_name(&name);
            def["name"] = Value::String(anon.clone());

            // GraphQL typeName
            if let Some(gql) = def.get_mut("graphql")
                && let Some(_tn) = gql.get("typeName").and_then(Value::as_str)
            {
                gql["typeName"] = Value::String(anon);
            }
        }

        // Fields
        if let Some(fields) = def.get_mut("fields").and_then(Value::as_array_mut) {
            for field in fields.iter_mut() {
                if let Some(n) = field.get("name").and_then(Value::as_str).map(String::from) {
                    let anon = self.anonymize_column(&n);
                    field["name"] = Value::String(anon);
                }
                // Field type: don't rename scalars, but rename object type references
                if let Some(t) = field.get("type").and_then(Value::as_str).map(String::from)
                    && !self.scalar_types.contains(&t)
                    && !t.ends_with('!')
                    && !t.starts_with('[')
                {
                    let anon = self.anonymize_type_name(&t);
                    field["type"] = Value::String(anon);
                }
                // Anonymize field description
                if let Some(desc) = field.get("description").and_then(Value::as_str) {
                    let len = desc.len();
                    field
                        .as_object_mut()
                        .unwrap()
                        .insert("description".to_string(), Value::String(lorem_ipsum(len)));
                }
            }
        }

        // dataConnectorTypeMapping
        if let Some(mappings) = def
            .get_mut("dataConnectorTypeMapping")
            .and_then(Value::as_array_mut)
        {
            for mapping in mappings.iter_mut() {
                // dataConnectorName
                if let Some(n) = mapping
                    .get("dataConnectorName")
                    .and_then(Value::as_str)
                    .map(String::from)
                {
                    mapping["dataConnectorName"] = Value::String(self.anonymize_connector(&n));
                }
                // dataConnectorObjectType (collection name)
                if let Some(n) = mapping
                    .get("dataConnectorObjectType")
                    .and_then(Value::as_str)
                    .map(String::from)
                {
                    mapping["dataConnectorObjectType"] =
                        Value::String(self.anonymize_collection(&n));
                }
                // fieldMapping keys and column names
                if let Some(fm) = mapping
                    .get_mut("fieldMapping")
                    .and_then(Value::as_object_mut)
                {
                    let entries: Vec<(String, Value)> =
                        fm.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    fm.clear();
                    for (orig_col, mut col_def) in entries {
                        let anon_col = self.anonymize_column(&orig_col);
                        // Inner column name
                        if let Some(inner) =
                            col_def.get_mut("column").and_then(|c| c.get_mut("name"))
                            && let Some(inner_name) = inner.as_str().map(String::from)
                        {
                            *inner = Value::String(self.anonymize_column(&inner_name));
                        }
                        fm.insert(anon_col, col_def);
                    }
                }
            }
        }
    }

    fn anonymize_dc_scalar_repr(&mut self, def: &mut Value) {
        if let Some(n) = def
            .get("dataConnectorName")
            .and_then(Value::as_str)
            .map(String::from)
        {
            def["dataConnectorName"] = Value::String(self.anonymize_connector(&n));
        }
        // Leave scalar type names alone
    }

    fn anonymize_boolean_expression(&mut self, def: &mut Value) {
        // Name and graphql typeName: these reference type names with _BoolExp suffix
        if let Some(name) = def.get("name").and_then(Value::as_str).map(String::from) {
            let anon = self.anonymize_type_name(&name);
            def["name"] = Value::String(anon.clone());
            if let Some(gql) = def.get_mut("graphql")
                && gql.get("typeName").is_some()
            {
                gql["typeName"] = Value::String(anon);
            }
        }

        // Operand scalar dataConnectorOperatorMapping
        if let Some(operand) = def.get_mut("operand") {
            if let Some(scalar) = operand.get_mut("scalar")
                && let Some(mappings) = scalar
                    .get_mut("dataConnectorOperatorMapping")
                    .and_then(Value::as_array_mut)
            {
                for m in mappings.iter_mut() {
                    if let Some(n) = m
                        .get("dataConnectorName")
                        .and_then(Value::as_str)
                        .map(String::from)
                    {
                        m["dataConnectorName"] = Value::String(self.anonymize_connector(&n));
                    }
                }
            }
            // Operand object: has fields referencing column names and bool exp types
            if let Some(obj) = operand.get_mut("object") {
                // The "type" field references an object type name
                if let Some(t) = obj.get("type").and_then(Value::as_str).map(String::from) {
                    obj["type"] = Value::String(self.anonymize_type_name(&t));
                }
                if let Some(fields) = obj
                    .get_mut("comparableFields")
                    .and_then(Value::as_array_mut)
                {
                    for field in fields.iter_mut() {
                        if let Some(n) = field
                            .get("fieldName")
                            .and_then(Value::as_str)
                            .map(String::from)
                        {
                            field["fieldName"] = Value::String(self.anonymize_column(&n));
                        }
                        if let Some(n) = field
                            .get("booleanExpressionType")
                            .and_then(Value::as_str)
                            .map(String::from)
                        {
                            field["booleanExpressionType"] =
                                Value::String(self.anonymize_type_name(&n));
                        }
                    }
                }
            }
        }
    }

    fn anonymize_order_by_expression(&mut self, def: &mut Value) {
        if let Some(name) = def.get("name").and_then(Value::as_str).map(String::from) {
            let anon = self.anonymize_type_name(&name);
            def["name"] = Value::String(anon.clone());
            if let Some(gql) = def.get_mut("graphql")
                && gql.get("expressionTypeName").is_some()
            {
                gql["expressionTypeName"] = Value::String(anon);
            }
        }

        // Operand object: has orderable fields
        if let Some(operand) = def.get_mut("operand")
            && let Some(obj) = operand.get_mut("object")
        {
            // The "orderedType" field references an object type name
            if let Some(t) = obj
                .get("orderedType")
                .and_then(Value::as_str)
                .map(String::from)
            {
                obj["orderedType"] = Value::String(self.anonymize_type_name(&t));
            }
            if let Some(fields) = obj.get_mut("orderableFields").and_then(Value::as_array_mut) {
                for field in fields.iter_mut() {
                    if let Some(n) = field
                        .get("fieldName")
                        .and_then(Value::as_str)
                        .map(String::from)
                    {
                        field["fieldName"] = Value::String(self.anonymize_column(&n));
                    }
                    if let Some(n) = field
                        .get("orderByExpression")
                        .and_then(Value::as_str)
                        .map(String::from)
                    {
                        field["orderByExpression"] = Value::String(self.anonymize_type_name(&n));
                    }
                }
            }
        }
    }

    fn anonymize_model(&mut self, def: &mut Value) {
        // Name and objectType
        if let Some(name) = def.get("name").and_then(Value::as_str).map(String::from) {
            let anon = self.anonymize_type_name(&name);
            def["name"] = Value::String(anon);
        }
        if let Some(name) = def
            .get("objectType")
            .and_then(Value::as_str)
            .map(String::from)
        {
            let anon = self.anonymize_type_name(&name);
            def["objectType"] = Value::String(anon);
        }

        // filterExpressionType
        if let Some(name) = def
            .get("filterExpressionType")
            .and_then(Value::as_str)
            .map(String::from)
        {
            def["filterExpressionType"] = Value::String(self.anonymize_type_name(&name));
        }

        // orderByExpression
        if let Some(name) = def
            .get("orderByExpression")
            .and_then(Value::as_str)
            .map(String::from)
        {
            def["orderByExpression"] = Value::String(self.anonymize_type_name(&name));
        }

        // aggregateExpression
        if let Some(name) = def
            .get("aggregateExpression")
            .and_then(Value::as_str)
            .map(String::from)
        {
            def["aggregateExpression"] = Value::String(self.anonymize_type_name(&name));
        }

        // source.collection and source.dataConnectorName
        if let Some(src) = def.get_mut("source") {
            if let Some(coll) = src
                .get("collection")
                .and_then(Value::as_str)
                .map(String::from)
            {
                src["collection"] = Value::String(self.anonymize_collection(&coll));
            }
            if let Some(n) = src
                .get("dataConnectorName")
                .and_then(Value::as_str)
                .map(String::from)
            {
                src["dataConnectorName"] = Value::String(self.anonymize_connector(&n));
            }
        }

        // graphql
        if let Some(gql) = def.get_mut("graphql") {
            if let Some(sm) = gql.get_mut("selectMany")
                && let Some(field) = sm
                    .get("queryRootField")
                    .and_then(Value::as_str)
                    .map(String::from)
            {
                sm["queryRootField"] = Value::String(self.anonymize_graphql_root_field(&field));
            }
            // selectUniques
            if let Some(sus) = gql.get_mut("selectUniques").and_then(Value::as_array_mut) {
                for su in sus.iter_mut() {
                    if let Some(field) = su
                        .get("queryRootField")
                        .and_then(Value::as_str)
                        .map(String::from)
                    {
                        su["queryRootField"] =
                            Value::String(self.anonymize_graphql_root_field(&field));
                    }
                    // uniqueIdentifier contains column names
                    if let Some(ids) = su.get_mut("uniqueIdentifier").and_then(Value::as_array_mut)
                    {
                        for id in ids.iter_mut() {
                            if let Some(s) = id.as_str().map(String::from) {
                                *id = Value::String(self.anonymize_column(&s));
                            }
                        }
                    }
                }
            }
        }
    }

    fn anonymize_model_permissions(&mut self, def: &mut Value) {
        if let Some(name) = def
            .get("modelName")
            .and_then(Value::as_str)
            .map(String::from)
        {
            def["modelName"] = Value::String(self.anonymize_type_name(&name));
        }
    }

    fn anonymize_type_permissions(&mut self, def: &mut Value) {
        if let Some(name) = def
            .get("typeName")
            .and_then(Value::as_str)
            .map(String::from)
        {
            def["typeName"] = Value::String(self.anonymize_type_name(&name));
        }

        // Field names in allowFields
        if let Some(perms) = def.get_mut("permissions") {
            self.anonymize_fields_in_permissions(perms);
        }
    }

    fn anonymize_fields_in_permissions(&mut self, perms: &mut Value) {
        if let Some(rules) = perms.get_mut("rulesBased").and_then(Value::as_array_mut) {
            for rule in rules.iter_mut() {
                if let Some(af) = rule.get_mut("allowFields")
                    && let Some(fields) = af.get_mut("fields").and_then(Value::as_array_mut)
                {
                    for f in fields.iter_mut() {
                        if let Some(s) = f.as_str().map(String::from) {
                            *f = Value::String(self.anonymize_column(&s));
                        }
                    }
                }
            }
        }
    }

    fn anonymize_command(&mut self, def: &mut Value) {
        // Commands have names, output types, arguments, source
        if let Some(name) = def.get("name").and_then(Value::as_str).map(String::from) {
            def["name"] = Value::String(self.anonymize_type_name(&name));
        }
        if let Some(src) = def.get_mut("source")
            && let Some(n) = src
                .get("dataConnectorName")
                .and_then(Value::as_str)
                .map(String::from)
        {
            src["dataConnectorName"] = Value::String(self.anonymize_connector(&n));
        }
    }

    fn anonymize_command_permissions(&mut self, def: &mut Value) {
        if let Some(name) = def
            .get("commandName")
            .and_then(Value::as_str)
            .map(String::from)
        {
            def["commandName"] = Value::String(self.anonymize_type_name(&name));
        }
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut root: Value = serde_json::from_str(&input).expect("Failed to parse JSON");

    let mut anonymizer = Anonymizer::new();
    anonymizer.process(&mut root);

    let output = serde_json::to_string(&root).expect("Failed to serialize JSON");
    print!("{output}");

    // Print stats to stderr
    eprintln!("Anonymization complete:");
    eprintln!("  Collections mapped: {}", anonymizer.collection_map.len());
    eprintln!("  Columns mapped: {}", anonymizer.column_map.len());
    eprintln!("  Connectors mapped: {}", anonymizer.connector_map.len());
    eprintln!(
        "  Schema segments mapped: {}",
        anonymizer.schema_segment_map.len()
    );

    Ok(())
}
