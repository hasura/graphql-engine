/// Macro to use serde's deserialize and schamars' JsonSchema impl OpenDd for any type using serde_path_to_error with path information
#[macro_export]
macro_rules! impl_OpenDd_default_for {
    ($ty: ty) => {
        impl open_dds::traits::OpenDd for $ty {
            fn deserialize(
                json: serde_json::Value,
            ) -> Result<Self, open_dds::traits::OpenDdDeserializeError> {
                ::serde_path_to_error::deserialize(json).map_err(|e| {
                    open_dds::traits::OpenDdDeserializeError {
                        path: open_dds::traits::JSONPath::from_serde_path(e.path()),
                        error: e.into_inner(),
                    }
                })
            }

            fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
                <Self as ::schemars::JsonSchema>::json_schema(gen)
            }

            fn _schema_name() -> String {
                <Self as ::schemars::JsonSchema>::schema_name()
            }

            fn _schema_is_referenceable() -> bool {
                <Self as ::schemars::JsonSchema>::is_referenceable()
            }
        }
    };
}

/// Macro to implement OpenDd for sequence like type containers
/// Ref: https://docs.rs/serde/latest/src/serde/de/impls.rs.html#885
#[macro_export]
macro_rules! seq_impl {
    ($ty:ident <T $(: $tbound1:ident $(+ $tbound2:ident)*)* $(, $typaram:ident : $bound1:ident $(+ $bound2:ident)*)*>, $is_unique:literal) => {
        impl<T $(, $typaram)*> OpenDd for $ty<T $(, $typaram)*>
            where
            T: OpenDd $(+ $tbound1 $(+ $tbound2)*)*, $($typaram: $bound1 $(+ $bound2)*,)*
        {
            fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError> {
                match json {
                    serde_json::Value::Array(arr) => arr
                        .into_iter()
                        .enumerate()
                        .map(|(idx, json)| {
                            T::deserialize(json).map_err(|e| OpenDdDeserializeError {
                                path: e.path.prepend_index(idx),
                                error: e.error,
                            })
                        })
                        .collect::<Result<$ty<T $(, $typaram)*>, OpenDdDeserializeError>>(),
                    _ => Err(OpenDdDeserializeError {
                        error: serde::de::Error::invalid_type(
                            serde::de::Unexpected::Other("not an array"),
                            &"array",
                        ),
                        path: JSONPath::new(),
                    }),
                }
            }

            fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> Schema {
                SchemaObject {
                    instance_type: Some(InstanceType::Array.into()),
                    array: Some(Box::new(ArrayValidation {
                        // None omits `unique_items` key in the serialized schema
                        unique_items: if $is_unique { Some(true) } else { None },
                        items: Some(gen_subschema_for::<T>(gen).into()),
                        ..Default::default()
                    })),
                    ..Default::default()
                }
                .into()
            }

            fn _schema_name() -> String {
                format!("Set_of_{}", T::_schema_name())
            }
        }
    }
}

/// Macro to implement OpenDd for map like type containers
/// Ref: https://docs.rs/serde/latest/src/serde/de/impls.rs.html#1387
#[macro_export]
macro_rules! map_impl {
    ($ty:ident <K $(: $kbound1:ident $(+ $kbound2:ident)*)*, V $(, $typaram:ident : $bound1:ident $(+ $bound2:ident)*)*>) => {
        impl<K, V $(, $typaram)*> OpenDd for $ty<K, V $(, $typaram)*>
        where
            K: OpenDd $(+ $kbound1 $(+ $kbound2)*)*,
            V: OpenDd,
            $($typaram: $bound1 $(+ $bound2)*),*
        {
            fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError> {
                match json {
                    serde_json::Value::Object(map) => map
                        .into_iter()
                        .map(|(k, v)| {
                            Ok((
                                K::deserialize(serde_json::Value::String(k.clone()))?,
                                V::deserialize(v).map_err(|e| OpenDdDeserializeError {
                                    path: e.path.prepend_key(k),
                                    error: e.error,
                                })?,
                            ))
                        })
                        .collect::<Result<$ty<K, V $(, $typaram)*>, OpenDdDeserializeError>>(),
                    _ => Err(OpenDdDeserializeError {
                        error: serde::de::Error::invalid_type(
                            serde::de::Unexpected::Other("not an object"),
                            &"object",
                        ),
                        path: JSONPath::new(),
                    }),
                }
            }

            fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> Schema {
                let subschema = gen_subschema_for::<V>(gen);
                SchemaObject {
                    instance_type: Some(InstanceType::Object.into()),
                    object: Some(Box::new(ObjectValidation {
                        additional_properties: Some(Box::new(subschema)),
                        ..Default::default()
                    })),
                    ..Default::default()
                }
                .into()
            }

            fn _schema_name() -> String {
                format!("Map_of_{}", V::_schema_name())
            }
        }
    }
}

/// Macro to implement JsonSchema for OpenDd types.
#[macro_export]
macro_rules! impl_JsonSchema_with_OpenDd_for {
    ($ty: ty) => {
        impl schemars::JsonSchema for $ty {
            fn is_referenceable() -> bool {
                <$ty as open_dds::traits::OpenDd>::_schema_is_referenceable()
            }

            fn schema_name() -> String {
                <$ty as open_dds::traits::OpenDd>::_schema_name()
            }

            fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
                <$ty as open_dds::traits::OpenDd>::json_schema(gen)
            }
        }
    };
}
