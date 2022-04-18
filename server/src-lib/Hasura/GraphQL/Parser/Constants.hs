{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | QuasiQuoted GraphQL constants used througout the codebase. By
-- moving all the Quasiquotes here we can eliminate extraneous
-- rebuilds of larger modules.
--
-- See: https://www.parsonsmatt.org/2021/07/12/template_haskell_performance_tips.html#recompilation-avoidance
module Hasura.GraphQL.Parser.Constants where

import Language.GraphQL.Draft.Syntax as G
import Language.GraphQL.Draft.Syntax.QQ as G

_A :: G.Name
_A = [G.name|A|]

_Boolean :: G.Name
_Boolean = [G.name|Boolean|]

_Bool :: G.Name
_Bool = [G.name|Bool|]

_Bytes :: G.Name
_Bytes = [G.name|Bytes|]

_B :: G.Name
_B = [G.name|B|]

_Connection :: G.Name
_Connection = [G.name|Connection|]

_Datetime :: G.Name
_Datetime = [G.name|Datetime|]

_Date :: G.Name
_Date = [G.name|Date|]

_Double :: G.Name
_Double = [G.name|Double|]

_ENUM :: G.Name
_ENUM = [G.name|ENUM|]

_Edge :: G.Name
_Edge = [G.name|Edge|]

_Float :: G.Name
_Float = [G.name|Float|]

_Geography :: G.Name
_Geography = [G.name|Geography|]

_ID :: G.Name
_ID = [G.name|ID|]

_INPUT_OBJECT :: G.Name
_INPUT_OBJECT = [G.name|INPUT_OBJECT|]

_INTERFACE :: G.Name
_INTERFACE = [G.name|INTERFACE|]

_Int :: G.Name
_Int = [G.name|Int|]

_LIST :: G.Name
_LIST = [G.name|LIST|]

_Mutation :: G.Name
_Mutation = [G.name|Mutation|]

_NON_NULL :: G.Name
_NON_NULL = [G.name|NON_NULL|]

_Node :: G.Name
_Node = [G.name|Node|]

_Number :: G.Name
_Number = [G.name|Number|]

_OBJECT :: G.Name
_OBJECT = [G.name|OBJECT|]

_PageInfo :: G.Name
_PageInfo = [G.name|PageInfo|]

_PresetValue :: G.Name
_PresetValue = [G.name|PresetValue|]

_Query :: G.Name
_Query = [G.name|Query|]

_SCALAR :: G.Name
_SCALAR = [G.name|SCALAR|]

_String :: G.Name
_String = [G.name|String|]

_Subscription :: G.Name
_Subscription = [G.name|Subscription|]

_Timestamp :: G.Name
_Timestamp = [G.name|Timestamp|]

_Time :: G.Name
_Time = [G.name|Time|]

_UNION :: G.Name
_UNION = [G.name|UNION|]

_UUID :: G.Name
_UUID = [G.name|UUID|]

__BigQuery_comparison_exp :: G.Name
__BigQuery_comparison_exp = [G.name|_BigQuery_comparison_exp|]

__one :: G.Name
__one = [G.name|_one|]

__MSSQL_comparison_exp :: G.Name
__MSSQL_comparison_exp = [G.name|_MSSQL_comparison_exp|]

__MySQL_comparison_exp :: G.Name
__MySQL_comparison_exp = [G.name|_MySQL_comparison_exp|]

__PLACEHOLDER :: G.Name
__PLACEHOLDER = [G.name|_PLACEHOLDER|]

___Directive :: G.Name
___Directive = [G.name|__Directive|]

___EnumValue :: G.Name
___EnumValue = [G.name|__EnumValue|]

__fields :: G.Name
__fields = [G.name|_fields|]

___Field :: G.Name
___Field = [G.name|__Field|]

___InputValue :: G.Name
___InputValue = [G.name|__InputValue|]

___Schema :: G.Name
___Schema = [G.name|__Schema|]

___TypeKind :: G.Name
___TypeKind = [G.name|__TypeKind|]

___Type :: G.Name
___Type = [G.name|__Type|]

___hasura_internal_typename :: G.Name
___hasura_internal_typename = [G.name|__hasura_internal_typename|]

___schema :: G.Name
___schema = [G.name|__schema|]

___typename :: G.Name
___typename = [G.name|__typename|]

___type :: G.Name
___type = [G.name|__type|]

__aggregate_fields :: G.Name
__aggregate_fields = [G.name|_aggregate_fields|]

__aggregate_order_by :: G.Name
__aggregate_order_by = [G.name|_aggregate_order_by|]

__aggregate :: G.Name
__aggregate = [G.name|_aggregate|]

__ancestor_any :: G.Name
__ancestor_any = [G.name|_ancestor_any|]

__ancestor :: G.Name
__ancestor = [G.name|_ancestor|]

__and :: G.Name
__and = [G.name|_and|]

__append :: G.Name
__append = [G.name|_append|]

__args :: G.Name
__args = [G.name|_args|]

__arr_rel_insert_input :: G.Name
__arr_rel_insert_input = [G.name|_arr_rel_insert_input|]

__bool_exp :: G.Name
__bool_exp = [G.name|_bool_exp|]

__by_pk :: G.Name
__by_pk = [G.name|_by_pk|]

__cast_exp :: G.Name
__cast_exp = [G.name|_cast_exp|]

__cast :: G.Name
__cast = [G.name|_cast|]

__comparison_exp :: G.Name
__comparison_exp = [G.name|_comparison_exp|]

__connection :: G.Name
__connection = [G.name|_connection|]

__constraint :: G.Name
__constraint = [G.name|_constraint|]

__contained_in :: G.Name
__contained_in = [G.name|_contained_in|]

__contains :: G.Name
__contains = [G.name|_contains|]

__delete_at_path :: G.Name
__delete_at_path = [G.name|_delete_at_path|]

__delete_elem :: G.Name
__delete_elem = [G.name|_delete_elem|]

__delete_key :: G.Name
__delete_key = [G.name|_delete_key|]

__descendant_any :: G.Name
__descendant_any = [G.name|_descendant_any|]

__descendant :: G.Name
__descendant = [G.name|_descendant|]

__enum :: G.Name
__enum = [G.name|_enum|]

__eq :: G.Name
__eq = [G.name|_eq|]

__gte :: G.Name
__gte = [G.name|_gte|]

__gt :: G.Name
__gt = [G.name|_gt|]

__has_keys_all :: G.Name
__has_keys_all = [G.name|_has_keys_all|]

__has_keys_any :: G.Name
__has_keys_any = [G.name|_has_keys_any|]

__has_key :: G.Name
__has_key = [G.name|_has_key|]

__if_matched :: G.Name
__if_matched = [G.name|_if_matched|]

__ilike :: G.Name
__ilike = [G.name|_ilike|]

__insert_input :: G.Name
__insert_input = [G.name|_insert_input|]

__insert_match_column :: G.Name
__insert_match_column = [G.name|_insert_match_column|]

__in :: G.Name
__in = [G.name|_in|]

__iregex :: G.Name
__iregex = [G.name|_iregex|]

__is_null :: G.Name
__is_null = [G.name|_is_null|]

__like :: G.Name
__like = [G.name|_like|]

__lte :: G.Name
__lte = [G.name|_lte|]

__lt :: G.Name
__lt = [G.name|_lt|]

__matches_any :: G.Name
__matches_any = [G.name|_matches_any|]

__matches_fulltext :: G.Name
__matches_fulltext = [G.name|_matches_fulltext|]

__matches :: G.Name
__matches = [G.name|_matches|]

__multiple_top_level_fields :: G.Name
__multiple_top_level_fields = [G.name|_multiple_top_level_fields|]

__mutation_backend :: G.Name
__mutation_backend = [G.name|_mutation_backend|]

__mutation_frontend :: G.Name
__mutation_frontend = [G.name|_mutation_frontend|]

__mutation_response :: G.Name
__mutation_response = [G.name|_mutation_response|]

__neq :: G.Name
__neq = [G.name|_neq|]

__nilike :: G.Name
__nilike = [G.name|_nilike|]

__nin :: G.Name
__nin = [G.name|_nin|]

__niregex :: G.Name
__niregex = [G.name|_niregex|]

__nlike :: G.Name
__nlike = [G.name|_nlike|]

__not :: G.Name
__not = [G.name|_not|]

__nregex :: G.Name
__nregex = [G.name|_nregex|]

__nsimilar :: G.Name
__nsimilar = [G.name|_nsimilar|]

__obj_rel_insert_input :: G.Name
__obj_rel_insert_input = [G.name|_obj_rel_insert_input|]

__on_conflict :: G.Name
__on_conflict = [G.name|_on_conflict|]

__order_by :: G.Name
__order_by = [G.name|_order_by|]

__or :: G.Name
__or = [G.name|_or|]

__prepend :: G.Name
__prepend = [G.name|_prepend|]

__query :: G.Name
__query = [G.name|_query|]

__regex :: G.Name
__regex = [G.name|_regex|]

__remote_rel_ :: G.Name
__remote_rel_ = [G.name|_remote_rel_|]

__scalar :: G.Name
__scalar = [G.name|_scalar|]

__select_column :: G.Name
__select_column = [G.name|_select_column|]

__similar :: G.Name
__similar = [G.name|_similar|]

__st_3d_d_within :: G.Name
__st_3d_d_within = [G.name|_st_3d_d_within|]

__st_3d_intersects :: G.Name
__st_3d_intersects = [G.name|_st_3d_intersects|]

__st_contains :: G.Name
__st_contains = [G.name|_st_contains|]

__st_crosses :: G.Name
__st_crosses = [G.name|_st_crosses|]

__st_d_within :: G.Name
__st_d_within = [G.name|_st_d_within|]

__st_equals :: G.Name
__st_equals = [G.name|_st_equals|]

__st_intersects_geom_nband :: G.Name
__st_intersects_geom_nband = [G.name|_st_intersects_geom_nband|]

__st_intersects_nband_geom :: G.Name
__st_intersects_nband_geom = [G.name|_st_intersects_nband_geom|]

__st_intersects_rast :: G.Name
__st_intersects_rast = [G.name|_st_intersects_rast|]

__st_intersects :: G.Name
__st_intersects = [G.name|_st_intersects|]

__st_overlaps :: G.Name
__st_overlaps = [G.name|_st_overlaps|]

__st_touches :: G.Name
__st_touches = [G.name|_st_touches|]

__st_within :: G.Name
__st_within = [G.name|_st_within|]

__update_column :: G.Name
__update_column = [G.name|_update_column|]

__ :: G.Name
__ = [G.name|_|]

_affected_rows :: G.Name
_affected_rows = [G.name|affected_rows|]

_after :: G.Name
_after = [G.name|after|]

_aggregate :: G.Name
_aggregate = [G.name|aggregate|]

_args :: G.Name
_args = [G.name|args|]

_asc_nulls_first :: G.Name
_asc_nulls_first = [G.name|asc_nulls_first|]

_asc_nulls_last :: G.Name
_asc_nulls_last = [G.name|asc_nulls_last|]

_asc :: G.Name
_asc = [G.name|asc|]

_avg :: G.Name
_avg = [G.name|avg|]

_a :: G.Name
_a = [G.name|a|]

_before :: G.Name
_before = [G.name|before|]

_boolExp :: G.Name
_boolExp = [G.name|boolExp|]

_b :: G.Name
_b = [G.name|b|]

_cached :: G.Name
_cached = [G.name|cached|]

_columns :: G.Name
_columns = [G.name|columns|]

_column :: G.Name
_column = [G.name|column|]

_constraint :: G.Name
_constraint = [G.name|constraint|]

_count :: G.Name
_count = [G.name|count|]

_created_at :: G.Name
_created_at = [G.name|created_at|]

_cursor :: G.Name
_cursor = [G.name|cursor|]

_data :: G.Name
_data = [G.name|data|]

_defaultValue :: G.Name
_defaultValue = [G.name|defaultValue|]

_delete_ :: G.Name
_delete_ = [G.name|delete_|]

_deprecationReason :: G.Name
_deprecationReason = [G.name|deprecationReason|]

_desc_nulls_first :: G.Name
_desc_nulls_first = [G.name|desc_nulls_first|]

_desc_nulls_last :: G.Name
_desc_nulls_last = [G.name|desc_nulls_last|]

_description :: G.Name
_description = [G.name|description|]

_desc :: G.Name
_desc = [G.name|desc|]

_directives :: G.Name
_directives = [G.name|directives|]

_distance :: G.Name
_distance = [G.name|distance|]

_distinct_on :: G.Name
_distinct_on = [G.name|distinct_on|]

_distinct :: G.Name
_distinct = [G.name|distinct|]

_edges :: G.Name
_edges = [G.name|edges|]

_endCursor :: G.Name
_endCursor = [G.name|endCursor|]

_enumValues :: G.Name
_enumValues = [G.name|enumValues|]

_errors :: G.Name
_errors = [G.name|errors|]

_fields :: G.Name
_fields = [G.name|fields|]

_first :: G.Name
_first = [G.name|first|]

_float8 :: G.Name
_float8 = [G.name|float8|]

_from :: G.Name
_from = [G.name|from|]

_geommin :: G.Name
_geommin = [G.name|geommin|]

_hasNextPage :: G.Name
_hasNextPage = [G.name|hasNextPage|]

_hasPreviousPage :: G.Name
_hasPreviousPage = [G.name|hasPreviousPage|]

_id :: G.Name
_id = [G.name|id|]

_if_matched :: G.Name
_if_matched = [G.name|if_matched|]

_if :: G.Name
_if = [G.name|if|]

_includeDeprecated :: G.Name
_includeDeprecated = [G.name|includeDeprecated|]

_include :: G.Name
_include = [G.name|include|]

_inputFields :: G.Name
_inputFields = [G.name|inputFields|]

_insert_ :: G.Name
_insert_ = [G.name|insert_|]

_interfaces :: G.Name
_interfaces = [G.name|interfaces|]

_isDeprecated :: G.Name
_isDeprecated = [G.name|isDeprecated|]

_isRepeatable :: G.Name
_isRepeatable = [G.name|isRepeatable|]

_kind :: G.Name
_kind = [G.name|kind|]

_last :: G.Name
_last = [G.name|last|]

_limit :: G.Name
_limit = [G.name|limit|]

_locations :: G.Name
_locations = [G.name|locations|]

_match_columns :: G.Name
_match_columns = [G.name|match_columns|]

_mutationType :: G.Name
_mutationType = [G.name|mutationType|]

_mutation_root :: G.Name
_mutation_root = [G.name|mutation_root|]

_name :: G.Name
_name = [G.name|name|]

_nband :: G.Name
_nband = [G.name|nband|]

_no_queries_available :: G.Name
_no_queries_available = [G.name|no_queries_available|]

_nodes :: G.Name
_nodes = [G.name|nodes|]

_node :: G.Name
_node = [G.name|node|]

_numeric :: G.Name
_numeric = [G.name|numeric|]

_objects :: G.Name
_objects = [G.name|objects|]

_object :: G.Name
_object = [G.name|object|]

_ofType :: G.Name
_ofType = [G.name|ofType|]

_offset :: G.Name
_offset = [G.name|offset|]

_on_conflict :: G.Name
_on_conflict = [G.name|on_conflict|]

_order_by :: G.Name
_order_by = [G.name|order_by|]

_output :: G.Name
_output = [G.name|output|]

_pageInfo :: G.Name
_pageInfo = [G.name|pageInfo|]

_path :: G.Name
_path = [G.name|path|]

_possibleTypes :: G.Name
_possibleTypes = [G.name|possibleTypes|]

_preset :: G.Name
_preset = [G.name|preset|]

_queryType :: G.Name
_queryType = [G.name|queryType|]

_query_root :: G.Name
_query_root = [G.name|query_root|]

_refresh :: G.Name
_refresh = [G.name|refresh|]

_returning :: G.Name
_returning = [G.name|returning|]

_skip :: G.Name
_skip = [G.name|skip|]

_st_d_within_geography_input :: G.Name
_st_d_within_geography_input = [G.name|st_d_within_geography_input|]

_st_d_within_input :: G.Name
_st_d_within_input = [G.name|st_d_within_input|]

_st_dwithin_input :: G.Name
_st_dwithin_input = [G.name|st_dwithin_input|]

_st_intersects_geom_nband_input :: G.Name
_st_intersects_geom_nband_input = [G.name|st_intersects_geom_nband_input|]

_st_intersects_nband_geom_input :: G.Name
_st_intersects_nband_geom_input = [G.name|st_intersects_nband_geom_input|]

_startCursor :: G.Name
_startCursor = [G.name|startCursor|]

_static :: G.Name
_static = [G.name|static|]

_stddev_pop :: G.Name
_stddev_pop = [G.name|stddev_pop|]

_stddev_samp :: G.Name
_stddev_samp = [G.name|stddev_samp|]

_stddev :: G.Name
_stddev = [G.name|stddev|]

_subscriptionType :: G.Name
_subscriptionType = [G.name|subscriptionType|]

_subscription_root :: G.Name
_subscription_root = [G.name|subscription_root|]

_sum :: G.Name
_sum = [G.name|sum|]

_transaction :: G.Name
_transaction = [G.name|transaction|]

_ttl :: G.Name
_ttl = [G.name|ttl|]

_types :: G.Name
_types = [G.name|types|]

_type :: G.Name
_type = [G.name|type|]

_update_columns :: G.Name
_update_columns = [G.name|update_columns|]

_update_ :: G.Name
_update_ = [G.name|update_|]

_use_spheroid :: G.Name
_use_spheroid = [G.name|use_spheroid|]

_uuid :: G.Name
_uuid = [G.name|uuid|]

_value :: G.Name
_value = [G.name|value|]

_var_pop :: G.Name
_var_pop = [G.name|var_pop|]

_var_samp :: G.Name
_var_samp = [G.name|var_samp|]

_variance :: G.Name
_variance = [G.name|variance|]

_where :: G.Name
_where = [G.name|where|]

_x :: G.Name
_x = [G.name|x|]
