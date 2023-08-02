{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | QuasiQuoted GraphQL constants used througout the codebase. By
-- moving all the Quasiquotes here we can eliminate extraneous
-- rebuilds of larger modules.
--
-- See: https://www.parsonsmatt.org/2021/07/12/template_haskell_performance_tips.html#recompilation-avoidance
module Hasura.Name where

import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

__ :: G.Name
__ = [G.name|_|]

-- * Sources

_no_queries_available :: G.Name
_no_queries_available = [G.name|no_queries_available|]

__mutation_backend :: G.Name
__mutation_backend = [G.name|_mutation_backend|]

__mutation_frontend :: G.Name
__mutation_frontend = [G.name|_mutation_frontend|]

__query :: G.Name
__query = [G.name|_query|]

__subscription :: G.Name
__subscription = [G.name|_subscription|]

-- * Directives

_preset :: G.Name
_preset = [G.name|preset|]

_static :: G.Name
_static = [G.name|static|]

_value :: G.Name
_value = [G.name|value|]

-- * Types

_Bool :: G.Name
_Bool = [G.name|Bool|]

_Double :: G.Name
_Double = [G.name|Double|]

_float8 :: G.Name
_float8 = [G.name|float8|]

_Number :: G.Name
_Number = [G.name|Number|]

_numeric :: G.Name
_numeric = [G.name|numeric|]

_mutation_root :: G.Name
_mutation_root = [G.name|mutation_root|]

_query_root :: G.Name
_query_root = [G.name|query_root|]

_subscription_root :: G.Name
_subscription_root = [G.name|subscription_root|]

__enum :: G.Name
__enum = [G.name|_enum|]

__scalar :: G.Name
__scalar = [G.name|_scalar|]

-- * Identifiers

_id :: G.Name
_id = [G.name|id|]

_session_variables :: G.Name
_session_variables = [G.name|session_variables|]

-- * Relationships

__remote_rel_ :: G.Name
__remote_rel_ = [G.name|_remote_rel_|]

-- * Select

-- ** Comparisons

__bool_exp :: G.Name
__bool_exp = [G.name|_bool_exp|]

__comparison_exp :: G.Name
__comparison_exp = [G.name|_comparison_exp|]

__BigQuery_comparison_exp :: G.Name
__BigQuery_comparison_exp = [G.name|_BigQuery_comparison_exp|]

__MSSQL_comparison_exp :: G.Name
__MSSQL_comparison_exp = [G.name|_MSSQL_comparison_exp|]

__cast :: G.Name
__cast = [G.name|_cast|]

__cast_exp :: G.Name
__cast_exp = [G.name|_cast_exp|]

__is_null :: G.Name
__is_null = [G.name|_is_null|]

__eq :: G.Name
__eq = [G.name|_eq|]

__neq :: G.Name
__neq = [G.name|_neq|]

__in :: G.Name
__in = [G.name|_in|]

__nin :: G.Name
__nin = [G.name|_nin|]

__gt :: G.Name
__gt = [G.name|_gt|]

__gte :: G.Name
__gte = [G.name|_gte|]

__lt :: G.Name
__lt = [G.name|_lt|]

__lte :: G.Name
__lte = [G.name|_lte|]

__contains :: G.Name
__contains = [G.name|_contains|]

__matches :: G.Name
__matches = [G.name|_matches|]

__ancestor :: G.Name
__ancestor = [G.name|_ancestor|]

__descendant :: G.Name
__descendant = [G.name|_descendant|]

__like :: G.Name
__like = [G.name|_like|]

__ilike :: G.Name
__ilike = [G.name|_ilike|]

__nlike :: G.Name
__nlike = [G.name|_nlike|]

__nilike :: G.Name
__nilike = [G.name|_nilike|]

__similar :: G.Name
__similar = [G.name|_similar|]

__nsimilar :: G.Name
__nsimilar = [G.name|_nsimilar|]

__regex :: G.Name
__regex = [G.name|_regex|]

__nregex :: G.Name
__nregex = [G.name|_nregex|]

__niregex :: G.Name
__niregex = [G.name|_niregex|]

__iregex :: G.Name
__iregex = [G.name|_iregex|]

__and :: G.Name
__and = [G.name|_and|]

__not :: G.Name
__not = [G.name|_not|]

__or :: G.Name
__or = [G.name|_or|]

-- ** Aggregation

_aggregate :: G.Name
_aggregate = [G.name|aggregate|]

_aggregate_bool_exp :: G.Name
_aggregate_bool_exp = [G.name|aggregate_bool_exp|]

_column :: G.Name
_column = [G.name|column|]

_columns :: G.Name
_columns = [G.name|columns|]

_nodes :: G.Name
_nodes = [G.name|nodes|]

_avg :: G.Name
_avg = [G.name|avg|]

_count :: G.Name
_count = [G.name|count|]

_stddev :: G.Name
_stddev = [G.name|stddev|]

_stddev_pop :: G.Name
_stddev_pop = [G.name|stddev_pop|]

_stddev_samp :: G.Name
_stddev_samp = [G.name|stddev_samp|]

_sum :: G.Name
_sum = [G.name|sum|]

_var_pop :: G.Name
_var_pop = [G.name|var_pop|]

_var_samp :: G.Name
_var_samp = [G.name|var_samp|]

_variance :: G.Name
_variance = [G.name|variance|]

__aggregate :: G.Name
__aggregate = [G.name|_aggregate|]

__aggregate_fields :: G.Name
__aggregate_fields = [G.name|_aggregate_fields|]

__fields :: G.Name
__fields = [G.name|_fields|]

-- ** Group By

_group_by :: G.Name
_group_by = [G.name|group_by|]

_keys :: G.Name
_keys = [G.name|keys|]

_group_key :: G.Name
_group_key = [G.name|group_key|]

-- * JSON

_path :: G.Name
_path = [G.name|path|]

-- ** Functions

_args :: G.Name
_args = [G.name|args|]

__args :: G.Name
__args = [G.name|_args|]

-- ** Distinct

_distinct :: G.Name
_distinct = [G.name|distinct|]

_distinct_on :: G.Name
_distinct_on = [G.name|distinct_on|]

__select_column :: G.Name
__select_column = [G.name|_select_column|]

-- ** Filtering

_where :: G.Name
_where = [G.name|where|]

-- ** Ordering

_order_by :: G.Name
_order_by = [G.name|order_by|]

_ASC :: G.Name
_ASC = [G.name|ASC|]

_asc :: G.Name
_asc = [G.name|asc|]

_asc_nulls_first :: G.Name
_asc_nulls_first = [G.name|asc_nulls_first|]

_asc_nulls_last :: G.Name
_asc_nulls_last = [G.name|asc_nulls_last|]

_DESC :: G.Name
_DESC = [G.name|DESC|]

_desc :: G.Name
_desc = [G.name|desc|]

_desc_nulls_first :: G.Name
_desc_nulls_first = [G.name|desc_nulls_first|]

_desc_nulls_last :: G.Name
_desc_nulls_last = [G.name|desc_nulls_last|]

_first :: G.Name
_first = [G.name|first|]

_last :: G.Name
_last = [G.name|last|]

_before :: G.Name
_before = [G.name|before|]

_after :: G.Name
_after = [G.name|after|]

__aggregate_order_by :: G.Name
__aggregate_order_by = [G.name|_aggregate_order_by|]

__order_by :: G.Name
__order_by = [G.name|_order_by|]

-- ** Limit

_limit :: G.Name
_limit = [G.name|limit|]

_offset :: G.Name
_offset = [G.name|offset|]

-- ** Geography

_distance :: G.Name
_distance = [G.name|distance|]

_from :: G.Name
_from = [G.name|from|]

_geommin :: G.Name
_geommin = [G.name|geommin|]

_nband :: G.Name
_nband = [G.name|nband|]

_st_d_within_geography_input :: G.Name
_st_d_within_geography_input = [G.name|st_d_within_geography_input|]

_st_d_within_input :: G.Name
_st_d_within_input = [G.name|st_d_within_input|]

_st_intersects_geom_nband_input :: G.Name
_st_intersects_geom_nband_input = [G.name|st_intersects_geom_nband_input|]

_st_intersects_nband_geom_input :: G.Name
_st_intersects_nband_geom_input = [G.name|st_intersects_nband_geom_input|]

_st_dwithin_input :: G.Name
_st_dwithin_input = [G.name|st_dwithin_input|]

_use_spheroid :: G.Name
_use_spheroid = [G.name|use_spheroid|]

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

-- ** Conflicts

_constraint :: G.Name
_constraint = [G.name|constraint|]

_if_matched :: G.Name
_if_matched = [G.name|if_matched|]

_match_columns :: G.Name
_match_columns = [G.name|match_columns|]

_on_conflict :: G.Name
_on_conflict = [G.name|on_conflict|]

_update_columns :: G.Name
_update_columns = [G.name|update_columns|]

__constraint :: G.Name
__constraint = [G.name|_constraint|]

__if_matched :: G.Name
__if_matched = [G.name|_if_matched|]

__on_conflict :: G.Name
__on_conflict = [G.name|_on_conflict|]

__update_column :: G.Name
__update_column = [G.name|_update_column|]

__PLACEHOLDER :: G.Name
__PLACEHOLDER = [G.name|_PLACEHOLDER|]

-- * Mutations

_insert :: G.Name
_insert = [G.name|insert|]

_update :: G.Name
_update = [G.name|update|]

_updates :: G.Name
_updates = [G.name|updates|]

_delete :: G.Name
_delete = [G.name|delete|]

_affected_rows :: G.Name
_affected_rows = [G.name|affected_rows|]

_data :: G.Name
_data = [G.name|data|]

_object :: G.Name
_object = [G.name|object|]

_objects :: G.Name
_objects = [G.name|objects|]

_one :: G.Name
_one = [G.name|one|]

_many :: G.Name
_many = [G.name|many|]

_returning :: G.Name
_returning = [G.name|returning|]

_transaction :: G.Name
_transaction = [G.name|transaction|]

__append :: G.Name
__append = [G.name|_append|]

__arr_rel_insert_input :: G.Name
__arr_rel_insert_input = [G.name|_arr_rel_insert_input|]

__delete_at_path :: G.Name
__delete_at_path = [G.name|_delete_at_path|]

__delete_elem :: G.Name
__delete_elem = [G.name|_delete_elem|]

__delete_key :: G.Name
__delete_key = [G.name|_delete_key|]

__insert_input :: G.Name
__insert_input = [G.name|_insert_input|]

__insert_match_column :: G.Name
__insert_match_column = [G.name|_insert_match_column|]

__mutation_response :: G.Name
__mutation_response = [G.name|_mutation_response|]

__prepend :: G.Name
__prepend = [G.name|_prepend|]

__obj_rel_insert_input :: G.Name
__obj_rel_insert_input = [G.name|_obj_rel_insert_input|]

-- * Mutation subscriptions

_created_at :: G.Name
_created_at = [G.name|created_at|]

_errors :: G.Name
_errors = [G.name|errors|]

_output :: G.Name
_output = [G.name|output|]

-- * Connection

_Connection :: G.Name
_Connection = [G.name|Connection|]

_Edge :: G.Name
_Edge = [G.name|Edge|]

_PageInfo :: G.Name
_PageInfo = [G.name|PageInfo|]

_batch_size :: G.Name
_batch_size = [G.name|batch_size|]

_connection :: G.Name
_connection = [G.name|connection|]

_cursor :: G.Name
_cursor = [G.name|cursor|]

_cursor_ordering :: G.Name
_cursor_ordering = [G.name|cursor_ordering|]

_edges :: G.Name
_edges = [G.name|edges|]

_initial_value :: G.Name
_initial_value = [G.name|initial_value|]

_node :: G.Name
_node = [G.name|node|]

_ordering :: G.Name
_ordering = [G.name|ordering|]

_pageInfo :: G.Name
_pageInfo = [G.name|pageInfo|]

_stream :: G.Name
_stream = [G.name|stream|]

_startCursor :: G.Name
_startCursor = [G.name|startCursor|]

_endCursor :: G.Name
_endCursor = [G.name|endCursor|]

_hasNextPage :: G.Name
_hasNextPage = [G.name|hasNextPage|]

_hasPreviousPage :: G.Name
_hasPreviousPage = [G.name|hasPreviousPage|]

__connection :: G.Name
__connection = [G.name|_connection|]

__stream_cursor_input :: G.Name
__stream_cursor_input = [G.name|_stream_cursor_input|]

__stream_cursor_value_input :: G.Name
__stream_cursor_value_input = [G.name|_stream_cursor_value_input|]

-- * Relay

_Node :: G.Name
_Node = [G.name|Node|]

-- * Introspection

___hasura_internal_typename :: G.Name
___hasura_internal_typename = [G.name|__hasura_internal_typename|]

-- * Apollo Federation

__service :: G.Name
__service = [G.name|_service|]

_key :: G.Name
_key = [G.name|key|]

_fields :: G.Name
_fields = [G.name|fields|]

_representations :: G.Name
_representations = [G.name|representations|]

__Any :: G.Name
__Any = [G.name|_Any|]

_sdl :: G.Name
_sdl = [G.name|sdl|]

__Service :: G.Name
__Service = [G.name|_Service|]

__Entity :: G.Name
__Entity = [G.name|_Entity|]

__entities :: G.Name
__entities = [G.name|_entities|]

-- * Aggregation Predicates

_arguments :: G.Name
_arguments = [G.name|arguments|]

_predicate :: G.Name
_predicate = [G.name|predicate|]

_filter :: G.Name
_filter = [G.name|filter|]

-- * Arrays

__array :: G.Name
__array = [G.name|_array|]
