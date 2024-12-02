# execute

Given an execution plan defined with the types in `plan-types`, execute all NDC
queries, remote predicates and remote joins, and return the `ndc_models::RowSet`
results. Each frontend is responsible for turning this into their own output
types.
