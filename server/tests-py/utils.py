from graphql.language import ast
from graphql.language.printer import print_ast

# Convert json value to its corresponding graphql representation
def json_to_graphql_ast(data):
    gqlTyMap = {
        str: ast.StringValue,
        bool: ast.BooleanValue,
        int: ast.IntValue,
        float: ast.FloatValue
    }
    for ty in gqlTyMap:
        if isinstance(data, ty):
            return gqlTyMap[ty](data)

    if isinstance(data, dict):
        field_nodes = []
        for key, val in data.items():
            assert isinstance(key, str)
            field_nodes.append(
                ast.ObjectField(
                    name=ast.Name(key),
                    value=json_to_graphql_ast(val)
                )
            )
        return ast.ObjectValue(fields=field_nodes)

    if isinstance(data, list):
        value_nodes = [ json_to_graphql_ast(x) for x in data ]
        return ast.ListValue(values=value_nodes)

def graphql_dumps(data):
    dataAst = json_to_graphql_ast(data)
    return print_ast(dataAst)
