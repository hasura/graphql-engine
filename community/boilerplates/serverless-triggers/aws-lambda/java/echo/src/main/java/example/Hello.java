package example;

import java.util.HashMap;
import java.util.Map;

import com.amazonaws.services.lambda.runtime.Context;
import com.amazonaws.services.lambda.runtime.RequestHandler;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;

public class Hello implements RequestHandler<Map<String, Object>, Map<String, Object>> {

    private static final int INTERNAL_SERVER_ERROR = 500;
    private final Notebook notebook;

    public Hello() {
        this.notebook = new Notebook();
    }

    @Override
    public Map<String, Object> handleRequest(Map<String, Object> request, Context context) {
        context.getLogger().log(request.toString());
        Map<String, Object> response = new HashMap<>();
        response.put("isBase64Encoded", false);
        try {
            String requestBody = (String) request.get("body");
            response.put("body", handleRequestBody(requestBody));
            response.put("statusCode", 200);
        } catch (Exception e) {
            response.put("body", e.toString());
            response.put("statusCode", INTERNAL_SERVER_ERROR);
        }
        return response;
    }

    private String handleRequestBody(String requestBody) {
        DocumentContext body = JsonPath.parse(requestBody);
        String table = body.read("$.table.name");
        if (table != null && table.equals("notes")) {
            Notebook.Operation operation = Notebook.Operation.valueOf(body.read("$.event.op"));
            Note newNote = body.read("$.event.data.new", Note.class);
            Note oldNote = body.read("$.event.data.old", Note.class);
            return notebook.handleOperation(operation, newNote, oldNote);
        } else {
            throw new UnsupportedOperationException("table: " + table);
        }
    }
}