package example;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class HelloTest {

    @Test
    void handleRequestDelete() throws IOException {
        Map<String, Object> request = new HashMap<>();
        request.put("body", loadResource("delete.json"));

        Map<String, Object> response = new Hello().handleRequest(request, new ContextStub());

        assertEquals(200, response.get("statusCode"));
        assertEquals("Note 1 deleted, with data: a", response.get("body"));
    }

    private String loadResource(String name) {
        return new BufferedReader(new InputStreamReader(
                ClassLoader.getSystemResourceAsStream(name)
        ))
                .lines()
                .collect(Collectors.joining("\n"));
    }

}