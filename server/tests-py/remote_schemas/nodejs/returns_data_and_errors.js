const express = require('express');
const path = require("path");
const fs = require('fs');
const app = express();
app.use(express.json());

app.post('/', (req, res) => {
    let fileName;
    switch(req.body.operationName) {
        case "IntrospectionQuery":
            fileName = 'introspection.json';
            break;
        default:
            switch(req.header('x-fake-operation-name')) {
                case "DataOnly":
                    fileName = 'data_only.json';
                    break;
                case "ErrorOnly":
                    fileName = 'error_only.json';
                    break;
                case "DataAndError":
                    fileName = 'data_and_error.json';
                    break;
                default:
                    throw new Error("expected a header `x-fake-operation-name` to be from the list [DataOnly, ErrorOnly, DataAndError]");
            }
    }
    fs.readFile(path.resolve(__dirname, 'returns_data_and_errors_responses', fileName), 'utf8', (err, data) => {
        if (err) {
            console.error(err);
            return;
        }
        res.json(JSON.parse(data));
    });
});
let port = process.env.PORT || 4000;
app.listen(port, () => {
    console.log(`🚀 Server ready at http://localhost::${port}`);
});
