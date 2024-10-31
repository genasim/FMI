const http = require("http");

const PORT = 8080;

const fetchTemplateHandler = (req, res) => {

}

const handlers = {
    'fetchTemplate': fetchTemplateHandler,
}

const server = http.createServer((req, res) => {
    
});

server.listen(PORT);