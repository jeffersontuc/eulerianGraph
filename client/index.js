var express = require('express');
var app = express();

app.use('/', express.static(__dirname));

app.get('/', function(req, res){
    res.sendfile('./index.html');
});

app.listen(9090, function(){
    console.log('Servidor rodando na porta 9090');
});
