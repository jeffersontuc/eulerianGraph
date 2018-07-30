app.service('indexService', function($http){
    var service = this;

    service.getHello = getHello;
    service.getNumberEdges = getNumberEdges;
    service.getNumberVertex = getNumberVertex;
    service.getDegrees = getDegrees;

    var url = 'http://localhost:3000/';


    function getHello(){
        return $http.get(url);
    }

    function getNumberEdges(){
        return $http.get(url + 'edges');
    }

    function getNumberVertex(){
        return $http.get(url + 'vertex');
    }

    function getDegrees(){
        return $http.get(url + 'degrees');
    }

    return service;
})