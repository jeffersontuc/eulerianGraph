app.service('indexService', function($http){
    var service = this;

    service.getHello = getHello;

    var url = 'http://localhost:3000/';


    function getHello(){
        return $http.get(url);
    }

    return service;
})