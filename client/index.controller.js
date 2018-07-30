app.controller('indexController', function(indexService){
    var vm = this;

    vm.title = 'Eulerian Graph';

    var getHello = function(){
        indexService.getHello().then(function(response){
            console.log(response.data);
        })
    }

    getHello();
});