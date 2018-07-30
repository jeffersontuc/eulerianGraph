app.controller('indexController', function(indexService){
    var vm = this;

    vm.title = 'Eulerian Graph';

    var getHello = function(){
        indexService.getNumberEdges().then(function(response){
            vm.numberEdges = response.data;
        })

        indexService.getNumberVertex().then(function(response){
            vm.numberVertex = response.data;
        })

        indexService.getDegrees().then(function(response){
            vm.degrees = response.data;
            console.log(vm.degrees);
        })

        indexService.getResult().then(function(response){
            console.log(response.data);
        })
    }

    getHello();
});