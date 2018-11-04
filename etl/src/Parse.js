"use strict";

var Data_List = require("../Data.List/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");


//Array (Tuple String String) -> String -> Array String
exports.addString = function(list) {
  return function(end) {
    
    console.log('list', list[0].value0, list[0].value1)
    console.log('end', end)
    return new Array(list[0] + end);
  }
}

//List (Tuple a b) -> List c -> List c
exports.remapIndexes = function(map) {
  //console.log('map', map)
  return function(list) {
    //console.log('list', list)
    var newList = [];
    for (var i = 0; i < map.length; i++){
      var a = map[i].value0;
      var b = map[i].value1;
      newList[b] = list[a]    
    }
    return newList;
  }
}
// var oldList = [4,5,6]
// var newList = []
// for (var (Tuple a b) in [Tuple 0 1, Tuple 1 2, Tuple 2 0]){
//   newList[b] = oldList[a]
// }