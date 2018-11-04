"use strict";

//Array (Tuple a b) -> Array c -> Array c
exports.remapIndexes = function(map) {
  return function(list) {
    var newList = [];
    for (var i = 0; i < map.length; i++){
      var a = map[i].value0;
      var b = map[i].value1;
      newList[b] = list[a]    
    }
    return newList;
  }
}
