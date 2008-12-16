thunk = function() { return 23; }

add1 = function(x) { return x + 1; }
sub1Broken = function(x) { return "tooth fairy"; }
add = function(x,y) { return x + y; };
div = function(x,y) { return x / y; };

privateFunction = function() { 
  throw "This is a private function"; 
}

moveRight = function() {
  var moved = makeCoords(this.x, this.y);
  moved.x = moved.x + 1;
  return moved;
}

makeCoords = function(x,y) {
  return { x: x, y: y, moveRight : moveRight };
}

filter = function(f,arr) {
  var result = [ ];
  for (var i = 0; i < arr.length; i++) {
    if (f(arr[i])) { result.push(arr[i]); }
  }
  return result;
}

curry = function(f,x) {
  return function(y) {
    return f(x,y);
  };
}

mkPair = function(x,y) {
  return [x,y];
};
