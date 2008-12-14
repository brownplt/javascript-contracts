thunk = function() { return 23; }

add1 = function(x) { return x + 1; }
sub1Broken = function(x) { return "tooth fairy"; }
add = function(x,y) { return x + y; };
div = function(x,y) { return x / y; };

privateFunction = function() { 
  throw "This is a private function"; 
}

makeCoords = function(x,y) {
  return { x: x, y: y };
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
