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

var iota = function(n) {
  var result = [ ];
  for (var i = 0; i < n; i++) {
    result[i] = i;
  }
  return result;
};

sum = function() {
  var r = 0;
  for (var i = 0; i < arguments.length; i++) {
    r += arguments[i];
  }
  return r;
};

reduceNumbers = function(init,f,args) {
  for (var i = 0; i < args.length; i++) {
    init = f(init,args[i]);
  }
  return init;
};

// TODO: Constructor contract!
window.Coords = function(x,y) {
  this.x = x;
  this.y = y;
}

moveCoords = function(c) {
  if (c instanceof window.Coords) {
    return makeCoords(c.x + 1, c.y + 1);
  }
  else {
    throw "expected instanceof Coords"
  }
}
