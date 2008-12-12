this.thunk = function() { return 23; }

this.add1 = function(x) { return x + 1; }
this.sub1Broken = function(x) { return "tooth fairy"; }
this.add = function(x,y) { return x + y; };
this.div = function(x,y) { return x / y; };

this.privateFunction = function() { 
  throw "This is a private function"; 
}

this.makeCoords = function(x,y) {
  return { x: x, y: y };
}
