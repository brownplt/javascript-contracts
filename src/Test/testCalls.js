if (add(2,3) != 5) { throw "wrong answer"; }

try {
  sub1Broken(23);
  throw "expected contract violation";
}
catch (_) { }

var pos = makeCoords(50,60);

if (pos.x != 50 && pos.y != 60) { throw "wrong answer"; }

if (div(50,5) != 10) { throw "wrong answer"; }

try {
  div(20,0);
  throw "expected contract violation";
}
catch (_) { }
