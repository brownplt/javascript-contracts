if (add(2,3) != 5) { throw "wrong answer"; }

try {
  sub1Broken(23);
  throw "expected contract violation";
}
catch (_) { }
