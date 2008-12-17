function deepEqual(lhs,rhs) { // somewhat akin to Scheme's equal
  if (typeof(rhs) == "object") {
    for (var ix in rhs) {
      if (!deepEqual(lhs[ix],rhs[ix])) { return false; }
    }
    return true;
  }
  if (rhs instanceof Array) {
    if (!(lhs instanceof Array)) {
      return false; 
    }
    else if (lhs.length != rhs.length) {
      return false;
    }
    else {
      for (var i = 0; i < rhs.length; i++) {
        if (!deepEq(lhs[i], rhs[i])) { return false; }
      }
      return true;
   }
  }
  else {
    return lhs == rhs;
  }
}

function test(resultThunk,expected) {
  try {
    var result = resultThunk();
    if (deepEqual(result,expected)) {
      return result;
    }
    else {
      print("Expected " + expected + "; result was " + result);
      throw "test failed";
    }
  }
  catch(e) {
    print("Expected " + expected + "; exception raised: " + e);
      throw "test failed";
  }
};

function testExn(resultThunk,expectedMsg) {
  try {
    var result = resultThunk();
    print("Expected exception " + expectedMsg + "; evaluated to " + result); 
  }
  catch(e) {
    if ((e.guilty && e.guilty.match(expectedMsg)) || 
        (e.match && e.match(expectedMsg))) {
      return true;
    }
    else {
     print("Excepted exception " + expectedMsg + "; got exception " + e);
     throw "test failed";
    }
  }
  throw "test failed";
}

// These get "macro-expanded" to thunk the result.
test(add(2,3), 5);
testExn(sub1Broken(10),"sub1Broken");
test(div(50,5), 10);
testExn(div(20,0), "client");

test(filter(function(x) { return x == 0; }, [1,2,0,3,0]), [0, 0]);
testExn(filter(function(x) { return x; }, [1,2,3]), "client");

test(curry(function(x,y) { return x + y; },50)(20), 70);

testExn(curry(function(x,y) { return "tooth fairy"; },50)(20), 
        "client");
testExn(curry("tooth fairy",23), "client");
testExn(curry(function(x,y) { throw "did not expect an arg"; },"tooth fairy")
             (23), 
        "client");
testExn(curry(function(x,y) { throw "did not expect an arg"; },50)
             ("tooth fairy"), 
        "client");

var myCoords = test(makeCoords(50,60), { x: 50, y: 60 });
test(myCoords.moveRight(), { x: 51, y: 60 });

test(mkPair(23,545), [23,545]);
test(iota(10),[0,1,2,3,4,5,6,7,8,9]);
test(sum(1,2,3,4,5,6,7,8,9,10), 55);
testExn(sum(1,2,3, "tooth fairy", 4, 5), "client");
test(sum(), 0);

test(reduceNumbers(0,sum,[1,2,3,4]),10);
testExn(reduceNumbers(0,function(x,y) { return y == 3 ? "tooth fairy" : x+y; },
                      [0,1,2,3,4,5]), "client");

test(moveCoords(new Coords(10,20)), { x: 11, y: 21 }); 
// The exception here is _not_ a contract violation.  The contract is under-
// specified and moveCoords itself throws the exception (hence the odd string).
testExn(moveCoords({ x: 10, y: 20 }), "expected instanceof Coords");
test(forever(10).val,10);
test(forever(10).next(20).val,20);
test(forever(10).next(20).next(30).val,30);
