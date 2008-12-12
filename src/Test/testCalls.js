function deepEqual(lhs,rhs) {
  if (typeof(rhs) == "object") {
    for (var ix in rhs) {
      if (!deepEqual(lhs[ix],rhs[ix])) { return false; }
    }
    return true;
  }
  else {
    return lhs == rhs;
  }
}

function test(resultThunk,expected) {
  try {
    var result = resultThunk();
    if (deepEqual(result,expected)) {
      return true;
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
    if (e == expectedMsg) {
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
testExn(sub1Broken(10),"flat contract violation");
test(makeCoords(50,60), { x: 50, y: 60 });
test(div(50,5), 10);
testExn(div(20,0), "flat contract violation");
