var contracts = { };

contracts.map = function(f,arr) {
  var dest = [ ];
  for (var i = 0; i < arr.length; i++) {
    dest.push(f(arr[i]));
  }
  return dest;
};

contracts.zipWith = function(f,arr1,arr2) {
  var dest = [ ];
  for (var i = 0; i < arr1.length; i++) {
    dest.push(f(arr1[i],arr2[i]));
  }
  return dest;
};

contracts.ContractViolationException  = function(guilty, expected, received, 
                                                 message) { 
  this.guilty = guilty;
  this.expected = expected;
  this.received = received;
  this.message = message;
};

contracts.ContractViolationException.prototype = { };
contracts.ContractViolationException.prototype.toString = function() {
  var guilty = typeof(this.guilty) == "string" 
                 ? this.guilty : this.guilty.value;
  return guilty + " violated a contract; expected " + this.expected
    + " but received " + this.received + "; " + this.message;
};
      

contracts.blame = function(guilty, expected, received, message) {
  throw new contracts.ContractViolationException(guilty,expected,received,
              message);
}

contracts.flat = function(pred,predName) {
  return {
    pred: function(val) { return pred(val); },
    server: function(s) {
      return function(val) {
        if (pred(val)) { return val; }
        else { contracts.blame(s,predName, val, 
                               "does not satisfy the predicate"); }
      };
    },
    client: function(s) {
      return function(val) { return val; };
    }
  };
};


contracts.unsizedArray = function(elt) {
  return {
    pred: function(val) {
      return val instanceof Array;
      for (var i = 0; i < val.length; i++) {
        if (!(elt.flat(val[i]))) { return false; }
      }
      return true;
    },
    server: function(s) {
      return function(val) {
        if (val instanceof Array) {
          return contracts.map(elt.server(s),val);
        }
        else {
          contracts.blame(s,"[ " + elt + ", ... ]", val, "not an array");
        }
      };
    },
    client: function(s) {
      return function(val) {
        if (val instanceof Array) {
          return contracts.map(elt.client(s),val);
        }
        else {
          return val;
        }
      }
    }
  };
};

contracts.fixedArray = function() {
  var elts = arguments;
  return {
    pred: function(val) {
      if  (!(val instanceof Array && val.length == elts.length)) {
        return false;
      }
      for (var i = 0; i < val.length; i++) {
        if (!elts[i].flat(val[i])) { return false; }
      }
      return true;
    },
    server: function(s) {
      return function(val) {
        if (val instanceof Array && val.length == elts.length) {
          var result = [ ];
          for (var i = 0; i < elts.length; i++) {
            result.push(elts[i].server(s)(val[i]));
          }
          return result;
        }
        else {
          contracts.blame(s, elts, val, "not an array of the right size");
        }
      };
    },
    client: function(s) {
      return function(val) {
        if (val instanceof Array && val.length == elts.length) {
          var result = [ ];
          for (var i = 0; i < elts.length; i++) {
            result.push(elts[i].client(s)(val[i]));
          }
          return result;
        }
        else {
          return val;
        }
      }
    }
  };
};

contracts.isUndefined = contracts.flat(function(val) { 
  return val === undefined;
});

contracts.varArityFunc = function(fixedArgs,restArgs,result) {
  return {
    isHigherOrder: true,
    flat: function(val) { return typeof(val) == "function"; },
    server: function(s) {
      return function(proc) {
        if (typeof(proc) == "function") {
          return function() {
            var guardedArgs = contracts.zipWith(function(ctc,arg) {
              return ctc.client(s)(arg);
            }, fixedArgs, arguments);
            for (var i = fixedArgs.length; i < arguments.length; i++) {
              guardedArgs.push(restArgs.client(s)(arguments[i]));
            }
            return result.server(s)(proc.apply(this, guardedArgs));
          };
        }
        else { contracts.blame(s, "a function", proc, "not a function"); }
      }
    },
    client: function(s) {
      return function(proc) {
        if (typeof(proc) == "function") {
          return function() {
            var guardedArgs = contracts.zipWith(function(ctc,arg) {
              return ctc.server(s)(arg);
            }, fixedArgs, arguments);
            for (var i = fixedArgs.length; i < arguments.length; i++) {
              guardedArgs.push(restArgs.server(s)(arguments[i]));
            }
            return result.client(s)(proc.apply(this, guardedArgs));
          };
        }
        else {
          return proc;
        }
      };
    }
  };
};

// Ensures value is an instanceof constr before checking that its shape
// matches sig. constrId is a human-readable name for the constructor.
contracts.instance = function(constr, constrId, sig) {
  return {
    flat: function(val) {
      return (typeof(val) == "object" || typeof(val) == "function") &&
             (val instanceof constr) && sig.flat(val);
    },
    server: function(s) { return function(val) {
      if ((typeof(val) == "object" || typeof(val) == "function") && 
          (val instanceof constr)) {
        return sig.server(s)(val);
      }
      else {
        contracts.blame(s, "instance of " + constrId, val, "wrong instance");
      }
    } },
    client: function(s) { return function(val) {
      if ((typeof(val) == "object" || typeof(val) == "function") && 
          (val instanceof constr)) {
        return sig.client(s)(val);
      }
      else {
        return val;
      }
    } }
  };
};

contracts.obj = function(sig) {
  return {
    flat: function(val) {
      for (var field in sig) {
        if (!(sig[field].flat(val[field]))) { return false; }
      }
      return true;
    },
    server: function(s) {
      return function (obj) {
        var constr = function() { };
        constr.prototype = obj;
        var guardedObj = new constr();
        
        for (var field in sig) {
          guardedObj[field] = sig[field].server(s)(obj[field]);
        }
        return guardedObj;
      };
    },
    client: function(s) {
      return function (obj) {
        var constr = function() { };
        constr.prototype = obj;
        var guardedObj = new constr();
        
        for (var field in sig) {
          guardedObj[field] = sig[field].client(s)(obj[field]);
        }
        return guardedObj;
      };
    }
  };
};

// pos is the name of val.  neg should be the name of the calling context.
// Since we do not rewrite call-sites, neg is simply "client".
// However, if ctc.isHigherOrder, we can determine the name of the calling
// context by examining the stack when val is called.
// if !ctc.isHigherOrder, the name of the calling context is the name of the
// definition point (neg)
contracts.guard = function(ctc,val,pos,neg) {
  if (ctc.isHigherOrder) {
    if (typeof(val) != "function") {
      contracts.blame(pos, "a function", proc, "not a function"); 
    }
    else {
      var deferredNeg = { value: "not called" };
      var fn = ctc.client(deferredNeg)(ctc.server(pos)(val));
      return function() {
        deferredNeg.value = contracts.stackTrace();
        return fn.apply(this,arguments);
      };
    }
  }
  else {
    return ctc.client(neg)(ctc.server(pos)(val));
  }
};

// Derived from http://eriwen.com/javascript/js-stack-trace/
contracts.stackTrace = function() {
  var callstack = [];
  var isCallstackPopulated = false;
  try {
      i.dont.exist+=0; //does not exist - that's the point
  } catch(e) {
      if (e.stack) { //Firefox
          var lines = e.stack.split("\n");
          for (var i = 0, len = lines.length; i < len; i++) {
              if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
                  callstack.push(lines[i]);
              }
          }
          //Remove call to printStackTrace()
          callstack.shift();
          isCallstackPopulated = true;
      }
      else if (window.opera && e.message) { //Opera
          var lines = e.message.split("\n");
          for (var i = 0, len = lines.length; i < len; i++) {
              if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
                  var entry = lines[i];
                  //Append next line also since it has the file info
                  if (lines[i+1]) {
                      entry += " at " + lines[i+1];
                      i++;
                  }
                  callstack.push(entry);
              }
          }
          //Remove call to printStackTrace()
          callstack.shift();
          isCallstackPopulated = true;
      }
  }
  if (!isCallstackPopulated) { //IE and Safari
      var currentFunction = arguments.callee.caller;
      while (currentFunction) {
          var fn = currentFunction.toString();
          //If we can't get the function name set to "anonymous"
          var fname = fn.substring(fn.indexOf("function") + 8, fn.indexOf("(")) || "anonymous";
          callstack.push(fname);
          currentFunction = currentFunction.caller;
      }
  }
  return callstack.join(" ");
};
