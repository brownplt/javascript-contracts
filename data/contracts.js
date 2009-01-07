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

contracts.blame = function(guilty,expected,received,message,loc) {
  var guiltyMsg = typeof(guilty) == "string" 
                    ? guilty : guilty.value;
  var msg = guiltyMsg + " violated the contract at " + loc + "; expected " + 
             expected + " but received " + received + "; " + message;
  var err = new Error(msg);
  err.guilty = msg;
  err.blamed = guiltyMsg;
  err.expected = expected;
  err.received = received;
  err.guardLoc = loc;
  try { console.log(err); } catch(_) { };
  throw err;
}

// The type of a contract combinator is: name -> args ... -> contract
//
// name is a human-readable name for the contract.  args .. are constructor-
// specified arguments and contract is the resulting contract.
 

contracts.flat = function(name) {
  return function(pred) {
    return {
      flat: function(val) { return pred(val); },
      server: function(s,loc) {
        return function(val) {
          if (flat(val)) { 
            return val; 
            }
          else { 
            contracts.blame(s,name,val,"does not satisfy the predicate",loc);
         }
        };
      },
      client: function(s,loc) {
        return function(val) { return val; };
      }
    };
  };
};


contracts.unsizedArray = function(name) {
  return function(elt) {
    return {
      flat: function(val) {
        return val instanceof Array;
        for (var i = 0; i < val.length; i++) {
          if (!(elt.flat(val[i]))) { return false; }
        }
        return true;
      },
      server: function(s,loc) {
        return function(val) {
          if (val instanceof Array) {
            return contracts.map(elt.server(s,loc),val);
          }
          else {
            contracts.blame(s, name, val, "not an array",loc);
          }
        };
      },
      client: function(s,loc) {
        return function(val) {
          if (val instanceof Array) {
            return contracts.map(elt.client(s,loc),val);
          }
          else {
            return val;
          }
        }
      }
    };
  };
};

contracts.fixedArray = function(name) {
  return function() {
    var elts = arguments;
    return {
      flat: function(val) {
        if  (!(val instanceof Array && val.length == elts.length)) {
          return false;
        }
        for (var i = 0; i < val.length; i++) {
          if (!elts[i].flat(val[i])) { return false; }
        }
        return true;
      },
      server: function(s,loc) {
        return function(val) {
          if (val instanceof Array && val.length == elts.length) {
            var result = [ ];
            for (var i = 0; i < elts.length; i++) {
              result.push(elts[i].server(s,loc)(val[i]));
            }
            return result;
          }
          else {
            contracts.blame(s,name,val,"not an array of the right size",loc);
          }
        };
      },
      client: function(s,loc) {
        return function(val) {
          if (val instanceof Array && val.length == elts.length) {
            var result = [ ];
            for (var i = 0; i < elts.length; i++) {
              result.push(elts[i].client(s,loc)(val[i]));
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
};

contracts.isUndefined = contracts.flat(function(val) { 
  return val === undefined;
});

contracts.varArityFunc = function(name) {
  return function(fixedArgs,restArgs,result) {
    return {
      isHigherOrder: true,
      flat: function(val) { return typeof(val) == "function"; },
      server: function(s,loc) {
        return function(proc) {
          if (typeof(proc) == "function") {
            return function() {
              var guardedArgs = contracts.zipWith(function(ctc,arg) {
                return ctc.client(s,loc)(arg);
              }, fixedArgs, arguments);
              for (var i = fixedArgs.length; i < arguments.length; i++) {
                guardedArgs.push(restArgs.client(s,loc)(arguments[i]));
              }
              return result.server(s,loc)(proc.apply(this, guardedArgs));
            };
          }
          else { contracts.blame(s,name, proc,"not a function",loc); }
        }
      },
      client: function(s,loc) {
        return function(proc) {
          if (typeof(proc) == "function") {
            return function() {
              var guardedArgs = contracts.zipWith(function(ctc,arg) {
                return ctc.server(s,loc)(arg);
              }, fixedArgs, arguments);
              for (var i = fixedArgs.length; i < arguments.length; i++) {
                guardedArgs.push(restArgs.server(s,loc)(arguments[i]));
              }
              return result.client(s,loc)(proc.apply(this, guardedArgs));
            };
          }
          else {
            return proc;
          }
        };
      }
    };
  };
};

// Ensures value is an instanceof constr before checking that its shape
// matches sig. constrId is a human-readable name for the constructor.
contracts.instance = function(name) {
  return function(constr, sig) {
    return {
      flat: function(val) {
        return (typeof(val) == "object" || typeof(val) == "function") &&
               (val instanceof constr) && sig.flat(val);
      },
      server: function(s,loc) { return function(val) {
        if ((typeof(val) == "object" || typeof(val) == "function") && 
            (val instanceof constr)) {
          return sig.server(s,loc)(val);
        }
        else {
          contracts.blame(s, name, val, "wrong instance",loc);
        }
      } },
      client: function(s,loc) { return function(val) {
        if ((typeof(val) == "object" || typeof(val) == "function") && 
            (val instanceof constr)) {
          return sig.client(s,loc)(val);
        }
        else {
          return val;
        }
      } }
    };
  };
};

contracts.obj = function(name) {
  return function(sig) {
    return {
      flat: function(val) {
        for (var field in sig) {
          if (!(sig[field].flat(val[field]))) { return false; }
        }
        return true;
      },
      server: function(s,loc) {
        return function (obj) {
          var constr = function() { };
          constr.prototype = obj;
          var guardedObj = new constr();
          
          for (var field in sig) {
            guardedObj[field] = sig[field].server(s,loc)(obj[field]);
          }
          return guardedObj;
        };
      },
      client: function(s,loc) {
        return function (obj) {
          var constr = function() { };
          constr.prototype = obj;
          var guardedObj = new constr();
          
          for (var field in sig) {
            guardedObj[field] = sig[field].client(s,loc)(obj[field]);
          }
          return guardedObj;
        };
      }
    };
  };
};

// pos is the name of val.  neg should be the name of the calling context.
// Since we do not rewrite call-sites, neg is simply "client".
// However, if ctc.isHigherOrder, we can determine the name of the calling
// context by examining the stack when val is called.
// if !ctc.isHigherOrder, the name of the calling context is the name of the
// definition point (neg)
contracts.guard = function(ctc,val,pos,neg,loc) {
  if (ctc.isHigherOrder) {
    if (typeof(val) != "function") {
      contracts.blame(pos, "a function", proc, "not a function","guard"); 
    }
    else {
      var deferredNeg = { value: "not called" };
      var fn = ctc.client(deferredNeg,loc)(ctc.server(pos,loc)(val));
      return function() {
        deferredNeg.value = contracts.stackTrace();
        return fn.apply(this,arguments);
      };
    }
  }
  else {
    return ctc.client(neg,loc)(ctc.server(pos,loc)(val));
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

  return callstack.length == 0 ? "client" : callstack.join(" ");
};
