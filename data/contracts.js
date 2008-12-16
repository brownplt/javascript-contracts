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

contracts.blame = function(x) {
  throw x;
}

contracts.unsizedArray = function(elt) {
  return {
    server: function(s) {
      return function(val) {
        if (val instanceof Array) {
          return contracts.map(elt.server(s),val);
        }
        else {
          contracts.blame(s);
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
          contracts.blame(s);
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

contracts.flat = function(pred) {
  return {
    server: function(s) {
      return function(val) {
        if (pred(val)) { return val; }
        else { contracts.blame(s); }
      };
    },
    client: function(s) {
      return function(val) { return val; };
    }
  };
};

contracts.isUndefined = contracts.flat(function(val) { 
  return val === undefined;
});

contracts.varArityFunc = function(fixedArgs,restArgs,result) {
  return {
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
        else { contracts.blame(s); }
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

contracts.func = function() {
  var args = [ ];
  var result = arguments[arguments.length - 1];
  for (var i = 0; i < arguments.length - 1; i++) {
    args.push(arguments[i]);
  }

  return {
    server: function(s) {
      return function(proc) {
        if (typeof(proc) == "function") {
          return function() {
            var guardedArgs = [ ];
            for (var i = 0; i < arguments.length; i++) {
              guardedArgs.push(args[i].client(s)(arguments[i]));
            }
            return result.server(s)(proc.apply(this, guardedArgs));
          };
        }
        else { contracts.blame(s); }
      }
    },
    client: function(s) {
      return function(proc) {
        if (typeof(proc) == "function") {
          return function() {
            var guardedArgs = [ ];
            for (var i = 0; i < arguments.length; i++) {
              guardedArgs.push(args[i].server(s)(arguments[i]));
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

contracts.obj = function(sig) {
  return {
    server: function(s) {
      return function (obj) {
        var guardedObj = { };
        for (var field in sig) {
          guardedObj[field] = sig[field].server(s)(obj[field]);
        }
        return guardedObj;
      };
    },
    client: function(s) {
      return function (obj) {
        var guardedObj = { };
        for (var field in sig) {
          guardedObj[field] = sig[field].client(s)(obj[field]);
        }
        return guardedObj;
      };
    }
  };
};

contracts.guard = function(ctc,val,pos,neg) {
  return ctc.client(neg)(ctc.server(pos)(val));
};

