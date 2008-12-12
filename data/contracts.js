var boilerplate = true;


var contracts = { };
contracts.blame = function(x) {
  throw x;
}

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

