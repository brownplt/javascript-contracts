window = {};
(function ()
 {
   var impl = {};
   (function ()
    {
      this.thunk = function ()
                   {
                     return 23.0;
                   };
      this.add1 = function (x)
                  {
                    return x + 1.0;
                  };
      this.sub1Broken = function (x)
                        {
                          return "tooth fairy";
                        };
      this.add = function (x, y)
                 {
                   return x + y;
                 };
      this.div = function (x, y)
                 {
                   return x / y;
                 };
      this.privateFunction = function ()
                             {
                               throw "This is a private function";
                             };
    }).apply(impl, []);
   {
     function isNumber(x)
     {
       return typeof (x) == "number";
     }
     function isString(x)
     {
       return typeof (x) == "string";
     }
   }
   window.thunk = function (proc)
                  {
                  if (typeof proc == "function")
                    return function ()
                           {
                           return (function (val)
                                   {
                                   if (isNumber(val))
                                     return val;
                                   else
                                     throw "contract violation";
                                   })(proc());
                           };
                  else
                    throw "contract violation";
                  }(impl.thunk);
   window.add1 = function (proc)
                 {
                 if (typeof proc == "function")
                   return function (arg0)
                          {
                          return (function (val)
                                  {
                                  if (isNumber(val))
                                    return val;
                                  else
                                    throw "contract violation";
                                  })(proc((function (val)
                                           {
                                           if (isNumber(val))
                                             return val;
                                           else
                                             throw "contract violation";
                                           })(arg0)));
                          };
                 else
                   throw "contract violation";
                 }(impl.add1);
   window.sub1Broken = function (proc)
                       {
                       if (typeof proc == "function")
                         return function (arg0)
                                {
                                return (function (val)
                                        {
                                        if (isNumber(val))
                                          return val;
                                        else
                                          throw "contract violation";
                                        })(proc((function (val)
                                                 {
                                                 if (isNumber(val))
                                                   return val;
                                                 else
                                                   throw "contract violation";
                                                 })(arg0)));
                                };
                       else
                         throw "contract violation";
                       }(impl.sub1Broken);
   window.add = function (proc)
                {
                if (typeof proc == "function")
                  return function (arg0, arg1)
                         {
                         return function (proc)
                                {
                                if (typeof proc == "function")
                                  return function ()
                                         {
                                         return (function (val)
                                                 {
                                                 if (isNumber(val))
                                                   return val;
                                                 else
                                                   throw "contract violation";
                                                 })(proc());
                                         };
                                else
                                  throw "contract violation";
                                }(proc((function (val)
                                        {
                                        if (isNumber(val))
                                          return val;
                                        else
                                          throw "contract violation";
                                        })(arg0), (function (val)
                                                   {
                                                   if (isNumber(val))
                                                     return val;
                                                   else
                                                     throw "contract violation";
                                                   })(arg1)));
                         };
                else
                  throw "contract violation";
                }(impl.add);
   window.div = function (proc)
                {
                if (typeof proc == "function")
                  return function (arg0, arg1)
                         {
                         return function (proc)
                                {
                                if (typeof proc == "function")
                                  return function ()
                                         {
                                         return (function (val)
                                                 {
                                                 if (isNumber(val))
                                                   return val;
                                                 else
                                                   throw "contract violation";
                                                 })(proc());
                                         };
                                else
                                  throw "contract violation";
                                }(proc((function (val)
                                        {
                                        if (isNumber(val))
                                          return val;
                                        else
                                          throw "contract violation";
                                        })(arg0), (function (val)
                                                   {
                                                   if (function (x)
                                                       {
                                                         x != 0.0;
                                                       }(val))
                                                     return val;
                                                   else
                                                     throw "contract violation";
                                                   })(arg1)));
                         };
                else
                  throw "contract violation";
                }(impl.div);
 })();
