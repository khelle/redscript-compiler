puts = function(msg) { console.log(msg); };Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};

a= 5;
b= 0xff;
c= "to jest String";
d= true;
e= null;
const f= void 0;
const g= {"a": "A","b": "B"};
const h= ["C","D"];
cpy= arr= [a,b,c,d,null,f,g,h];
for(var _i=0, _limit=(cpy).length; _i<_limit; _i++) {
var record = cpy [_i];
puts(record);
};

