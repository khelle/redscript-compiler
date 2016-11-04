puts = function(msg) { console.log(msg); };Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};

a= process.argv[2];
b= process.argv[3];
h= 1;
c= 1;
if ((true && (a === 5 || b === "haslo") && h-- && ++c && h instanceof Array === false)) {
val= 1;
} else {
val= 0;
};
puts("status operacji = " + val);
h= 1;
c= 1;
val= ((true && (a === 5 || b === "haslo") && h-- && ++c && h instanceof Array === false)) ? 1 : 0;
puts("status operacji = " + val);

