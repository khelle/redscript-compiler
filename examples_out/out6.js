puts = function(msg) { console.log(msg); };Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};

fruits= ["Apple","Banana","Orange","Raspberry"];
puts("===================================");
i= 0;
while (i < fruits.length) {
puts(fruits[i]);
i= i + 1;
};
puts("===================================");
i= 0;
while(!(i === fruits.length)) {
puts(fruits[i]);
i= i + 1;
};
puts("===================================");
for(var key in fruits) {
puts(fruits[key]);
};
puts("===================================");
for(var key in fruits) {
var fruit = fruits [key];
puts(fruit);
};
puts("===================================");
for(var _i=0, _limit=(fruits).length; _i<_limit; _i++) {
var fruit = fruits [_i];
puts(fruit);
};

