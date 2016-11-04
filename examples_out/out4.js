puts = function(msg) { console.log(msg); };Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};

Sum = function(num) {
if (num === 0) {
return  0;
} else {
return  num + Sum(num - 1);
};
};
puts(Sum(10));
(function() {
a= 10;
puts(a);
})();
myOwnLabel :
puts("This is labelled block");
;
animal= 'cat';
switch(animal) {
case 'dog' :
puts('woof');
break;
case 'lizard' :
puts('...');

break;
case 'lion' :
case 'cat' :
puts('meow');
break;
default :
puts('undefined animal');
};
a= 10;
try {
if (a > 5) {
throw  "This is Error message.";
};
} catch (err) {
puts("there was error : " + err);
} finally {
puts("closing app");
};
a= {prop: "Hidden Property"};
with (a) {
puts(prop);
};

