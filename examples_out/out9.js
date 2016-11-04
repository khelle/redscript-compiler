puts = function(msg) { console.log(msg); };Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};

console.log('test');
test= null;
test= void 0;
test= 0xff;
arr= [5,2,"a"];
test= {"name": "TestModule","val": 5};
delete test;
a= "A";
b= "B";
c= b= a;
if ((true && (a === 5 | b === "lemur") && h++ || --c || typeof h === "String")) {
val= 0;
} else {
val= 1;
};
var SomeObj = Class.extend({},{
param: "test"});
A = function() {
return  "A";
};
C = function() {
return  "C";
};
D = function() {
return  "D";
};
const con= new SomeObj();
doSmthA = function() {
myOwn= 55;
return  (myOwn) ? true : false;
};
myLabel :
tmp= "this is label";
;
(function() {
tmp= "this is private";
})();
throw  "Error";
try {
v= "a";
} catch (errObj) {
v= "b";
} finally {
v= "c";
};
with (A) {
nop;
};
debugger;
var Animal = Class.extend({},{
HearMe: function() {
return  "nothing";
}
});
var Dog = Class.extend(Animal.prototype,{
name: "Dog",HearMe: function() {
return  this;
}
});
for(var _i=0, _limit=(t).length; _i<_limit; _i++) {
var val = t [_i];
continue;
};
for(var key in t) {
};
for(var key in t) {
var val = t [key];
break;
};
while (somthTrue) {
doSmth();
};
while(!(somthTrue)) {
doSmth();
};

