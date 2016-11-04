puts = function(msg) { console.log(msg); };Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};

var Animal = Class.extend({},{
HearMe: function() {
return  "nothing";
}
});
var Dog = Class.extend(Animal.prototype,{
name: "Dog",GetType: function() {
return  this;
}
});
Dog.prototype.HearMe = function() {
return  "Woof!";
};
Shop = {
items: ["A","B","C"],getItem: function(id) {
return  this;
}
};
puts(Shop.getItem(1));

