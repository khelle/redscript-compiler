puts = function(msg) { console.log(msg); };Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};

var Animal = Class.extend({},{
type: "not defined",getType: function() {
return  this.type;
}
});
var Dog = Class.extend(Animal.prototype,{
type: "mammal"});
var Cat = Class.extend(Animal.prototype,{
type: "mammal"});
Dog.prototype.HearMe = function() {
puts("Woof!");
};
Cat.prototype.HearMe = function() {
puts("Meow...");
};
PetShop = {
store: [new Dog(),new Cat()],BuyRandom: function() {
return  this.store[Math.floor(Math.random() * 2)];
}
};
pet= PetShop.BuyRandom();
pet.HearMe();

