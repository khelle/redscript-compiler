puts = function(msg) { console.log(msg); };Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};

(function() {
$name= "Kamil";
$nick= "Skie";

return {
name: $name,nick: $nick}
})();

