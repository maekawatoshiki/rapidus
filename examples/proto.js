function Person() { this.name = "??" }
Person.prototype.show = function() { 
  console.log(this.name, this.family) 
};

function Browns() { this.family = "Brown";}
Browns.prototype = new Person();

function James() { this.name = "James" }
James.prototype = new Browns();

var greens = new Browns();
console.log(greens);
greens.show();

var james = new James();
james.show();
// while ((james === null) == false) {
//   console.log(james.__proto__, james.__proto__.constructor === Person, james.__proto__.constructor === Object)
//   james = james.__proto__
// }
