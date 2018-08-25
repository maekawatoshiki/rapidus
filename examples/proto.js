function Person() { this.name = "??" }
Person.prototype.show = function() { 
  console.log(this.name, this.family) 
};

function Browns() { this.family = "Brown";}
Browns.prototype = new Person();

function James() { this.name = "James" }
James.prototype = new Browns();

var greens = new Browns();
greens.show();

var james = new James();
james.show();
