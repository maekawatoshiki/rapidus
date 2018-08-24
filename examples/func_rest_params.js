function User(name, mail, ...data) {
  var favorites = [], i = 0;
  while (i < data.length) {
    favorites.push(data[i]);
    i += 1;
  }
  return {
    name, mail, favorites,
    show: function () {
      console.log("username:", this.name, ", mail:", this.mail);
      console.log("  favorites:", this.favorites);
    }
  }
}

var u1 = User("uint256_t", "aa@aa.com", "Rust", "C++");
var u2 = User("someone", "bb@bb.com", "Ruby");
u1.show();
u2.show();
