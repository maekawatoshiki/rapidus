let obj = { 
  _x: 0,
  get x (   ) { return this._x },
  set x (val) { this._x = val   }
}

let results = [];

results.push(obj.x)
obj.x = 123
results.push(obj.x)
results
