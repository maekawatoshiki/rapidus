function AVLTree() {
  this.kind = "empty";
  this.depth = 0;

  this.insert = function(val) {
    if (this.kind == "empty") {
      this.kind = "val";
      this.val = val;
      this.depth = 1;
    } else if (this.kind == "val") {
      if (this.val == val) return;
      this.kind = "node";
      this.lhs = new AVLTree();
      this.rhs = new AVLTree();
      if (val < this.val) {
        this.lhs.insert(val);
        this.balanceL();
      } else if (this.val < val) {
        this.rhs.insert(val);
        this.balanceR();
      }
    } else if (this.kind == "node") {
      if (this.val == val) return;
      if (val < this.val) {
        this.lhs.insert(val);
        this.balanceL();
      } else if (this.val < val) {
        this.rhs.insert(val);
        this.balanceR();
      }
    }
  }

  this.bias = function() {
    return this.lhs.depth - this.rhs.depth;
  }

  this.balanceL = function() {
    if (this.bias() >= 2) {
      if (this.lhs.bias() >= 0)
        this.rotateR();
      else
        this.rotateLR();
    } else {
      this.compute_max_depth();
    }
  }

  this.balanceR = function() {
    if (this.bias() <= -2) {
      if (this.rhs.bias() <= 0)
        this.rotateL();
      else
        this.rotateRL();
    } else {
      this.compute_max_depth();
    }
  }

  this.rotateL = function() {
    if (this.kind != "node") throw 'unreachable';
    let old_this_val = this.val;
    let old_this_rhs_lhs = this.rhs.lhs;
    let old_this_lhs = this.lhs;
    this.val = this.rhs.val;
    this.rhs = this.rhs.rhs || new AVLTree();
    this.lhs = new AVLTree();
    this.lhs.val = old_this_val;
    this.lhs.kind = old_this_rhs_lhs == undefined &&
      old_this_lhs == undefined ? "val" : "node";
    if (this.lhs.kind == "node") {
      this.lhs.rhs = old_this_rhs_lhs || new AVLTree();
      this.lhs.lhs = old_this_lhs || new AVLTree();
    }
    this.lhs.compute_max_depth();
    this.compute_max_depth();
  };

  this.rotateR = function() {
    if (this.kind != "node") throw 'unreachable';
    let old_this_val = this.val;
    let old_this_lhs_rhs = this.lhs.rhs;
    let old_this_rhs = this.rhs;
    this.val = this.lhs.val;
    this.lhs = this.lhs.lhs || new AVLTree();
    this.rhs = new AVLTree();
    this.rhs.val = old_this_val;
    this.rhs.kind = old_this_lhs_rhs == undefined &&
      old_this_rhs == undefined ? "val" : "node";
    if (this.rhs.kind == "node") {
      this.rhs.lhs = old_this_lhs_rhs || new AVLTree();
      this.rhs.rhs = old_this_rhs || new AVLTree();
    }
    this.rhs.compute_max_depth();
    this.compute_max_depth();
  };

  this.rotateLR = function() {
    this.lhs.rotateL();
    this.rotateR();
  };

  this.rotateRL = function() {
    this.rhs.rotateR();
    this.rotateL();
  };

  this.compute_max_depth = function() {
    if (this.kind == "empty") {
      this.depth = 0;
      return 0;
    } else if (this.kind == "val") {
      this.depth = 1;
      return 1;
    } else if (this.kind == "node") {
      let lhs_depth = this.lhs.depth;
      let rhs_depth = this.rhs.depth;
      let depth = 1 + (lhs_depth > rhs_depth ? lhs_depth : rhs_depth);
      this.depth = depth;
      return this.depth;
    } else {
      throw "unreachable";
    }
  };

  this.dump = function() {
    this.__dump(0);
  };

  this.__dump = function(depth) {
    let indent = "";
    for (let i = 0; i < depth; i++)
      indent += " ";
    if (this.kind == "empty") {
      // console.log(indent + "empty");
    } else if (this.kind == "node") {
      console.log(indent + this.val);
      this.lhs.__dump(depth + 1);
      this.rhs.__dump(depth + 1);
    } else if (this.kind == "val") {
      console.log(indent + this.val);
    }
  };

  this.verify = function() {
    if (this.kind == "empty") {
      return 0;
    } else if (this.kind == "val") {
      return 1;
    } else if (this.kind == "node") {
      let lhs_depth = this.lhs.verify();
      let rhs_depth = this.rhs.verify();
      let bias = lhs_depth - rhs_depth;
      if (bias <= -2 || 2 <= bias) throw "Error: >2 depth difference not allowed in AVL Tree"
      let max_depth = 1 + (lhs_depth > rhs_depth ? lhs_depth : rhs_depth);
      return max_depth;
    } else {
      throw "unreachable";
    }
  };
}

let tree = new AVLTree();

for (let i = 0; i < 20; i++)
  tree.insert(Math.random() * 100 | 0);

tree.dump();
tree.verify();
