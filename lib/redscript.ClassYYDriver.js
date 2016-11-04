ClassYYDriver = function(TreeWalker) {
  this.TreeWalker = TreeWalker;
  this.errors = [];

  /* Print generated code */
  this.Log = function() {
    code = this.TreeWalker.GenerateCode();
    typeof console !== 'undefined' ? console.log(code) : print(code);    
    return code;
  };
  
  /* Log error */
  this.LogError = function(msg) {
    this.errors.push(msg);
    return msg;
  };
  
  return this;
};

exports.getInstance = ClassYYDriver;