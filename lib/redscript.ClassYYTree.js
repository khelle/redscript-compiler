ClassYYTree = function(rootInstance) {
  this.root = rootInstance;
                                   
  /* Add Node */
  this.SetRoot = function(node) {
    this.root = node;
    return this;
  }; 
  
  /* Generate output code */
  this.GenerateCode = function() {
    return YYCode.IncludeLibs() + '\n\n' + this.root.GenerateCode(0);
    //return JSON.stringify(this.root, false, '\t');
    //return YYCode.IncludeLibs() + '\n\n' + this.root.GenerateCode(0) + '\n\n' + JSON.stringify(this.root, false, '\t');
  };
  
  return this; 
};

exports.getInstance = ClassYYTree;