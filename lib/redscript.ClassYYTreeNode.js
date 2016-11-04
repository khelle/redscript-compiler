/* Declare TreeNode */
ClassYYTreeNode = function(data) {
  this.handles = [];
  this.children = {};  
  this.data = false;
  this.currentSet = 0;
  this.stopFlagSet = false;
  this.joinOperator = '';
  this.joinOperatorH = '';
  
  /* Add helper handles */
  this.AddHandles = function(nodes) {
    if (nodes instanceof Array) {
      for (i in nodes) {
        this.AddHandle(nodes[i]);
      }
    } else {
      this.AddHandle(nodes);
    }
    return this;  
  };
  
  /* Add helper */
  this.AddHandle = function(handle) {
    this.handles.push(handle);
    return this;
  };
  
  this.AddChildrenSet = function(nodes) {
    this.currentSet++;
    this.AddChildren(nodes,this.currentSet-1);
    return this;
  };
    
  /* Add Children */
  this.AddChildren = function(nodes, index) {
    var i;
    
    if (typeof index == 'undefined' || !index) { index = 0; }
    
    if (nodes instanceof Array) {
      for (i in nodes) {
        this.AddChild(nodes[i], index);
      }
    } else {
      this.AddChild(nodes, index);
    }
    return this;
  };
      
  /* Add Child Node */
  this.AddChild = function(node, index) {
    if (node === null) {
      return this;
    }
    if (typeof this.children[index] == 'undefined') {
      this.children[index] = [];
    }
    this.children[index].push(node);
    return this;
  };
  
  /* Set data value of current node */
  this.Val = function(data) {
    this.data = {};
    this.data.type = data[0];
    this.data.name = data[1];
    this.data.data = (data.length > 0) ? data.splice(2) : [];
    return this;
  }; 
  
  /* Generate code */
  this.GenerateCode = function(ci) {
    var pattern, h, c, len, emptyCommand, endOfCommand;
    var hs, cs;
    var i, j, key;
    var that,s,m;
    
    pattern = YYCode.GetPattern(this.data);
    h = [];
    c = [];
    emptyCommand = false;
    endOfCommand = true;
        
    if (this.data === false) {
      emptyCommand = true;
    }
    
    len = this.handles.length;
    if (len > 0) {
      for (i=len-1; i>=0; i--) {
        h.push(this.handles[i].GenerateCode(ci));
      }
    }    

    i = 0;
    for (key in this.children) {
      c.push([]);
      for (j=this.children[key].length-1; j>=0; j--) {
        c[i].push(this.children[key][j].GenerateCode(ci+1));
      }
      i++;
      endOfCommand = false;
    }
    
    //console.log(this.data.name);
    //console.log(JSON.stringify(c,false,'\t'));
    
    //d = (endOfCommand && !emptyCommand) ? ';\n' : '';  
    d = (this.stopFlagSet) ? ';\n' : '';
    
    if (typeof this.data.name !== 'undefined' && this.data.name !== null) {
      name = this.data.name;
      if (this.data.type == 'KEYWORD') { name = name.toLowerCase(); };
      name = name.replace('@','this.'); 
      pattern = pattern.replace(/%name/gi,name);
    }
    if (h.length > 0) {
      hs = h.join(this.joinOperatorH);
      pattern = pattern.replace(/%hs/gi,hs);
    } else {
      pattern = pattern.replace(/%hs/gi,'');
    }
    
    if (c.length > 0) {
      cs = '';
      for (i=0; i<c.length; i++) {
        cs += c[i].join(this.joinOperator);
      }
      pattern = pattern.replace(/%cs/gi,cs);
    } else {                         
      pattern = pattern.replace(/%cs/gi,'');
    }
    
    if (typeof this.data.data !== 'undefined' && this.data.data.length > 0) {
      pattern = pattern.replace(/%ds/gi,this.data.data.join(','));
    }
    
    pattern = pattern.replace(/%h\[([0-9]+)\]/gi,function(s,m) {
      return h[m];
    });
       
    pattern = pattern.replace(/%c\[([0-9]+)\]/gi,function(s,m) {
      return c[m];
    });
        
    pattern = pattern.replace(/%d\[([0-9]+)\]/gi,(function(s,m,that) {
      return function(s,m) {
        var t;
        if (typeof that.data != 'undefined' && that.data.type == 'OP') {
          t = new String(that.data.data[m]).toLowerCase();
        } else {
          t = that.data.data[m];
        }
        if (typeof t == 'undefined' || t == 'undefined') {
          return '';
        } else {
          return t;
        }
      }
    })(s,m,this));     
    
    return pattern.replace(/^[ ]+|[ ]+$/g,"")+d;
  };
  
  /* set stop flag */
  this.StopFlag = function() {
    this.stopFlagSet = true;
    return this;
  };
  
  /* set join op */
  this.SetJoin = function(op) {
    this.joinOperator = op;
    return this;
  };
  
  /* set join op */
  this.SetJoinH = function(op) {
    this.joinOperatorH = op;
    return this;
  };  
    
  if (typeof data != 'undefined' && data !== false) {
    this.Val(data);
  }  

  return this;
};

exports.getInstance = ClassYYTreeNode;