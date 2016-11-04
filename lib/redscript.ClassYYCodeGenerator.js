/* Declare CodeGenerator */
ClassYYCodeGenerator = function() {
  /* Generate Code */
  this.GetPattern = function(data) {
    var s;  
  
    if (typeof data === 'undefined' || data === null) {
      return '';
    } else if (data === false) {
      return '%cs';
    }

    switch (data.type) {
      case 'VARIABLE':        s = this.ParseVariable(data); break;
      case 'OP':              s = this.ParseOp(data);       break;
      case 'VALUE':           s = this.ParseValue(data);    break;
      case 'BRACES':          s = this.ParseBraces(data);   break;
      case 'ARRAY':           s = this.ParseArray(data);    break;
      case 'PROPERTY':        s = this.ParseProperty(data); break;
      case 'CONST_VARIABLE':  s = this.ParseConst(data);    break;
      case 'KEYWORD':         s = this.ParseKeyword(data);  break;
      case 'IF':              s = this.ParseIf(data);       break;
      case 'FUNCTION':        s = this.ParseFunction(data); break;
      case 'LOOP':            s = this.ParseLoop(data);     break;
      case 'WITH':            s = this.ParseWith(data);     break;
      case 'SWITCH':          s = this.ParseSwitch(data);   break;
      case 'WHEN':            s = this.ParseWhen(data);     break;
      case 'DEFAULT':         s = this.ParseDefault(data);  break;
      case 'LABEL':           s = this.ParseLabel(data);    break;
      case 'TRY':             s = this.ParseTry(data);      break;
      case 'CLASS':           s = this.ParseClass(data);    break;
      case 'OBJECT':          s = this.ParseObject(data);   break;
      case 'METHOD':          s = this.ParseMethod(data);   break;
      case 'METHOD_OUT':      s = this.ParseMethodOut(data);break;
      case 'METHOD_EXT':      s = this.ParseMethodExt(data);break;
      case 'MODULE':          s = this.ParseModule(data);   break;
      case 'CHECK':           s = this.ParseCheck(data);    break;    
      case 'PRIVATE':         s = this.ParsePrivate(data);  break;   
      default:                s = '';      
    }
    return s;
  };
  
  /* Parsers */
  this.ParseVariable = function(data) {
    return '%name%cs';
  };
  
  this.ParseOp = function(data) {
    if (typeof data.data[1] != 'undefined' && data.data[1] !== null) {
      return '%d[0]%cs%d[1]';
    } else if (typeof data.data[2] != 'undefined' && data.data[2] !== null) {
      return (data.data[2] == 'SYM') ? '%c[0] %d[0] %c[1]' : '%cs%d[0]';  
    } else if (data.data[0] == '=' || data.data[0].toUpperCase() == 'NEW' || data.data[0].toUpperCase() == 'TYPEOF' || data.data[0].toUpperCase() == 'INSTANCEOF' || data.data[0].toUpperCase() == 'VOID') {
      return '%d[0] %cs';
    } else {
      return '%d[0]%cs ';
    }
  };
  
  this.ParseValue = function(data) {
    return '%d[0]';
  };
  
  this.ParseBraces = function(data) {
    return '{%cs}';
  };
  
  this.ParseArray = function(data) {
    return '[%cs]';
  };

  this.ParseProperty = function(data) {
    return '%name: %cs';
  }; 
  
  this.ParseConst = function(data) {
    return 'const %name%cs';
  };
  
  this.ParseKeyword = function(data) {
    return '%name %d[0] %hs';
  };
  
  this.ParseIf = function(data) {
    if (typeof data.data[0] == 'undefined') {
      return 'if (%hs) {\n%cs}';
    } else {
      return 'if (%hs) {\n%c[0]} else {\n%c[1]}';
    }
  };
  
  this.ParseFunction = function(data) {
    if (data.name === null) {
      return 'function(%ds) {\n%cs}';
    } else {
      return '%name = function(%ds) {\n%cs}';
    }
  };
  
  this.ParseLoop = function(data) {
    if (data.name == 'WHILE') {
      return 'while (%hs) {\n%cs}';
    } else if (data.name == 'UNTIL') {
      return 'while(!(%hs)) {\n%cs}';
    } else if (data.name == 'FOR') {
      if (data.data[0] == 'OF') {
        return 'for(var _i=0, _limit=(%hs).length; _i<_limit; _i++) {\nvar %d[1] = %hs [_i];\n%cs}';
      } else if (typeof data.data[2] === 'undefined') {
        return 'for(var %d[1] in %hs) {\n%cs}';
      } else {
        return 'for(var %d[1] in %hs) {\nvar %d[2] = %hs [%d[1]];\n%cs}';
      }
    }
  };
  
  this.ParseWith = function(data) {
    return 'with (%hs) {\n%cs}';
  };
  
  this.ParseSwitch = function(data) {
    return 'switch(%hs) {\n%cs}';
  };

  this.ParseWhen = function(data) {
    if (typeof data.data[0] !== 'undefined' && data.data[0] !== null) {
      return 'case %hs :\n%cs\nbreak;\n';
    } else {
      return 'case %hs :\n%cs';
    }
  };
  
  this.ParseDefault = function(data) {
    return 'default :\n%cs';
  };
  
  this.ParseLabel = function(data) {
    return '%name :\n%cs';
  };
  
  this.ParseTry = function(data) {
    if (typeof data.data[0] !== 'undefined' && data.data[0] !== null) {
      return 'try {\n%c[0]} catch (%hs) {\n%c[1]} finally {\n%c[2]}';
    } else {
      return 'try {\n%c[0]} catch (%hs) {\n%c[1]}';
    }
  };
  
  this.ParseClass = function(data) {
    if (typeof data.data[0] !== 'undefined' && data.data[0] !== null) {
      return 'var %name = Class.extend(%d[0].prototype,{\n%cs})';
    } else {
      return 'var %name = Class.extend({},{\n%cs})';
    }
  };
  
  this.ParseObject = function(data) {
    if (typeof data.data[0] !== 'undefined' && data.data[0] !== null) {
      return '%name = {\n%cs}';
    } else {
      return '%name = {\n%cs}';
    }
  };
  
  this.ParseMethod = function(data) {
    return '%name: function(%ds) {\n%cs}\n';
  };
  
  this.ParseMethodOut = function(data) {
    return '%name = function(%ds) {\n%cs}';
  };
  
  this.ParseMethodExt = function(data) {
    return '%name = function(%ds) {\n%cs}';
  };      
  
  this.ParseModule = function(data) {
    if (typeof data.data[0] !== 'undefined' && data.data[0] !== null) {
      return '(function() {\n%cs\nreturn {\n%hs}\n})()';
    } else {
      return '(function() {\n%cs})()';
    }
  };
    
  this.ParseCheck = function(data) {
    return '%hs ? %c[0] : %c[1]';
  };    
  
  this.ParsePrivate = function(data) {
    return '(function() {\n%cs})()';
  };            
        
  this.IncludeLibs = function() {
    var t;
    t = [];
    t.push('puts = function(msg) { console.log(msg); };')
    t.push('Class = { extend: function(p,data) { var o, key; o = function() {}; for (key in p) { o.prototype[key] = p[key]; } for (key in data) { o.prototype[key] = data[key];	} o.prototype.extend = Class.extend; return o; }};');    
    return t.join('');
  };      
        
  return this;
};

exports.getInstance = ClassYYCodeGenerator;