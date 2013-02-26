
			// File : /okcontent/js/Oryx/Cookies.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Oryx/Cookies.js";
			
function setOkCookie(name,value,expires){setCookie(name,value,expires);}
function deleteOkCookie(name){deleteCookie(name);}
function getPageTopDomain(){var host=window.location.host;var parts=host.split('.');var res=parts[parts.length-2]+"."+parts[parts.length-1];if(res.indexOf(':')!=-1){res=res.substr(0,res.indexOf(':'));}
return res;}
function secondsFromNow(sec){res=new Date();res.setTime(new Date().getTime()+sec*1000);return res;}
function setCookie(name,value,expires){var curCookie=name+"="+escape(value)+
((expires)?"; expires="+expires.toGMTString():"")+"; path=/"+"; domain="+getPageTopDomain();document.cookie=curCookie;}
function deleteCookie(name){if(getCookie(name)){document.cookie=name+"="+"; domain="+getPageTopDomain()+"; path=/"+"; expires=Thu, 01-Jan-70 00:00:01 GMT";}}
function getCookie(name){var dc=document.cookie;var prefix=name+"=";var begin=dc.indexOf("; "+prefix);if(begin==-1){begin=dc.indexOf(prefix);if(begin!=0)return null;}
else
begin+=2;var end=document.cookie.indexOf(";",begin);if(end==-1)
end=dc.length;return unescape(dc.substring(begin+prefix.length,end));}
function addNewCoresToCookie(file_nums,hash){var old_cookie=getCookie("core");var old_cookie_parts=[];var new_cookie=old_cookie;if(old_cookie)
old_cookie_parts=old_cookie.split("_");if(!old_cookie||!old_cookie_parts||old_cookie_parts.length<1||old_cookie_parts[0]!=hash){new_cookie=hash;}
var new_addition=file_nums.join(".");for(var i=1;i<old_cookie_parts.length&&old_cookie_parts[0]==hash;i++){var old_files=old_cookie_parts[i].split(".");for(var j=0;j<old_files.length;j++){for(var k=0;k<file_nums.length;k++){if(file_nums[k]==old_files[j]){return;}}}}
new_cookie+="_"+new_addition;setCookie("core",new_cookie,secondsFromNow(86400*30));}
function setMiniCookie(name,value){NanoCookie.set(name,value,{ms:3600*1000});}
function deleteMiniCookie(name){NanoCookie.remove(name);}
function getMiniCookie(name){return NanoCookie.get(name);}
NanoCookie={set:function(key,value,expires,test){var cookies=this.deserialize();var d=new Date();var obj=new Object();obj["k"]=key;obj["v"]=value;if(expires.ms)
obj["e"]=d.getTime()+parseInt(expires.ms);else if(expires.gmt)
obj["e"]=expires.gmt;else
obj["e"]=expires.date.getTime();if(isNaN(obj["e"]))
obj["e"]=0;var found=false;for(var i=0;i<cookies.length;i++){if(cookies[i]["k"]==key){cookies[i]=obj;found=true;}}
if(!found)
cookies.push(obj);this.serializeAndStore(cookies);},get:function(key){var cookies=this.deserialize();for(var i=0;i<cookies.length;i++){if(cookies[i]["k"]==key){return cookies[i]["v"];}}
return null;},getAll:function(nano_cookie_str){var cookies=this.deserialize(nano_cookie_str);var res=[];for(var i=0;i<cookies.length;i++){var obj=new Object();obj["key"]=cookies[i]["k"];obj["value"]=cookies[i]["v"];obj["expires"]=new Date(cookies[i]["e"]);res.push(obj);}
return res;},findRegExp:function(regexp){var cookies=this.deserialize();var res=[];for(var i=0;i<cookies.length;i++){if(regexp.test(cookies[i]["k"])){var obj=new Object();obj["key"]=cookies[i]["k"];obj["value"]=cookies[i]["v"];obj["expires"]=new Date(cookies[i]["e"]);res.push(obj);}}
return res;},remove:function(key){this.set(key,"",{ms:-1},true);},removeAll:function(){deleteCookie("nano");},deserialize:function(nano_cookie_str){var x=nano_cookie_str?nano_cookie_str:getCookie("nano");var result=[];if(!x)
return result;else{var individuals=x.split("|");for(var i=0;i<individuals.length;i++){var obj=new Object();var pairs=individuals[i].split(",");for(var j=0;j<pairs.length;j++){var pair=pairs[j].split("=");var val=unescape(pair[1]);var pval=parseInt(val);if(pval==val&&!isNaN(pval))
val=pval;obj[pair[0]]=val;}
result.push(obj);}
result.sort(this.compareDates);var d=new Date();var any_stripped=false;while(result.length>0&&new Date(result[result.length-1]["e"])<d){result.length--;any_stripped=true;}
return result;}},serializeAndStore:function(cookies){var res="";for(var i=0;i<cookies.length;i++){res+=(i==0)?"":"|";res+="k="+escape(cookies[i]["k"]);res+=",e="+escape(cookies[i]["e"]);res+=",v="+escape(cookies[i]["v"]);}
setCookie("nano",res,secondsFromNow(3600*24*365));},compareDates:function(a,b){if(a["e"]&&b["e"])
return((a["e"]<b["e"])?1:((a["e"]>b["e"])?-1:0));}};
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/browser_detect.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/browser_detect.js";
			
var BrowserDetect={init:function(){this.browser=this.searchString(this.dataBrowser)||"An unknown browser";this.version=this.searchVersion(navigator.userAgent)||this.searchVersion(navigator.appVersion)||"an unknown version";this.OS=this.searchString(this.dataOS)||"an unknown OS";},searchString:function(data){for(var i=0;i<data.length;i++){var dataString=data[i].string;var dataProp=data[i].prop;this.versionSearchString=data[i].versionSearch||data[i].identity;if(dataString){if(dataString.indexOf(data[i].subString)!=-1)
return data[i].identity;}
else if(dataProp)
return data[i].identity;}},searchVersion:function(dataString){var index=dataString.indexOf(this.versionSearchString);if(index==-1)return;return parseFloat(dataString.substring(index+this.versionSearchString.length+1));},dataBrowser:[{string:navigator.userAgent,subString:"Chrome",identity:"Chrome"},{string:navigator.userAgent,subString:"OmniWeb",versionSearch:"OmniWeb/",identity:"OmniWeb"},{string:navigator.vendor,subString:"Apple",identity:"Safari",versionSearch:"Version"},{prop:window.opera,identity:"Opera"},{string:navigator.vendor,subString:"iCab",identity:"iCab"},{string:navigator.vendor,subString:"KDE",identity:"Konqueror"},{string:navigator.userAgent,subString:"Firefox",identity:"Firefox"},{string:navigator.vendor,subString:"Camino",identity:"Camino"},{string:navigator.userAgent,subString:"Netscape",identity:"Netscape"},{string:navigator.userAgent,subString:"MSIE",identity:"Explorer",versionSearch:"MSIE"},{string:navigator.userAgent,subString:"Gecko",identity:"Mozilla",versionSearch:"rv"},{string:navigator.userAgent,subString:"Mozilla",identity:"Netscape",versionSearch:"Mozilla"}],dataOS:[{string:navigator.platform,subString:"Win",identity:"Windows"},{string:navigator.platform,subString:"Mac",identity:"Mac"},{string:navigator.userAgent,subString:"iPhone",identity:"iPhone/iPod"},{string:navigator.platform,subString:"Linux",identity:"Linux"}]};BrowserDetect.init();
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/prototype-1.7.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/prototype-1.7.js";
			
var Prototype={Version:'1.7',Browser:(function(){var ua=navigator.userAgent;var isOpera=Object.prototype.toString.call(window.opera)=='[object Opera]';return{IE:!!window.attachEvent&&!isOpera,Opera:isOpera,WebKit:ua.indexOf('AppleWebKit/')>-1,Gecko:ua.indexOf('Gecko')>-1&&ua.indexOf('KHTML')===-1,MobileSafari:/Apple.*Mobile/.test(ua)}})(),BrowserFeatures:{XPath:!!document.evaluate,SelectorsAPI:!!document.querySelector,ElementExtensions:(function(){var constructor=window.Element||window.HTMLElement;return!!(constructor&&constructor.prototype);})(),SpecificElementExtensions:(function(){if(typeof window.HTMLDivElement!=='undefined')
return true;var div=document.createElement('div'),form=document.createElement('form'),isSupported=false;if(div['__proto__']&&(div['__proto__']!==form['__proto__'])){isSupported=true;}
div=form=null;return isSupported;})()},ScriptFragment:'<script[^>]*>([\\S\\s]*?)<\/script>',JSONFilter:/^\/\*-secure-([\s\S]*)\*\/\s*$/,emptyFunction:function(){},K:function(x){return x}};if(Prototype.Browser.MobileSafari)
Prototype.BrowserFeatures.SpecificElementExtensions=false;var Abstract={};var Try={these:function(){var returnValue;for(var i=0,length=arguments.length;i<length;i++){var lambda=arguments[i];try{returnValue=lambda();break;}catch(e){}}
return returnValue;}};var Class=(function(){var IS_DONTENUM_BUGGY=(function(){for(var p in{toString:1}){if(p==='toString')return false;}
return true;})();function subclass(){};function create(){var parent=null,properties=$A(arguments);if(Object.isFunction(properties[0]))
parent=properties.shift();function klass(){this.initialize.apply(this,arguments);}
Object.extend(klass,Class.Methods);klass.superclass=parent;klass.subclasses=[];if(parent){subclass.prototype=parent.prototype;klass.prototype=new subclass;parent.subclasses.push(klass);}
for(var i=0,length=properties.length;i<length;i++)
klass.addMethods(properties[i]);if(!klass.prototype.initialize)
klass.prototype.initialize=Prototype.emptyFunction;klass.prototype.constructor=klass;return klass;}
function addMethods(source){var ancestor=this.superclass&&this.superclass.prototype,properties=Object.keys(source);if(IS_DONTENUM_BUGGY){if(source.toString!=Object.prototype.toString)
properties.push("toString");if(source.valueOf!=Object.prototype.valueOf)
properties.push("valueOf");}
for(var i=0,length=properties.length;i<length;i++){var property=properties[i],value=source[property];if(ancestor&&Object.isFunction(value)&&value.argumentNames()[0]=="$super"){var method=value;value=(function(m){return function(){return ancestor[m].apply(this,arguments);};})(property).wrap(method);value.valueOf=method.valueOf.bind(method);value.toString=method.toString.bind(method);}
this.prototype[property]=value;}
return this;}
return{create:create,Methods:{addMethods:addMethods}};})();(function(){var _toString=Object.prototype.toString,NULL_TYPE='Null',UNDEFINED_TYPE='Undefined',BOOLEAN_TYPE='Boolean',NUMBER_TYPE='Number',STRING_TYPE='String',OBJECT_TYPE='Object',FUNCTION_CLASS='[object Function]',BOOLEAN_CLASS='[object Boolean]',NUMBER_CLASS='[object Number]',STRING_CLASS='[object String]',ARRAY_CLASS='[object Array]',DATE_CLASS='[object Date]',NATIVE_JSON_STRINGIFY_SUPPORT=window.JSON&&typeof JSON.stringify==='function'&&JSON.stringify(0)==='0'&&typeof JSON.stringify(Prototype.K)==='undefined';function Type(o){switch(o){case null:return NULL_TYPE;case(void 0):return UNDEFINED_TYPE;}
var type=typeof o;switch(type){case'boolean':return BOOLEAN_TYPE;case'number':return NUMBER_TYPE;case'string':return STRING_TYPE;}
return OBJECT_TYPE;}
function extend(destination,source){for(var property in source)
destination[property]=source[property];return destination;}
function inspect(object){try{if(isUndefined(object))return'undefined';if(object===null)return'null';return object.inspect?object.inspect():String(object);}catch(e){if(e instanceof RangeError)return'...';throw e;}}
function toJSON(value){return Str('',{'':value},[]);}
function Str(key,holder,stack){var value=holder[key],type=typeof value;if(Type(value)===OBJECT_TYPE&&typeof value.toJSON==='function'){value=value.toJSON(key);}
var _class=_toString.call(value);switch(_class){case NUMBER_CLASS:case BOOLEAN_CLASS:case STRING_CLASS:value=value.valueOf();}
switch(value){case null:return'null';case true:return'true';case false:return'false';}
type=typeof value;switch(type){case'string':return value.inspect(true);case'number':return isFinite(value)?String(value):'null';case'object':for(var i=0,length=stack.length;i<length;i++){if(stack[i]===value){throw new TypeError();}}
stack.push(value);var partial=[];if(_class===ARRAY_CLASS){for(var i=0,length=value.length;i<length;i++){var str=Str(i,value,stack);partial.push(typeof str==='undefined'?'null':str);}
partial='['+partial.join(',')+']';}else{var keys=Object.keys(value);for(var i=0,length=keys.length;i<length;i++){var key=keys[i],str=Str(key,value,stack);if(typeof str!=="undefined"){partial.push(key.inspect(true)+':'+str);}}
partial='{'+partial.join(',')+'}';}
stack.pop();return partial;}}
function stringify(object){return JSON.stringify(object);}
function toQueryString(object){return $H(object).toQueryString();}
function toHTML(object){return object&&object.toHTML?object.toHTML():String.interpret(object);}
function keys(object){if(Type(object)!==OBJECT_TYPE){throw new TypeError();}
var results=[];for(var property in object){if(object.hasOwnProperty(property)){results.push(property);}}
return results;}
function values(object){var results=[];for(var property in object)
results.push(object[property]);return results;}
function clone(object){return extend({},object);}
function isElement(object){return!!(object&&object.nodeType==1);}
function isArray(object){return _toString.call(object)===ARRAY_CLASS;}
var hasNativeIsArray=(typeof Array.isArray=='function')&&Array.isArray([])&&!Array.isArray({});if(hasNativeIsArray){isArray=Array.isArray;}
function isHash(object){return object instanceof Hash;}
function isFunction(object){return _toString.call(object)===FUNCTION_CLASS;}
function isString(object){return _toString.call(object)===STRING_CLASS;}
function isNumber(object){return _toString.call(object)===NUMBER_CLASS;}
function isDate(object){return _toString.call(object)===DATE_CLASS;}
function isUndefined(object){return typeof object==="undefined";}
extend(Object,{extend:extend,inspect:inspect,toJSON:NATIVE_JSON_STRINGIFY_SUPPORT?stringify:toJSON,toQueryString:toQueryString,toHTML:toHTML,keys:Object.keys||keys,values:values,clone:clone,isElement:isElement,isArray:isArray,isHash:isHash,isFunction:isFunction,isString:isString,isNumber:isNumber,isDate:isDate,isUndefined:isUndefined});})();Object.extend(Function.prototype,(function(){var slice=Array.prototype.slice;function update(array,args){var arrayLength=array.length,length=args.length;while(length--)array[arrayLength+length]=args[length];return array;}
function merge(array,args){array=slice.call(array,0);return update(array,args);}
function argumentNames(){var names=this.toString().match(/^[\s\(]*function[^(]*\(([^)]*)\)/)[1].replace(/\/\/.*?[\r\n]|\/\*(?:.|[\r\n])*?\*\//g,'').replace(/\s+/g,'').split(',');return names.length==1&&!names[0]?[]:names;}
function bind(context){if(arguments.length<2&&Object.isUndefined(arguments[0]))return this;var __method=this,args=slice.call(arguments,1);return function(){var a=merge(args,arguments);return __method.apply(context,a);}}
function bindAsEventListener(context){var __method=this,args=slice.call(arguments,1);return function(event){var a=update([event||window.event],args);return __method.apply(context,a);}}
function curry(){if(!arguments.length)return this;var __method=this,args=slice.call(arguments,0);return function(){var a=merge(args,arguments);return __method.apply(this,a);}}
function delay(timeout){var __method=this,args=slice.call(arguments,1);timeout=timeout*1000;return window.setTimeout(function(){return __method.apply(__method,args);},timeout);}
function defer(){var args=update([0.01],arguments);return this.delay.apply(this,args);}
function wrap(wrapper){var __method=this;return function(){var a=update([__method.bind(this)],arguments);return wrapper.apply(this,a);}}
function methodize(){if(this._methodized)return this._methodized;var __method=this;return this._methodized=function(){var a=update([this],arguments);return __method.apply(null,a);};}
return{argumentNames:argumentNames,bind:bind,bindAsEventListener:bindAsEventListener,curry:curry,delay:delay,defer:defer,wrap:wrap,methodize:methodize}})());(function(proto){function toISOString(){return this.getUTCFullYear()+'-'+
(this.getUTCMonth()+1).toPaddedString(2)+'-'+
this.getUTCDate().toPaddedString(2)+'T'+
this.getUTCHours().toPaddedString(2)+':'+
this.getUTCMinutes().toPaddedString(2)+':'+
this.getUTCSeconds().toPaddedString(2)+'Z';}
function toJSON(){return this.toISOString();}
if(!proto.toISOString)proto.toISOString=toISOString;if(!proto.toJSON)proto.toJSON=toJSON;})(Date.prototype);RegExp.prototype.match=RegExp.prototype.test;RegExp.escape=function(str){return String(str).replace(/([.*+?^=!:${}()|[\]\/\\])/g,'\\$1');};var PeriodicalExecuter=Class.create({initialize:function(callback,frequency){this.callback=callback;this.frequency=frequency;this.currentlyExecuting=false;this.registerCallback();},registerCallback:function(){this.timer=setInterval(this.onTimerEvent.bind(this),this.frequency*1000);},execute:function(){this.callback(this);},stop:function(){if(!this.timer)return;clearInterval(this.timer);this.timer=null;},onTimerEvent:function(){if(!this.currentlyExecuting){try{this.currentlyExecuting=true;this.execute();this.currentlyExecuting=false;}catch(e){this.currentlyExecuting=false;throw e;}}}});Object.extend(String,{interpret:function(value){return value==null?'':String(value);},specialChar:{'\b':'\\b','\t':'\\t','\n':'\\n','\f':'\\f','\r':'\\r','\\':'\\\\'}});Object.extend(String.prototype,(function(){var NATIVE_JSON_PARSE_SUPPORT=window.JSON&&typeof JSON.parse==='function'&&JSON.parse('{"test": true}').test;function prepareReplacement(replacement){if(Object.isFunction(replacement))return replacement;var template=new Template(replacement);return function(match){return template.evaluate(match)};}
function gsub(pattern,replacement){var result='',source=this,match;replacement=prepareReplacement(replacement);if(Object.isString(pattern))
pattern=RegExp.escape(pattern);if(!(pattern.length||pattern.source)){replacement=replacement('');return replacement+source.split('').join(replacement)+replacement;}
while(source.length>0){if(match=source.match(pattern)){result+=source.slice(0,match.index);result+=String.interpret(replacement(match));source=source.slice(match.index+match[0].length);}else{result+=source,source='';}}
return result;}
function sub(pattern,replacement,count){replacement=prepareReplacement(replacement);count=Object.isUndefined(count)?1:count;return this.gsub(pattern,function(match){if(--count<0)return match[0];return replacement(match);});}
function scan(pattern,iterator){this.gsub(pattern,iterator);return String(this);}
function truncate(length,truncation){length=length||30;truncation=Object.isUndefined(truncation)?'...':truncation;return this.length>length?this.slice(0,length-truncation.length)+truncation:String(this);}
function strip(){return this.replace(/^\s+/,'').replace(/\s+$/,'');}
function stripTags(){return this.replace(/<\w+(\s+("[^"]*"|'[^']*'|[^>])+)?>|<\/\w+>/gi,'');}
function stripScripts(){return this.replace(new RegExp(Prototype.ScriptFragment,'img'),'');}
function extractScripts(){var matchAll=new RegExp(Prototype.ScriptFragment,'img'),matchOne=new RegExp(Prototype.ScriptFragment,'im');return(this.match(matchAll)||[]).map(function(scriptTag){return(scriptTag.match(matchOne)||['',''])[1];});}
function evalScripts(){return this.extractScripts().map(function(script){return eval(script)});}
function escapeHTML(){return this.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');}
function unescapeHTML(){return this.stripTags().replace(/&lt;/g,'<').replace(/&gt;/g,'>').replace(/&amp;/g,'&');}
function toQueryParams(separator){var match=this.strip().match(/([^?#]*)(#.*)?$/);if(!match)return{};return match[1].split(separator||'&').inject({},function(hash,pair){if((pair=pair.split('='))[0]){var key=decodeURIComponent(pair.shift()),value=pair.length>1?pair.join('='):pair[0];if(value!=undefined)value=decodeURIComponent(value);if(key in hash){if(!Object.isArray(hash[key]))hash[key]=[hash[key]];hash[key].push(value);}
else hash[key]=value;}
return hash;});}
function toArray(){return this.split('');}
function succ(){return this.slice(0,this.length-1)+
String.fromCharCode(this.charCodeAt(this.length-1)+1);}
function times(count){return count<1?'':new Array(count+1).join(this);}
function camelize(){return this.replace(/-+(.)?/g,function(match,chr){return chr?chr.toUpperCase():'';});}
function capitalize(){return this.charAt(0).toUpperCase()+this.substring(1).toLowerCase();}
function underscore(){return this.replace(/::/g,'/').replace(/([A-Z]+)([A-Z][a-z])/g,'$1_$2').replace(/([a-z\d])([A-Z])/g,'$1_$2').replace(/-/g,'_').toLowerCase();}
function dasherize(){return this.replace(/_/g,'-');}
function inspect(useDoubleQuotes){var escapedString=this.replace(/[\x00-\x1f\\]/g,function(character){if(character in String.specialChar){return String.specialChar[character];}
return'\\u00'+character.charCodeAt().toPaddedString(2,16);});if(useDoubleQuotes)return'"'+escapedString.replace(/"/g,'\\"')+'"';return"'"+escapedString.replace(/'/g,'\\\'')+"'";}
function unfilterJSON(filter){return this.replace(filter||Prototype.JSONFilter,'$1');}
function isJSON(){var str=this;if(str.blank())return false;str=str.replace(/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g,'@');str=str.replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g,']');str=str.replace(/(?:^|:|,)(?:\s*\[)+/g,'');return(/^[\],:{}\s]*$/).test(str);}
function evalJSON(sanitize){var json=this.unfilterJSON(),cx=/[\u0000\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g;if(cx.test(json)){json=json.replace(cx,function(a){return'\\u'+('0000'+a.charCodeAt(0).toString(16)).slice(-4);});}
try{if(!sanitize||json.isJSON())return eval('('+json+')');}catch(e){}
throw new SyntaxError('Badly formed JSON string: '+this.inspect());}
function parseJSON(){var json=this.unfilterJSON();return JSON.parse(json);}
function include(pattern){return this.indexOf(pattern)>-1;}
function startsWith(pattern){return this.lastIndexOf(pattern,0)===0;}
function endsWith(pattern){var d=this.length-pattern.length;return d>=0&&this.indexOf(pattern,d)===d;}
function empty(){return this=='';}
function blank(){return/^\s*$/.test(this);}
function interpolate(object,pattern){return new Template(this,pattern).evaluate(object);}
return{gsub:gsub,sub:sub,scan:scan,truncate:truncate,strip:String.prototype.trim||strip,stripTags:stripTags,stripScripts:stripScripts,extractScripts:extractScripts,evalScripts:evalScripts,escapeHTML:escapeHTML,unescapeHTML:unescapeHTML,toQueryParams:toQueryParams,parseQuery:toQueryParams,toArray:toArray,succ:succ,times:times,camelize:camelize,capitalize:capitalize,underscore:underscore,dasherize:dasherize,inspect:inspect,unfilterJSON:unfilterJSON,isJSON:isJSON,evalJSON:NATIVE_JSON_PARSE_SUPPORT?parseJSON:evalJSON,include:include,startsWith:startsWith,endsWith:endsWith,empty:empty,blank:blank,interpolate:interpolate};})());var Template=Class.create({initialize:function(template,pattern){this.template=template.toString();this.pattern=pattern||Template.Pattern;},evaluate:function(object){if(object&&Object.isFunction(object.toTemplateReplacements))
object=object.toTemplateReplacements();return this.template.gsub(this.pattern,function(match){if(object==null)return(match[1]+'');var before=match[1]||'';if(before=='\\')return match[2];var ctx=object,expr=match[3],pattern=/^([^.[]+|\[((?:.*?[^\\])?)\])(\.|\[|$)/;match=pattern.exec(expr);if(match==null)return before;while(match!=null){var comp=match[1].startsWith('[')?match[2].replace(/\\\\]/g,']'):match[1];ctx=ctx[comp];if(null==ctx||''==match[3])break;expr=expr.substring('['==match[3]?match[1].length:match[0].length);match=pattern.exec(expr);}
return before+String.interpret(ctx);});}});Template.Pattern=/(^|.|\r|\n)(#\{(.*?)\})/;var $break={};var Enumerable=(function(){function each(iterator,context){var index=0;try{this._each(function(value){iterator.call(context,value,index++);});}catch(e){if(e!=$break)throw e;}
return this;}
function eachSlice(number,iterator,context){var index=-number,slices=[],array=this.toArray();if(number<1)return array;while((index+=number)<array.length)
slices.push(array.slice(index,index+number));return slices.collect(iterator,context);}
function all(iterator,context){iterator=iterator||Prototype.K;var result=true;this.each(function(value,index){result=result&&!!iterator.call(context,value,index);if(!result)throw $break;});return result;}
function any(iterator,context){iterator=iterator||Prototype.K;var result=false;this.each(function(value,index){if(result=!!iterator.call(context,value,index))
throw $break;});return result;}
function collect(iterator,context){iterator=iterator||Prototype.K;var results=[];this.each(function(value,index){results.push(iterator.call(context,value,index));});return results;}
function detect(iterator,context){var result;this.each(function(value,index){if(iterator.call(context,value,index)){result=value;throw $break;}});return result;}
function findAll(iterator,context){var results=[];this.each(function(value,index){if(iterator.call(context,value,index))
results.push(value);});return results;}
function grep(filter,iterator,context){iterator=iterator||Prototype.K;var results=[];if(Object.isString(filter))
filter=new RegExp(RegExp.escape(filter));this.each(function(value,index){if(filter.match(value))
results.push(iterator.call(context,value,index));});return results;}
function include(object){if(Object.isFunction(this.indexOf))
if(this.indexOf(object)!=-1)return true;var found=false;this.each(function(value){if(value==object){found=true;throw $break;}});return found;}
function inGroupsOf(number,fillWith){fillWith=Object.isUndefined(fillWith)?null:fillWith;return this.eachSlice(number,function(slice){while(slice.length<number)slice.push(fillWith);return slice;});}
function inject(memo,iterator,context){this.each(function(value,index){memo=iterator.call(context,memo,value,index);});return memo;}
function invoke(method){var args=$A(arguments).slice(1);return this.map(function(value){return value[method].apply(value,args);});}
function max(iterator,context){iterator=iterator||Prototype.K;var result;this.each(function(value,index){value=iterator.call(context,value,index);if(result==null||value>=result)
result=value;});return result;}
function min(iterator,context){iterator=iterator||Prototype.K;var result;this.each(function(value,index){value=iterator.call(context,value,index);if(result==null||value<result)
result=value;});return result;}
function partition(iterator,context){iterator=iterator||Prototype.K;var trues=[],falses=[];this.each(function(value,index){(iterator.call(context,value,index)?trues:falses).push(value);});return[trues,falses];}
function pluck(property){var results=[];this.each(function(value){results.push(value[property]);});return results;}
function reject(iterator,context){var results=[];this.each(function(value,index){if(!iterator.call(context,value,index))
results.push(value);});return results;}
function sortBy(iterator,context){return this.map(function(value,index){return{value:value,criteria:iterator.call(context,value,index)};}).sort(function(left,right){var a=left.criteria,b=right.criteria;return a<b?-1:a>b?1:0;}).pluck('value');}
function toArray(){return this.map();}
function zip(){var iterator=Prototype.K,args=$A(arguments);if(Object.isFunction(args.last()))
iterator=args.pop();var collections=[this].concat(args).map($A);return this.map(function(value,index){return iterator(collections.pluck(index));});}
function size(){return this.toArray().length;}
function inspect(){return'#<Enumerable:'+this.toArray().inspect()+'>';}
return{each:each,eachSlice:eachSlice,all:all,every:all,any:any,some:any,collect:collect,map:collect,detect:detect,findAll:findAll,select:findAll,filter:findAll,grep:grep,include:include,member:include,inGroupsOf:inGroupsOf,inject:inject,invoke:invoke,max:max,min:min,partition:partition,pluck:pluck,reject:reject,sortBy:sortBy,toArray:toArray,entries:toArray,zip:zip,size:size,inspect:inspect,find:detect};})();function $A(iterable){if(!iterable)return[];if('toArray'in Object(iterable))return iterable.toArray();var length=iterable.length||0,results=new Array(length);while(length--)results[length]=iterable[length];return results;}
function $w(string){if(!Object.isString(string))return[];string=string.strip();return string?string.split(/\s+/):[];}
Array.from=$A;(function(){var arrayProto=Array.prototype,slice=arrayProto.slice,_each=arrayProto.forEach;function each(iterator,context){for(var i=0,length=this.length>>>0;i<length;i++){if(i in this)iterator.call(context,this[i],i,this);}}
if(!_each)_each=each;function clear(){this.length=0;return this;}
function first(){return this[0];}
function last(){return this[this.length-1];}
function compact(){return this.select(function(value){return value!=null;});}
function flatten(){return this.inject([],function(array,value){if(Object.isArray(value))
return array.concat(value.flatten());array.push(value);return array;});}
function without(){var values=slice.call(arguments,0);return this.select(function(value){return!values.include(value);});}
function reverse(inline){return(inline===false?this.toArray():this)._reverse();}
function uniq(sorted){return this.inject([],function(array,value,index){if(0==index||(sorted?array.last()!=value:!array.include(value)))
array.push(value);return array;});}
function intersect(array){return this.uniq().findAll(function(item){return array.detect(function(value){return item===value});});}
function clone(){return slice.call(this,0);}
function size(){return this.length;}
function inspect(){return'['+this.map(Object.inspect).join(', ')+']';}
function indexOf(item,i){i||(i=0);var length=this.length;if(i<0)i=length+i;for(;i<length;i++)
if(this[i]===item)return i;return-1;}
function lastIndexOf(item,i){i=isNaN(i)?this.length:(i<0?this.length+i:i)+1;var n=this.slice(0,i).reverse().indexOf(item);return(n<0)?n:i-n-1;}
function concat(){var array=slice.call(this,0),item;for(var i=0,length=arguments.length;i<length;i++){item=arguments[i];if(Object.isArray(item)&&!('callee'in item)){for(var j=0,arrayLength=item.length;j<arrayLength;j++)
array.push(item[j]);}else{array.push(item);}}
return array;}
Object.extend(arrayProto,Enumerable);if(!arrayProto._reverse)
arrayProto._reverse=arrayProto.reverse;Object.extend(arrayProto,{_each:_each,clear:clear,first:first,last:last,compact:compact,flatten:flatten,without:without,reverse:reverse,uniq:uniq,intersect:intersect,clone:clone,toArray:clone,size:size,inspect:inspect});var CONCAT_ARGUMENTS_BUGGY=(function(){return[].concat(arguments)[0][0]!==1;})(1,2)
if(CONCAT_ARGUMENTS_BUGGY)arrayProto.concat=concat;if(!arrayProto.indexOf)arrayProto.indexOf=indexOf;if(!arrayProto.lastIndexOf)arrayProto.lastIndexOf=lastIndexOf;})();function $H(object){return new Hash(object);};var Hash=Class.create(Enumerable,(function(){function initialize(object){this._object=Object.isHash(object)?object.toObject():Object.clone(object);}
function _each(iterator){for(var key in this._object){var value=this._object[key],pair=[key,value];pair.key=key;pair.value=value;iterator(pair);}}
function set(key,value){return this._object[key]=value;}
function get(key){if(this._object[key]!==Object.prototype[key])
return this._object[key];}
function unset(key){var value=this._object[key];delete this._object[key];return value;}
function toObject(){return Object.clone(this._object);}
function keys(){return this.pluck('key');}
function values(){return this.pluck('value');}
function index(value){var match=this.detect(function(pair){return pair.value===value;});return match&&match.key;}
function merge(object){return this.clone().update(object);}
function update(object){return new Hash(object).inject(this,function(result,pair){result.set(pair.key,pair.value);return result;});}
function toQueryPair(key,value){if(Object.isUndefined(value))return key;return key+'='+encodeURIComponent(String.interpret(value));}
function toQueryString(){return this.inject([],function(results,pair){var key=encodeURIComponent(pair.key),values=pair.value;if(values&&typeof values=='object'){if(Object.isArray(values)){var queryValues=[];for(var i=0,len=values.length,value;i<len;i++){value=values[i];queryValues.push(toQueryPair(key,value));}
return results.concat(queryValues);}}else results.push(toQueryPair(key,values));return results;}).join('&');}
function inspect(){return'#<Hash:{'+this.map(function(pair){return pair.map(Object.inspect).join(': ');}).join(', ')+'}>';}
function clone(){return new Hash(this);}
return{initialize:initialize,_each:_each,set:set,get:get,unset:unset,toObject:toObject,toTemplateReplacements:toObject,keys:keys,values:values,index:index,merge:merge,update:update,toQueryString:toQueryString,inspect:inspect,toJSON:toObject,clone:clone};})());Hash.from=$H;Object.extend(Number.prototype,(function(){function toColorPart(){return this.toPaddedString(2,16);}
function succ(){return this+1;}
function times(iterator,context){$R(0,this,true).each(iterator,context);return this;}
function toPaddedString(length,radix){var string=this.toString(radix||10);return'0'.times(length-string.length)+string;}
function abs(){return Math.abs(this);}
function round(){return Math.round(this);}
function ceil(){return Math.ceil(this);}
function floor(){return Math.floor(this);}
return{toColorPart:toColorPart,succ:succ,times:times,toPaddedString:toPaddedString,abs:abs,round:round,ceil:ceil,floor:floor};})());function $R(start,end,exclusive){return new ObjectRange(start,end,exclusive);}
var ObjectRange=Class.create(Enumerable,(function(){function initialize(start,end,exclusive){this.start=start;this.end=end;this.exclusive=exclusive;}
function _each(iterator){var value=this.start;while(this.include(value)){iterator(value);value=value.succ();}}
function include(value){if(value<this.start)
return false;if(this.exclusive)
return value<this.end;return value<=this.end;}
return{initialize:initialize,_each:_each,include:include};})());var Ajax={getTransport:function(){return Try.these(function(){return new XMLHttpRequest()},function(){return new ActiveXObject('Msxml2.XMLHTTP')},function(){return new ActiveXObject('Microsoft.XMLHTTP')})||false;},activeRequestCount:0};Ajax.Responders={responders:[],_each:function(iterator){this.responders._each(iterator);},register:function(responder){if(!this.include(responder))
this.responders.push(responder);},unregister:function(responder){this.responders=this.responders.without(responder);},dispatch:function(callback,request,transport,json){this.each(function(responder){if(Object.isFunction(responder[callback])){try{responder[callback].apply(responder,[request,transport,json]);}catch(e){}}});}};Object.extend(Ajax.Responders,Enumerable);Ajax.Responders.register({onCreate:function(){Ajax.activeRequestCount++},onComplete:function(){Ajax.activeRequestCount--}});Ajax.Base=Class.create({initialize:function(options){this.options={method:'post',asynchronous:true,contentType:'application/x-www-form-urlencoded',encoding:'UTF-8',parameters:'',evalJSON:true,evalJS:true};Object.extend(this.options,options||{});this.options.method=this.options.method.toLowerCase();if(Object.isHash(this.options.parameters))
this.options.parameters=this.options.parameters.toObject();}});Ajax.Request=Class.create(Ajax.Base,{_complete:false,initialize:function($super,url,options){$super(options);this.transport=Ajax.getTransport();this.request(url);},request:function(url){this.url=url;this.method=this.options.method;var params=Object.isString(this.options.parameters)?this.options.parameters:Object.toQueryString(this.options.parameters);if(!['get','post'].include(this.method)){params+=(params?'&':'')+"_method="+this.method;this.method='post';}
if(params&&this.method==='get'){this.url+=(this.url.include('?')?'&':'?')+params;}
this.parameters=params.toQueryParams();try{var response=new Ajax.Response(this);if(this.options.onCreate)this.options.onCreate(response);Ajax.Responders.dispatch('onCreate',this,response);this.transport.open(this.method.toUpperCase(),this.url,this.options.asynchronous);if(this.options.asynchronous)this.respondToReadyState.bind(this).defer(1);this.transport.onreadystatechange=this.onStateChange.bind(this);this.setRequestHeaders();this.body=this.method=='post'?(this.options.postBody||params):null;this.transport.send(this.body);if(!this.options.asynchronous&&this.transport.overrideMimeType)
this.onStateChange();}
catch(e){this.dispatchException(e);}},onStateChange:function(){var readyState=this.transport.readyState;if(readyState>1&&!((readyState==4)&&this._complete))
this.respondToReadyState(this.transport.readyState);},setRequestHeaders:function(){var headers={'X-Requested-With':'XMLHttpRequest','X-Prototype-Version':Prototype.Version,'Accept':'text/javascript, text/html, application/xml, text/xml, */*'};if(this.method=='post'){headers['Content-type']=this.options.contentType+
(this.options.encoding?'; charset='+this.options.encoding:'');if(this.transport.overrideMimeType&&(navigator.userAgent.match(/Gecko\/(\d{4})/)||[0,2005])[1]<2005)
headers['Connection']='close';}
if(typeof this.options.requestHeaders=='object'){var extras=this.options.requestHeaders;if(Object.isFunction(extras.push))
for(var i=0,length=extras.length;i<length;i+=2)
headers[extras[i]]=extras[i+1];else
$H(extras).each(function(pair){headers[pair.key]=pair.value});}
for(var name in headers)
this.transport.setRequestHeader(name,headers[name]);},success:function(){var status=this.getStatus();return!status||(status>=200&&status<300)||status==304;},getStatus:function(){try{if(this.transport.status===1223)return 204;return this.transport.status||0;}catch(e){return 0}},respondToReadyState:function(readyState){var state=Ajax.Request.Events[readyState],response=new Ajax.Response(this);if(state=='Complete'){try{this._complete=true;(this.options['on'+response.status]||this.options['on'+(this.success()?'Success':'Failure')]||Prototype.emptyFunction)(response,response.headerJSON);}catch(e){this.dispatchException(e);}
var contentType=response.getHeader('Content-type');if(this.options.evalJS=='force'||(this.options.evalJS&&this.isSameOrigin()&&contentType&&contentType.match(/^\s*(text|application)\/(x-)?(java|ecma)script(;.*)?\s*$/i)))
this.evalResponse();}
try{(this.options['on'+state]||Prototype.emptyFunction)(response,response.headerJSON);Ajax.Responders.dispatch('on'+state,this,response,response.headerJSON);}catch(e){this.dispatchException(e);}
if(state=='Complete'){this.transport.onreadystatechange=Prototype.emptyFunction;}},isSameOrigin:function(){var m=this.url.match(/^\s*https?:\/\/[^\/]*/);return!m||(m[0]=='#{protocol}//#{domain}#{port}'.interpolate({protocol:location.protocol,domain:document.domain,port:location.port?':'+location.port:''}));},getHeader:function(name){try{return this.transport.getResponseHeader(name)||null;}catch(e){return null;}},evalResponse:function(){try{return eval((this.transport.responseText||'').unfilterJSON());}catch(e){this.dispatchException(e);}},dispatchException:function(exception){(this.options.onException||Prototype.emptyFunction)(this,exception);Ajax.Responders.dispatch('onException',this,exception);}});Ajax.Request.Events=['Uninitialized','Loading','Loaded','Interactive','Complete'];Ajax.Response=Class.create({initialize:function(request){this.request=request;var transport=this.transport=request.transport,readyState=this.readyState=transport.readyState;if((readyState>2&&!Prototype.Browser.IE)||readyState==4){this.status=this.getStatus();this.statusText=this.getStatusText();this.responseText=String.interpret(transport.responseText);this.headerJSON=this._getHeaderJSON();}
if(readyState==4){var xml=transport.responseXML;this.responseXML=Object.isUndefined(xml)?null:xml;this.responseJSON=this._getResponseJSON();}},status:0,statusText:'',getStatus:Ajax.Request.prototype.getStatus,getStatusText:function(){try{return this.transport.statusText||'';}catch(e){return''}},getHeader:Ajax.Request.prototype.getHeader,getAllHeaders:function(){try{return this.getAllResponseHeaders();}catch(e){return null}},getResponseHeader:function(name){return this.transport.getResponseHeader(name);},getAllResponseHeaders:function(){return this.transport.getAllResponseHeaders();},_getHeaderJSON:function(){var json=this.getHeader('X-JSON');if(!json)return null;json=decodeURIComponent(escape(json));try{return json.evalJSON(this.request.options.sanitizeJSON||!this.request.isSameOrigin());}catch(e){this.request.dispatchException(e);}},_getResponseJSON:function(){var options=this.request.options;if(!options.evalJSON||(options.evalJSON!='force'&&!(this.getHeader('Content-type')||'').include('application/json'))||this.responseText.blank())
return null;try{return this.responseText.evalJSON(options.sanitizeJSON||!this.request.isSameOrigin());}catch(e){this.request.dispatchException(e);}}});Ajax.Updater=Class.create(Ajax.Request,{initialize:function($super,container,url,options){this.container={success:(container.success||container),failure:(container.failure||(container.success?null:container))};options=Object.clone(options);var onComplete=options.onComplete;options.onComplete=(function(response,json){this.updateContent(response.responseText);if(Object.isFunction(onComplete))onComplete(response,json);}).bind(this);$super(url,options);},updateContent:function(responseText){var receiver=this.container[this.success()?'success':'failure'],options=this.options;if(!options.evalScripts)responseText=responseText.stripScripts();if(receiver=$(receiver)){if(options.insertion){if(Object.isString(options.insertion)){var insertion={};insertion[options.insertion]=responseText;receiver.insert(insertion);}
else options.insertion(receiver,responseText);}
else receiver.update(responseText);}}});Ajax.PeriodicalUpdater=Class.create(Ajax.Base,{initialize:function($super,container,url,options){$super(options);this.onComplete=this.options.onComplete;this.frequency=(this.options.frequency||2);this.decay=(this.options.decay||1);this.updater={};this.container=container;this.url=url;this.start();},start:function(){this.options.onComplete=this.updateComplete.bind(this);this.onTimerEvent();},stop:function(){this.updater.options.onComplete=undefined;clearTimeout(this.timer);(this.onComplete||Prototype.emptyFunction).apply(this,arguments);},updateComplete:function(response){if(this.options.decay){this.decay=(response.responseText==this.lastText?this.decay*this.options.decay:1);this.lastText=response.responseText;}
this.timer=this.onTimerEvent.bind(this).delay(this.decay*this.frequency);},onTimerEvent:function(){this.updater=new Ajax.Updater(this.container,this.url,this.options);}});function $(element){if(arguments.length>1){for(var i=0,elements=[],length=arguments.length;i<length;i++)
elements.push($(arguments[i]));return elements;}
if(Object.isString(element))
element=document.getElementById(element);return Element.extend(element);}
if(Prototype.BrowserFeatures.XPath){document._getElementsByXPath=function(expression,parentElement){var results=[];var query=document.evaluate(expression,$(parentElement)||document,null,XPathResult.ORDERED_NODE_SNAPSHOT_TYPE,null);for(var i=0,length=query.snapshotLength;i<length;i++)
results.push(Element.extend(query.snapshotItem(i)));return results;};}
if(!Node)var Node={};if(!Node.ELEMENT_NODE){Object.extend(Node,{ELEMENT_NODE:1,ATTRIBUTE_NODE:2,TEXT_NODE:3,CDATA_SECTION_NODE:4,ENTITY_REFERENCE_NODE:5,ENTITY_NODE:6,PROCESSING_INSTRUCTION_NODE:7,COMMENT_NODE:8,DOCUMENT_NODE:9,DOCUMENT_TYPE_NODE:10,DOCUMENT_FRAGMENT_NODE:11,NOTATION_NODE:12});}
(function(global){function shouldUseCache(tagName,attributes){if(tagName==='select')return false;if('type'in attributes)return false;return true;}
var HAS_EXTENDED_CREATE_ELEMENT_SYNTAX=(function(){try{var el=document.createElement('<input name="x">');return el.tagName.toLowerCase()==='input'&&el.name==='x';}
catch(err){return false;}})();var element=global.Element;global.Element=function(tagName,attributes){attributes=attributes||{};tagName=tagName.toLowerCase();var cache=Element.cache;if(HAS_EXTENDED_CREATE_ELEMENT_SYNTAX&&attributes.name){tagName='<'+tagName+' name="'+attributes.name+'">';delete attributes.name;return Element.writeAttribute(document.createElement(tagName),attributes);}
if(!cache[tagName])cache[tagName]=Element.extend(document.createElement(tagName));var node=shouldUseCache(tagName,attributes)?cache[tagName].cloneNode(false):document.createElement(tagName);return Element.writeAttribute(node,attributes);};Object.extend(global.Element,element||{});if(element)global.Element.prototype=element.prototype;})(this);Element.idCounter=1;Element.cache={};Element._purgeElement=function(element){var uid=element._prototypeUID;if(uid){Element.stopObserving(element);element._prototypeUID=void 0;delete Element.Storage[uid];}}
Element.Methods={visible:function(element){return $(element).style.display!='none';},toggle:function(element){element=$(element);Element[Element.visible(element)?'hide':'show'](element);return element;},hide:function(element){element=$(element);element.style.display='none';return element;},show:function(element){element=$(element);element.style.display='';return element;},remove:function(element){element=$(element);element.parentNode.removeChild(element);return element;},update:(function(){var SELECT_ELEMENT_INNERHTML_BUGGY=(function(){var el=document.createElement("select"),isBuggy=true;el.innerHTML="<option value=\"test\">test</option>";if(el.options&&el.options[0]){isBuggy=el.options[0].nodeName.toUpperCase()!=="OPTION";}
el=null;return isBuggy;})();var TABLE_ELEMENT_INNERHTML_BUGGY=(function(){try{var el=document.createElement("table");if(el&&el.tBodies){el.innerHTML="<tbody><tr><td>test</td></tr></tbody>";var isBuggy=typeof el.tBodies[0]=="undefined";el=null;return isBuggy;}}catch(e){return true;}})();var LINK_ELEMENT_INNERHTML_BUGGY=(function(){try{var el=document.createElement('div');el.innerHTML="<link>";var isBuggy=(el.childNodes.length===0);el=null;return isBuggy;}catch(e){return true;}})();var ANY_INNERHTML_BUGGY=SELECT_ELEMENT_INNERHTML_BUGGY||TABLE_ELEMENT_INNERHTML_BUGGY||LINK_ELEMENT_INNERHTML_BUGGY;var SCRIPT_ELEMENT_REJECTS_TEXTNODE_APPENDING=(function(){var s=document.createElement("script"),isBuggy=false;try{s.appendChild(document.createTextNode(""));isBuggy=!s.firstChild||s.firstChild&&s.firstChild.nodeType!==3;}catch(e){isBuggy=true;}
s=null;return isBuggy;})();function update(element,content){element=$(element);var purgeElement=Element._purgeElement;var descendants=element.getElementsByTagName('*'),i=descendants.length;while(i--)purgeElement(descendants[i]);if(content&&content.toElement)
content=content.toElement();if(Object.isElement(content))
return element.update().insert(content);content=Object.toHTML(content);var tagName=element.tagName.toUpperCase();if(tagName==='SCRIPT'&&SCRIPT_ELEMENT_REJECTS_TEXTNODE_APPENDING){element.text=content;return element;}
if(ANY_INNERHTML_BUGGY){if(tagName in Element._insertionTranslations.tags){while(element.firstChild){element.removeChild(element.firstChild);}
Element._getContentFromAnonymousElement(tagName,content.stripScripts()).each(function(node){element.appendChild(node)});}else if(LINK_ELEMENT_INNERHTML_BUGGY&&Object.isString(content)&&content.indexOf('<link')>-1){while(element.firstChild){element.removeChild(element.firstChild);}
var nodes=Element._getContentFromAnonymousElement(tagName,content.stripScripts(),true);nodes.each(function(node){element.appendChild(node)});}
else{element.innerHTML=content.stripScripts();}}
else{element.innerHTML=content.stripScripts();}
content.evalScripts.bind(content).defer();return element;}
return update;})(),replace:function(element,content){element=$(element);if(content&&content.toElement)content=content.toElement();else if(!Object.isElement(content)){content=Object.toHTML(content);var range=element.ownerDocument.createRange();range.selectNode(element);content.evalScripts.bind(content).defer();content=range.createContextualFragment(content.stripScripts());}
element.parentNode.replaceChild(content,element);return element;},insert:function(element,insertions){element=$(element);if(Object.isString(insertions)||Object.isNumber(insertions)||Object.isElement(insertions)||(insertions&&(insertions.toElement||insertions.toHTML)))
insertions={bottom:insertions};var content,insert,tagName,childNodes;for(var position in insertions){content=insertions[position];position=position.toLowerCase();insert=Element._insertionTranslations[position];if(content&&content.toElement)content=content.toElement();if(Object.isElement(content)){insert(element,content);continue;}
content=Object.toHTML(content);tagName=((position=='before'||position=='after')?element.parentNode:element).tagName.toUpperCase();childNodes=Element._getContentFromAnonymousElement(tagName,content.stripScripts());if(position=='top'||position=='after')childNodes.reverse();childNodes.each(insert.curry(element));content.evalScripts.bind(content).defer();}
return element;},wrap:function(element,wrapper,attributes){element=$(element);if(Object.isElement(wrapper))
$(wrapper).writeAttribute(attributes||{});else if(Object.isString(wrapper))wrapper=new Element(wrapper,attributes);else wrapper=new Element('div',wrapper);if(element.parentNode)
element.parentNode.replaceChild(wrapper,element);wrapper.appendChild(element);return wrapper;},inspect:function(element){element=$(element);var result='<'+element.tagName.toLowerCase();$H({'id':'id','className':'class'}).each(function(pair){var property=pair.first(),attribute=pair.last(),value=(element[property]||'').toString();if(value)result+=' '+attribute+'='+value.inspect(true);});return result+'>';},recursivelyCollect:function(element,property,maximumLength){element=$(element);maximumLength=maximumLength||-1;var elements=[];while(element=element[property]){if(element.nodeType==1)
elements.push(Element.extend(element));if(elements.length==maximumLength)
break;}
return elements;},ancestors:function(element){return Element.recursivelyCollect(element,'parentNode');},descendants:function(element){return Element.select(element,"*");},firstDescendant:function(element){element=$(element).firstChild;while(element&&element.nodeType!=1)element=element.nextSibling;return $(element);},immediateDescendants:function(element){var results=[],child=$(element).firstChild;while(child){if(child.nodeType===1){results.push(Element.extend(child));}
child=child.nextSibling;}
return results;},previousSiblings:function(element,maximumLength){return Element.recursivelyCollect(element,'previousSibling');},nextSiblings:function(element){return Element.recursivelyCollect(element,'nextSibling');},siblings:function(element){element=$(element);return Element.previousSiblings(element).reverse().concat(Element.nextSiblings(element));},match:function(element,selector){element=$(element);if(Object.isString(selector))
return Prototype.Selector.match(element,selector);return selector.match(element);},up:function(element,expression,index){element=$(element);if(arguments.length==1)return $(element.parentNode);var ancestors=Element.ancestors(element);return Object.isNumber(expression)?ancestors[expression]:Prototype.Selector.find(ancestors,expression,index);},down:function(element,expression,index){element=$(element);if(arguments.length==1)return Element.firstDescendant(element);return Object.isNumber(expression)?Element.descendants(element)[expression]:Element.select(element,expression)[index||0];},previous:function(element,expression,index){element=$(element);if(Object.isNumber(expression))index=expression,expression=false;if(!Object.isNumber(index))index=0;if(expression){return Prototype.Selector.find(element.previousSiblings(),expression,index);}else{return element.recursivelyCollect("previousSibling",index+1)[index];}},next:function(element,expression,index){element=$(element);if(Object.isNumber(expression))index=expression,expression=false;if(!Object.isNumber(index))index=0;if(expression){return Prototype.Selector.find(element.nextSiblings(),expression,index);}else{var maximumLength=Object.isNumber(index)?index+1:1;return element.recursivelyCollect("nextSibling",index+1)[index];}},select:function(element){element=$(element);var expressions=Array.prototype.slice.call(arguments,1).join(', ');return Prototype.Selector.select(expressions,element);},adjacent:function(element){element=$(element);var expressions=Array.prototype.slice.call(arguments,1).join(', ');return Prototype.Selector.select(expressions,element.parentNode).without(element);},identify:function(element){element=$(element);var id=Element.readAttribute(element,'id');if(id)return id;do{id='anonymous_element_'+Element.idCounter++}while($(id));Element.writeAttribute(element,'id',id);return id;},readAttribute:function(element,name){element=$(element);if(Prototype.Browser.IE){var t=Element._attributeTranslations.read;if(t.values[name])return t.values[name](element,name);if(t.names[name])name=t.names[name];if(name.include(':')){return(!element.attributes||!element.attributes[name])?null:element.attributes[name].value;}}
return element.getAttribute(name);},writeAttribute:function(element,name,value){element=$(element);var attributes={},t=Element._attributeTranslations.write;if(typeof name=='object')attributes=name;else attributes[name]=Object.isUndefined(value)?true:value;for(var attr in attributes){name=t.names[attr]||attr;value=attributes[attr];if(t.values[attr])name=t.values[attr](element,value);if(value===false||value===null)
element.removeAttribute(name);else if(value===true)
element.setAttribute(name,name);else element.setAttribute(name,value);}
return element;},getHeight:function(element){return Element.getDimensions(element).height;},getWidth:function(element){return Element.getDimensions(element).width;},classNames:function(element){return new Element.ClassNames(element);},hasClassName:function(element,className){if(!(element=$(element)))return;var elementClassName=element.className;return(elementClassName.length>0&&(elementClassName==className||new RegExp("(^|\\s)"+className+"(\\s|$)").test(elementClassName)));},addClassName:function(element,className){if(!(element=$(element)))return;if(!Element.hasClassName(element,className))
element.className+=(element.className?' ':'')+className;return element;},removeClassName:function(element,className){if(!(element=$(element)))return;element.className=element.className.replace(new RegExp("(^|\\s+)"+className+"(\\s+|$)"),' ').strip();return element;},toggleClassName:function(element,className){if(!(element=$(element)))return;return Element[Element.hasClassName(element,className)?'removeClassName':'addClassName'](element,className);},cleanWhitespace:function(element){element=$(element);var node=element.firstChild;while(node){var nextNode=node.nextSibling;if(node.nodeType==3&&!/\S/.test(node.nodeValue))
element.removeChild(node);node=nextNode;}
return element;},empty:function(element){return $(element).innerHTML.blank();},descendantOf:function(element,ancestor){element=$(element),ancestor=$(ancestor);if(element.compareDocumentPosition)
return(element.compareDocumentPosition(ancestor)&8)===8;if(ancestor.contains)
return ancestor.contains(element)&&ancestor!==element;while(element=element.parentNode)
if(element==ancestor)return true;return false;},scrollTo:function(element){element=$(element);var pos=Element.cumulativeOffset(element);window.scrollTo(pos[0],pos[1]);return element;},getStyle:function(element,style){element=$(element);style=style=='float'?'cssFloat':style.camelize();var value=element.style[style];if(!value||value=='auto'){var css=document.defaultView.getComputedStyle(element,null);value=css?css[style]:null;}
if(style=='opacity')return value?parseFloat(value):1.0;return value=='auto'?null:value;},getOpacity:function(element){return $(element).getStyle('opacity');},setStyle:function(element,styles){element=$(element);var elementStyle=element.style,match;if(Object.isString(styles)){element.style.cssText+=';'+styles;return styles.include('opacity')?element.setOpacity(styles.match(/opacity:\s*(\d?\.?\d*)/)[1]):element;}
for(var property in styles)
if(property=='opacity')element.setOpacity(styles[property]);else
elementStyle[(property=='float'||property=='cssFloat')?(Object.isUndefined(elementStyle.styleFloat)?'cssFloat':'styleFloat'):property]=styles[property];return element;},setOpacity:function(element,value){element=$(element);element.style.opacity=(value==1||value==='')?'':(value<0.00001)?0:value;return element;},makePositioned:function(element){element=$(element);var pos=Element.getStyle(element,'position');if(pos=='static'||!pos){element._madePositioned=true;element.style.position='relative';if(Prototype.Browser.Opera){element.style.top=0;element.style.left=0;}}
return element;},undoPositioned:function(element){element=$(element);if(element._madePositioned){element._madePositioned=undefined;element.style.position=element.style.top=element.style.left=element.style.bottom=element.style.right='';}
return element;},makeClipping:function(element){element=$(element);if(element._overflow)return element;element._overflow=Element.getStyle(element,'overflow')||'auto';if(element._overflow!=='hidden')
element.style.overflow='hidden';return element;},undoClipping:function(element){element=$(element);if(!element._overflow)return element;element.style.overflow=element._overflow=='auto'?'':element._overflow;element._overflow=null;return element;},clonePosition:function(element,source){var options=Object.extend({setLeft:true,setTop:true,setWidth:true,setHeight:true,offsetTop:0,offsetLeft:0},arguments[2]||{});source=$(source);var p=Element.viewportOffset(source),delta=[0,0],parent=null;element=$(element);if(Element.getStyle(element,'position')=='absolute'){parent=Element.getOffsetParent(element);delta=Element.viewportOffset(parent);}
if(parent==document.body){delta[0]-=document.body.offsetLeft;delta[1]-=document.body.offsetTop;}
if(options.setLeft)element.style.left=(p[0]-delta[0]+options.offsetLeft)+'px';if(options.setTop)element.style.top=(p[1]-delta[1]+options.offsetTop)+'px';if(options.setWidth)element.style.width=source.offsetWidth+'px';if(options.setHeight)element.style.height=source.offsetHeight+'px';return element;}};Object.extend(Element.Methods,{getElementsBySelector:Element.Methods.select,childElements:Element.Methods.immediateDescendants});Element._attributeTranslations={write:{names:{className:'class',htmlFor:'for'},values:{}}};if(Prototype.Browser.Opera){Element.Methods.getStyle=Element.Methods.getStyle.wrap(function(proceed,element,style){switch(style){case'height':case'width':if(!Element.visible(element))return null;var dim=parseInt(proceed(element,style),10);if(dim!==element['offset'+style.capitalize()])
return dim+'px';var properties;if(style==='height'){properties=['border-top-width','padding-top','padding-bottom','border-bottom-width'];}
else{properties=['border-left-width','padding-left','padding-right','border-right-width'];}
return properties.inject(dim,function(memo,property){var val=proceed(element,property);return val===null?memo:memo-parseInt(val,10);})+'px';default:return proceed(element,style);}});Element.Methods.readAttribute=Element.Methods.readAttribute.wrap(function(proceed,element,attribute){if(attribute==='title')return element.title;return proceed(element,attribute);});}
else if(Prototype.Browser.IE){Element.Methods.getStyle=function(element,style){element=$(element);style=(style=='float'||style=='cssFloat')?'styleFloat':style.camelize();var value=element.style[style];if(!value&&element.currentStyle)value=element.currentStyle[style];if(style=='opacity'){if(value=(element.getStyle('filter')||'').match(/alpha\(opacity=(.*)\)/))
if(value[1])return parseFloat(value[1])/100;return 1.0;}
if(value=='auto'){if((style=='width'||style=='height')&&(element.getStyle('display')!='none'))
return element['offset'+style.capitalize()]+'px';return null;}
return value;};Element.Methods.setOpacity=function(element,value){function stripAlpha(filter){return filter.replace(/alpha\([^\)]*\)/gi,'');}
element=$(element);var currentStyle=element.currentStyle;if((currentStyle&&!currentStyle.hasLayout)||(!currentStyle&&element.style.zoom=='normal'))
element.style.zoom=1;var filter=element.getStyle('filter'),style=element.style;if(value==1||value===''){(filter=stripAlpha(filter))?style.filter=filter:style.removeAttribute('filter');return element;}else if(value<0.00001)value=0;style.filter=stripAlpha(filter)+'alpha(opacity='+(value*100)+')';return element;};Element._attributeTranslations=(function(){var classProp='className',forProp='for',el=document.createElement('div');el.setAttribute(classProp,'x');if(el.className!=='x'){el.setAttribute('class','x');if(el.className==='x'){classProp='class';}}
el=null;el=document.createElement('label');el.setAttribute(forProp,'x');if(el.htmlFor!=='x'){el.setAttribute('htmlFor','x');if(el.htmlFor==='x'){forProp='htmlFor';}}
el=null;return{read:{names:{'class':classProp,'className':classProp,'for':forProp,'htmlFor':forProp},values:{_getAttr:function(element,attribute){return element.getAttribute(attribute);},_getAttr2:function(element,attribute){return element.getAttribute(attribute,2);},_getAttrNode:function(element,attribute){var node=element.getAttributeNode(attribute);return node?node.value:"";},_getEv:(function(){var el=document.createElement('div'),f;el.onclick=Prototype.emptyFunction;var value=el.getAttribute('onclick');if(String(value).indexOf('{')>-1){f=function(element,attribute){attribute=element.getAttribute(attribute);if(!attribute)return null;attribute=attribute.toString();attribute=attribute.split('{')[1];attribute=attribute.split('}')[0];return attribute.strip();};}
else if(value===''){f=function(element,attribute){attribute=element.getAttribute(attribute);if(!attribute)return null;return attribute.strip();};}
el=null;return f;})(),_flag:function(element,attribute){return $(element).hasAttribute(attribute)?attribute:null;},style:function(element){return element.style.cssText.toLowerCase();},title:function(element){return element.title;}}}}})();Element._attributeTranslations.write={names:Object.extend({cellpadding:'cellPadding',cellspacing:'cellSpacing'},Element._attributeTranslations.read.names),values:{checked:function(element,value){element.checked=!!value;},style:function(element,value){element.style.cssText=value?value:'';}}};Element._attributeTranslations.has={};$w('colSpan rowSpan vAlign dateTime accessKey tabIndex '+'encType maxLength readOnly longDesc frameBorder').each(function(attr){Element._attributeTranslations.write.names[attr.toLowerCase()]=attr;Element._attributeTranslations.has[attr.toLowerCase()]=attr;});(function(v){Object.extend(v,{href:v._getAttr2,src:v._getAttr2,type:v._getAttr,action:v._getAttrNode,disabled:v._flag,checked:v._flag,readonly:v._flag,multiple:v._flag,onload:v._getEv,onunload:v._getEv,onclick:v._getEv,ondblclick:v._getEv,onmousedown:v._getEv,onmouseup:v._getEv,onmouseover:v._getEv,onmousemove:v._getEv,onmouseout:v._getEv,onfocus:v._getEv,onblur:v._getEv,onkeypress:v._getEv,onkeydown:v._getEv,onkeyup:v._getEv,onsubmit:v._getEv,onreset:v._getEv,onselect:v._getEv,onchange:v._getEv});})(Element._attributeTranslations.read.values);if(Prototype.BrowserFeatures.ElementExtensions){(function(){function _descendants(element){var nodes=element.getElementsByTagName('*'),results=[];for(var i=0,node;node=nodes[i];i++)
if(node.tagName!=="!")
results.push(node);return results;}
Element.Methods.down=function(element,expression,index){element=$(element);if(arguments.length==1)return element.firstDescendant();return Object.isNumber(expression)?_descendants(element)[expression]:Element.select(element,expression)[index||0];}})();}}
else if(Prototype.Browser.Gecko&&/rv:1\.8\.0/.test(navigator.userAgent)){Element.Methods.setOpacity=function(element,value){element=$(element);element.style.opacity=(value==1)?0.999999:(value==='')?'':(value<0.00001)?0:value;return element;};}
else if(Prototype.Browser.WebKit){Element.Methods.setOpacity=function(element,value){element=$(element);element.style.opacity=(value==1||value==='')?'':(value<0.00001)?0:value;if(value==1)
if(element.tagName.toUpperCase()=='IMG'&&element.width){element.width++;element.width--;}else try{var n=document.createTextNode(' ');element.appendChild(n);element.removeChild(n);}catch(e){}
return element;};}
if('outerHTML'in document.documentElement){Element.Methods.replace=function(element,content){element=$(element);if(content&&content.toElement)content=content.toElement();if(Object.isElement(content)){element.parentNode.replaceChild(content,element);return element;}
content=Object.toHTML(content);var parent=element.parentNode,tagName=parent.tagName.toUpperCase();if(Element._insertionTranslations.tags[tagName]){var nextSibling=element.next(),fragments=Element._getContentFromAnonymousElement(tagName,content.stripScripts());parent.removeChild(element);if(nextSibling)
fragments.each(function(node){parent.insertBefore(node,nextSibling)});else
fragments.each(function(node){parent.appendChild(node)});}
else element.outerHTML=content.stripScripts();content.evalScripts.bind(content).defer();return element;};}
Element._returnOffset=function(l,t){var result=[l,t];result.left=l;result.top=t;return result;};Element._getContentFromAnonymousElement=function(tagName,html,force){var div=new Element('div'),t=Element._insertionTranslations.tags[tagName];var workaround=false;if(t)workaround=true;else if(force){workaround=true;t=['','',0];}
if(workaround){div.innerHTML='&nbsp;'+t[0]+html+t[1];div.removeChild(div.firstChild);for(var i=t[2];i--;){div=div.firstChild;}}
else{div.innerHTML=html;}
return $A(div.childNodes);};Element._insertionTranslations={before:function(element,node){element.parentNode.insertBefore(node,element);},top:function(element,node){element.insertBefore(node,element.firstChild);},bottom:function(element,node){element.appendChild(node);},after:function(element,node){element.parentNode.insertBefore(node,element.nextSibling);},tags:{TABLE:['<table>','</table>',1],TBODY:['<table><tbody>','</tbody></table>',2],TR:['<table><tbody><tr>','</tr></tbody></table>',3],TD:['<table><tbody><tr><td>','</td></tr></tbody></table>',4],SELECT:['<select>','</select>',1]}};(function(){var tags=Element._insertionTranslations.tags;Object.extend(tags,{THEAD:tags.TBODY,TFOOT:tags.TBODY,TH:tags.TD});})();Element.Methods.Simulated={hasAttribute:function(element,attribute){attribute=Element._attributeTranslations.has[attribute]||attribute;var node=$(element).getAttributeNode(attribute);return!!(node&&node.specified);}};Element.Methods.ByTag={};Object.extend(Element,Element.Methods);(function(div){if(!Prototype.BrowserFeatures.ElementExtensions&&div['__proto__']){window.HTMLElement={};window.HTMLElement.prototype=div['__proto__'];Prototype.BrowserFeatures.ElementExtensions=true;}
div=null;})(document.createElement('div'));Element.extend=(function(){function checkDeficiency(tagName){if(typeof window.Element!='undefined'){var proto=window.Element.prototype;if(proto){var id='_'+(Math.random()+'').slice(2),el=document.createElement(tagName);proto[id]='x';var isBuggy=(el[id]!=='x');delete proto[id];el=null;return isBuggy;}}
return false;}
function extendElementWith(element,methods){for(var property in methods){var value=methods[property];if(Object.isFunction(value)&&!(property in element))
element[property]=value.methodize();}}
var HTMLOBJECTELEMENT_PROTOTYPE_BUGGY=checkDeficiency('object');if(Prototype.BrowserFeatures.SpecificElementExtensions){if(HTMLOBJECTELEMENT_PROTOTYPE_BUGGY){return function(element){if(element&&typeof element._extendedByPrototype=='undefined'){var t=element.tagName;if(t&&(/^(?:object|applet|embed)$/i.test(t))){extendElementWith(element,Element.Methods);extendElementWith(element,Element.Methods.Simulated);extendElementWith(element,Element.Methods.ByTag[t.toUpperCase()]);}}
return element;}}
return Prototype.K;}
var Methods={},ByTag=Element.Methods.ByTag;var extend=Object.extend(function(element){if(!element||typeof element._extendedByPrototype!='undefined'||element.nodeType!=1||element==window)return element;var methods=Object.clone(Methods),tagName=element.tagName.toUpperCase();if(ByTag[tagName])Object.extend(methods,ByTag[tagName]);extendElementWith(element,methods);element._extendedByPrototype=Prototype.emptyFunction;return element;},{refresh:function(){if(!Prototype.BrowserFeatures.ElementExtensions){Object.extend(Methods,Element.Methods);Object.extend(Methods,Element.Methods.Simulated);}}});extend.refresh();return extend;})();if(document.documentElement.hasAttribute){Element.hasAttribute=function(element,attribute){return element.hasAttribute(attribute);};}
else{Element.hasAttribute=Element.Methods.Simulated.hasAttribute;}
Element.addMethods=function(methods){var F=Prototype.BrowserFeatures,T=Element.Methods.ByTag;if(!methods){Object.extend(Form,Form.Methods);Object.extend(Form.Element,Form.Element.Methods);Object.extend(Element.Methods.ByTag,{"FORM":Object.clone(Form.Methods),"INPUT":Object.clone(Form.Element.Methods),"SELECT":Object.clone(Form.Element.Methods),"TEXTAREA":Object.clone(Form.Element.Methods),"BUTTON":Object.clone(Form.Element.Methods)});}
if(arguments.length==2){var tagName=methods;methods=arguments[1];}
if(!tagName)Object.extend(Element.Methods,methods||{});else{if(Object.isArray(tagName))tagName.each(extend);else extend(tagName);}
function extend(tagName){tagName=tagName.toUpperCase();if(!Element.Methods.ByTag[tagName])
Element.Methods.ByTag[tagName]={};Object.extend(Element.Methods.ByTag[tagName],methods);}
function copy(methods,destination,onlyIfAbsent){onlyIfAbsent=onlyIfAbsent||false;for(var property in methods){var value=methods[property];if(!Object.isFunction(value))continue;if(!onlyIfAbsent||!(property in destination))
destination[property]=value.methodize();}}
function findDOMClass(tagName){var klass;var trans={"OPTGROUP":"OptGroup","TEXTAREA":"TextArea","P":"Paragraph","FIELDSET":"FieldSet","UL":"UList","OL":"OList","DL":"DList","DIR":"Directory","H1":"Heading","H2":"Heading","H3":"Heading","H4":"Heading","H5":"Heading","H6":"Heading","Q":"Quote","INS":"Mod","DEL":"Mod","A":"Anchor","IMG":"Image","CAPTION":"TableCaption","COL":"TableCol","COLGROUP":"TableCol","THEAD":"TableSection","TFOOT":"TableSection","TBODY":"TableSection","TR":"TableRow","TH":"TableCell","TD":"TableCell","FRAMESET":"FrameSet","IFRAME":"IFrame"};if(trans[tagName])klass='HTML'+trans[tagName]+'Element';if(window[klass])return window[klass];klass='HTML'+tagName+'Element';if(window[klass])return window[klass];klass='HTML'+tagName.capitalize()+'Element';if(window[klass])return window[klass];var element=document.createElement(tagName),proto=element['__proto__']||element.constructor.prototype;element=null;return proto;}
var elementPrototype=window.HTMLElement?HTMLElement.prototype:Element.prototype;if(F.ElementExtensions){copy(Element.Methods,elementPrototype);copy(Element.Methods.Simulated,elementPrototype,true);}
if(F.SpecificElementExtensions){for(var tag in Element.Methods.ByTag){var klass=findDOMClass(tag);if(Object.isUndefined(klass))continue;copy(T[tag],klass.prototype);}}
Object.extend(Element,Element.Methods);delete Element.ByTag;if(Element.extend.refresh)Element.extend.refresh();Element.cache={};};document.viewport={getDimensions:function(){return{width:this.getWidth(),height:this.getHeight()};},getScrollOffsets:function(){return Element._returnOffset(window.pageXOffset||document.documentElement.scrollLeft||document.body.scrollLeft,window.pageYOffset||document.documentElement.scrollTop||document.body.scrollTop);}};(function(viewport){var B=Prototype.Browser,doc=document,element,property={};function getRootElement(){if(B.WebKit&&!doc.evaluate)
return document;if(B.Opera&&window.parseFloat(window.opera.version())<9.5)
return document.body;return document.documentElement;}
function define(D){if(!element)element=getRootElement();property[D]='client'+D;viewport['get'+D]=function(){return element[property[D]]};return viewport['get'+D]();}
viewport.getWidth=define.curry('Width');viewport.getHeight=define.curry('Height');})(document.viewport);Element.Storage={UID:1};Element.addMethods({getStorage:function(element){if(!(element=$(element)))return;var uid;if(element===window){uid=0;}else{if(typeof element._prototypeUID==="undefined")
element._prototypeUID=Element.Storage.UID++;uid=element._prototypeUID;}
if(!Element.Storage[uid])
Element.Storage[uid]=$H();return Element.Storage[uid];},store:function(element,key,value){if(!(element=$(element)))return;if(arguments.length===2){Element.getStorage(element).update(key);}else{Element.getStorage(element).set(key,value);}
return element;},retrieve:function(element,key,defaultValue){if(!(element=$(element)))return;var hash=Element.getStorage(element),value=hash.get(key);if(Object.isUndefined(value)){hash.set(key,defaultValue);value=defaultValue;}
return value;},clone:function(element,deep){if(!(element=$(element)))return;var clone=element.cloneNode(deep);clone._prototypeUID=void 0;if(deep){var descendants=Element.select(clone,'*'),i=descendants.length;while(i--){descendants[i]._prototypeUID=void 0;}}
return Element.extend(clone);},purge:function(element){if(!(element=$(element)))return;var purgeElement=Element._purgeElement;purgeElement(element);var descendants=element.getElementsByTagName('*'),i=descendants.length;while(i--)purgeElement(descendants[i]);return null;}});(function(){function toDecimal(pctString){var match=pctString.match(/^(\d+)%?$/i);if(!match)return null;return(Number(match[1])/100);}
function getPixelValue(value,property,context){var element=null;if(Object.isElement(value)){element=value;value=element.getStyle(property);}
if(value===null){return null;}
if((/^(?:-)?\d+(\.\d+)?(px)?$/i).test(value)){return window.parseFloat(value);}
var isPercentage=value.include('%'),isViewport=(context===document.viewport);if(/\d/.test(value)&&element&&element.runtimeStyle&&!(isPercentage&&isViewport)){var style=element.style.left,rStyle=element.runtimeStyle.left;element.runtimeStyle.left=element.currentStyle.left;element.style.left=value||0;value=element.style.pixelLeft;element.style.left=style;element.runtimeStyle.left=rStyle;return value;}
if(element&&isPercentage){context=context||element.parentNode;var decimal=toDecimal(value);var whole=null;var position=element.getStyle('position');var isHorizontal=property.include('left')||property.include('right')||property.include('width');var isVertical=property.include('top')||property.include('bottom')||property.include('height');if(context===document.viewport){if(isHorizontal){whole=document.viewport.getWidth();}else if(isVertical){whole=document.viewport.getHeight();}}else{if(isHorizontal){whole=$(context).measure('width');}else if(isVertical){whole=$(context).measure('height');}}
return(whole===null)?0:whole*decimal;}
return 0;}
function toCSSPixels(number){if(Object.isString(number)&&number.endsWith('px')){return number;}
return number+'px';}
function isDisplayed(element){var originalElement=element;while(element&&element.parentNode){var display=element.getStyle('display');if(display==='none'){return false;}
element=$(element.parentNode);}
return true;}
var hasLayout=Prototype.K;if('currentStyle'in document.documentElement){hasLayout=function(element){if(!element.currentStyle.hasLayout){element.style.zoom=1;}
return element;};}
function cssNameFor(key){if(key.include('border'))key=key+'-width';return key.camelize();}
Element.Layout=Class.create(Hash,{initialize:function($super,element,preCompute){$super();this.element=$(element);Element.Layout.PROPERTIES.each(function(property){this._set(property,null);},this);if(preCompute){this._preComputing=true;this._begin();Element.Layout.PROPERTIES.each(this._compute,this);this._end();this._preComputing=false;}},_set:function(property,value){return Hash.prototype.set.call(this,property,value);},set:function(property,value){throw"Properties of Element.Layout are read-only.";},get:function($super,property){var value=$super(property);return value===null?this._compute(property):value;},_begin:function(){if(this._prepared)return;var element=this.element;if(isDisplayed(element)){this._prepared=true;return;}
var originalStyles={position:element.style.position||'',width:element.style.width||'',visibility:element.style.visibility||'',display:element.style.display||''};element.store('prototype_original_styles',originalStyles);var position=element.getStyle('position'),width=element.getStyle('width');if(width==="0px"||width===null){element.style.display='block';width=element.getStyle('width');}
var context=(position==='fixed')?document.viewport:element.parentNode;element.setStyle({position:'absolute',visibility:'hidden',display:'block'});var positionedWidth=element.getStyle('width');var newWidth;if(width&&(positionedWidth===width)){newWidth=getPixelValue(element,'width',context);}else if(position==='absolute'||position==='fixed'){newWidth=getPixelValue(element,'width',context);}else{var parent=element.parentNode,pLayout=$(parent).getLayout();newWidth=pLayout.get('width')-
this.get('margin-left')-
this.get('border-left')-
this.get('padding-left')-
this.get('padding-right')-
this.get('border-right')-
this.get('margin-right');}
element.setStyle({width:newWidth+'px'});this._prepared=true;},_end:function(){var element=this.element;var originalStyles=element.retrieve('prototype_original_styles');element.store('prototype_original_styles',null);element.setStyle(originalStyles);this._prepared=false;},_compute:function(property){var COMPUTATIONS=Element.Layout.COMPUTATIONS;if(!(property in COMPUTATIONS)){throw"Property not found.";}
return this._set(property,COMPUTATIONS[property].call(this,this.element));},toObject:function(){var args=$A(arguments);var keys=(args.length===0)?Element.Layout.PROPERTIES:args.join(' ').split(' ');var obj={};keys.each(function(key){if(!Element.Layout.PROPERTIES.include(key))return;var value=this.get(key);if(value!=null)obj[key]=value;},this);return obj;},toHash:function(){var obj=this.toObject.apply(this,arguments);return new Hash(obj);},toCSS:function(){var args=$A(arguments);var keys=(args.length===0)?Element.Layout.PROPERTIES:args.join(' ').split(' ');var css={};keys.each(function(key){if(!Element.Layout.PROPERTIES.include(key))return;if(Element.Layout.COMPOSITE_PROPERTIES.include(key))return;var value=this.get(key);if(value!=null)css[cssNameFor(key)]=value+'px';},this);return css;},inspect:function(){return"#<Element.Layout>";}});Object.extend(Element.Layout,{PROPERTIES:$w('height width top left right bottom border-left border-right border-top border-bottom padding-left padding-right padding-top padding-bottom margin-top margin-bottom margin-left margin-right padding-box-width padding-box-height border-box-width border-box-height margin-box-width margin-box-height'),COMPOSITE_PROPERTIES:$w('padding-box-width padding-box-height margin-box-width margin-box-height border-box-width border-box-height'),COMPUTATIONS:{'height':function(element){if(!this._preComputing)this._begin();var bHeight=this.get('border-box-height');if(bHeight<=0){if(!this._preComputing)this._end();return 0;}
var bTop=this.get('border-top'),bBottom=this.get('border-bottom');var pTop=this.get('padding-top'),pBottom=this.get('padding-bottom');if(!this._preComputing)this._end();return bHeight-bTop-bBottom-pTop-pBottom;},'width':function(element){if(!this._preComputing)this._begin();var bWidth=this.get('border-box-width');if(bWidth<=0){if(!this._preComputing)this._end();return 0;}
var bLeft=this.get('border-left'),bRight=this.get('border-right');var pLeft=this.get('padding-left'),pRight=this.get('padding-right');if(!this._preComputing)this._end();return bWidth-bLeft-bRight-pLeft-pRight;},'padding-box-height':function(element){var height=this.get('height'),pTop=this.get('padding-top'),pBottom=this.get('padding-bottom');return height+pTop+pBottom;},'padding-box-width':function(element){var width=this.get('width'),pLeft=this.get('padding-left'),pRight=this.get('padding-right');return width+pLeft+pRight;},'border-box-height':function(element){if(!this._preComputing)this._begin();var height=element.offsetHeight;if(!this._preComputing)this._end();return height;},'border-box-width':function(element){if(!this._preComputing)this._begin();var width=element.offsetWidth;if(!this._preComputing)this._end();return width;},'margin-box-height':function(element){var bHeight=this.get('border-box-height'),mTop=this.get('margin-top'),mBottom=this.get('margin-bottom');if(bHeight<=0)return 0;return bHeight+mTop+mBottom;},'margin-box-width':function(element){var bWidth=this.get('border-box-width'),mLeft=this.get('margin-left'),mRight=this.get('margin-right');if(bWidth<=0)return 0;return bWidth+mLeft+mRight;},'top':function(element){var offset=element.positionedOffset();return offset.top;},'bottom':function(element){var offset=element.positionedOffset(),parent=element.getOffsetParent(),pHeight=parent.measure('height');var mHeight=this.get('border-box-height');return pHeight-mHeight-offset.top;},'left':function(element){var offset=element.positionedOffset();return offset.left;},'right':function(element){var offset=element.positionedOffset(),parent=element.getOffsetParent(),pWidth=parent.measure('width');var mWidth=this.get('border-box-width');return pWidth-mWidth-offset.left;},'padding-top':function(element){return getPixelValue(element,'paddingTop');},'padding-bottom':function(element){return getPixelValue(element,'paddingBottom');},'padding-left':function(element){return getPixelValue(element,'paddingLeft');},'padding-right':function(element){return getPixelValue(element,'paddingRight');},'border-top':function(element){return getPixelValue(element,'borderTopWidth');},'border-bottom':function(element){return getPixelValue(element,'borderBottomWidth');},'border-left':function(element){return getPixelValue(element,'borderLeftWidth');},'border-right':function(element){return getPixelValue(element,'borderRightWidth');},'margin-top':function(element){return getPixelValue(element,'marginTop');},'margin-bottom':function(element){return getPixelValue(element,'marginBottom');},'margin-left':function(element){return getPixelValue(element,'marginLeft');},'margin-right':function(element){return getPixelValue(element,'marginRight');}}});if('getBoundingClientRect'in document.documentElement){Object.extend(Element.Layout.COMPUTATIONS,{'right':function(element){var parent=hasLayout(element.getOffsetParent());var rect=element.getBoundingClientRect(),pRect=parent.getBoundingClientRect();return(pRect.right-rect.right).round();},'bottom':function(element){var parent=hasLayout(element.getOffsetParent());var rect=element.getBoundingClientRect(),pRect=parent.getBoundingClientRect();return(pRect.bottom-rect.bottom).round();}});}
Element.Offset=Class.create({initialize:function(left,top){this.left=left.round();this.top=top.round();this[0]=this.left;this[1]=this.top;},relativeTo:function(offset){return new Element.Offset(this.left-offset.left,this.top-offset.top);},inspect:function(){return"#<Element.Offset left: #{left} top: #{top}>".interpolate(this);},toString:function(){return"[#{left}, #{top}]".interpolate(this);},toArray:function(){return[this.left,this.top];}});function getLayout(element,preCompute){return new Element.Layout(element,preCompute);}
function measure(element,property){return $(element).getLayout().get(property);}
function getDimensions(element){element=$(element);var display=Element.getStyle(element,'display');if(display&&display!=='none'){return{width:element.offsetWidth,height:element.offsetHeight};}
var style=element.style;var originalStyles={visibility:style.visibility,position:style.position,display:style.display};var newStyles={visibility:'hidden',display:'block'};if(originalStyles.position!=='fixed')
newStyles.position='absolute';Element.setStyle(element,newStyles);var dimensions={width:element.offsetWidth,height:element.offsetHeight};Element.setStyle(element,originalStyles);return dimensions;}
function getOffsetParent(element){element=$(element);if(isDocument(element)||isDetached(element)||isBody(element)||isHtml(element))
return $(document.body);var isInline=(Element.getStyle(element,'display')==='inline');if(!isInline&&element.offsetParent)return $(element.offsetParent);while((element=element.parentNode)&&element!==document.body){if(Element.getStyle(element,'position')!=='static'){return isHtml(element)?$(document.body):$(element);}}
return $(document.body);}
function cumulativeOffset(element){element=$(element);var valueT=0,valueL=0;if(element.parentNode){do{valueT+=element.offsetTop||0;valueL+=element.offsetLeft||0;element=element.offsetParent;}while(element);}
return new Element.Offset(valueL,valueT);}
function positionedOffset(element){element=$(element);var layout=element.getLayout();var valueT=0,valueL=0;do{valueT+=element.offsetTop||0;valueL+=element.offsetLeft||0;element=element.offsetParent;if(element){if(isBody(element))break;var p=Element.getStyle(element,'position');if(p!=='static')break;}}while(element);valueL-=layout.get('margin-top');valueT-=layout.get('margin-left');return new Element.Offset(valueL,valueT);}
function cumulativeScrollOffset(element){var valueT=0,valueL=0;do{valueT+=element.scrollTop||0;valueL+=element.scrollLeft||0;element=element.parentNode;}while(element);return new Element.Offset(valueL,valueT);}
function viewportOffset(forElement){element=$(element);var valueT=0,valueL=0,docBody=document.body;var element=forElement;do{valueT+=element.offsetTop||0;valueL+=element.offsetLeft||0;if(element.offsetParent==docBody&&Element.getStyle(element,'position')=='absolute')break;}while(element=element.offsetParent);element=forElement;do{if(element!=docBody){valueT-=element.scrollTop||0;valueL-=element.scrollLeft||0;}}while(element=element.parentNode);return new Element.Offset(valueL,valueT);}
function absolutize(element){element=$(element);if(Element.getStyle(element,'position')==='absolute'){return element;}
var offsetParent=getOffsetParent(element);var eOffset=element.viewportOffset(),pOffset=offsetParent.viewportOffset();var offset=eOffset.relativeTo(pOffset);var layout=element.getLayout();element.store('prototype_absolutize_original_styles',{left:element.getStyle('left'),top:element.getStyle('top'),width:element.getStyle('width'),height:element.getStyle('height')});element.setStyle({position:'absolute',top:offset.top+'px',left:offset.left+'px',width:layout.get('width')+'px',height:layout.get('height')+'px'});return element;}
function relativize(element){element=$(element);if(Element.getStyle(element,'position')==='relative'){return element;}
var originalStyles=element.retrieve('prototype_absolutize_original_styles');if(originalStyles)element.setStyle(originalStyles);return element;}
if(Prototype.Browser.IE){getOffsetParent=getOffsetParent.wrap(function(proceed,element){element=$(element);if(isDocument(element)||isDetached(element)||isBody(element)||isHtml(element))
return $(document.body);var position=element.getStyle('position');if(position!=='static')return proceed(element);element.setStyle({position:'relative'});var value=proceed(element);element.setStyle({position:position});return value;});positionedOffset=positionedOffset.wrap(function(proceed,element){element=$(element);if(!element.parentNode)return new Element.Offset(0,0);var position=element.getStyle('position');if(position!=='static')return proceed(element);var offsetParent=element.getOffsetParent();if(offsetParent&&offsetParent.getStyle('position')==='fixed')
hasLayout(offsetParent);element.setStyle({position:'relative'});var value=proceed(element);element.setStyle({position:position});return value;});}else if(Prototype.Browser.Webkit){cumulativeOffset=function(element){element=$(element);var valueT=0,valueL=0;do{valueT+=element.offsetTop||0;valueL+=element.offsetLeft||0;if(element.offsetParent==document.body)
if(Element.getStyle(element,'position')=='absolute')break;element=element.offsetParent;}while(element);return new Element.Offset(valueL,valueT);};}
Element.addMethods({getLayout:getLayout,measure:measure,getDimensions:getDimensions,getOffsetParent:getOffsetParent,cumulativeOffset:cumulativeOffset,positionedOffset:positionedOffset,cumulativeScrollOffset:cumulativeScrollOffset,viewportOffset:viewportOffset,absolutize:absolutize,relativize:relativize});function isBody(element){return element.nodeName.toUpperCase()==='BODY';}
function isHtml(element){return element.nodeName.toUpperCase()==='HTML';}
function isDocument(element){return element.nodeType===Node.DOCUMENT_NODE;}
function isDetached(element){return element!==document.body&&!Element.descendantOf(element,document.body);}
if('getBoundingClientRect'in document.documentElement){Element.addMethods({viewportOffset:function(element){element=$(element);if(isDetached(element))return new Element.Offset(0,0);var rect=element.getBoundingClientRect(),docEl=document.documentElement;return new Element.Offset(rect.left-docEl.clientLeft,rect.top-docEl.clientTop);}});}})();window.$$=function(){var expression=$A(arguments).join(', ');return Prototype.Selector.select(expression,document);};Prototype.Selector=(function(){function select(){throw new Error('Method "Prototype.Selector.select" must be defined.');}
function match(){throw new Error('Method "Prototype.Selector.match" must be defined.');}
function find(elements,expression,index){index=index||0;var match=Prototype.Selector.match,length=elements.length,matchIndex=0,i;for(i=0;i<length;i++){if(match(elements[i],expression)&&index==matchIndex++){return Element.extend(elements[i]);}}}
function extendElements(elements){for(var i=0,length=elements.length;i<length;i++){Element.extend(elements[i]);}
return elements;}
var K=Prototype.K;return{select:select,match:match,find:find,extendElements:(Element.extend===K)?K:extendElements,extendElement:Element.extend};})();Prototype._original_property=window.Sizzle;(function(){var chunker=/((?:\((?:\([^()]+\)|[^()]+)+\)|\[(?:[[^[]]*\]|['"][^'"]*['"]|[^[\]'"]+)+\]|\\.|[^ >+~,(\[\\]+)+|[>+~])(\s*,\s*)?((?:.|\r|\n)*)/g,done=0,toString=Object.prototype.toString,hasDuplicate=false,baseHasDuplicate=true;[0,0].sort(function(){baseHasDuplicate=false;return 0;});var Sizzle=function(selector,context,results,seed){results=results||[];var origContext=context=context||document;if(context.nodeType!==1&&context.nodeType!==9){return[];}
if(!selector||typeof selector!=="string"){return results;}
var parts=[],m,set,checkSet,check,mode,extra,prune=true,contextXML=isXML(context),soFar=selector;while((chunker.exec(""),m=chunker.exec(soFar))!==null){soFar=m[3];parts.push(m[1]);if(m[2]){extra=m[3];break;}}
if(parts.length>1&&origPOS.exec(selector)){if(parts.length===2&&Expr.relative[parts[0]]){set=posProcess(parts[0]+parts[1],context);}else{set=Expr.relative[parts[0]]?[context]:Sizzle(parts.shift(),context);while(parts.length){selector=parts.shift();if(Expr.relative[selector])
selector+=parts.shift();set=posProcess(selector,set);}}}else{if(!seed&&parts.length>1&&context.nodeType===9&&!contextXML&&Expr.match.ID.test(parts[0])&&!Expr.match.ID.test(parts[parts.length-1])){var ret=Sizzle.find(parts.shift(),context,contextXML);context=ret.expr?Sizzle.filter(ret.expr,ret.set)[0]:ret.set[0];}
if(context){var ret=seed?{expr:parts.pop(),set:makeArray(seed)}:Sizzle.find(parts.pop(),parts.length===1&&(parts[0]==="~"||parts[0]==="+")&&context.parentNode?context.parentNode:context,contextXML);set=ret.expr?Sizzle.filter(ret.expr,ret.set):ret.set;if(parts.length>0){checkSet=makeArray(set);}else{prune=false;}
while(parts.length){var cur=parts.pop(),pop=cur;if(!Expr.relative[cur]){cur="";}else{pop=parts.pop();}
if(pop==null){pop=context;}
Expr.relative[cur](checkSet,pop,contextXML);}}else{checkSet=parts=[];}}
if(!checkSet){checkSet=set;}
if(!checkSet){throw"Syntax error, unrecognized expression: "+(cur||selector);}
if(toString.call(checkSet)==="[object Array]"){if(!prune){results.push.apply(results,checkSet);}else if(context&&context.nodeType===1){for(var i=0;checkSet[i]!=null;i++){if(checkSet[i]&&(checkSet[i]===true||checkSet[i].nodeType===1&&contains(context,checkSet[i]))){results.push(set[i]);}}}else{for(var i=0;checkSet[i]!=null;i++){if(checkSet[i]&&checkSet[i].nodeType===1){results.push(set[i]);}}}}else{makeArray(checkSet,results);}
if(extra){Sizzle(extra,origContext,results,seed);Sizzle.uniqueSort(results);}
return results;};Sizzle.uniqueSort=function(results){if(sortOrder){hasDuplicate=baseHasDuplicate;results.sort(sortOrder);if(hasDuplicate){for(var i=1;i<results.length;i++){if(results[i]===results[i-1]){results.splice(i--,1);}}}}
return results;};Sizzle.matches=function(expr,set){return Sizzle(expr,null,null,set);};Sizzle.find=function(expr,context,isXML){var set,match;if(!expr){return[];}
for(var i=0,l=Expr.order.length;i<l;i++){var type=Expr.order[i],match;if((match=Expr.leftMatch[type].exec(expr))){var left=match[1];match.splice(1,1);if(left.substr(left.length-1)!=="\\"){match[1]=(match[1]||"").replace(/\\/g,"");set=Expr.find[type](match,context,isXML);if(set!=null){expr=expr.replace(Expr.match[type],"");break;}}}}
if(!set){set=context.getElementsByTagName("*");}
return{set:set,expr:expr};};Sizzle.filter=function(expr,set,inplace,not){var old=expr,result=[],curLoop=set,match,anyFound,isXMLFilter=set&&set[0]&&isXML(set[0]);while(expr&&set.length){for(var type in Expr.filter){if((match=Expr.match[type].exec(expr))!=null){var filter=Expr.filter[type],found,item;anyFound=false;if(curLoop==result){result=[];}
if(Expr.preFilter[type]){match=Expr.preFilter[type](match,curLoop,inplace,result,not,isXMLFilter);if(!match){anyFound=found=true;}else if(match===true){continue;}}
if(match){for(var i=0;(item=curLoop[i])!=null;i++){if(item){found=filter(item,match,i,curLoop);var pass=not^!!found;if(inplace&&found!=null){if(pass){anyFound=true;}else{curLoop[i]=false;}}else if(pass){result.push(item);anyFound=true;}}}}
if(found!==undefined){if(!inplace){curLoop=result;}
expr=expr.replace(Expr.match[type],"");if(!anyFound){return[];}
break;}}}
if(expr==old){if(anyFound==null){throw"Syntax error, unrecognized expression: "+expr;}else{break;}}
old=expr;}
return curLoop;};var Expr=Sizzle.selectors={order:["ID","NAME","TAG"],match:{ID:/#((?:[\w\u00c0-\uFFFF-]|\\.)+)/,CLASS:/\.((?:[\w\u00c0-\uFFFF-]|\\.)+)/,NAME:/\[name=['"]*((?:[\w\u00c0-\uFFFF-]|\\.)+)['"]*\]/,ATTR:/\[\s*((?:[\w\u00c0-\uFFFF-]|\\.)+)\s*(?:(\S?=)\s*(['"]*)(.*?)\3|)\s*\]/,TAG:/^((?:[\w\u00c0-\uFFFF\*-]|\\.)+)/,CHILD:/:(only|nth|last|first)-child(?:\((even|odd|[\dn+-]*)\))?/,POS:/:(nth|eq|gt|lt|first|last|even|odd)(?:\((\d*)\))?(?=[^-]|$)/,PSEUDO:/:((?:[\w\u00c0-\uFFFF-]|\\.)+)(?:\((['"]*)((?:\([^\)]+\)|[^\2\(\)]*)+)\2\))?/},leftMatch:{},attrMap:{"class":"className","for":"htmlFor"},attrHandle:{href:function(elem){return elem.getAttribute("href");}},relative:{"+":function(checkSet,part,isXML){var isPartStr=typeof part==="string",isTag=isPartStr&&!/\W/.test(part),isPartStrNotTag=isPartStr&&!isTag;if(isTag&&!isXML){part=part.toUpperCase();}
for(var i=0,l=checkSet.length,elem;i<l;i++){if((elem=checkSet[i])){while((elem=elem.previousSibling)&&elem.nodeType!==1){}
checkSet[i]=isPartStrNotTag||elem&&elem.nodeName===part?elem||false:elem===part;}}
if(isPartStrNotTag){Sizzle.filter(part,checkSet,true);}},">":function(checkSet,part,isXML){var isPartStr=typeof part==="string";if(isPartStr&&!/\W/.test(part)){part=isXML?part:part.toUpperCase();for(var i=0,l=checkSet.length;i<l;i++){var elem=checkSet[i];if(elem){var parent=elem.parentNode;checkSet[i]=parent.nodeName===part?parent:false;}}}else{for(var i=0,l=checkSet.length;i<l;i++){var elem=checkSet[i];if(elem){checkSet[i]=isPartStr?elem.parentNode:elem.parentNode===part;}}
if(isPartStr){Sizzle.filter(part,checkSet,true);}}},"":function(checkSet,part,isXML){var doneName=done++,checkFn=dirCheck;if(!/\W/.test(part)){var nodeCheck=part=isXML?part:part.toUpperCase();checkFn=dirNodeCheck;}
checkFn("parentNode",part,doneName,checkSet,nodeCheck,isXML);},"~":function(checkSet,part,isXML){var doneName=done++,checkFn=dirCheck;if(typeof part==="string"&&!/\W/.test(part)){var nodeCheck=part=isXML?part:part.toUpperCase();checkFn=dirNodeCheck;}
checkFn("previousSibling",part,doneName,checkSet,nodeCheck,isXML);}},find:{ID:function(match,context,isXML){if(typeof context.getElementById!=="undefined"&&!isXML){var m=context.getElementById(match[1]);return m?[m]:[];}},NAME:function(match,context,isXML){if(typeof context.getElementsByName!=="undefined"){var ret=[],results=context.getElementsByName(match[1]);for(var i=0,l=results.length;i<l;i++){if(results[i].getAttribute("name")===match[1]){ret.push(results[i]);}}
return ret.length===0?null:ret;}},TAG:function(match,context){return context.getElementsByTagName(match[1]);}},preFilter:{CLASS:function(match,curLoop,inplace,result,not,isXML){match=" "+match[1].replace(/\\/g,"")+" ";if(isXML){return match;}
for(var i=0,elem;(elem=curLoop[i])!=null;i++){if(elem){if(not^(elem.className&&(" "+elem.className+" ").indexOf(match)>=0)){if(!inplace)
result.push(elem);}else if(inplace){curLoop[i]=false;}}}
return false;},ID:function(match){return match[1].replace(/\\/g,"");},TAG:function(match,curLoop){for(var i=0;curLoop[i]===false;i++){}
return curLoop[i]&&isXML(curLoop[i])?match[1]:match[1].toUpperCase();},CHILD:function(match){if(match[1]=="nth"){var test=/(-?)(\d*)n((?:\+|-)?\d*)/.exec(match[2]=="even"&&"2n"||match[2]=="odd"&&"2n+1"||!/\D/.test(match[2])&&"0n+"+match[2]||match[2]);match[2]=(test[1]+(test[2]||1))-0;match[3]=test[3]-0;}
match[0]=done++;return match;},ATTR:function(match,curLoop,inplace,result,not,isXML){var name=match[1].replace(/\\/g,"");if(!isXML&&Expr.attrMap[name]){match[1]=Expr.attrMap[name];}
if(match[2]==="~="){match[4]=" "+match[4]+" ";}
return match;},PSEUDO:function(match,curLoop,inplace,result,not){if(match[1]==="not"){if((chunker.exec(match[3])||"").length>1||/^\w/.test(match[3])){match[3]=Sizzle(match[3],null,null,curLoop);}else{var ret=Sizzle.filter(match[3],curLoop,inplace,true^not);if(!inplace){result.push.apply(result,ret);}
return false;}}else if(Expr.match.POS.test(match[0])||Expr.match.CHILD.test(match[0])){return true;}
return match;},POS:function(match){match.unshift(true);return match;}},filters:{enabled:function(elem){return elem.disabled===false&&elem.type!=="hidden";},disabled:function(elem){return elem.disabled===true;},checked:function(elem){return elem.checked===true;},selected:function(elem){elem.parentNode.selectedIndex;return elem.selected===true;},parent:function(elem){return!!elem.firstChild;},empty:function(elem){return!elem.firstChild;},has:function(elem,i,match){return!!Sizzle(match[3],elem).length;},header:function(elem){return/h\d/i.test(elem.nodeName);},text:function(elem){return"text"===elem.type;},radio:function(elem){return"radio"===elem.type;},checkbox:function(elem){return"checkbox"===elem.type;},file:function(elem){return"file"===elem.type;},password:function(elem){return"password"===elem.type;},submit:function(elem){return"submit"===elem.type;},image:function(elem){return"image"===elem.type;},reset:function(elem){return"reset"===elem.type;},button:function(elem){return"button"===elem.type||elem.nodeName.toUpperCase()==="BUTTON";},input:function(elem){return/input|select|textarea|button/i.test(elem.nodeName);}},setFilters:{first:function(elem,i){return i===0;},last:function(elem,i,match,array){return i===array.length-1;},even:function(elem,i){return i%2===0;},odd:function(elem,i){return i%2===1;},lt:function(elem,i,match){return i<match[3]-0;},gt:function(elem,i,match){return i>match[3]-0;},nth:function(elem,i,match){return match[3]-0==i;},eq:function(elem,i,match){return match[3]-0==i;}},filter:{PSEUDO:function(elem,match,i,array){var name=match[1],filter=Expr.filters[name];if(filter){return filter(elem,i,match,array);}else if(name==="contains"){return(elem.textContent||elem.innerText||"").indexOf(match[3])>=0;}else if(name==="not"){var not=match[3];for(var i=0,l=not.length;i<l;i++){if(not[i]===elem){return false;}}
return true;}},CHILD:function(elem,match){var type=match[1],node=elem;switch(type){case'only':case'first':while((node=node.previousSibling)){if(node.nodeType===1)return false;}
if(type=='first')return true;node=elem;case'last':while((node=node.nextSibling)){if(node.nodeType===1)return false;}
return true;case'nth':var first=match[2],last=match[3];if(first==1&&last==0){return true;}
var doneName=match[0],parent=elem.parentNode;if(parent&&(parent.sizcache!==doneName||!elem.nodeIndex)){var count=0;for(node=parent.firstChild;node;node=node.nextSibling){if(node.nodeType===1){node.nodeIndex=++count;}}
parent.sizcache=doneName;}
var diff=elem.nodeIndex-last;if(first==0){return diff==0;}else{return(diff%first==0&&diff/first>=0);}}},ID:function(elem,match){return elem.nodeType===1&&elem.getAttribute("id")===match;},TAG:function(elem,match){return(match==="*"&&elem.nodeType===1)||elem.nodeName===match;},CLASS:function(elem,match){return(" "+(elem.className||elem.getAttribute("class"))+" ").indexOf(match)>-1;},ATTR:function(elem,match){var name=match[1],result=Expr.attrHandle[name]?Expr.attrHandle[name](elem):elem[name]!=null?elem[name]:elem.getAttribute(name),value=result+"",type=match[2],check=match[4];return result==null?type==="!=":type==="="?value===check:type==="*="?value.indexOf(check)>=0:type==="~="?(" "+value+" ").indexOf(check)>=0:!check?value&&result!==false:type==="!="?value!=check:type==="^="?value.indexOf(check)===0:type==="$="?value.substr(value.length-check.length)===check:type==="|="?value===check||value.substr(0,check.length+1)===check+"-":false;},POS:function(elem,match,i,array){var name=match[2],filter=Expr.setFilters[name];if(filter){return filter(elem,i,match,array);}}}};var origPOS=Expr.match.POS;for(var type in Expr.match){Expr.match[type]=new RegExp(Expr.match[type].source+/(?![^\[]*\])(?![^\(]*\))/.source);Expr.leftMatch[type]=new RegExp(/(^(?:.|\r|\n)*?)/.source+Expr.match[type].source);}
var makeArray=function(array,results){array=Array.prototype.slice.call(array,0);if(results){results.push.apply(results,array);return results;}
return array;};try{Array.prototype.slice.call(document.documentElement.childNodes,0);}catch(e){makeArray=function(array,results){var ret=results||[];if(toString.call(array)==="[object Array]"){Array.prototype.push.apply(ret,array);}else{if(typeof array.length==="number"){for(var i=0,l=array.length;i<l;i++){ret.push(array[i]);}}else{for(var i=0;array[i];i++){ret.push(array[i]);}}}
return ret;};}
var sortOrder;if(document.documentElement.compareDocumentPosition){sortOrder=function(a,b){if(!a.compareDocumentPosition||!b.compareDocumentPosition){if(a==b){hasDuplicate=true;}
return 0;}
var ret=a.compareDocumentPosition(b)&4?-1:a===b?0:1;if(ret===0){hasDuplicate=true;}
return ret;};}else if("sourceIndex"in document.documentElement){sortOrder=function(a,b){if(!a.sourceIndex||!b.sourceIndex){if(a==b){hasDuplicate=true;}
return 0;}
var ret=a.sourceIndex-b.sourceIndex;if(ret===0){hasDuplicate=true;}
return ret;};}else if(document.createRange){sortOrder=function(a,b){if(!a.ownerDocument||!b.ownerDocument){if(a==b){hasDuplicate=true;}
return 0;}
var aRange=a.ownerDocument.createRange(),bRange=b.ownerDocument.createRange();aRange.setStart(a,0);aRange.setEnd(a,0);bRange.setStart(b,0);bRange.setEnd(b,0);var ret=aRange.compareBoundaryPoints(Range.START_TO_END,bRange);if(ret===0){hasDuplicate=true;}
return ret;};}
(function(){var form=document.createElement("div"),id="script"+(new Date).getTime();form.innerHTML="<a name='"+id+"'/>";var root=document.documentElement;root.insertBefore(form,root.firstChild);if(!!document.getElementById(id)){Expr.find.ID=function(match,context,isXML){if(typeof context.getElementById!=="undefined"&&!isXML){var m=context.getElementById(match[1]);return m?m.id===match[1]||typeof m.getAttributeNode!=="undefined"&&m.getAttributeNode("id").nodeValue===match[1]?[m]:undefined:[];}};Expr.filter.ID=function(elem,match){var node=typeof elem.getAttributeNode!=="undefined"&&elem.getAttributeNode("id");return elem.nodeType===1&&node&&node.nodeValue===match;};}
root.removeChild(form);root=form=null;})();(function(){var div=document.createElement("div");div.appendChild(document.createComment(""));if(div.getElementsByTagName("*").length>0){Expr.find.TAG=function(match,context){var results=context.getElementsByTagName(match[1]);if(match[1]==="*"){var tmp=[];for(var i=0;results[i];i++){if(results[i].nodeType===1){tmp.push(results[i]);}}
results=tmp;}
return results;};}
div.innerHTML="<a href='#'></a>";if(div.firstChild&&typeof div.firstChild.getAttribute!=="undefined"&&div.firstChild.getAttribute("href")!=="#"){Expr.attrHandle.href=function(elem){return elem.getAttribute("href",2);};}
div=null;})();if(document.querySelectorAll)(function(){var oldSizzle=Sizzle,div=document.createElement("div");div.innerHTML="<p class='TEST'></p>";if(div.querySelectorAll&&div.querySelectorAll(".TEST").length===0){return;}
Sizzle=function(query,context,extra,seed){context=context||document;if(!seed&&context.nodeType===9&&!isXML(context)){try{return makeArray(context.querySelectorAll(query),extra);}catch(e){}}
return oldSizzle(query,context,extra,seed);};for(var prop in oldSizzle){Sizzle[prop]=oldSizzle[prop];}
div=null;})();if(document.getElementsByClassName&&document.documentElement.getElementsByClassName)(function(){var div=document.createElement("div");div.innerHTML="<div class='test e'></div><div class='test'></div>";if(div.getElementsByClassName("e").length===0)
return;div.lastChild.className="e";if(div.getElementsByClassName("e").length===1)
return;Expr.order.splice(1,0,"CLASS");Expr.find.CLASS=function(match,context,isXML){if(typeof context.getElementsByClassName!=="undefined"&&!isXML){return context.getElementsByClassName(match[1]);}};div=null;})();function dirNodeCheck(dir,cur,doneName,checkSet,nodeCheck,isXML){var sibDir=dir=="previousSibling"&&!isXML;for(var i=0,l=checkSet.length;i<l;i++){var elem=checkSet[i];if(elem){if(sibDir&&elem.nodeType===1){elem.sizcache=doneName;elem.sizset=i;}
elem=elem[dir];var match=false;while(elem){if(elem.sizcache===doneName){match=checkSet[elem.sizset];break;}
if(elem.nodeType===1&&!isXML){elem.sizcache=doneName;elem.sizset=i;}
if(elem.nodeName===cur){match=elem;break;}
elem=elem[dir];}
checkSet[i]=match;}}}
function dirCheck(dir,cur,doneName,checkSet,nodeCheck,isXML){var sibDir=dir=="previousSibling"&&!isXML;for(var i=0,l=checkSet.length;i<l;i++){var elem=checkSet[i];if(elem){if(sibDir&&elem.nodeType===1){elem.sizcache=doneName;elem.sizset=i;}
elem=elem[dir];var match=false;while(elem){if(elem.sizcache===doneName){match=checkSet[elem.sizset];break;}
if(elem.nodeType===1){if(!isXML){elem.sizcache=doneName;elem.sizset=i;}
if(typeof cur!=="string"){if(elem===cur){match=true;break;}}else if(Sizzle.filter(cur,[elem]).length>0){match=elem;break;}}
elem=elem[dir];}
checkSet[i]=match;}}}
var contains=document.compareDocumentPosition?function(a,b){return a.compareDocumentPosition(b)&16;}:function(a,b){return a!==b&&(a.contains?a.contains(b):true);};var isXML=function(elem){return elem.nodeType===9&&elem.documentElement.nodeName!=="HTML"||!!elem.ownerDocument&&elem.ownerDocument.documentElement.nodeName!=="HTML";};var posProcess=function(selector,context){var tmpSet=[],later="",match,root=context.nodeType?[context]:context;while((match=Expr.match.PSEUDO.exec(selector))){later+=match[0];selector=selector.replace(Expr.match.PSEUDO,"");}
selector=Expr.relative[selector]?selector+"*":selector;for(var i=0,l=root.length;i<l;i++){Sizzle(selector,root[i],tmpSet);}
return Sizzle.filter(later,tmpSet);};window.Sizzle=Sizzle;})();;(function(engine){var extendElements=Prototype.Selector.extendElements;function select(selector,scope){return extendElements(engine(selector,scope||document));}
function match(element,selector){return engine.matches(selector,[element]).length==1;}
Prototype.Selector.engine=engine;Prototype.Selector.select=select;Prototype.Selector.match=match;})(Sizzle);window.Sizzle=Prototype._original_property;delete Prototype._original_property;var Form={reset:function(form){form=$(form);form.reset();return form;},serializeElements:function(elements,options){if(typeof options!='object')options={hash:!!options};else if(Object.isUndefined(options.hash))options.hash=true;var key,value,submitted=false,submit=options.submit,accumulator,initial;if(options.hash){initial={};accumulator=function(result,key,value){if(key in result){if(!Object.isArray(result[key]))result[key]=[result[key]];result[key].push(value);}else result[key]=value;return result;};}else{initial='';accumulator=function(result,key,value){return result+(result?'&':'')+encodeURIComponent(key)+'='+encodeURIComponent(value);}}
return elements.inject(initial,function(result,element){if(!element.disabled&&element.name){key=element.name;value=$(element).getValue();if(value!=null&&element.type!='file'&&(element.type!='submit'||(!submitted&&submit!==false&&(!submit||key==submit)&&(submitted=true)))){result=accumulator(result,key,value);}}
return result;});}};Form.Methods={serialize:function(form,options){return Form.serializeElements(Form.getElements(form),options);},getElements:function(form){var elements=$(form).getElementsByTagName('*'),element,arr=[],serializers=Form.Element.Serializers;for(var i=0;element=elements[i];i++){arr.push(element);}
return arr.inject([],function(elements,child){if(serializers[child.tagName.toLowerCase()])
elements.push(Element.extend(child));return elements;})},getInputs:function(form,typeName,name){form=$(form);var inputs=form.getElementsByTagName('input');if(!typeName&&!name)return $A(inputs).map(Element.extend);for(var i=0,matchingInputs=[],length=inputs.length;i<length;i++){var input=inputs[i];if((typeName&&input.type!=typeName)||(name&&input.name!=name))
continue;matchingInputs.push(Element.extend(input));}
return matchingInputs;},disable:function(form){form=$(form);Form.getElements(form).invoke('disable');return form;},enable:function(form){form=$(form);Form.getElements(form).invoke('enable');return form;},findFirstElement:function(form){var elements=$(form).getElements().findAll(function(element){return'hidden'!=element.type&&!element.disabled;});var firstByIndex=elements.findAll(function(element){return element.hasAttribute('tabIndex')&&element.tabIndex>=0;}).sortBy(function(element){return element.tabIndex}).first();return firstByIndex?firstByIndex:elements.find(function(element){return/^(?:input|select|textarea)$/i.test(element.tagName);});},focusFirstElement:function(form){form=$(form);var element=form.findFirstElement();if(element)element.activate();return form;},request:function(form,options){form=$(form),options=Object.clone(options||{});var params=options.parameters,action=form.readAttribute('action')||'';if(action.blank())action=window.location.href;options.parameters=form.serialize(true);if(params){if(Object.isString(params))params=params.toQueryParams();Object.extend(options.parameters,params);}
if(form.hasAttribute('method')&&!options.method)
options.method=form.method;return new Ajax.Request(action,options);}};Form.Element={focus:function(element){$(element).focus();return element;},select:function(element){$(element).select();return element;}};Form.Element.Methods={serialize:function(element){element=$(element);if(!element.disabled&&element.name){var value=element.getValue();if(value!=undefined){var pair={};pair[element.name]=value;return Object.toQueryString(pair);}}
return'';},getValue:function(element){element=$(element);var method=element.tagName.toLowerCase();return Form.Element.Serializers[method](element);},setValue:function(element,value){element=$(element);var method=element.tagName.toLowerCase();Form.Element.Serializers[method](element,value);return element;},clear:function(element){$(element).value='';return element;},present:function(element){return $(element).value!='';},activate:function(element){element=$(element);try{element.focus();if(element.select&&(element.tagName.toLowerCase()!='input'||!(/^(?:button|reset|submit)$/i.test(element.type))))
element.select();}catch(e){}
return element;},disable:function(element){element=$(element);element.disabled=true;return element;},enable:function(element){element=$(element);element.disabled=false;return element;}};var Field=Form.Element;var $F=Form.Element.Methods.getValue;Form.Element.Serializers=(function(){function input(element,value){switch(element.type.toLowerCase()){case'checkbox':case'radio':return inputSelector(element,value);default:return valueSelector(element,value);}}
function inputSelector(element,value){if(Object.isUndefined(value))
return element.checked?element.value:null;else element.checked=!!value;}
function valueSelector(element,value){if(Object.isUndefined(value))return element.value;else element.value=value;}
function select(element,value){if(Object.isUndefined(value))
return(element.type==='select-one'?selectOne:selectMany)(element);var opt,currentValue,single=!Object.isArray(value);for(var i=0,length=element.length;i<length;i++){opt=element.options[i];currentValue=this.optionValue(opt);if(single){if(currentValue==value){opt.selected=true;return;}}
else opt.selected=value.include(currentValue);}}
function selectOne(element){var index=element.selectedIndex;return index>=0?optionValue(element.options[index]):null;}
function selectMany(element){var values,length=element.length;if(!length)return null;for(var i=0,values=[];i<length;i++){var opt=element.options[i];if(opt.selected)values.push(optionValue(opt));}
return values;}
function optionValue(opt){return Element.hasAttribute(opt,'value')?opt.value:opt.text;}
return{input:input,inputSelector:inputSelector,textarea:valueSelector,select:select,selectOne:selectOne,selectMany:selectMany,optionValue:optionValue,button:valueSelector};})();Abstract.TimedObserver=Class.create(PeriodicalExecuter,{initialize:function($super,element,frequency,callback){$super(callback,frequency);this.element=$(element);this.lastValue=this.getValue();},execute:function(){var value=this.getValue();if(Object.isString(this.lastValue)&&Object.isString(value)?this.lastValue!=value:String(this.lastValue)!=String(value)){this.callback(this.element,value);this.lastValue=value;}}});Form.Element.Observer=Class.create(Abstract.TimedObserver,{getValue:function(){return Form.Element.getValue(this.element);}});Form.Observer=Class.create(Abstract.TimedObserver,{getValue:function(){return Form.serialize(this.element);}});Abstract.EventObserver=Class.create({initialize:function(element,callback){this.element=$(element);this.callback=callback;this.lastValue=this.getValue();if(this.element.tagName.toLowerCase()=='form')
this.registerFormCallbacks();else
this.registerCallback(this.element);},onElementEvent:function(){var value=this.getValue();if(this.lastValue!=value){this.callback(this.element,value);this.lastValue=value;}},registerFormCallbacks:function(){Form.getElements(this.element).each(this.registerCallback,this);},registerCallback:function(element){if(element.type){switch(element.type.toLowerCase()){case'checkbox':case'radio':Event.observe(element,'click',this.onElementEvent.bind(this));break;default:Event.observe(element,'change',this.onElementEvent.bind(this));break;}}}});Form.Element.EventObserver=Class.create(Abstract.EventObserver,{getValue:function(){return Form.Element.getValue(this.element);}});Form.EventObserver=Class.create(Abstract.EventObserver,{getValue:function(){return Form.serialize(this.element);}});(function(){var Event={KEY_BACKSPACE:8,KEY_TAB:9,KEY_RETURN:13,KEY_ESC:27,KEY_LEFT:37,KEY_UP:38,KEY_RIGHT:39,KEY_DOWN:40,KEY_DELETE:46,KEY_HOME:36,KEY_END:35,KEY_PAGEUP:33,KEY_PAGEDOWN:34,KEY_INSERT:45,cache:{}};var docEl=document.documentElement;var MOUSEENTER_MOUSELEAVE_EVENTS_SUPPORTED='onmouseenter'in docEl&&'onmouseleave'in docEl;var isIELegacyEvent=function(event){return false;};if(window.attachEvent){if(window.addEventListener){isIELegacyEvent=function(event){return!(event instanceof window.Event);};}else{isIELegacyEvent=function(event){return true;};}}
var _isButton;function _isButtonForDOMEvents(event,code){return event.which?(event.which===code+1):(event.button===code);}
var legacyButtonMap={0:1,1:4,2:2};function _isButtonForLegacyEvents(event,code){return event.button===legacyButtonMap[code];}
function _isButtonForWebKit(event,code){switch(code){case 0:return event.which==1&&!event.metaKey;case 1:return event.which==2||(event.which==1&&event.metaKey);case 2:return event.which==3;default:return false;}}
if(window.attachEvent){if(!window.addEventListener){_isButton=_isButtonForLegacyEvents;}else{_isButton=function(event,code){return isIELegacyEvent(event)?_isButtonForLegacyEvents(event,code):_isButtonForDOMEvents(event,code);}}}else if(Prototype.Browser.WebKit){_isButton=_isButtonForWebKit;}else{_isButton=_isButtonForDOMEvents;}
function isLeftClick(event){return _isButton(event,0)}
function isMiddleClick(event){return _isButton(event,1)}
function isRightClick(event){return _isButton(event,2)}
function element(event){event=Event.extend(event);var node=event.target,type=event.type,currentTarget=event.currentTarget;if(currentTarget&&currentTarget.tagName){if(type==='load'||type==='error'||(type==='click'&&currentTarget.tagName.toLowerCase()==='input'&&currentTarget.type==='radio'))
node=currentTarget;}
if(node.nodeType==Node.TEXT_NODE)
node=node.parentNode;return Element.extend(node);}
function findElement(event,expression){var element=Event.element(event);if(!expression)return element;while(element){if(Object.isElement(element)&&Prototype.Selector.match(element,expression)){return Element.extend(element);}
element=element.parentNode;}}
function pointer(event){return{x:pointerX(event),y:pointerY(event)};}
function pointerX(event){var docElement=document.documentElement,body=document.body||{scrollLeft:0};return event.pageX||(event.clientX+
(docElement.scrollLeft||body.scrollLeft)-
(docElement.clientLeft||0));}
function pointerY(event){var docElement=document.documentElement,body=document.body||{scrollTop:0};return event.pageY||(event.clientY+
(docElement.scrollTop||body.scrollTop)-
(docElement.clientTop||0));}
function stop(event){Event.extend(event);event.preventDefault();event.stopPropagation();event.stopped=true;}
Event.Methods={isLeftClick:isLeftClick,isMiddleClick:isMiddleClick,isRightClick:isRightClick,element:element,findElement:findElement,pointer:pointer,pointerX:pointerX,pointerY:pointerY,stop:stop};var methods=Object.keys(Event.Methods).inject({},function(m,name){m[name]=Event.Methods[name].methodize();return m;});if(window.attachEvent){function _relatedTarget(event){var element;switch(event.type){case'mouseover':case'mouseenter':element=event.fromElement;break;case'mouseout':case'mouseleave':element=event.toElement;break;default:return null;}
return Element.extend(element);}
var additionalMethods={stopPropagation:function(){this.cancelBubble=true},preventDefault:function(){this.returnValue=false},inspect:function(){return'[object Event]'}};Event.extend=function(event,element){if(!event)return false;if(!isIELegacyEvent(event))return event;if(event._extendedByPrototype)return event;event._extendedByPrototype=Prototype.emptyFunction;var pointer=Event.pointer(event);Object.extend(event,{target:event.srcElement||element,relatedTarget:_relatedTarget(event),pageX:pointer.x,pageY:pointer.y});Object.extend(event,methods);Object.extend(event,additionalMethods);return event;};}else{Event.extend=Prototype.K;}
if(window.addEventListener){Event.prototype=window.Event.prototype||document.createEvent('HTMLEvents').__proto__;Object.extend(Event.prototype,methods);}
function _createResponder(element,eventName,handler){var registry=Element.retrieve(element,'prototype_event_registry');if(Object.isUndefined(registry)){CACHE.push(element);registry=Element.retrieve(element,'prototype_event_registry',$H());}
var respondersForEvent=registry.get(eventName);if(Object.isUndefined(respondersForEvent)){respondersForEvent=[];registry.set(eventName,respondersForEvent);}
if(respondersForEvent.pluck('handler').include(handler))return false;var responder;if(eventName.include(":")){responder=function(event){if(Object.isUndefined(event.eventName))
return false;if(event.eventName!==eventName)
return false;Event.extend(event,element);handler.call(element,event);};}else{if(!MOUSEENTER_MOUSELEAVE_EVENTS_SUPPORTED&&(eventName==="mouseenter"||eventName==="mouseleave")){if(eventName==="mouseenter"||eventName==="mouseleave"){responder=function(event){Event.extend(event,element);var parent=event.relatedTarget;while(parent&&parent!==element){try{parent=parent.parentNode;}
catch(e){parent=element;}}
if(parent===element)return;handler.call(element,event);};}}else{responder=function(event){Event.extend(event,element);handler.call(element,event);};}}
responder.handler=handler;respondersForEvent.push(responder);return responder;}
function _destroyCache(){for(var i=0,length=CACHE.length;i<length;i++){Event.stopObserving(CACHE[i]);CACHE[i]=null;}}
var CACHE=[];if(Prototype.Browser.IE)
window.attachEvent('onunload',_destroyCache);if(Prototype.Browser.WebKit)
window.addEventListener('unload',Prototype.emptyFunction,false);var _getDOMEventName=Prototype.K,translations={mouseenter:"mouseover",mouseleave:"mouseout"};if(!MOUSEENTER_MOUSELEAVE_EVENTS_SUPPORTED){_getDOMEventName=function(eventName){return(translations[eventName]||eventName);};}
function observe(element,eventName,handler){element=$(element);var responder=_createResponder(element,eventName,handler);if(!responder)return element;if(eventName.include(':')){if(element.addEventListener)
element.addEventListener("dataavailable",responder,false);else{element.attachEvent("ondataavailable",responder);element.attachEvent("onlosecapture",responder);}}else{var actualEventName=_getDOMEventName(eventName);if(element.addEventListener)
element.addEventListener(actualEventName,responder,false);else
element.attachEvent("on"+actualEventName,responder);}
return element;}
function stopObserving(element,eventName,handler){element=$(element);var registry=Element.retrieve(element,'prototype_event_registry');if(!registry)return element;if(!eventName){registry.each(function(pair){var eventName=pair.key;stopObserving(element,eventName);});return element;}
var responders=registry.get(eventName);if(!responders)return element;if(!handler){responders.each(function(r){stopObserving(element,eventName,r.handler);});return element;}
var i=responders.length,responder;while(i--){if(responders[i].handler===handler){responder=responders[i];break;}}
if(!responder)return element;if(eventName.include(':')){if(element.removeEventListener)
element.removeEventListener("dataavailable",responder,false);else{element.detachEvent("ondataavailable",responder);element.detachEvent("onlosecapture",responder);}}else{var actualEventName=_getDOMEventName(eventName);if(element.removeEventListener)
element.removeEventListener(actualEventName,responder,false);else
element.detachEvent('on'+actualEventName,responder);}
registry.set(eventName,responders.without(responder));return element;}
function fire(element,eventName,memo,bubble){element=$(element);if(Object.isUndefined(bubble))
bubble=true;if(element==document&&document.createEvent&&!element.dispatchEvent)
element=document.documentElement;var event;if(document.createEvent){event=document.createEvent('HTMLEvents');event.initEvent('dataavailable',bubble,true);}else{event=document.createEventObject();event.eventType=bubble?'ondataavailable':'onlosecapture';}
event.eventName=eventName;event.memo=memo||{};if(document.createEvent)
element.dispatchEvent(event);else
element.fireEvent(event.eventType,event);return Event.extend(event);}
Event.Handler=Class.create({initialize:function(element,eventName,selector,callback){this.element=$(element);this.eventName=eventName;this.selector=selector;this.callback=callback;this.handler=this.handleEvent.bind(this);},start:function(){Event.observe(this.element,this.eventName,this.handler);return this;},stop:function(){Event.stopObserving(this.element,this.eventName,this.handler);return this;},handleEvent:function(event){var element=Event.findElement(event,this.selector);if(element)this.callback.call(this.element,event,element);}});function on(element,eventName,selector,callback){element=$(element);if(Object.isFunction(selector)&&Object.isUndefined(callback)){callback=selector,selector=null;}
return new Event.Handler(element,eventName,selector,callback).start();}
Object.extend(Event,Event.Methods);Object.extend(Event,{fire:fire,observe:observe,stopObserving:stopObserving,on:on});Element.addMethods({fire:fire,observe:observe,stopObserving:stopObserving,on:on});Object.extend(document,{fire:fire.methodize(),observe:observe.methodize(),stopObserving:stopObserving.methodize(),on:on.methodize(),loaded:false});if(window.Event)Object.extend(window.Event,Event);else window.Event=Event;})();(function(){var timer;function fireContentLoadedEvent(){if(document.loaded)return;if(timer)window.clearTimeout(timer);document.loaded=true;document.fire('dom:loaded');}
function checkReadyState(){if(document.readyState==='complete'){document.stopObserving('readystatechange',checkReadyState);fireContentLoadedEvent();}}
function pollDoScroll(){try{document.documentElement.doScroll('left');}
catch(e){timer=pollDoScroll.defer();return;}
fireContentLoadedEvent();}
if(document.addEventListener){document.addEventListener('DOMContentLoaded',fireContentLoadedEvent,false);}else{document.observe('readystatechange',checkReadyState);if(window==top)
timer=pollDoScroll.defer();}
Event.observe(window,'load',fireContentLoadedEvent);})();Element.addMethods();Hash.toQueryString=Object.toQueryString;var Toggle={display:Element.toggle};Element.Methods.childOf=Element.Methods.descendantOf;var Insertion={Before:function(element,content){return Element.insert(element,{before:content});},Top:function(element,content){return Element.insert(element,{top:content});},Bottom:function(element,content){return Element.insert(element,{bottom:content});},After:function(element,content){return Element.insert(element,{after:content});}};var $continue=new Error('"throw $continue" is deprecated, use "return" instead');var Position={includeScrollOffsets:false,prepare:function(){this.deltaX=window.pageXOffset||document.documentElement.scrollLeft||document.body.scrollLeft||0;this.deltaY=window.pageYOffset||document.documentElement.scrollTop||document.body.scrollTop||0;},within:function(element,x,y){if(this.includeScrollOffsets)
return this.withinIncludingScrolloffsets(element,x,y);this.xcomp=x;this.ycomp=y;this.offset=Element.cumulativeOffset(element);return(y>=this.offset[1]&&y<this.offset[1]+element.offsetHeight&&x>=this.offset[0]&&x<this.offset[0]+element.offsetWidth);},withinIncludingScrolloffsets:function(element,x,y){var offsetcache=Element.cumulativeScrollOffset(element);this.xcomp=x+offsetcache[0]-this.deltaX;this.ycomp=y+offsetcache[1]-this.deltaY;this.offset=Element.cumulativeOffset(element);return(this.ycomp>=this.offset[1]&&this.ycomp<this.offset[1]+element.offsetHeight&&this.xcomp>=this.offset[0]&&this.xcomp<this.offset[0]+element.offsetWidth);},overlap:function(mode,element){if(!mode)return 0;if(mode=='vertical')
return((this.offset[1]+element.offsetHeight)-this.ycomp)/element.offsetHeight;if(mode=='horizontal')
return((this.offset[0]+element.offsetWidth)-this.xcomp)/element.offsetWidth;},cumulativeOffset:Element.Methods.cumulativeOffset,positionedOffset:Element.Methods.positionedOffset,absolutize:function(element){Position.prepare();return Element.absolutize(element);},relativize:function(element){Position.prepare();return Element.relativize(element);},realOffset:Element.Methods.cumulativeScrollOffset,offsetParent:Element.Methods.getOffsetParent,page:Element.Methods.viewportOffset,clone:function(source,target,options){options=options||{};return Element.clonePosition(target,source,options);}};if(!document.getElementsByClassName)document.getElementsByClassName=function(instanceMethods){function iter(name){return name.blank()?null:"[contains(concat(' ', @class, ' '), ' "+name+" ')]";}
instanceMethods.getElementsByClassName=Prototype.BrowserFeatures.XPath?function(element,className){className=className.toString().strip();var cond=/\s/.test(className)?$w(className).map(iter).join(''):iter(className);return cond?document._getElementsByXPath('.//*'+cond,element):[];}:function(element,className){className=className.toString().strip();var elements=[],classNames=(/\s/.test(className)?$w(className):null);if(!classNames&&!className)return elements;var nodes=$(element).getElementsByTagName('*');className=' '+className+' ';for(var i=0,child,cn;child=nodes[i];i++){if(child.className&&(cn=' '+child.className+' ')&&(cn.include(className)||(classNames&&classNames.all(function(name){return!name.toString().blank()&&cn.include(' '+name+' ');}))))
elements.push(Element.extend(child));}
return elements;};return function(className,parentElement){return $(parentElement||document.body).getElementsByClassName(className);};}(Element.Methods);Element.ClassNames=Class.create();Element.ClassNames.prototype={initialize:function(element){this.element=$(element);},_each:function(iterator){this.element.className.split(/\s+/).select(function(name){return name.length>0;})._each(iterator);},set:function(className){this.element.className=className;},add:function(classNameToAdd){if(this.include(classNameToAdd))return;this.set($A(this).concat(classNameToAdd).join(' '));},remove:function(classNameToRemove){if(!this.include(classNameToRemove))return;this.set($A(this).without(classNameToRemove).join(' '));},toString:function(){return $A(this).join(' ');}};Object.extend(Element.ClassNames.prototype,Enumerable);(function(){window.Selector=Class.create({initialize:function(expression){this.expression=expression.strip();},findElements:function(rootElement){return Prototype.Selector.select(this.expression,rootElement);},match:function(element){return Prototype.Selector.match(element,this.expression);},toString:function(){return this.expression;},inspect:function(){return"#<Selector: "+this.expression+">";}});Object.extend(Selector,{matchElements:function(elements,expression){var match=Prototype.Selector.match,results=[];for(var i=0,length=elements.length;i<length;i++){var element=elements[i];if(match(element,expression)){results.push(Element.extend(element));}}
return results;},findElement:function(elements,expression,index){index=index||0;var matchIndex=0,element;for(var i=0,length=elements.length;i<length;i++){element=elements[i];if(Prototype.Selector.match(element,expression)&&index===matchIndex++){return Element.extend(element);}}},findChildElements:function(element,expressions){var selector=expressions.toArray().join(', ');return Prototype.Selector.select(selector,element||document);}});})();
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/Oryx/Utilities.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Oryx/Utilities.js";
			
Ajax.Request.prototype.abort=function(){this.transport.onreadystatechange=Prototype.emptyFunction;this.transport.abort();if(Ajax.activeRequestCount)Ajax.activeRequestCount--;};var Utilities={initRightBar:function(activate){if(!activate)return;Event.observe(window,'resize',this.adjustForRightBarAd.bindAsEventListener(this));window.display_right_bar_ad=this.adjustForRightBarAd()?'now':'later';},adjustForRightBarAd:function(event){if(HAS_ADFREE==='0'&&document.viewport.getWidth()>=1148){$(document.body).addClassName('expanded');if(window.display_right_bar_ad&&window.display_right_bar_ad=='later')
this.displayRightBarAd();if(typeof AdFreeAd!='undefined'&&AdFreeAd.hassetup){AdFreeAd.show();}
return true;}
else if(HAS_ADFREE==='0'){$(document.body).removeClassName('expanded');if(typeof AdFreeAd!='undefined'&&AdFreeAd.hassetup){AdFreeAd.hide();}
return false;}},displayRightBarAd:function(){$('right_bar_ad').update($F('right_bar_ad_code'));window.display_right_bar_ad=true;},handleEnter:function(e,f)
{f=f||function(){};var Ucode=e.keyCode?e.keyCode:e.charCode;if(Ucode==13){f();}
e=(e)?e:((window.event)?window.event:"");if(e){return!(e.keyCode==13||e.which==13);}},preloadImages:function()
{var preload_arr=new Array();for(var i=0;i<arguments.length;i++){var preload_img=new Image();preload_img.src=arguments[i];preload_arr.push(preload_img);}},focusFirstElementOnDom:function(id){document.observe('dom:loaded',function(){var form=$(id),first=form.findFirstElement();first.focus();});},detect_email_typo:function(str){var result,typos={'gmail':['gmaul','gmial'],'googlemail':['googlemial'],'yahoo':['yhaoo','yahooo','yaho'],'hotmail':['homail','hormail','htomail','hotmaill'],'comcast':['comast'],'windowslive':['winsdowslive'],'.com':['.cm']};for(var typo in typos){var max=typos[typo].length;for(var i=0;i<max;i++){if(str.match(typos[typo][i])){if(typo.match(typos[typo][i])){if(!str.match(typo)){result={error:true,re:typos[typo][i],str:typo};}}else{result={error:true,re:typos[typo][i],str:typo};}}}}
return result;},fix_email_typo:function(context,str){var self=this,typo=new this.detect_email_typo(str),debug=false;var error_message=context.previous('.error_message'),error_label=error_message.down('.error_label'),revert_typo=error_message.down('.revert_typo');if(debug){var typo_cache=context.value;typo_results=typo.error?typo_cache+' - match: '+typo.str:typo_cache+' - match: none';console.log(typo_results);}
if(typo.error){var re=new RegExp(typo.re,'g'),_str=str.replace(re,typo.str);context.value=_str;self.fix_email_typo(context,_str);error_label.innerHTML='We guess you meant&nbsp;'+'<i>'+context.value.split('@').pop()+'</i>';revert_typo.show();}else{revert_typo.observe('click',function(e){Event.stop(e);context.value=self.user_text;error_label.innerHTML='Lookin\' good!';this.hide();if(context.next('input')){context.next('input').focus();}});}},test_email_typo:function(id){function fire_event(element,event){if(document.createEventObject){var ev=document.createEventObject();return element.fireEvent('on'+event,ev);}else{var ev=document.createEvent('HTMLEvents');ev.initEvent(event,true,true);return!element.dispatchEvent(ev);}}
var input=$(id),args=['johndoe@yaho.cm','janedoe@gmial.cm','jackdoe@homail.cm'],i=0,dur=400,timer=setInterval(function(){if(i<args.length){input.value=args[i];setTimeout(function(){fire_event(input,'change');},dur/2);i++;}else{clearInterval(timer);console.log('Test run complete.');}},dur);},init_email_typo:function(id){var self=this;$(id).observe('change',function(){self.user_text=this.value;self.fix_email_typo(this,this.value);});},inArray:function(needle,array){if(isEmpty(array))return false;for(var i=0;i<array.length;i++)if(array[i]==needle)return true;return false;},stripBreaks:function(str)
{var res=str.replace(/\n/g," ");res=res.replace(/\n/g," ");return res;},stripComments:function(str)
{str=str.replace(/<!--[\w\s\/\-,]*-->/g," ");return str;},jogf:function(string,key)
{for(var item in key)
string=string.replace("%"+item,key[item]);return string;},roundNumber:function(num,dec){return Math.round(num*Math.pow(10,dec))/Math.pow(10,dec);},jackForm:function(form,url,success,dependent,localize)
{this.parameters=$(form).serialize();this.success=success;var pass=this;new Ajax.Request(url,{parameters:pass.parameters,method:"post",onSuccess:pass.success});},setDefaultText:function(objt,text,class_only)
{var obj=objt,txt=text;util.doOnDomLoad(function(){if($(obj)&&$(obj).value.empty()){obj=$(obj);if(obj.value==''&&!class_only)
obj.value=txt;$(obj).observe("focus",function(){if(obj.value==txt)
if(!class_only)
obj.value='';$(obj).addClassName('focus');if(obj.hasClassName('password')&&!(BrowserDetect.browser=='Explorer'&&BrowserDetect.version<=7))
obj.writeAttribute('type','password');else if(obj.hasClassName('password')){var pass=new Element('input',{type:'password'});if(obj.id)pass.writeAttribute('id',obj.id);if(obj.name)pass.writeAttribute('name',obj.name);if(obj.className)pass.writeAttribute('class',obj.className);if(obj.maxlength)pass.writeAttribute('maxlength',obj.maxlength);if(obj.onkeyup)pass.writeAttribute('onkeyup',obj.onkeyup);obj.insert({after:pass}).remove();pass.focus();}});$(obj).observe("blur",function(){if(obj.value==''||obj.value==txt){if(!class_only)
obj.value=txt;$(obj).removeClassName('focus');if(obj.hasClassName('password')&&!(BrowserDetect.browser=='Explorer'&&BrowserDetect.version<=7))
obj.writeAttribute('type','text');}});}});},toggle_set:new Array(),triggers:new Array(),toggle:function(obj,bit,destiny,set,trig_obj)
{var element=$(obj),display=element.getStyle("display"),trigger;if(trig_obj)
trigger=$(trig_obj);else
trigger=false;if(set||set==0){if(typeof(this.toggle_set[set])=="undefined")this.toggle_set[set]=new Array();this.toggle_set[set][this.toggle_set[set].length]=element;}
destiny=(destiny?destiny:"block");if(display=="none"){if(set||set==0)for(iter=0;iter<this.toggle_set[set].length;++iter)this.toggle_set[set][iter].style.display="none";element.style.display=destiny;}
else element.style.display="none";if(trigger&&(set||set==0)){if(typeof(this.triggers[set])=="undefined")this.triggers[set]=new Array();this.triggers[set][this.triggers[set].length]=trigger;}
if((set||set==0)&&typeof(this.triggers[set])!="undefined"){for(iter=0;iter<this.triggers[set].length;++iter)this.triggers[set][iter].removeClassName("active");if(display=="none"&&trigger!=false)
trigger.addClassName("active");else if(trigger!=false)
trigger.removeClassName("active");}},buddyCallWrapper:function(params,addRemove,notify,optmessage){params.ajax=1;new Ajax.Request("/profile",{parameters:params,onSuccess:function(response){var text="";switch(parseInt(response.responseText)){case 0:text="Removed!";if(addRemove==1)text=(notify?"User saved! They've been sent a message to let them know they're one of your favorites.":"User Saved!");break;case 12:text="User saved! They've been sent a message to let them know they're one of your favorites.";break;default:text="Couldn't save for some reason. Sorry. Try later.";break;}
var target=(optmessage?optmessage:'save_buttons');if($(target))$(target).innerHTML='<a class="buddy_removed" href="#nogo">'+text+'</a>';if(Mailbox&&Mailbox.m_current_thread)Mailbox.addSystemMessage('buddy',text);}});},updateStats:function(name,value,type,hash,optional_params){new Ajax.Request("/poststat",{method:"get",parameters:{"name":name,"value":value,"type":type,"hash":hash,"rnd":Math.random()},onSuccess:util.updateStats_cb.bindAsEventListener(util,optional_params)});},updateStats_cb:function(transport,optional_params){var res=transport.responseText.evalJSON();if(res.error){alert("Stat posting error"+res.error);}
if(optional_params&&optional_params["cb"]){optional_params.cb();}
if(optional_params&&optional_params["redirect_to"]){window.location.href=optional_params["redirect_to"];}},registerSuggestionActivity:function(suggestion_key){new Ajax.Request('/sookie/suggestions_activity',{parameters:{key:suggestion_key,cache_bust:new Date().getTime()}});},displayAdvert:function(position,keywords,isloggedin,slot_name)
{slot_name=slot_name||"Other";switch(position)
{case"sky":slot_addon="Sky";break;case"LB":slot_addon="Ldr";break;case"rect":slot_addon="Box";break;}
if(position!="transitional"){slot_name=slot_name+"_"+slot_addon;}
if(position=="Left"||position=="sky"){width=160;height=600;}else if(position=="Top"||position=="LB"){width=728;height=90;}else if(position=="Middle"||position=="rect"){width=300;height=250;}else if(position=="Right3"){width=200;height=500;}else if(position=="Transitional"||position=="transitional"){width=500;height=500;}else{width=40;height=40;}
var url="http://ads.okcimg.com/google/ad_manager?ad_slot="+slot_name+"&keywords="+keywords+"&pass_height="+height+"&pass_width="+width;document.write("<iframe id=\"ad_frame_"+position+"\"  width="+width+" height="+height+" marginwidth=0 marginheight=0 hspace=0 vspace=0"+" frameborder=0 scrolling=no bordercolor=\"#000000\""+" src=\""+url+"\"></iframe>");},toggleBadAdOverlay:function(){var overlay=$('bad_ads_overlay');if(overlay.visible()){overlay.hide();}else{overlay.show();$('bad_ads_form').show();$('bad_ad_error').hide();$('bad_ad_thanks').hide();}},reportBadAd:function(){if($F('bad_ad_type')=='none'){$('bad_ad_error').show().innerHTML='Please tell us what&rsquo;s wrong with the ad.';return false;}
if($F('bad_ad_description').length<10){$('bad_ad_error').show().innerHTML='Your description of the bad ad is too short.';return false;}
Feedback.send('BadAds',$F('bad_ad_type'),$F('bad_ad_description'),Math.floor((new Date).valueOf()/1000),{'page':window.location.href,'category':$F('bad_ad_type')});$('bad_ads_form').hide();$('bad_ad_error').hide();$('bad_ad_thanks').show();this.toggleBadAdOverlay.delay(2);},adjustMCHeight:function(){var height=0;if($('left_bar')&&$('main_content')){height=$('left_bar').getHeight();if(typeof AdFreeAd!='undefined'&&AdFreeAd.hassetup&&AdFreeAd.adcontainer.getHeight()>height){height=AdFreeAd.adcontainer.getHeight();}}
if(height){height+=100;if(BrowserDetect&&BrowserDetect.browser=="Explorer"&&BrowserDetect.version==6)
$('main_content').style.height=height+"px";else
$('main_content').style.minHeight=height+"px";}},getElementsByClassName:function(parent,class_name){var o=parent.getElementsByTagName('*');var a=new Array();for(var i=0;i<o.length;i++){if(o[i].attributes["class"]&&o[i].attributes["class"].value.toString().indexOf(class_name)!=-1){a.push(o[i]);}}
return a;},clearRecentlyViewed:function(){AjaxSookie.remove("recently_viewed");if($("lsRecent"))$("lsRecent").hide();else if($("section_recent")){if($("section_recent").hasClassName('last-child')){$("section_recent").previous().addClassName('last-child');}
$("section_recent").hide();}
if($("lsRecentContent"))$("lsRecentContent").hide();},startTimer:function(label){if(!this.timerStats){this.timerStats={};}
if(!this.timerStats[label]){this.timerStats[label]={"starts":[],"stops":[]};}
this.timerStats[label].starts.push(new Date());},stopTimer:function(label){if(!this.timerStats||!this.timerStats[label]||this.timerStats[label].stops.length>=this.timerStats[label].starts.length){alert("prematurely called stopTimer. Make sure you call startTimer first for label "+label);}
else{this.timerStats[label].stops.push(new Date());}},summarizeTimers:function(){var res="";for(var key in this.timerStats){var label_time=0;var samples=this.timerStats[key].stops.length;for(var i=0;i<samples;i++){label_time+=this.timerStats[key].stops[i].getTime()-this.timerStats[key].starts[i].getTime();}
res+="<br /> "+key+": "+(label_time/samples)+"ms avg. -- "+samples+" sample(s) -- "+label_time+"ms total";}
return res;},doOnDomLoad:function(func){if(!this.m_dom_load_funcs){this.m_dom_load_funcs=[];}
this.m_dom_load_funcs.push(func);},executeDomLoad:function(){if(this.m_dom_load_funcs){for(var i=0;i<this.m_dom_load_funcs.length;i++){setTimeout(this.m_dom_load_funcs[i],500);}}},toggleClass:function(b,c){var hasClass=b.attributes['class'].value.search(c);if(hasClass!=-1)
b.className=b.className.replace(new RegExp(' '+c+'\\b'),'');else
b.className+=' '+c;},floatADivOnDomLoad:function(id,parentid){util.doOnDomLoad(function(){if($(id))document.observe('scroll',util.floatADivOnScrollHandler.bind(this,id,parentid));if(window.attachEvent&&$(id))window.attachEvent("onscroll",util.floatADivOnScrollHandler.bind(this,id,parentid));});},floatADivOnScrollHandler:function(id,parentid){var b=$(id);var b_height=b.getHeight();var par=$(parentid);var coff=par.cumulativeOffset()[1];var csoff=par.cumulativeScrollOffset()[1];var bottom_y=$('page').cumulativeOffset()[1]+$('page').getHeight()-$('page').cumulativeScrollOffset()[1];if(coff<csoff&&b_height<bottom_y)
b.setStyle({position:'fixed',marginTop:'0px'});else if(coff<csoff)
b.setStyle({position:'fixed',marginTop:(bottom_y-b_height)+'px'})
else
b.setStyle({position:'static',marginTop:'0px'});},toMask:function(){var res=0;if(typeof arguments[0]=="object")
bits=arguments[0];else
bits=arguments;for(var i=0;i<bits.length;i++){if(bits[i]>31||bits[i]<0)
throw("fillBitsInMask expects ints in [0..30] due to JS bitwise limitations");res=res|Math.pow(2,bits[i]);}
return res;},fromMaskToList:function(mask){var res=[];for(var i=0;i<31;i++)
if(Math.pow(2,i)&mask)
res.push(i);return res;},isBitSetInMask:function(mask,bit){return Math.pow(2,bit)&mask?1:0;},flipMobile:function(){var url=location.href.replace('m.okcupid','okcupid').replace(/[&?](en|dis)able_mobile=1/g,'').split('#');url[0]+=(url[0].indexOf('?')!=-1?'&':'?')+'enable_mobile=1';location.href=url.join('#');},polygonBoundary:function(bounds,point)
{var polyX=bounds[0],polyY=bounds[1],j=polyX.length-1,odd=false,x=point[0],y=point[1];for(i=0;i<polyX.length;i++){if(polyY[i]<y&&polyY[j]>=y||polyY[j]<y&&polyY[i]>=y){if(polyX[i]+(y-polyY[i])/(polyY[j]-polyY[i])*(polyX[j]-polyX[i])<x){odd=!odd;}}
j=i;}
return odd;},postToUrl:function(path,params){var form=new Element('form',{method:'post',action:path,style:'display: none;'});for(var key in params){form.insert({bottom:new Element('input',{type:'hidden',name:key,value:params[key]})});}
$(document.body).insert({bottom:form});form.submit();},thumbnailAction:function(){this.has_thumbnail?location.href='/profile':this.showUploader();},placeOnTop:function(el,adj){if(!$(el)||!GrazeCoverManager||!GrazeCoverManager.HighestKnownZIndex)return;var highest=GrazeCoverManager.HighestKnownZIndex+adj;if(+($(el).style.zIndex)<highest)$(el).style.zIndex=highest;},progressmessages:{error_quota:{title:'Upload limit reached!',subtitle:'',content:'You can only upload 10 profile photos. Delete some to upload more.',buttons:['uploader_viewphotos']},error_badfile:{title:'Oops!',subtitle:'',content:'We didn&rsquo;t understand that file. Try again!',buttons:['uploader_startover_button']},success:{title:'Success',subtitle:'Your new photo is up',content:'You look fantastic!',buttons:['uploader_viewphotos','uploader_newphoto']},success_primary:{title:'Success',subtitle:'Primary photo updated',content:'We think it&rsquo;s your best one too!',buttons:['uploader_viewphotos','uploader_newphoto']}},ten_reached:false,is_leftbar:false,showUploader:function(params){if(typeof ProfilePhotos!='undefined'&&ProfilePhotos.current_album&&$('self_upload')){ProfilePhotos.showUploader();return;}
if(params&&params.leftbar)this.is_leftbar=true;var params={Cropper:true,CommitStatus:0,DontHide:true};var showerror=(params&&params.showerror)?params.showerror:false;if(this.ten_reached)showerror='error_quota';if(showerror)params.FailureError=showerror;UploadManager.showUploader(this.uploadSuccess.bind(this),this.uploadFailure.bind(this),this.showProgress.bind(this),params);},showProgress:function(){if(!$('uploader'))return;if(!$('uploader_beaker')){$('uploader').insert({bottom:'<div id="uploader_beaker"></div>'});}
util.placeOnTop('uploader_beaker',2);},hideProgress:function(){if($('uploader_beaker'))$('uploader_beaker').remove();},uploadSuccess:function(okuploader,response){if(response&&response.error=='E_QUOTA'){util.hideProgress();this.updateUploaderBox('error_quota',response);}else{var cropinfo=okuploader.cropinfo;var res1='<img src="http://akcdn.okccdn.com/php/load_okc_image.php/images/160x160/160x160/'+cropinfo.tn_upper_left_x+'x'+cropinfo.tn_upper_left_y+'/'+cropinfo.tn_lower_right_x+'x'+cropinfo.tn_lower_right_y+'/2/'+okuploader.Picid+'.jpg"/>';response.thumb=res1;response.picid=okuploader.Picid;util.hideProgress();this.updateUploaderBox('success',response);if($('leftbar_thumb')&&$('leftbar_thumb').innerHTML.replace(/\s/gi,'')==''){$('leftbar_thumb').update(res1);if($('uploader_primary'))$('uploader_primary').hide();}
if(this.is_leftbar){util.updateStats('photos uploaded - leftbar',1,'counter','Von3Fq0sjOwqsuCbA0/gqVkYR2A=');}
if($('new_user_steps')){var new_user_div=$('new_user_steps').select('li.upload_photo');if(new_user_div.length){new_user_div.first().hide();util.updateStats('photos uploaded - new user home',1,'counter','w9vkKyifA7ij74rmmHHJR6KqJlw=');}}
else if($('self_upload')&&$('thumb0')&&!$('thumb0').visible()){var res2='<img src="http://akcdn.okccdn.com/php/load_okc_image.php/images/160x160/160x160/'+cropinfo.tn_upper_left_x+'x'+cropinfo.tn_upper_left_y+'/'+cropinfo.tn_lower_right_x+'x'+cropinfo.tn_lower_right_y+'/2/'+okuploader.Picid+'.jpg"/>';$('thumb0_a').update(res2);$('self_upload').hide();$('thumb0').show();if(!this.is_leftbar)util.updateStats('photos uploaded - profile',1,'counter','kPvk8TxkF+umsJhHTneDpHLKrl4=');}
else if($('self_upload')&&$('thumb0')){if(!this.is_leftbar)util.updateStats('photos uploaded - profile',1,'counter','kPvk8TxkF+umsJhHTneDpHLKrl4=');}
else if($('approved_box')){window.location='/freeupgrade';}
if($('uploader')&&response&&response.pics&&response.pics.length>=11){if($('uploader_newphoto'))$('uploader_newphoto').hide();this.ten_reached=true;}
util.has_thumbnail=true;}},uploadFailure:function(showerror){showerror=(typeof showerror=='string')?showerror:'error_badfile';util.updateUploaderBox(showerror);return;},updateUploaderBox:function(message,response){if(!util.progressmessages[message])return;var text=util.progressmessages[message],content='',thumb='';if(message.search('error')!=-1){content='<p class="oknotice_error">'+text.content+'</p>';}else{content='<p>'+text.content+'</p>';}
if(message=='success'){if(response){$('uploader_content').addClassName('withpic');if($('uploader_viewphotos')){var link=$('uploader_viewphotos').down('a').getAttribute('href');thumb='<div id="uploaded_thumb"><a href="'+link+'">'+response.thumb+'</a></div>';}else{thumb='<div id="uploaded_thumb">'+response.thumb+'</div>';}
$('uploader_h4').insert({before:thumb});if(response.pics&&response.pics.length>1){$('uploader_primary').setAttribute('rel',response.picid);if(!this.inArray('uploader_primary',text.buttons)){text.buttons.push('uploader_primary');}}}}
if($('uploader_sr_text'))$('uploader_sr_text').hide();$('uploader').down('h3').update(text.title);$('uploader_h4').update(text.subtitle);$('uploader_content').update(content);$('uploader_buttons').show().immediateDescendants().invoke('hide');for(var i=0;i<text.buttons.length;i++){var button=$(text.buttons[i]);if(button)button.show();}
if(this.ten_reached&&$('uploader_newphoto')){$('uploader_newphoto').hide();}
if(message.search('error')!=-1){$('uploader_next','uploader_finished','uploader_h4').invoke('hide');}
if(message=='success'&&response===false&&$('uploader_newphoto')){$('uploader_newphoto').down('a').setAttribute('onclick','ProfilePhotos.showUploader(); return false;');}
if($('uploader_viewphotos')){if($('albums')&&$('albums').visible()){var viewphotos=$('uploader_viewphotos').down('a');viewphotos.setAttribute('href','');viewphotos.setAttribute('onclick',"$('uploader').hide(); ProfilePhotos.showAlbum('0'); return false;");}else if($('full_albums')){$('uploader_viewphotos').hide();$('uploader_close').show();}}},place:function(el,adj){if(!$(el))return;if(document.viewport.getScrollOffsets().top>91){$(el).setStyle({top:(document.viewport.getScrollOffsets().top)+'px'});}
else{$(el).setStyle({top:'100px'});}
this.placeOnTop(el,adj);},makePrimary:function(){if(!$('uploader_primary')||$('uploader_primary').getAttribute('rel')=='')return;this.showProgress();var picid=$('uploader_primary').getAttribute('rel');$('uploader_primary').hide();var moveparams={albumid:0,picid:picid,"picture.move_to_ajax":1,position:0};var params={picid:picid,albumid:0,"picture.set_highlight_ajax":1};new Ajax.Request('/photoupload',{parameters:moveparams,onSuccess:function(){if(typeof ProfilePhotos!='undefined'&&ProfilePhotos.current_album==0&&$('self_upload')){ProfilePhotos.updatePrimary(picid);this.updateUploaderBox('success_primary');this.hideProgress();}else{new Ajax.Request('/photoupload',{parameters:params,onSuccess:this.makePrimary_cb.bind(this)});}}.bindAsEventListener(this,params)});},makePrimary_cb:function(transport){var response=transport.responseText.evalJSON();if(response.status=='1'){var thumb=$('uploaded_thumb').down('a').innerHTML;$('leftbar_thumb').update(thumb);if($('self_upload')){if($('thumb0_a')){$('thumb0_a').update(thumb);}
if($('album_thumb_0')){$('album_thumb_0').update(thumb);}}
if($('thread')){$('thread').select('li.from_me a.photo').invoke('update',thumb);}
this.updateUploaderBox('success_primary',response);}
this.hideProgress();},reloadAds:function(){var ads=['ad','leaderboard','skyscraper_floater_region','skyscraper_floater_region2','home_ad','profile_ad','quickmatch_ad'],ad;for(var i=0;i<ads.length;i++){ad=$(ads[i]);if(this.isOnScreen(ad)&&ad.down('iframe')&&!ad.down('iframe').hasClassName('takeover')){ad.update(ad.innerHTML);}}},isOnScreen:function(el){el=$(el);if(!el||!el.visible())return false;var elem_top=el.cumulativeOffset().top;var elem_btm=elem_top+el.getHeight();var view_top=document.viewport.getScrollOffsets().top;var view_btm=view_top+document.viewport.getHeight();if(elem_btm<view_top||elem_top>view_btm)
return false;else
return true;},supportsTransitions:function(){if(typeof this.transitions_supported!='undefined')
return this.transitions_supported;this.transitions_supported=false;var rules=[{css:'-webkit-transition',js:'webkitTransition'},{css:'-moz-transition',js:'MozTransition'},{css:'-ms-transition',js:'MSTransition'},{css:'-o-transition',js:'OTransition'}];var style='',div=document.createElement('div'),i;for(i=0;i<rules.length;i++){style+=rules[i].css+': color 1s linear; ';}
div.innerHTML='<div style="'+style+'"></div>';for(i=0;i<rules.length;i++){if(div.firstChild.style[rules[i].js]!==undefined){this.transitions_supported=true;break;}}
delete div;return this.transitions_supported;},isTouch:function(){return("createTouch"in document);},isKeyAChar:function(keycode){if(keycode>=48&&keycode<=90)return true;if(keycode>=96&&keycode<=111)return true;if(keycode>=186&&keycode<=222)return true;return false;},truncateMultilineText:function(el){if(!$(el))return;el=$(el);var parent=$(el.parentNode);if(!parent.getWidth()||!parent.getHeight())return;if(parent.getStyle('overflow')!='hidden')return;var height=parent.getHeight();parent.setStyle({position:'relative'});el.setStyle({display:'block',position:'absolute',visibility:'visible'});while(el.getHeight()>height){el.innerHTML=el.innerHTML.replace(/\W*\s(\S)*$/,'&hellip;');}},verticallyCenter:function(el,force){el=$(el);if(!el||util.isOnScreen(el)&&!force)return;var top=document.viewport.getScrollOffsets().top+75;if(!el.up('#main_content')){top+=125;}
el.setStyle({top:top+'px'});}};Object.extend(Element.Methods,{getValues:function(element,class_name){var vals=element.select('input.'+class_name).collect(function(input){return input.checked?input.value:0}).without(0);if(vals.length>1){return vals;}
else if(vals.length==1){return vals[0];}
else{return null;}},showOrPulse:function(element){element.visible()?element.pulsate({pulses:2,duration:0.6}):element.show();},removeClassNames:function(element,class_names){for(var i=0;i<class_names.length;i++){element.removeClassName(class_names[i]);}
return element;}});Element.addMethods();var TRL={deg:0.0,save_interval:null,stop_val:-1,initialize:function(offset){if(BrowserDetect.browser!='Chrome'||(offset&&offset==this.stop_val))
return;this.deg=offset||this.deg;if(this.deg){document.body.style.webkitTransform='rotate('+this.deg+'deg)';}
this.setupListeners();},setupListeners:function(){document.observe('mousemove',this.mouseMove.bind(this));},mouseMove:function(){if(this.stopped)
return;this.deg+=0.002;document.body.style.webkitTransform='rotate('+this.deg+'deg)';clearInterval(this.save_interval);this.save_interval=setTimeout(this.save.bind(this),500);},save:function(val){new Ajax.Request('/trl',{parameters:{d:val||this.deg}});},stop:function(){this.stopped=true;this.deg=0.0;this.save(this.stop_val);document.body.style.webkitTransform='rotate('+this.deg+'deg)';}};function checkForm(tab){if((FormId=="profileeditform")&&(tab=="profileedit"||tab=="details")){tab=(tab=="details"?tab:"essays");$("tab-"+OldId).removeClassName("tab-on");$("tab-"+tab).addClassName("tab-on");displayDiv(tab);}else{if(FormId!='picturerows'&&SerializedForm!=Form.serialize(FormId)){$(FormId).action+="?tab="+tab;if(FormId=="settingsform"){if(!document.getElementById("settingsSubmit").disabled){$(FormId).submit();}else{alert("Please enter your password to save your settings.");document.getElementById("oldPassword").focus();}}else{$(FormId).submit();}}else{if(tab=="details"){page="profileedit";}else{page=tab;}
document.location.href="/"+page+"?tab="+tab;}}}
var util=Utilities;if(typeof Array.prototype.push=='undefined')
Array.prototype.push=function(){var i=0;b=this.length;a=arguments;for(i;i<a.length;i++)this[b+i]=a[i];return this.length;};function $RF(el,radioGroup){if($(el).type&&$(el).type.toLowerCase()=='radio'){var radioGroup=$(el).name;var el=$(el).form;}else if($(el).tagName.toLowerCase()!='form'){return false;}
var checked=$(el).getInputs('radio',radioGroup).find(function(re){return re.checked;});return(checked)?$F(checked):null;}
util.doOnDomLoad(function(){util.adjustMCHeight();});var GOOGLE_PUNCH=false;var DC=DataCollect=({tag:'<iframe src="http://pixel.33across.com/ps/?tt=iframe&amp;pid=441&amp;event_cgi"width="1"height="1"frameborder="0"style="visibility:hidden;"></iframe>',types:{view:{act:'pv'},message:{act:'m'},vote:{act:'v'},poke:{act:'p'}},initialize:function(){for(var type in this.types){this['record'+type.capitalize()]=this.record.bind(this,type);}
return this;},record:function(type,data){if(type!='view'){data.s=data.s||data.v;delete data.v;data.r=data.r||data.o;delete data.o;}
var event_data=Object.extend(this.types[type],data);var event_cgi=Object.toQueryString(event_data);var event_tag=this.tag.gsub('event_cgi',event_cgi);if($('dcollect'))$('dcollect').insert({bottom:event_tag});}}).initialize();var TimeTracker={handler:null,stats:{},get:function(){var cookies=NanoCookie.getAll(),bundle;for(var i=0;i<cookies.length;i++){if(cookies[i].key.indexOf('timetracking_')==0){bundle=cookies[i].value;try{bundle=bundle.evalJSON();}catch(e){bundle=undefined;}
if(bundle&&bundle.n&&bundle.h&&bundle.v){NanoCookie.remove(cookies[i].key);util.updateStats(bundle.n,bundle.v,'value',bundle.h);}}}},start:function(key,statname,stathash,minutes){var now=new Date().getTime();this.stats[key]={statname:statname,stathash:stathash,starttime:now};if(minutes){this.stats[key].upperlimit=minutes*60*1000;}
this.monitor();},end:function(key){if(!this.stats[key])return;var now=new Date().getTime(),diff,stat,bundle;stat=this.stats[key];diff=now-stat.starttime;if(stat.upperlimit&&diff>stat.upperlimit){diff=stat.upperlimit;}
util.updateStats(stat.statname,diff,'value',stat.stathash);delete this.stats[key];this.monitor();},monitor:function(){var hasstats=false;for(var i in this.stats){if(this.stats[i]!=null&&typeof(this.stats[i])!='undefined'){hasstats=true;break;}}
if(hasstats){UnloadHandler.add('timetracker',this.update.bind(this));}else{UnloadHandler.remove('timetracker');}},update:function(){var now=new Date().getTime(),stat,diff,bundle;for(var i in this.stats){stat=this.stats[i];diff=now-stat.starttime;if(stat.upperlimit&&diff>stat.upperlimit){diff=stat.upperlimit;}
bundle='{ "n": "'+stat.statname+'", "h": "'+stat.stathash+'", "v": "'+diff+'" }';NanoCookie.set('timetracking_'+i,bundle,{ms:1000*60*10});}}};var UnloadHandler={functions:{},add:function(key,func){if(!this.functions[key]){this.functions[key]=func;}
if(window.onbeforeunload==null||typeof(window.onbeforeunload)=='undefined'){window.onbeforeunload=this.run.bind(this);}},has:function(){if(!this.functions)return false;for(var i in this.functions){if(typeof this.functions[i]=='function'){return true;}}
return false;},remove:function(key){if(this.functions[key]){delete this.functions[key];}
if(window.onbeforeunload!=null&&typeof(window.onbeforeunload)!='undefined'&&!this.has()){window.onbeforeunload=null;}},run:function(){if(!this.functions)return;var output='';for(var i in this.functions){if(typeof this.functions[i]=='function'){output=this.functions[i]();if(output!='')return output;}}}};var AdChecker={count:0,init:function(sookie_count){if($(document.body).hasClassName('hastakeover'))return;if(!sookie_count||typeof sookie_count!='number'){sookie_count=0;}
sookie_count=Math.floor(sookie_count);this.count=sookie_count;var checkresults=this.check();if(checkresults==1){this.count+=1;}
else if(checkresults==-1){if(this.count>0)
util.updateStats('adchecker - rescinded',1,'counter','QuLjIwMh/2YtiRa2c987oC1XdbI=');this.count=0;}
else{return;}
if(this.count>=5){this.react();}
if(sookie_count!=this.count&&this.count<=5){AjaxSookie.update('adchecker',this.count,86400*5,'bfa8b71323a05ab15a460bbd6791530d2be83b46');}},check:function(){var ads={'ad':250,'leaderboard':90,'skyscraper_floater_region':600,'skyscraper_floater_region2':600,'home_ad':250},ad,adcount=0,blocked=false,hasads=false;for(var i in ads){if($(i)&&$(i).down('iframe')&&!$(i).down('iframe').hasClassName('takeover')){hasads=true;adcount+=1;ad=$(i).down('iframe');if(!ad.visible()||ad.getHeight()!=ads[i]){blocked=true;}}}
if(adcount&&blocked){return 1;}else if(adcount){if(Math.floor(Math.random()*100)==0){util.updateStats('adchecker - shown',1,'counter','deFOfFRce3U/hCIYYY67qJxA1tY=');}
return-1;}else{return 0;}},react:function(){if(typeof AdFreeAd!='undefined'&&AdFreeAd.ready){AdFreeAd.setup();}}};var BadAdChecker={checktimer:1000,removetimer:100,has:false,listareas:[],listcached:false,statupdated:0,found:0,goodcycles:0,init:function(has){if(!has){setTimeout(this.check.bind(this),this.checktimer);}else{this.has=true;setTimeout(this.remove.bind(this),this.removetimer);}},check:function(){if(this.has||typeof AjaxSookie=='undefined')return;var body=document.body.textContent||document.body.innerHTML;if(body.search('ads not by this site')!=-1||body.search('ads not by site')!=-1||body.search('clicks.ads2srv.com')!=-1){this.has=true;AjaxSookie.set('has_bad_ads',1,86400*365,'b5a8cf1c890032f74c1671075edd78cc2e9e7f3e');}
if(!this.has){this.checktimer*=2;setTimeout(this.check.bind(this),this.checktimer);}else{setTimeout(this.remove.bind(this),this.removetimer);}},getBadAds:function(element,type,term){if(!element||!element.select||!term||!type)return[];var i,length,els,list=[];els=element.select(type);for(i=0,length=els.length;i<length;i++){if(els[i].visible&&els[i].visible()&&els[i].innerHTML&&els[i].innerHTML!=''){if(els[i].innerHTML.search('clicks.ads2srv.com')!=-1||els[i].innerHTML.search(term)!=-1){list.push(els[i]);}}}
return list;},getList:function(){if($('site_morenav')){this.listareas.push({parent:$('site_morenav'),lielem:'li',string:'Sexy'});}
if($('visitors')){this.listareas.push({parent:$('visitors'),lielem:'li',string:'Stacy244'});}
if($('section_matches')&&$('section_matches').down('ul')){this.listareas.push({parent:$('section_matches').down('ul'),lielem:'li',string:'Stacy244'});}
if($('main_column')&&$('main_column').down('div.user_list')){this.listareas.push({parent:$('main_column')&&$('main_column').down('div.user_list'),lielem:'div.user_row_item',string:'Stacy244'});}
this.listcached=true;},remove:function(){var i,length,list=[],area,centers,badad,removed,threehundred;if(!this.listcached){this.getList();}
if(!this.listareas||!this.listareas.length){return;}
for(i=0,length=this.listareas.length;i<length;i++){area=this.listareas[i];list=list.concat(this.getBadAds(area.parent,area.lielem,area.string));}
centers=document.getElementsByTagName('center');if(centers&&centers.length){for(i=0,length=centers.length;i<length;i++){badad=$(centers[i]).down('iframe');if(badad&&badad.getAttribute('replaced')=='true'){list.push($(centers[i]));}}}
threehundred=$('main_content').select('div.okad div.frame_interior');if(threehundred&&threehundred.length){threehundred=threehundred[0];threehundred=threehundred.down('iframe[replaced="true"]');if(threehundred){if(threehundred.next().innerHTML.search('not by this site')!=-1){list.push(threehundred.next());}
list.push(threehundred);}}
if(list.length){if(this.statupdated<4){util.updateStats('vidsaver ads - ad removed',list.length,'counter','bzjn5HLbbhkyv3QtNmxs3j9/4d0=');this.statupdated+=1;}else if(this.statupdated==4){util.updateStats('vidsaver ads - ad removed - more than 3x',1,'counter','Yck2vmADRvE0IwYZGBkyCkABM+g=');}
this.found+=1;this.goodcycles=0;}else{this.goodcycles+=1;}
for(i=0,length=list.length;i<length;i++){if(list[i]&&(list[i].tagName=='CENTER'||list[i].tagName=='IFRAME')&&list[i].remove){list[i].remove();}else if(list[i].hide){list[i].hide();}}
if(this.found&&this.goodcycles<200||!this.found&&this.goodcycles<300){setTimeout(this.remove.bind(this),this.removetimer);}}};
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/Oryx/Branding.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Oryx/Branding.js";
			
var currentdate=new Date();var currenttime=currentdate.getTime();var Branding={on:true,actives:[],opened:false,opened_once:false,check_for_brand:function(o)
{for(iter=0;iter<this.actives.length;iter++)
if($(o).hasClassName(this.actives[iter])){return this.actives[iter];}
return false;},campaigns:{venus_smooth:{classname:"venus_smooth",on:true,global:true,badwords:false,statname:"ads - venus smooth - im",stathash:"hmfKQAmk3geGhAGprzv38gTLfwk=",pixel:'',clickthru:"http://ad.doubleclick.net/clk;247177418;72254679;w;pc=[TPAS_ID]",metrocode:"",polygons:[]}}}
if(Branding.on)
for(var brand in Branding.campaigns)
if(Branding.campaigns[brand].on){Branding.actives.push(brand);}
var MessageSponsor={on:true,adinfo:null,backup:null,initialize:function(campaign,adinfo,params){if(!this.on||!campaign||!$(campaign)||!adinfo||!params)return;this.adinfo=adinfo;this.adinfo.campaign=campaign;if(params.backup)this.backup=params.backup;var params={base:1,page:'Mailbox',pageurl:params.pageurl,cachebust:params.cachebust,authid:params.authid,format:'background'};new Ajax.Request('/daisy',{parameters:params,onSuccess:this.initialize_cb.bind(this)});},initialize_cb:function(transport){try{var response=transport.responseText.evalJSON();}catch(e){var response=false;}
if(response.background_switch=='86'||response.background_switch=='122'){$('topmessage').addClassName('extrapadding');$(this.adinfo.campaign).show();var stat=this.adinfo.stat.replace('####','view');util.updateStats(stat,1,'counter',this.adinfo.viewhash);}else if($('backup')){$('topmessage').addClassName('extrapadding');$('backup').show();if(this.backup)this.backup();}},click:function(){if(!this.on||!this.adinfo)return;var adinfo=this.adinfo;var stat=this.adinfo.stat.replace('####','click');util.updateStats(stat,1,'counter',this.adinfo.clickhash);}};
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/prototype_abort.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/prototype_abort.js";
			
if(typeof(Ajax)!="undefined"){Ajax.Request.prototype.abort=function(){var abort_type=typeof this.transport.abort;if(abort_type=="function"){this.transport.onreadystatechange=Prototype.emptyFunction;this.transport.abort();Ajax.activeRequestCount--;}};}
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/StatHat.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/StatHat.js";
			
var _StatHat=_StatHat||[];_StatHat.push(['_setUser','MTkgx-fIQMnKc2ehIy0Y2YyUoQ~~']);(function(){var sh=document.createElement('script');sh.type='text/javascript';sh.async=true;sh.src=('https:'==document.location.protocol?'https://':'http://')+'www.stathat.com/javascripts/api.js';var s=document.getElementsByTagName('script')[0];s.parentNode.insertBefore(sh,s);})();var StatWrap={debug:false,push:function(args){console.log('StatWrap.push called');if(this.debug){console.log('Stat call intercepted');}else{window._StatHat.push(args);}}}
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/Daisy.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Daisy.js";
			
(function(){'use strict';window.Daisy={message_log:[],init_time:null,init:function(){var self=this,receiveMessage;if(!this.init_time){this.init_time=(new Date()).getTime();Event.observe(window,'message',function(e){self.message_log.push(e);});(function(){self.recordPassbackStats();}).delay(10);}else{}},getAds:function(){var ads={},i,msg,msgdata;for(i=0;i<this.message_log.length;i+=1){msg=this.message_log[i];if(msg.origin===window.location.origin||msg.origin==="http://ads.okcimg.com"){try{msgdata=this.message_log[i].data.evalJSON();msgdata.timeStamp=msg.timeStamp;ads[msgdata.format]=ads[msgdata.format]||[];ads[msgdata.format].push(msgdata);}catch(e){}}}
return ads;},getCurrentAds:function(){var ads=this.getAds(),current_ads={},format;for(format in ads){if(ads.hasOwnProperty(format)){current_ads[format]=ads[format].last();}}
return current_ads;},getPassbacks:function(){var ads=this.getAds(),passbacks={},format,i,delay;for(format in ads){if(ads.hasOwnProperty(format)){passbacks[format]=[];for(i=0;i<ads[format].length-1;i+=1){delay=ads[format][i+1].timeStamp-ads[format][i].timeStamp;delay=Math.min(Math.max(delay,0),5000);ads[format][i].passbackDelay=delay;passbacks[format].push(ads[format][i]);}}}
return passbacks;},recordPassbackStats:function(){var self=this;var passbacks=self.getPassbacks(),format,i,pb;if(window.location.origin==='http://www.okcupid.com'&&Math.random()<0.99){return null;}
for(format in passbacks){if(passbacks.hasOwnProperty(format)){for(i=0;i<passbacks[format].length;i+=1){pb=passbacks[format][i];if(pb.next==='AdMeld-RTB'||pb.next==='admeld_rtb_t2'){StatWrap.push(['_trackValue','1W5Oa2BOs0PHFQbZ5LwbkCAzUTBG',pb.passbackDelay]);}else if(pb.next==='okrubicon'){StatWrap.push(['_trackValue','SIZMm_lvJOitmf2F3OjEpSBOQkdK',pb.passbackDelay]);}else if(pb.next==='google_adx'){StatWrap.push(['_trackValue','zKGmlnsnypSB7OJv6VCVxiBpUFRl',pb.passbackDelay]);}else if(pb.next==='openx_rtb'){StatWrap.push(['_trackValue','RsTUDCxuyDOiAAbsJAofIiBnNVNi',pb.passbackDelay]);}else if(pb.next==='pubmatic_rtb'||pb.next==='pubmatic_rtb_t2'){StatWrap.push(['_trackValue','3f4rATksoarksrQAfZcWnyBoMnh1dw~~',pb.passbackDelay]);}}}}}};})();
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/fancyinputs/fancyinputs.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/fancyinputs/fancyinputs.js";
			
var FancyInput=Class.create({initialize:function(id,params){if(!$(id))return;this.input=$(id);this.paren=$(this.input.parentNode);this.label=this.paren.down('label');if(this.paren.down('.forgotpasslink')){this.forgot=this.paren.down('.forgotpasslink');this.forgoterror=this.paren.down('.okfeedback');}
this.resize_timeout=null;this.width=0;this.maxwidth=0;this.expwidth=0;this.original_width=this.paren.getWidth();this.submitting=false;if($(this.input.up('form'))){this.form=$(this.input.up('form'));}
if(params){if(params.submit_cb){this.submit_cb=params.submit_cb;}}
this.time_in=0.1;this.time_out=0.1;if(this.input&&this.label&&this.paren){if(document.loaded)
this.setup(this,id);else
util.doOnDomLoad(this.setup.bind(this,id));}},setup:function(id){this.fixInput();this.checkField();if(this.isEmpty()){this.showLabel();}
this.input.observe('focus',this.handleFocus.bind(this));this.input.observe('blur',this.handleBlur.bind(this));this.input.observe('change',this.handleChange.bind(this));this.label.observe('click',function(){this.input.focus();}.bind(this));var descendants=this.paren.immediateDescendants();for(var i=0;i<descendants.length;i++){if(descendants[i].tagName=='A'){descendants[i].observe('click',this.submit.bind(this));}}},checkField:function(){setInterval(function(){if(this.isEmpty()){this.showLabel();}else{this.hideLabel();}}.bind(this),100);},handleKeyup:function(event){this.resizeInputOnType();if(this.paren.hasClassName('forgotmsg')&&util.isKeyAChar(event.keyCode)){this.paren.removeClassName('forgotmsg');}
if(!this.isEmpty()){this.hideLabel();}else{var i=setInterval(function(){if(this.isEmpty()){clearInterval(i);setTimeout(this.showLabel.bind(this),100);}}.bind(this),50);}
if(event.keyCode==13){if(!this.submitting){this.submit();this.submitting=true;setTimeout(function(){this.submitting=false;}.bind(this),2000);}}},handleFocus:function(){this.paren.addClassName('focused');this.resizeInputOnClick();this.input.observe('keyup',this.handleKeyup.bind(this));},handleBlur:function(){this.paren.removeClassName('focused');if(this.isEmpty()){this.showLabel();}
this.resizeInputOnClick();this.resizeInputOnType();},handleChange:function(){this.input.stopObserving('keyup');},forgotOver:function(){if(!this.forgot)return;if(this.paren.hasClassName('forgotmsg'))return;this.paren.addClassName('showforgot');},forgotOut:function(){if(!this.forgot)return;this.paren.removeClassName('showforgot');},check:function(page){if(this.checking||!SCREENNAME||SCREENNAME=='')return;if(this.forgoterror&&this.forgoterror.visible()){this.forgoterror.style.visibility='hidden';setTimeout(function(){this.forgoterror.style.visibility='visible';}.bind(this),100);}
this.checking=true;var stats={upgrade:{name:'fancyinputs - clicked forgot pass link - upgrade',hash:'3nbyEMr8NM5jMuxK7gJXkvdyuio='},settings:{name:'fancyinputs - clicked forgot pass link - settings',hash:'BXEivBsDn3Nbrktmwodu2CycuXI='}}
if(page&&page!=''&&stats[page]){util.updateStats(stats[page].name,1,'counter',stats[page].hash);}
var origin=window.location.protocol+'//'+window.location.host;new Ajax.Request(origin+'/lostpassword',{parameters:{okc_api:1,email:SCREENNAME},onSuccess:this.check_cb.bind(this)});},check_cb:function(transport){var response=transport.responseText.evalJSON(),result='ERROR';this.checking=false;if(response&&(response.message=='SUCCESS'||response.message=='REQUEST_LIMIT')){result='SUCCESS';}
if(PasswordShadowboxForm&&PasswordShadowboxForm.channel!=''){PasswordShadowboxForm.socket.send('{"resetpw": "'+result+'"}');return;}
if(result=='SUCCESS'){this.forgoterror.className='okfeedback';this.forgoterror.down('p.message').innerHTML='We emailed you a Reset Password link!';}else{this.forgoterror.className='okfeedback invalid';this.forgoterror.down('p.message').innerHTML='Something went awry; try again later!';}
this.paren.removeClassName('error').addClassName('forgotmsg');this.paren.removeClassName('showforgot');this.input.focus();if($('delete_password_error'))$('delete_password_error').hide();if($('downgrade_password_error'))$('downgrade_password_error').hide();},submit:function(event){if(this.submit_cb){eval(this.submit_cb);}else if(this.form){this.form.submit();}
if(event){event.preventDefault();Event.stop(event);}
return false;},isEmpty:function(){if(this.input.value===''){return true;}
return false;},hideLabel:function(){this.label.hide();},showLabel:function(){this.label.show();},getInputWidth:function(){if(!$('textwidth_'+this.input.id)){return this.input.value.length*8;}else{$('textwidth_'+this.input.id).style.maxWidth=this.maxwidth+'px';$('textwidth_'+this.input.id).innerHTML=this.input.value.replace(' ','&nbsp;');return $('textwidth_'+this.input.id).getWidth();}},resizeInputOnType:function(){if(!this.input.hasClassName('expand_on_type')||!this.input.getAttribute('data-max-width'))return;if(!this.maxwidth){this.maxwidth=+(this.input.getAttribute('data-max-width'));}
var padding=this.paren.getWidth()-this.input.getWidth();var width=this.getInputWidth()+padding;if(width>this.maxwidth){width=this.maxwidth;}
if(width<this.original_width){width=this.original_width;}
if(width>this.original_width){this.paren.addClassName('type_expanded');}else{this.paren.removeClassName('type_expanded');}
this.resize(width);},resizeInputOnClick:function(){if(!this.input.hasClassName('expand_on_click')||!this.input.getAttribute('data-exp-width'))return;if(!this.expwidth){this.expwidth=+(this.input.getAttribute('data-exp-width'));}
var width;if(this.isEmpty()&&!this.paren.hasClassName('focused')){width=this.original_width;this.paren.removeClassName('click_expanded');}else if(this.paren.hasClassName('focused')){width=this.expwidth;this.paren.addClassName('click_expanded');}
if(width>this.expwidth){width=this.expwidth;}
if(width<this.original_width){width=this.original_width;}
if(width)this.resize(width);},fixInput:function(){var width=this.label.getWidth();if(width==0){setTimeout(this.fixInput.bind(this),1000);}
width-=parseInt(this.label.getStyle('paddingLeft'),10);width-=parseInt(this.label.getStyle('paddingRight'),10);this.input.style.width=width+'px';},resize:function(width){if(!width){var width=this.original_width;}
if(this.paren.getWidth()==width&&this.label.getWidth()==this.input.getWidth())
return;this.width=width;this.resizeFrameByFrame();},resizeFrameByFrame:function(){clearTimeout(this.resize_timeout);if(!this.width){return;}
if(util.supportsTransitions()){this.paren.style.width=this.width+'px';setTimeout(this.fixInput.bind(this),125);return;}
var friction=0.45;var currentwidth=this.paren.getWidth();var delta=Math.abs(currentwidth-this.width);var framerate=60;if(delta<=1){this.paren.style.width=this.width+'px';this.fixInput();}else{delta=Math.ceil(delta*friction);if(this.width<currentwidth)delta*=-1;this.paren.style.width=(currentwidth+delta)+'px';this.resize_timeout=setTimeout(this.resizeFrameByFrame.bind(this),1000/framerate);}}});
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/md5.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/md5.js";
			
function get_md5_bit(in_str,which_bit)
{while(which_bit>127){in_str=hex_md5(in_str);which_bit-=128;}
var hex=hex_md5(in_str);var c_pos=hex.length-1-Math.floor(which_bit/4);var c=hex.charAt(c_pos);var base10=parseInt("0x"+c,16);var char_bit=which_bit%4;while(char_bit>0){base10=base10>>1;char_bit--;}
return base10%2;}
function get_experiment_group(uid_str,experiment_str)
{var hex=hex_md5(uid_str+experiment_str);var last_char=hex.charAt(hex.length-1);return parseInt("0x"+last_char,16);}
var hexcase=0;var b64pad="";var chrsz=8;function hex_md5(s){return binl2hex(core_md5(str2binl(s),s.length*chrsz));}
function b64_md5(s){return binl2b64(core_md5(str2binl(s),s.length*chrsz));}
function str_md5(s){return binl2str(core_md5(str2binl(s),s.length*chrsz));}
function hex_hmac_md5(key,data){return binl2hex(core_hmac_md5(key,data));}
function b64_hmac_md5(key,data){return binl2b64(core_hmac_md5(key,data));}
function str_hmac_md5(key,data){return binl2str(core_hmac_md5(key,data));}
function md5_vm_test()
{return hex_md5("abc")=="900150983cd24fb0d6963f7d28e17f72";}
function core_md5(x,len)
{x[len>>5]|=0x80<<((len)%32);x[(((len+64)>>>9)<<4)+14]=len;var a=1732584193;var b=-271733879;var c=-1732584194;var d=271733878;for(var i=0;i<x.length;i+=16)
{var olda=a;var oldb=b;var oldc=c;var oldd=d;a=md5_ff(a,b,c,d,x[i+0],7,-680876936);d=md5_ff(d,a,b,c,x[i+1],12,-389564586);c=md5_ff(c,d,a,b,x[i+2],17,606105819);b=md5_ff(b,c,d,a,x[i+3],22,-1044525330);a=md5_ff(a,b,c,d,x[i+4],7,-176418897);d=md5_ff(d,a,b,c,x[i+5],12,1200080426);c=md5_ff(c,d,a,b,x[i+6],17,-1473231341);b=md5_ff(b,c,d,a,x[i+7],22,-45705983);a=md5_ff(a,b,c,d,x[i+8],7,1770035416);d=md5_ff(d,a,b,c,x[i+9],12,-1958414417);c=md5_ff(c,d,a,b,x[i+10],17,-42063);b=md5_ff(b,c,d,a,x[i+11],22,-1990404162);a=md5_ff(a,b,c,d,x[i+12],7,1804603682);d=md5_ff(d,a,b,c,x[i+13],12,-40341101);c=md5_ff(c,d,a,b,x[i+14],17,-1502002290);b=md5_ff(b,c,d,a,x[i+15],22,1236535329);a=md5_gg(a,b,c,d,x[i+1],5,-165796510);d=md5_gg(d,a,b,c,x[i+6],9,-1069501632);c=md5_gg(c,d,a,b,x[i+11],14,643717713);b=md5_gg(b,c,d,a,x[i+0],20,-373897302);a=md5_gg(a,b,c,d,x[i+5],5,-701558691);d=md5_gg(d,a,b,c,x[i+10],9,38016083);c=md5_gg(c,d,a,b,x[i+15],14,-660478335);b=md5_gg(b,c,d,a,x[i+4],20,-405537848);a=md5_gg(a,b,c,d,x[i+9],5,568446438);d=md5_gg(d,a,b,c,x[i+14],9,-1019803690);c=md5_gg(c,d,a,b,x[i+3],14,-187363961);b=md5_gg(b,c,d,a,x[i+8],20,1163531501);a=md5_gg(a,b,c,d,x[i+13],5,-1444681467);d=md5_gg(d,a,b,c,x[i+2],9,-51403784);c=md5_gg(c,d,a,b,x[i+7],14,1735328473);b=md5_gg(b,c,d,a,x[i+12],20,-1926607734);a=md5_hh(a,b,c,d,x[i+5],4,-378558);d=md5_hh(d,a,b,c,x[i+8],11,-2022574463);c=md5_hh(c,d,a,b,x[i+11],16,1839030562);b=md5_hh(b,c,d,a,x[i+14],23,-35309556);a=md5_hh(a,b,c,d,x[i+1],4,-1530992060);d=md5_hh(d,a,b,c,x[i+4],11,1272893353);c=md5_hh(c,d,a,b,x[i+7],16,-155497632);b=md5_hh(b,c,d,a,x[i+10],23,-1094730640);a=md5_hh(a,b,c,d,x[i+13],4,681279174);d=md5_hh(d,a,b,c,x[i+0],11,-358537222);c=md5_hh(c,d,a,b,x[i+3],16,-722521979);b=md5_hh(b,c,d,a,x[i+6],23,76029189);a=md5_hh(a,b,c,d,x[i+9],4,-640364487);d=md5_hh(d,a,b,c,x[i+12],11,-421815835);c=md5_hh(c,d,a,b,x[i+15],16,530742520);b=md5_hh(b,c,d,a,x[i+2],23,-995338651);a=md5_ii(a,b,c,d,x[i+0],6,-198630844);d=md5_ii(d,a,b,c,x[i+7],10,1126891415);c=md5_ii(c,d,a,b,x[i+14],15,-1416354905);b=md5_ii(b,c,d,a,x[i+5],21,-57434055);a=md5_ii(a,b,c,d,x[i+12],6,1700485571);d=md5_ii(d,a,b,c,x[i+3],10,-1894986606);c=md5_ii(c,d,a,b,x[i+10],15,-1051523);b=md5_ii(b,c,d,a,x[i+1],21,-2054922799);a=md5_ii(a,b,c,d,x[i+8],6,1873313359);d=md5_ii(d,a,b,c,x[i+15],10,-30611744);c=md5_ii(c,d,a,b,x[i+6],15,-1560198380);b=md5_ii(b,c,d,a,x[i+13],21,1309151649);a=md5_ii(a,b,c,d,x[i+4],6,-145523070);d=md5_ii(d,a,b,c,x[i+11],10,-1120210379);c=md5_ii(c,d,a,b,x[i+2],15,718787259);b=md5_ii(b,c,d,a,x[i+9],21,-343485551);a=safe_add(a,olda);b=safe_add(b,oldb);c=safe_add(c,oldc);d=safe_add(d,oldd);}
return Array(a,b,c,d);}
function md5_cmn(q,a,b,x,s,t)
{return safe_add(bit_rol(safe_add(safe_add(a,q),safe_add(x,t)),s),b);}
function md5_ff(a,b,c,d,x,s,t)
{return md5_cmn((b&c)|((~b)&d),a,b,x,s,t);}
function md5_gg(a,b,c,d,x,s,t)
{return md5_cmn((b&d)|(c&(~d)),a,b,x,s,t);}
function md5_hh(a,b,c,d,x,s,t)
{return md5_cmn(b^c^d,a,b,x,s,t);}
function md5_ii(a,b,c,d,x,s,t)
{return md5_cmn(c^(b|(~d)),a,b,x,s,t);}
function core_hmac_md5(key,data)
{var bkey=str2binl(key);if(bkey.length>16)bkey=core_md5(bkey,key.length*chrsz);var ipad=Array(16),opad=Array(16);for(var i=0;i<16;i++)
{ipad[i]=bkey[i]^0x36363636;opad[i]=bkey[i]^0x5C5C5C5C;}
var hash=core_md5(ipad.concat(str2binl(data)),512+data.length*chrsz);return core_md5(opad.concat(hash),512+128);}
function safe_add(x,y)
{var lsw=(x&0xFFFF)+(y&0xFFFF);var msw=(x>>16)+(y>>16)+(lsw>>16);return(msw<<16)|(lsw&0xFFFF);}
function bit_rol(num,cnt)
{return(num<<cnt)|(num>>>(32-cnt));}
function str2binl(str)
{var bin=Array();var mask=(1<<chrsz)-1;for(var i=0;i<str.length*chrsz;i+=chrsz)
bin[i>>5]|=(str.charCodeAt(i/chrsz)&mask)<<(i%32);return bin;}
function binl2str(bin)
{var str="";var mask=(1<<chrsz)-1;for(var i=0;i<bin.length*32;i+=chrsz)
str+=String.fromCharCode((bin[i>>5]>>>(i%32))&mask);return str;}
function binl2hex(binarray)
{var hex_tab=hexcase?"0123456789ABCDEF":"0123456789abcdef";var str="";for(var i=0;i<binarray.length*4;i++)
{str+=hex_tab.charAt((binarray[i>>2]>>((i%4)*8+4))&0xF)+
hex_tab.charAt((binarray[i>>2]>>((i%4)*8))&0xF);}
return str;}
function binl2b64(binarray)
{var tab="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";var str="";for(var i=0;i<binarray.length*4;i+=3)
{var triplet=(((binarray[i>>2]>>8*(i%4))&0xFF)<<16)|(((binarray[i+1>>2]>>8*((i+1)%4))&0xFF)<<8)|((binarray[i+2>>2]>>8*((i+2)%4))&0xFF);for(var j=0;j<4;j++)
{if(i*8+j*6>binarray.length*32)str+=b64pad;else str+=tab.charAt((triplet>>6*(3-j))&0x3F);}}
return str;}
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/OkLocation.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/OkLocation.js";
			
if(typeof Prototype=='undefined'||Prototype.Version<"1.6"){throw("OkLocation requires Prototype 1.6 or higher.");}
var OkLocation=Class.create({initialize:function(params){this.AjaxPath="/locquery";this.Status="pending";this.FormType="text";this.TempId="loc_"+Math.round(Math.random()*1000000000);this.Params=params;this.LocId="";this.PreviousQueryString="";this.CurrentQueryString="";this.PreprocessedQueryString="";this.PossibleMatches=new Array();this.AjaxResult=new Object;this.OldQueryString="";this.startExperience();},reset:function(){this.Status="pending";if($("find_btn"))$("find_btn").style.display="inline";this.LocId="";this.FormType="text";$(this.Params.loc_id_input_name).value="";$(this.Params.query_input_name).value="";this.CurrentQueryString="";this.PreprocessedQueryString="";this.PossibleMatches=new Array();this.AjaxResult=new Object;if(this.Params.new_signup==true){this.drawNewInputs();}else{this.drawInputs();}
if(this.Params.cb_reset){this.Params.cb_reset(this);}
$("lquery-notice").innerHTML="";$("zip_or_city").removeClassName("input_error");if(Signup){Signup.submitButtonToggle('on');}
$('lquery-notice').removeClassName('multi_ball');},getStatus:function(){return this.Status;},getTypedQuery:function(){if($('text_'+this.TempId)){return $F('text_'+this.TempId);}},updatePreprocessedQueryString:function(){if(this.Params.query_preprocess){this.PreprocessedQueryString=this.Params.query_preprocess(this.getTypedQuery());}else{this.PreprocessedQueryString=this.getTypedQuery();}},submitFormIfLocationReady:function(form_id,failure_cb,loc_mandatory){if(this.Status=="success"||(!loc_mandatory&&this.getTypedQuery()=="")){$(form_id).submit();}
else if(loc_mandatory&&this.getTypedQuery()==""){alert('You must enter a location!');}
else{failure_cb(this);}},startExperience:function(){if(this.Params.previous_loc_id&&this.Params.previous_loc_id!=0&&this.Params.previous_loc_id!=""){this.Status="success";this.LocId=this.Params.previous_loc_id;}
if(this.Params.previous_query_string){this.PreviousQueryString=this.Params.previous_query_string;this.CurrentQueryString=this.PreviousQueryString;}
this.drawInputs();},drawInputs:function(){var res="";if(this.FormType=="select"){res+='<select id="sel_'+this.TempId+'">';res+='<option value=""> - choose one - </option>';res+='<option value="reset"> - start over - </option>';for(var i=0;i<this.PossibleMatches.length;i++){res+='<option value="'+this.PossibleMatches[i].locid+'">'+this.PossibleMatches[i].text+'</option>';}
res+='</select>';this.Status="pulldown";}
else{if(this.Params.replaceEnter==true)
res+='<input class="location_input" onkeydown="if(event.keyCode==13 || event.which==13) {'+this.Params.newEnter+';return false;}" type="text" id="text_'+this.TempId+'" name="text_'+this.TempId+'" value="'+this.CurrentQueryString+'"/>';else
res+='<input class="location_input plain" type="text" id="text_'+this.TempId+'" name="text_'+this.TempId+'" value="'+this.CurrentQueryString+'"/>';}
var input_type="hidden";var id_label="";var query_label="";if(this.Params.debug){input_type="text";id_label="LocID hidden input ('"+this.Params.loc_id_input_name+"'): ";query_label="Query hidden input ('"+this.Params.query_input_name+"'): ";}
res+=id_label+'<input type="'+input_type+'" name="'+this.Params.loc_id_input_name
+'" id="'+this.Params.loc_id_input_name
+'" name="'+this.Params.loc_id_input_name
+'" value="'+this.LocId+'" />';res+=query_label+'<input type="'+input_type+'" name="'+this.Params.query_input_name
+'" id="'+this.Params.query_input_name
+'" name="'+this.Params.query_input_name
+'" value="'+this.QueryString+'" />';$(this.Params.dest_div).innerHTML=res;if(this.FormType=="select"){Event.observe('sel_'+this.TempId,'change',this.selectChange.bindAsEventListener(this));}
else{Event.observe('text_'+this.TempId,'keyup',this.textChange.bindAsEventListener(this));Event.observe('text_'+this.TempId,'blur',this.performLookup.bindAsEventListener(this));}},drawNewInputs:function(){var res="";if(this.FormType=="select"){if(Signup)
Signup.submitButtonToggle('off');res+="<span class='title'>Multiple matches found.</span>";res+="<span class='sub-title'>Pick one:</span>";res+="<ul id='cities'>"
var inputValue=$("zip_or_city").value;var total=0;var matches=0;var locid;var query;var matchesText;for(var i=0;i<this.PossibleMatches.length;i++){if(inputValue==this.PossibleMatches[i].text){if(matches==0){locid=this.PossibleMatches[i].locid;query=this.AjaxResult.query;matchesText=this.PossibleMatches[i].text;res+='<li class="city-selections"><a href="#" onclick="citySelected('+this.PossibleMatches[i].locid+', \''+this.AjaxResult.query+'\', \''+this.PossibleMatches[i].text+'\'); if(didBlur != true || !didBlur){didBlur = false;} return false; ">'+this.PossibleMatches[i].text+'</a></li>';}
matches++;total++;}else{total++;res+='<li class="city-selections"><a href="#" onclick="citySelected('+this.PossibleMatches[i].locid+', \''+this.AjaxResult.query+'\', \''+this.PossibleMatches[i].text+'\'); if(didBlur != true || !didBlur){didBlur = false;} return false; ">'+this.PossibleMatches[i].text+'</a></li>';}}
res+="</ul>";}
else{if(this.Params.replaceEnter==true)
res+='<input class="location_input" onkeydown="if(event.keyCode==13 || event.which==13) {'+this.Params.newEnter+';return false;}" type="text" id="text_'+this.TempId+'" name="text_'+this.TempId+'" value="'+this.CurrentQueryString+'"/>';else
res+='<input class="location_input" type="text" id="text_'+this.TempId+'" name="text_'+this.TempId+'" value="'+this.CurrentQueryString+'"/>';}
var input_type="hidden";var id_label="";var query_label="";if(this.Params.debug){input_type="text";id_label="LocID hidden input ('"+this.Params.loc_id_input_name+"'): ";query_label="Query hidden input ('"+this.Params.query_input_name+"'): ";}
if(this.FormType=="select"){cityInputs="";cityInputs+='<input class="location_input" type="text" id="text_'+this.TempId+'" name="text_'+this.TempId+'" value="'+this.CurrentQueryString+'"/>';cityInputs+=id_label+'<input type="'+input_type+'" name="'+this.Params.loc_id_input_name
+'" id="'+this.Params.loc_id_input_name
+'" name="'+this.Params.loc_id_input_name
+'" value="'+this.LocId+'" />';cityInputs+=query_label+'<input type="'+input_type+'" name="'+this.Params.query_input_name
+'" id="'+this.Params.query_input_name
+'" name="'+this.Params.query_input_name
+'" value="'+this.LocId+'" />';if($("zip_or_city").value!=""){if(total!=matches){$('city-input').innerHTML=cityInputs;$('lquery-notice').addClassName('multi_ball');$('lquery-notice').innerHTML="<div class='validation_error multi_ball'><span class='tip'></span>"+res+"</div>";apply_zip_error_border();Event.observe('sel_'+this.TempId,'change',this.selectChange.bindAsEventListener(this));}else{citySelected(locid,query,matchesText,true);}}}
else{res+=id_label+'<input type="'+input_type+'" name="'+this.Params.loc_id_input_name
+'" id="'+this.Params.loc_id_input_name
+'" name="'+this.Params.loc_id_input_name
+'" value="'+this.LocId+'" />';res+=query_label+'<input type="'+input_type+'" name="'+this.Params.query_input_name
+'" id="'+this.Params.query_input_name
+'" name="'+this.Params.query_input_name
+'" value="'+this.QueryString+'" />';$(this.Params.dest_div).innerHTML=res;Event.observe('text_'+this.TempId,'keyup',this.textChange.bindAsEventListener(this));Event.observe('text_'+this.TempId,'blur',this.performLookup.bindAsEventListener(this));}},textChange:function(){if($F(this.Params.query_input_name)!=this.getTypedQuery()){this.Status="pending";this.LocId="";$(this.Params.loc_id_input_name).value="";if(this.Params.cb_lost_success){this.Params.cb_lost_success(this);}
$(this.Params.query_input_name).value=this.getTypedQuery();this.CurrentQueryString=this.getTypedQuery();this.updatePreprocessedQueryString();}},selectChange:function(){var id=$F("sel_"+this.TempId);if(id=="reset"||id==""){this.reset();}
else{this.Status="success";this.LocId=id;$(this.Params.loc_id_input_name).value=this.LocId;$(this.Params.query_input_name).value=this.AjaxResult.query;if(this.Params.cb_success){this.Params.cb_success(this);}}
$(this.Params.loc_id_input_name).value=this.LocId;},performLookup:function(){this.updatePreprocessedQueryString();var req=new Ajax.Request(this.AjaxPath,{method:"get",parameters:{func:"query",query:this.PreprocessedQueryString,cbust:Math.round(Math.random()*1000000000)},onSuccess:this.performLookup_cb.bindAsEventListener(this),onFailure:function(){alert("Location lookup failed!");}});},performLookup_cb:function(data){this.AjaxResult=data.responseText.evalJSON();if(this.AjaxResult.query==""&&this.AjaxResult.locid==""){if($("lquery-notice").hasClassName('multi_ball')==false||this.OldQueryString!=$F("zip_or_city")){var temp=$F("zip_or_city");this.reset();$("zip_or_city").value=temp;checkLocation(1);}}
if($("zip_or_city")){this.OldQueryString=$F("zip_or_city");}else{this.OldQueryString="";};if(this.getTypedQuery()&&this.PreprocessedQueryString==this.AjaxResult.query){if(this.AjaxResult.locid&&this.AjaxResult.locid!=0&&this.AjaxResult.locid!=""){this.handleSuccess();}
else{this.handleError();}}},handleError:function(){this.LocId="";if(this.AjaxResult.results.length==0){$("lquery-notice").innerHTML="";if(typeof Signup!='undefined'){$("zip_or_city").removeClassName("input_error");Signup.submitButtonToggle('on');}}
else if(this.AjaxResult.results.length>0){this.FormType="select";this.PossibleMatches=this.AjaxResult.results;if(this.Params.new_signup==true){this.drawNewInputs();}
else{this.drawInputs();}}
if(this.Params.cb_error){this.Params.cb_error(this);}},handleSuccess:function(){this.Status="success";this.LocId=this.AjaxResult.locid;$(this.Params.loc_id_input_name).value=this.LocId;if(this.Params.cb_success){this.Params.cb_success(this);}}});
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/OkLoginJoin.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/OkLoginJoin.js";
			
function killEnterAll(e){var Ucode=e.keyCode?e.keyCode:e.charCode;if(Ucode==13){}
e=(e)?e:((window.event)?window.event:"");if(e){return!(e.keyCode==13||e.which==13);}}
var OKLJ=Class.create({inititalize:function(){},login:function(screenname,password,params){var ajax_params={"ajax":1,"username":screenname,"password":password,enable:params.enable?1:0};var req=new Ajax.Request("/login",{method:'post',parameters:ajax_params,onSuccess:this.login_cb.bindAsEventListener(this,params),onFailure:this.login_cb_failed.bindAsEventListener(this,params)});return req;},login_cb:function(data,params){var response=data.responseText.evalJSON();this.tempHoldings={'data':data,'params':params};if(response.hq_okc_authlink&&response.hq_okc_authlink!=""){$(document.body).insert({bottom:'<iframe style="display:none;" onload="OkLoginJoin.login_cb_post_authlink()" src="'+response.hq_okc_authlink+'" id="auth-ajax-login"></iframe>'});setTimeout("$('auth-ajax-login').writeAttribute('src', '"+response.hq_okc_authlink+"')",100);setTimeout("OkLoginJoin.login_cb_post_authlink()",1000);}
else{this.login_cb_post_authlink();}},login_cb_post_authlink:function(){var data=this.tempHoldings.data;var params=this.tempHoldings.params;var response=data.responseText.evalJSON();if(response.status=="success"){if(params.success_cb){params.success_cb(response);}
if(params.success_redirect){document.location.href=params.success_redirect;}}
else{if(params.failure_cb){params.failure_cb(response);}
if(params.failure_redirect){document.location.href=params.failure_redirect;}}},login_cb_failed:function(data,params){alert("AJAX login Failed!");},logout:function(params){var ajax_params={"ajax":1};var req=new Ajax.Request("/logout",{method:'post',parameters:ajax_params,onSuccess:this.logout_cb.bindAsEventListener(this,params),onFailure:this.logout_cb_failed.bindAsEventListener(this,params)});return req;},logout_cb:function(data,params){var response=data.responseText.evalJSON();if(response.status=="success"){if(params.success_cb){params.success_cb(response);}
if(params.success_redirect){document.location.href=params.success_redirect;}
Element.insert(document.body,{bottom:'<iframe style="display:none;" src="http://www.okcupid.com/logout" id="auth-ajax-okc-logout"></iframe>'});}
else{alert("Logout somehow failed");}},logout_cb_failed:function(data,params){alert("AJAX logout Failed!");},lostPassword:function(screenname_or_email,params){var ajax_params={"ajax":1,"email":screenname_or_email};var req=new Ajax.Request("/lostpassword",{method:'post',parameters:ajax_params,onSuccess:this.lostPassword_cb.bindAsEventListener(this,params),onFailure:this.lostPassword_cb_failed.bindAsEventListener(this,params)});return req;},lostPassword_cb:function(data,params){var response=data.responseText.evalJSON();if(response.status=="SUCCESS"){if(params.success_cb){params.success_cb(response);}
if(params.success_redirect){document.location.href=params.success_redirect;}}
else{if(params.failure_cb){params.failure_cb(response);}
if(params.failure_redirect){document.location.href=params.failure_redirect;}}},lostPassword_cb_failed:function(data,params){alert("AJAX lostPassword Failed!");},optionalParams:{},join:function(dest_div,signup_path,cgi_extras,params){this.optionalJoinParams=params;var default_params={"ajax":1,"reqs":0,"cbust":Math.random()};var ajax_params=this.combineTwoObjs(default_params,cgi_extras);ajax_params["next_page"]=signup_path;var req=new Ajax.Updater(dest_div,"/signup",{method:'post',evalScripts:true,parameters:ajax_params});},join_cb:function(data){this.tempHoldings={'data':data};if(data.hq_okc_authlink&&data.hq_okc_authlink!=""){Element.insert(document.body,{bottom:'<iframe style="display:none;" onload="OkLoginJoin.join_cb_post_authlink()" src="'+data.hq_okc_authlink+'" id="auth-ajax-join"></iframe>'});setTimeout("OkLoginJoin.join_cb_post_authlink()",1000);}
else{this.join_cb_post_authlink()}},join_cb_post_authlink:function(){var data=this.tempHoldings.data;if(this.optionalJoinParams.success_cb){this.optionalJoinParams.success_cb(data);}
if(this.optionalJoinParams.success_redirect){document.location.href=this.optionalJoinParams.success_redirect;}},combineTwoObjs:function(o1,o2){var res=new Object();for(var key in o1){res[key]=o1[key];}
for(var key in o2){res[key]=o2[key];}
return res;}});var OkLoginJoin=new OKLJ();
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/Oryx/AjaxSookies.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Oryx/AjaxSookies.js";
			
var AjaxSookie={url:'/sookie/remote_sookie.html',clear:function(n,cb){new Ajax.Request(this.url,{method:'get',parameters:{name:n,'delete':1,rand:Math.random()},onSuccess:function(transport){if(cb)cb(transport);}});},remove:function(n,cb){this.clear(n,cb);},set:function(n,v,lifetime,name_hash,cb){this.update(n,v,lifetime,name_hash,false,false,cb);},update:function(n,v,lifetime,name_hash,list_append,dict_merge,cb){try{validate=Object.toJSON(v);validate.evalJSON();}
catch(e){alert("Not a valid JSON object!");return;}
params={name:n,name_hash:name_hash,lifetime:lifetime,value:Object.toJSON(v),rand:Math.random()}
if(list_append){params.list_append=1;}
if(dict_merge){params.dict_merge=1;}
new Ajax.Request(this.url,{method:'get',parameters:params,onSuccess:function(transport){var resp=transport.responseText.evalJSON();if(resp.status==119){alert("Failed to update sookie. name_hash didn't match.");}
else if(cb){cb(transport);}}});}}
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/Oryx/DateTime.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Oryx/DateTime.js";
			
if(document.CurrentGMT){}
else{CurrentGMT=new Date();}
var MonthArray=['January','February','March','April','May','June','July','August','September','October','November','December'];var dateFormat=function(){var token=/d{1,4}|m{1,4}|yy(?:yy)?|([HhMsTt])\1?|[LloSZ]|"[^"]*"|'[^']*'/g,timezone=/\b(?:[PMCEA][SDP]T|(?:Pacific|Mountain|Central|Eastern|Atlantic) (?:Standard|Daylight|Prevailing) Time|(?:GMT|UTC)(?:[-+]\d{4})?)\b/g,timezoneClip=/[^-+\dA-Z]/g,pad=function(val,len){val=String(val);len=len||2;while(val.length<len)val="0"+val;return val;};return function(date,mask,utc){var dF=dateFormat;if(arguments.length==1&&Object.prototype.toString.call(date)=="[object String]"&&!/\d/.test(date)){mask=date;date=undefined;}
date=date?new Date(date):new Date;if(isNaN(date))throw SyntaxError("invalid date");mask=String(dF.masks[mask]||mask||dF.masks["default"]);if(mask.slice(0,4)=="UTC:"){mask=mask.slice(4);utc=true;}
var _=utc?"getUTC":"get",d=date[_+"Date"](),D=date[_+"Day"](),m=date[_+"Month"](),y=date[_+"FullYear"](),H=date[_+"Hours"](),M=date[_+"Minutes"](),s=date[_+"Seconds"](),L=date[_+"Milliseconds"](),o=utc?0:date.getTimezoneOffset(),flags={d:d,dd:pad(d),ddd:dF.i18n.dayNames[D],dddd:dF.i18n.dayNames[D+7],m:m+1,mm:pad(m+1),mmm:dF.i18n.monthNames[m],mmmm:dF.i18n.monthNames[m+12],yy:String(y).slice(2),yyyy:y,h:H%12||12,hh:pad(H%12||12),H:H,HH:pad(H),M:M,MM:pad(M),s:s,ss:pad(s),l:pad(L,3),L:pad(L>99?Math.round(L/10):L),t:H<12?"a":"p",tt:H<12?"am":"pm",T:H<12?"A":"P",TT:H<12?"AM":"PM",Z:utc?"UTC":(String(date).match(timezone)||[""]).pop().replace(timezoneClip,""),o:(o>0?"-":"+")+pad(Math.floor(Math.abs(o)/60)*100+Math.abs(o)%60,4),S:["th","st","nd","rd"][d%10>3?0:(d%100-d%10!=10)*d%10]};return mask.replace(token,function($0){return $0 in flags?flags[$0]:$0.slice(1,$0.length-1);});};}();dateFormat.masks={"default":"ddd mmm dd yyyy HH:MM:ss",shortDate:"m/d/yy",mediumDate:"mmm d, yyyy",longDate:"mmmm d, yyyy",fullDate:"dddd, mmmm d, yyyy",shortTime:"h:MM TT",mediumTime:"h:MM:ss TT",longTime:"h:MM:ss TT Z",isoDate:"yyyy-mm-dd",isoTime:"HH:MM:ss",isoDateTime:"yyyy-mm-dd'T'HH:MM:ss",isoUtcDateTime:"UTC:yyyy-mm-dd'T'HH:MM:ss'Z'"};dateFormat.i18n={dayNames:["Sun","Mon","Tue","Wed","Thu","Fri","Sat","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"],monthNames:["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","January","February","March","April","May","June","July","August","September","October","November","December"]};Date.prototype.format=function(mask,utc){return dateFormat(this,mask,utc);};var MASKS={JUSTNOW:'<em>Just now!</em>',DATE_YEAR:'mmm d, yyyy',DATE:'mmm d',FULLDATE:'mmmm d',FULLDATE_YEAR:'mmmm d, yyyy',HOUR:'h:MMtt',HOUR_IM:'h:MM:sstt'};var MAIL_FORMAT=0,PROFILE_FORMAT=1,TEST_FORMAT=2,JOURNAL_FORMAT=3,SIMPLE_DATE=4,HQ_CONTEST_FORMAT=5,HQ_BROWSE_MONTH=6,HQ_BROWSE_FORMAT=7,IM_FORMAT=8,IM_CONVERSATION_FORMAT=9,MESSAGE_FORMAT=10,IM_PERIODIC_FORMAT=11;var LEGACY_TIME_FORMATS=['MAIL_FORMAT','PROFILE_FORMAT','TEST_FORMAT','JOURNAL_FORMAT','SIMPLE_DATE','HQ_CONTEST_FORMAT','HQ_BROWSE_MONTH','HQ_BROWSE_FORMAT','IM_FORMAT','IM_CONVERSATION_FORMAT','MESSAGE_FORMAT','IM_PERIODIC_FORMAT'];function tzo(){var d=new Date();return d.getTimezoneOffset()*60*1000;}
function md(y,m,d,h,min,sec){var when=new Date(new Date(y,m,d,h,min,sec).getTime()-tzo());document.write(makeSmartDateString(when,MAIL_FORMAT));}
function TextMonth(intMonth,write_only){var monthstr=MonthArray[intMonth];if(write_only)document.write(monthstr);else return monthstr;}
function consolidateTimeFormat(format){if(format=='HQ_CONTEST_FORMAT'||format=='HQ_BROWSE_FORMAT'){format='MESSAGE_FORMAT';}
if(format=='TEST_FORMAT'){format='PROFILE_FORMAT';}
if(format=='BILLING_FORMAT'){format='MAIL_FORMAT';}
return format;}
function getLastMidnight(){var lastmidnight=new Date();lastmidnight=lastmidnight-1000*60*60*lastmidnight.getHours()
-1000*60*lastmidnight.getMinutes()
-1000*lastmidnight.getSeconds()
-lastmidnight.getMilliseconds();return lastmidnight;}
function makeSmartDateString(d,format){if(typeof format=='number'){format=LEGACY_TIME_FORMATS[format];if(!format)format=FancyDate.defaultformat;}
var their_now=new Date();var our_now=new Date();var their_time_error;var min_ago,lastmidnight,lastlastmidnight,isthisyear,str='';if(CurrentGMT){our_now=CurrentGMT;their_time_error=(their_now-CurrentGMT)/1000;}
else{our_now=their_now;their_time_error=0;}
min_ago=Math.round((our_now-d)/60000);lastmidnight=getLastMidnight();lastlastmidnight=lastmidnight-1000*60*60*24;isthisyear=our_now.getYear()==d.getYear();format=consolidateTimeFormat(format);switch(format){case'SIMPLE_DATE':if(their_now.getYear()!=d.getYear()){str=dateFormat(d,MASKS.DATE_YEAR)}
else{str=dateFormat(d,MASKS.DATE);}
break;case'MAIL_FORMAT':str=dateFormat(d,MASKS.DATE_YEAR);break;case'MESSAGE_FORMAT':str=dateFormat(d,MASKS.DATE_YEAR+' – '+MASKS.HOUR);break;case'JOURNAL_FORMAT':if(min_ago<2){str=MASKS.JUSTNOW;}
else if(min_ago<60){str=min_ago+"&nbsp;minutes ago";}
else if(d>lastmidnight){str="Today – "+dateFormat(d,MASKS.HOUR);}
else if(d>lastlastmidnight){str="Yesterday – "+dateFormat(d,MASKS.HOUR);}
else if(!isthisyear){str=dateFormat(d,MASKS.FULLDATE_YEAR);}
else{str=dateFormat(d,MASKS.FULLDATE);}
break;case'PROFILE_FORMAT':if(min_ago<2){str=MASKS.JUSTNOW;}
else if(d>lastmidnight){str="Today";}
else if(d>lastlastmidnight){str="Yesterday";}
else if(!isthisyear){str=dateFormat(d,MASKS.FULLDATE_YEAR);}
else{str=dateFormat(d,MASKS.FULLDATE);}
break;case'IM_FORMAT':str=dateFormat(d,MASKS.HOUR_IM);break;case'IM_PERIODIC_FORMAT':if(d>lastmidnight){str=dateFormat(d,MASKS.HOUR);}
else if(d>lastlastmidnight){str="Yesterday – "+dateFormat(d,MASKS.HOUR);}
else if(!isthisyear){str=dateFormat(d,MASKS.DATE_YEAR+' – '+MASKS.HOUR);}else{str=dateFormat(d,MASKS.DATE+' – '+MASKS.HOUR);}
break;case'SIMPLE_HOUR':str=dateFormat(d,MASKS.HOUR);break;case'IM_CONVERSATION_FORMAT':if(min_ago<2){str=MASKS.JUSTNOW;}
else if(min_ago<60){str=min_ago+"&nbsp;minutes ago";}
else if(d>lastmidnight){str="Today – "+dateFormat(d,MASKS.HOUR);}
else if(d>lastlastmidnight){str="Yesterday – "+dateFormat(d,MASKS.HOUR);}
else if(!isthisyear){str=dateFormat(d,MASKS.DATE_YEAR+' – '+MASKS.HOUR);}else{str=dateFormat(d,MASKS.DATE+' – '+MASKS.HOUR);}
break;case'DAY':if(d>lastmidnight){str="today";}
else if(d>lastlastmidnight){str="yesterday";}
else if(!isthisyear){str=dateFormat(d,MASKS.FULLDATE_YEAR);}
else{str=dateFormat(d,MASKS.FULLDATE);}
break;default:break;}
return str;}
var FancyDate={defaultformat:'JOURNAL_FORMAT',refreshtime:1000*60,list:[],maid:null,add:function(el,timestamp,format){var element=document.getElementById(el);if(!element){alert(el+' is not a valid FancyDate element!');return;}
if(!format||format==''){format=this.defaultformat;}
format=consolidateTimeFormat(format);if(!timestamp){var timestamp=new Date();}else{timestamp=new Date(timestamp*1000);}
var bundle={element:element,time:timestamp,format:format};this.list.push(bundle);this.update(bundle);this.monitor();},monitor:function(){if(this.maid){return;}
var _this=this;this.updateGMT();this.maid=setTimeout(function(){_this.updateAll();},this.refreshtime);},update:function(item){if(!item||!item.element||!item.time){return false;}
var str=makeSmartDateString(item.time,item.format);if(str!=item.element.innerHTML){item.element.innerHTML=str;}
return true;},updateGMT:function(){CurrentGMT=new Date(CurrentGMT.getTime()+this.refreshtime);},clear:function(){this.list=[];clearTimeout(this.maid);},updateAll:function(){if(!this.list.length){clearTimeout(this.maid);return;}
var i,length,_this=this;for(length=this.list.length,i=length-1;i>=0;i--){if(!this.update(this.list[i])){this.list.splice(i,1);}}
this.updateGMT();this.maid=setTimeout(function(){_this.updateAll();},this.refreshtime);}};
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/Oryx/HotKeys.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Oryx/HotKeys.js";
			
var HotKeys=Class.create({initialize:function(){this.ctrl=false;this.shift=false;this.opt=false;this.stack="";this.clamp=false;this.extant=false;this.hardclear=false;},translations:{_13:"ENTER",_96:"0",_97:"1",_98:"2",_99:"3",_100:"4",_101:"5",_102:"6",_103:"7",_104:"8",_105:"9",_107:"+",_61:"+",_187:"+",_109:"-",_189:"-"},disable:function(){this.clamp=true;},enable:function(hardclear){if(this.clamp){this.clamp=false;return;}
this.hardclear=(hardclear?true:false);if(this.extant)return;this.extant=true;var pass=this;document.observe("keyup",function(e){if(pass.clamp)return;if(e.keyCode==17){pass.ctrl=false;pass.stack="";}
else if(e.keyCode==16){pass.shift=false;pass.stack="";}
else if(e.keyCode==18){pass.opt=false;pass.stack="";}
else if(pass.hardclear){pass.stack="";}
pass.state();return;});document.observe("keydown",function(e){if(pass.clamp)return;if(e.keyCode==17)pass.ctrl=true;else if(e.keyCode==16)pass.shift=true;else if(e.keyCode==18)pass.opt=true;else{if(pass.translations["_"+e.keyCode]){pass.stack+=pass.translations["_"+e.keyCode];}else{pass.stack+=String.fromCharCode(e.keyCode).toUpperCase();}}
pass.state();return;});document.observe("mousedown",function(){pass.ctrl=false;pass.state();});},state:function(){if(this.ctrl)ctr_text="CTRL+";else ctr_text="";if(this.shift)sft_text="SHIFT+";else sft_text="";if(this.opt)opt_text="OPT+";else opt_text="";if(this.stack)act_text=this.stack;else act_text="";pressed=ctr_text+sft_text+opt_text+act_text;if(this[pressed]){if(this[pressed].toggle){if(!this[pressed].toggleState)
{this[pressed].toggleState=true;this[pressed].toggle.on();}else{this[pressed].toggleState=false;this[pressed].toggle.off();}}else{this[pressed]();}}
this.stack='';}});var KeyCombos=new HotKeys();
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/Oryx/Template.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Oryx/Template.js";
			
var TemplateUtils={mobilepopup_hider:null,showMoreFaves:function(){if(!$('favorites_list'))return;var revealed=0,left=0,count=0,favorites=$('favorites_list').childNodes,show_text='';if($('favorites_showmore').down('span').innerHTML=='all'){count=4;}else{count=+($('favorites_showmore').down('span').innerHTML.replace(' more',''));}
for(var i=0;i<favorites.length;i++){if($(favorites[i]).hasClassName&&$(favorites[i]).hasClassName('hidden')){if(revealed<count){$(favorites[i]).removeClassName('hidden');revealed++;}else{left++;}}}
if(left==0){$('favorites_showmore').hide();}else if(left==count+1){$('favorites_showmore').down('span').update('all');}else{$('favorites_showmore').down('span').update(count+' more');}},checkSigninForm:function(){var un=$('sidebar_signin_username'),pw=$('sidebar_signin_password'),bt=$('sidebar_signin_button');if(!un||!pw||!bt)return;if(un.value!=''&&pw.value!=''){bt.addClassName('active');}
if(un.value==''||pw.value==''){bt.removeClassName('active');}},checkSigninFormService:function(){this.checkSigninForm();setTimeout(this.checkSigninFormService.bind(this),500);},checkSigninSubmit:function(){if($('sidebar_signin_username').value==''){$('sidebar_signin_username').focus();}else if($('sidebar_signin_password').value==''){$('sidebar_signin_password').focus();}else{$('sidebar_signin_form').submit();}},showForgot:function(){$('sidebar_signin_password').up('span.fancy_input').addClassName('forgot_password');},hideForgot:function(){$('sidebar_signin_password').up('span.fancy_input').removeClassName('forgot_password');}};var LostPassword={checker:null,show:function(){util.updateStats('signup - lost password - clicks',1,'counter','+Y6RsldxsrP/AUDxl4kN1WqE6g8=');this.clear();$('forgot_password_box').addClassName('visible');$('forgot_password_field').focus();},hide:function(){$('forgot_password_box').removeClassName('visible');$('forgot_password_field').value='';setTimeout(this.clear,200);},clear:function(){var is_visible=$('forgot_password_box').hasClassName('visible');var new_class=is_visible?'shadowbox visible':'shadowbox';$('forgot_password_box').className=new_class;$('forgot_password_submit').addClassName('green').removeClassName('gray');},validate:function(){if($('forgot_password_box').hasClassName('error'))return;$('forgot_password_field').value=$('forgot_password_field').value.replace(/\s/gi,'');var entry=$('forgot_password_field').value;var params={cf:'lostpassword_desktop',ajax:'1',email:entry};this.clear();if(this.checker)this.checker.abort();if(entry==''||entry.length<4){$('forgot_password_box').addClassName('error').addClassName('empty');$('forgot_password_submit').removeClassName('green').addClassName('gray');return;}
this.checker=new Ajax.Request('/lostpassword',{parameters:params,onSuccess:this.validate_cb.bindAsEventListener(this,entry)});},validate_cb:function(transport,entry){var response=transport.responseText.evalJSON();var isemail=(entry.indexOf('@')>-1||entry.indexOf('.')>-1)?true:false;if(response.status=='SUCCESS'){$('forgot_password_box').addClassName('success');}else if(response.status=='NOT_FOUND'){var newclass=isemail?'email':'username';$('forgot_password_box').addClassName('error').addClassName(newclass);$('forgot_password_submit').removeClassName('green').addClassName('gray');}else{$('forgot_password_box').addClassName('error').addClassName('general');$('forgot_password_submit').removeClassName('green').addClassName('gray');}}};var MenuReady=false;var GNS={updated_view_time:false,handleNoNotifications:function(){$('nav_notifications').addClassName('nonotifications');$('site_notifications').addClassName('nonotifications');},decrementMessage:function(){if(!$('total_notifications'))return;var messages=+($('total_notifications').getAttribute('data-messages')),total=+($('total_notifications').innerHTML),message_words='';messages--;total--;if(messages<0)messages=0;if(total<0)total=0;if($('site_notifications_4')){if(messages==0){$('site_notifications_4').hide();}
$('total_notifications').setAttribute('data-messages',messages);message_words=messages+' New Message'+(messages>1?'s':'');$('site_notifications_4').down('span.desc').innerHTML=message_words;}
if(total==0){this.handleNoNotifications();}
$('total_notifications').innerHTML=total;if($('total_notifications_dropdown'))
$('total_notifications_dropdown').innerHTML=total+(total>1?' Notifications':' Notification');this.correct();},close:function(){$('nav_notifications').removeClassName('attention').removeClassName('unread');setTimeout(function(){$('site_notifications').removeClassName('unread');},500);},update:function(){if(this.updated_view_time)return;new Ajax.Request('/gns/remote',{parameters:{update_view_time:1},onSuccess:this.update_cb.bindAsEventListener(this)});},update_cb:function(transport){var response=transport.responseText.evalJSON();if(response.status==0)
this.updated_view_time=true;},correct:function(){if(!$('total_notifications')||!$('site_notifications'))return;$('site_notifications').setAttribute('style','');var width,wholewidth,left=+($('site_notifications').getStyle('left').replace('px','')),total=$('total_notifications');wholewidth=total.getWidth();wholewidth-=+(total.getStyle('paddingLeft').replace('px',''));wholewidth-=+(total.getStyle('paddingRight').replace('px',''));width=Math.ceil(wholewidth/2);$('site_notifications').style.left=(width+left)+'px';}};var DropdownMenus={hovers:[],clickers:[],hide:function(){for(var i=0;i<this.clickers.length;i++){this.clickers[i].hide();}
for(var i=0;i<this.hovers.length;i++){this.hovers[i].hide();}},fixTouchLinks:function(element){if(!util.isTouch())return;if(!element.hasClassName('menu'))return;var links=element.select('a');for(var i=0;i<links.length;i++){links[i].observe('touchend',function(){window.location=this.getAttribute('href');});}}};var HoverDropdown=Class.create({timeout:null,trig:null,menu:null,initialize:function(trig,menu){if(!$(trig)||!$(menu)){console.log('ERROR: Invalid hover menu!');return false;}
this.trig=$(trig);this.menu=$(menu);DropdownMenus.hovers.push(this);DropdownMenus.fixTouchLinks(this.trig);DropdownMenus.fixTouchLinks(this.menu);},show:function(){DropdownMenus.hide();this.stifle();this.menu.addClassName('visible');this.trig.addClassName('visible');},stifle:function(){if(this.timeout){clearTimeout(this.timeout);}},dispel:function(){if(!this.menu.hasClassName('visible'))return;this.timeout=setTimeout(this.hide.bind(this),50);},hide:function(){this.menu.removeClassName('visible');this.trig.removeClassName('visible');}});var ClickDropdown=Class.create({initialize:function(trig,menu,params){if(!$(trig)||!$(menu)){console.log('ERROR: Invalid click menu!');return false;}
this.trig=$(trig);this.menu=$(menu);this.opening=false;this.hovertime=1500;this.hovertimeout=null;this_ready_count=0;this.boundclick=null;DropdownMenus.clickers.push(this);DropdownMenus.fixTouchLinks(this.trig);DropdownMenus.fixTouchLinks(this.menu);if(params){if(params.init_cb&&typeof params.init_cb=='function'){params.init_cb();}
if(params.show_cb&&typeof params.show_cb=='function'){this.show_cb=params.show_cb;}
if(params.hide_cb&&typeof params.hide_cb=='function'){this.hide_cb=params.hide_cb;}}},show:function(){this.cancelShowOnHover();if(this.menu.hasClassName('visible'))return;DropdownMenus.hide();this.menu.addClassName('visible');this.trig.addClassName('visible');this.observe();this.opening=true;setTimeout(function(){this.opening=false;}.bind(this),50);if(this.show_cb)this.show_cb();},hide:function(){this.cancelShowOnHover();if(this.opening||!this.menu.hasClassName('visible'))return;this.menu.removeClassName('visible');this.trig.removeClassName('visible');Event.stopObserving(document.body,'click',this.boundclick);if(this.hide_cb)this.hide_cb();},showOnHover:function(){this.hovertimeout=setTimeout(this.show.bind(this),this.hovertime);},cancelShowOnHover:function(){clearTimeout(this.hovertimeout);},click:function(event){var hide=true;if(hide)this.hide();},observe:function(){if(MenuReady){this.boundclick=this.click.bindAsEventListener(this);Event.observe(document.body,'click',this.boundclick);}else if(this.ready_count<5){this.ready_count++;setTimeout(this.observe.bind(this),500);}else{util.updateStats('menu item - could not observe clicks',1,'counter','U2+cE6ox9da66zi3O2sP3EOgRrw=');}}});
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/Oryx/Timezone.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/Oryx/Timezone.js";
			
var DetectTimezone={sookiename:'timezone_info',sookiehash:'c012ce7bb343ebee389e48f513824fa79d66c436',offset:0,initialize:function(sookie){if(sookie&&sookie.is_dst&&sookie.dst_offset){sookie.offset+=sookie.dst_offset;}
var offset=0-(new Date().getTimezoneOffset());if(!sookie||offset!=sookie.offset){this.update(offset);if(typeof util!='undefined'&&util.updateStats)
util.updateStats('timezone updated',1,'counter','X3U+IfBEBIpAECH/gapKGTmDE64=');}},update:function(offset){if(!offset)return;var now=new Date().getTime();var t=Math.floor(now/1000)+86400*365*5;var newsookie={offset:offset};if(typeof AjaxSookie!='undefined'&&AjaxSookie.set){AjaxSookie.set(this.sookiename,newsookie,t,this.sookiehash);}else if(typeof App!='undefined'&&App.setSookie){App.setSookie(this.sookiename,newsookie,t,this.sookiehash);}else if(typeof util!='undefined'&&util.setSookie){util.setSookie(this.sookiename,newsookie,t,this.sookiehash);}}}
			AUTOCORE_SELF_CHECK = null;
		
			// File : /okcontent/js/service/loginout.js
			AUTOCORE_SELF_CHECK = "/okcontent/js/service/loginout.js";
			
var Logout={slot_name:null,force:null,specialcampaign:false,adwidth:0,adheight:0,initialize:function(mobiletype,authid,slot_name,force,specialcampaign){this.slot_name=slot_name;this.force=force;this.specialcampaign=specialcampaign;if(!mobiletype)
this.fillAd(authid);this.resize();},fillAd:function(authid){var params={base:1,page:'Logout',format:'logout_req'};if(authid)
params.authid=authid;new Ajax.Request('/daisy?format=logout_req&base=1',{parameters:params,onSuccess:this.fillAd_cb.bindAsEventListener(this)});},fillAd_cb:function(transport){var response=transport.responseText.evalJSON();var type='box';var width=300;var height=250;var format='logout_req';var idname='logout_ad';var minwidth='';var src=null;if(response.networkstr!=4&&response.width&&response.height){type=response.networkstr;width=response.width;height=response.height;format=response.formatstr||format;}
minwidth=width;if(this.specialcampaign){}else{$('advert').update(new Element('iframe',{src:src||'/daisy?format='+format+'&force='+type+'&base=1&page='+this.slot_name,marginwidth:'0',marginheight:'0',frameborder:'0',scrolling:'no',id:idname}).setStyle({width:width+'px',height:height+'px'}));if(minwidth){$('ad').style.minWidth=minwidth+'px';if($('logout_container')&&$('sidebar'))
$('logout_container').style.minWidth=(minwidth+30+$('sidebar').getWidth())+'px';}
this.resize();this.adwidth=width;this.adheight=height;this.changeBodyType();if(type=='dfp'){$(document.body).addClassName('eighthundred');$(idname).setStyle({width:'800px',height:'600px'});}}},check:function(){setTimeout(this.check.bind(this),100);if(!$('logout_ad'))return;if(!$('logout_ad').contentWindow)return;if($('logout_ad').contentWindow.location.hash=='')return;var params=$('logout_ad').contentWindow.location.hash.substr(1).toQueryParams();var width=+(params.width);var height=+(params.height);if(this.adheight!=height||this.adwidth!=width){$('logout_ad').setStyle({width:width+'px',height:height+'px'});this.adheight=height;this.adwidth=width;this.changeBodyType();}},changeBodyType:function(){if(this.adwidth<=600){$(document.body).removeClassName('eighthundred');$(document.body).removeClassName('fullscreen');}
if(this.adwidth>600&&this.adwidth<=800){$(document.body).addClassName('eighthundred');$(document.body).removeClassName('fullscreen');}
if(this.adwidth>800){$(document.body).removeClassName('eighthundred');$(document.body).addClassName('fullscreen');}},resize:function(){if(!$(document.body).hasClassName('fullscreen'))return;var view=document.viewport.getDimensions(),left=$('left_bar').getDimensions(),width,height,ad_width,header_height,height_offset,min_width,min_height;header_height=$('header').getHeight();height_offset=+($('main_content').getStyle('borderTopWidth').replace('px',''));height_offset+=+($('main_content').getStyle('paddingBottom').replace('px',''));height=view.height-$('header').getHeight()-height_offset;width=view.width-left.width-+($('left_bar').getStyle('marginLeft').replace('px',''))-header_height;min_height=left.height+$('left_bar').cumulativeOffset().top;if($('ad').down('iframe')){min_width=$('ad').down('iframe').getWidth()+100;}else if($('goodbye')){min_width=$('goodbye').getWidth()+100;}else{min_width=300;}
height=(min_height>height)?min_height:height;width=(min_width>width)?min_width:width;$('main_content').style.minHeight=height+'px !important';$('main_content').style.width=width+'px';}};function cookieTestStuff(serverdate){if(!clientHasCookiesEnabled()){$("cookie_error").innerHTML="Cookies are not enabled in your browser. You must have cookies enabled to use OkCupid.";$("cookie_error").style.display="block";return false;}
if(secondsOff=clientTimeIncorrect(serverdate)){$("cookie_error").innerHTML="Your computer's clock seems to be off by "+formatOffTime(secondsOff)+"!<br />Please set your time and date correctly to prevent issues with OkCupid.";$("cookie_error").style.display="block";return false;}}
function clientTimeIncorrect(serverTime){var thresholdInHours=6;var clientTime=Math.round((new Date().getTime())/1000);var serverTime=parseInt(serverTime);var secondsOff=Math.abs(clientTime-serverTime);if(secondsOff>(thresholdInHours*3600)){return secondsOff;}else{return false;}}
function clientHasCookiesEnabled(){var cookieName="cookietest";setCookie(cookieName,true);var enabled=getCookie(cookieName);deleteCookie(cookieName);return enabled;}
function formatOffTime(secondsOff){var hoursOff=Math.round(secondsOff/3600);var returnString;if(hoursOff>24){var daysOff=Math.round(hoursOff/24);return daysOff+(daysOff==1?' day':' days');}else{return hoursOff+(hoursOff==1?' hour':' hours');}}
function observeFormsForEnterAndSubmit(){var focussed="";document.observe("dom:loaded",function(){var forms=document.forms;for(i=0;i<forms.length;++i){passin=forms[i];items=passin.getElementsByTagName("input");for(j=0;j<items.length;++j){items[j].cacheit=passin.id;items[j].onfocus=function(){focussed=this.cacheit;};}}});KeyCombos.enable(true);KeyCombos["ENTER"]=function(){$(focussed).submit();}}
			AUTOCORE_SELF_CHECK = null;
		
addNewCoresToCookie(["0", "1", "3", "4", "5", "6", "22", "23", "27", "33", "40", "41", "43", "45", "49", "53", "54", "87"], "44f0be9a");