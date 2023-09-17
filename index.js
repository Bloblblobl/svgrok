(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.aW.ax === region.a5.ax)
	{
		return 'on line ' + region.aW.ax;
	}
	return 'on lines ' + region.aW.ax + ' through ' + region.a5.ax;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bU,
		impl.b8,
		impl.b6,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		ab: func(record.ab),
		aX: record.aX,
		aT: record.aT
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.ab;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aX;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.aT) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bU,
		impl.b8,
		impl.b6,
		function(sendToApp, initialModel) {
			var view = impl.b9;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bU,
		impl.b8,
		impl.b6,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aV && impl.aV(sendToApp)
			var view = impl.b9;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.bK);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.b7) && (_VirtualDom_doc.title = title = doc.b7);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.bZ;
	var onUrlRequest = impl.b_;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aV: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bq === next.bq
							&& curr.bd === next.bd
							&& curr.bm.a === next.bm.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		bU: function(flags)
		{
			return A3(impl.bU, flags, _Browser_getUrl(), key);
		},
		b9: impl.b9,
		b8: impl.b8,
		b6: impl.b6
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { bS: 'hidden', bL: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { bS: 'mozHidden', bL: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { bS: 'msHidden', bL: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { bS: 'webkitHidden', bL: 'webkitvisibilitychange' }
		: { bS: 'hidden', bL: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		b2: _Browser_getScene(),
		bE: {
			j: _Browser_window.pageXOffset,
			h: _Browser_window.pageYOffset,
			aY: _Browser_doc.documentElement.clientWidth,
			aO: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aY: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		aO: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			b2: {
				aY: node.scrollWidth,
				aO: node.scrollHeight
			},
			bE: {
				j: node.scrollLeft,
				h: node.scrollTop,
				aY: node.clientWidth,
				aO: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			b2: _Browser_getScene(),
			bE: {
				j: x,
				h: y,
				aY: _Browser_doc.documentElement.clientWidth,
				aO: _Browser_doc.documentElement.clientHeight
			},
			E: {
				j: x + rect.left,
				h: y + rect.top,
				aY: rect.width,
				aO: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.u) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.x),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.x);
		} else {
			var treeLen = builder.u * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.z) : builder.z;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.u);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.x) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.x);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{z: nodeList, u: (len / $elm$core$Array$branchFactor) | 0, x: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {a9: fragment, bd: host, c: path, bm: port_, bq: protocol, br: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$SetViewBox = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $author$project$ViewBox$fromViewport = function (_v0) {
	var scene = _v0.b2;
	return {bG: scene.aO, bH: scene.aY, aO: scene.aO, ac: 0, ad: 0, aY: scene.aY};
};
var $elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var $author$project$ViewBox$zoom = F2(
	function (factor, viewBox) {
		return _Utils_update(
			viewBox,
			{aO: viewBox.bG / factor, aY: viewBox.bH / factor});
	});
var $author$project$Main$getInitialViewBoxFromViewport = function (zoomFactor) {
	return A2(
		$elm$core$Task$perform,
		A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				$author$project$Main$SetViewBox,
				$author$project$ViewBox$zoom(zoomFactor)),
			$author$project$ViewBox$fromViewport),
		$elm$browser$Browser$Dom$getViewport);
};
var $author$project$Main$Neutral = {$: 0};
var $author$project$Path$init = {A: _List_Nil, y: $elm$core$Maybe$Nothing, w: _List_Nil};
var $author$project$ViewBox$init = {bG: 0, bH: 0, aO: 0, ac: 0, ad: 0, aY: 0};
var $author$project$Point$zero = {j: 0, h: 0};
var $author$project$Main$initModel = {Y: _List_Nil, ay: false, b: $author$project$Point$zero, c: $author$project$Path$init, o: '', T: _List_Nil, d: $author$project$Main$Neutral, N: _List_Nil, m: $author$project$ViewBox$init, X: 10};
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		$author$project$Main$initModel,
		$author$project$Main$getInitialViewBoxFromViewport($author$project$Main$initModel.X));
};
var $author$project$Main$KeyDown = function (a) {
	return {$: 9, a: a};
};
var $author$project$Main$KeyUp = function (a) {
	return {$: 10, a: a};
};
var $author$project$Main$MouseMove = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$MouseUp = {$: 7};
var $author$project$Main$SetCanDrag = {$: 8};
var $author$project$Main$WindowResized = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $Gizra$elm_keyboard_event$Keyboard$Event$KeyboardEvent = F7(
	function (altKey, ctrlKey, key, keyCode, metaKey, repeat, shiftKey) {
		return {bI: altKey, bO: ctrlKey, bW: key, bh: keyCode, bX: metaKey, b0: repeat, b3: shiftKey};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $Gizra$elm_keyboard_event$Keyboard$Event$decodeKey = $elm$json$Json$Decode$maybe(
	A2(
		$elm$json$Json$Decode$andThen,
		function (key) {
			return $elm$core$String$isEmpty(key) ? $elm$json$Json$Decode$fail('empty key') : $elm$json$Json$Decode$succeed(key);
		},
		A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string)));
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $Gizra$elm_keyboard_event$Keyboard$Event$decodeNonZero = A2(
	$elm$json$Json$Decode$andThen,
	function (code) {
		return (!code) ? $elm$json$Json$Decode$fail('code was zero') : $elm$json$Json$Decode$succeed(code);
	},
	$elm$json$Json$Decode$int);
var $Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyCode = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2($elm$json$Json$Decode$field, 'keyCode', $Gizra$elm_keyboard_event$Keyboard$Event$decodeNonZero),
			A2($elm$json$Json$Decode$field, 'which', $Gizra$elm_keyboard_event$Keyboard$Event$decodeNonZero),
			A2($elm$json$Json$Decode$field, 'charCode', $Gizra$elm_keyboard_event$Keyboard$Event$decodeNonZero),
			$elm$json$Json$Decode$succeed(0)
		]));
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$A = {$: 0};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Add = {$: 85};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Alt = {$: 32};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Ambiguous = function (a) {
	return {$: 89, a: a};
};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$B = {$: 1};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Backspace = {$: 38};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$C = {$: 2};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$CapsLock = {$: 34};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$ChromeSearch = {$: 59};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Command = {$: 58};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Ctrl = function (a) {
	return {$: 31, a: a};
};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$D = {$: 3};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Decimal = {$: 87};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Delete = {$: 39};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Divide = {$: 88};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Down = {$: 29};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$E = {$: 4};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Eight = {$: 52};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$End = {$: 42};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Enter = {$: 37};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Escape = {$: 36};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F = {$: 5};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F1 = {$: 62};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F10 = {$: 71};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F11 = {$: 72};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F12 = {$: 73};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F2 = {$: 63};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F3 = {$: 64};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F4 = {$: 65};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F5 = {$: 66};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F6 = {$: 67};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F7 = {$: 68};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F8 = {$: 69};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$F9 = {$: 70};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Five = {$: 49};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Four = {$: 48};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$G = {$: 6};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$H = {$: 7};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Home = {$: 43};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$I = {$: 8};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Insert = {$: 54};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$J = {$: 9};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$K = {$: 10};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$L = {$: 11};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Left = {$: 26};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$M = {$: 12};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Multiply = {$: 84};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$N = {$: 13};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Nine = {$: 53};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumLock = {$: 60};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadEight = {$: 82};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadFive = {$: 79};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadFour = {$: 78};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadNine = {$: 83};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadOne = {$: 75};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadSeven = {$: 81};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadSix = {$: 80};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadThree = {$: 77};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadTwo = {$: 76};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadZero = {$: 74};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$O = {$: 14};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$One = {$: 45};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$P = {$: 15};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$PageDown = {$: 41};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$PageUp = {$: 40};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$PauseBreak = {$: 56};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$PrintScreen = {$: 55};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Q = {$: 16};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$R = {$: 17};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Right = {$: 27};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$S = {$: 18};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$ScrollLock = {$: 61};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Seven = {$: 51};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Shift = function (a) {
	return {$: 30, a: a};
};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Six = {$: 50};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Spacebar = {$: 35};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Subtract = {$: 86};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$T = {$: 19};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Tab = {$: 33};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Three = {$: 47};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Two = {$: 46};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$U = {$: 20};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Unknown = function (a) {
	return {$: 90, a: a};
};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Up = {$: 28};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$V = {$: 21};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$W = {$: 22};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Windows = {$: 57};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$X = {$: 23};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Y = {$: 24};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Z = {$: 25};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$Zero = {$: 44};
var $SwiftsNamesake$proper_keyboard$Keyboard$Key$fromCode = function (keyCode) {
	switch (keyCode) {
		case 8:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Backspace;
		case 9:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Tab;
		case 13:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Enter;
		case 16:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Shift($elm$core$Maybe$Nothing);
		case 17:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Ctrl($elm$core$Maybe$Nothing);
		case 18:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Alt;
		case 19:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$PauseBreak;
		case 20:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$CapsLock;
		case 27:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Escape;
		case 32:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Spacebar;
		case 33:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$PageUp;
		case 34:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$PageDown;
		case 35:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$End;
		case 36:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Home;
		case 37:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Left;
		case 38:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Up;
		case 39:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Right;
		case 40:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Down;
		case 44:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$PrintScreen;
		case 45:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Insert;
		case 46:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Delete;
		case 48:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Zero;
		case 49:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$One;
		case 50:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Two;
		case 51:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Three;
		case 52:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Four;
		case 53:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Five;
		case 54:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Six;
		case 55:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Seven;
		case 56:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Eight;
		case 57:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Nine;
		case 65:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$A;
		case 66:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$B;
		case 67:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$C;
		case 68:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$D;
		case 69:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$E;
		case 70:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F;
		case 71:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$G;
		case 72:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$H;
		case 73:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$I;
		case 74:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$J;
		case 75:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$K;
		case 76:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$L;
		case 77:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$M;
		case 78:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$N;
		case 79:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$O;
		case 80:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$P;
		case 81:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Q;
		case 82:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$R;
		case 83:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$S;
		case 84:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$T;
		case 85:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$U;
		case 86:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$V;
		case 87:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$W;
		case 88:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$X;
		case 89:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Y;
		case 90:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Z;
		case 91:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Ambiguous(
				_List_fromArray(
					[$SwiftsNamesake$proper_keyboard$Keyboard$Key$Windows, $SwiftsNamesake$proper_keyboard$Keyboard$Key$Command, $SwiftsNamesake$proper_keyboard$Keyboard$Key$ChromeSearch]));
		case 96:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadZero;
		case 97:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadOne;
		case 98:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadTwo;
		case 99:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadThree;
		case 100:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadFour;
		case 101:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadFive;
		case 102:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadSix;
		case 103:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadSeven;
		case 104:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadEight;
		case 105:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadNine;
		case 106:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Multiply;
		case 107:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Add;
		case 109:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Subtract;
		case 110:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Decimal;
		case 111:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Divide;
		case 112:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F1;
		case 113:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F2;
		case 114:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F3;
		case 115:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F4;
		case 116:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F5;
		case 117:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F6;
		case 118:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F7;
		case 119:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F8;
		case 120:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F9;
		case 121:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F10;
		case 122:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F11;
		case 123:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$F12;
		case 144:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$NumLock;
		case 145:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$ScrollLock;
		default:
			return $SwiftsNamesake$proper_keyboard$Keyboard$Key$Unknown(keyCode);
	}
};
var $elm$json$Json$Decode$map7 = _Json_map7;
var $Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyboardEvent = A8(
	$elm$json$Json$Decode$map7,
	$Gizra$elm_keyboard_event$Keyboard$Event$KeyboardEvent,
	A2($elm$json$Json$Decode$field, 'altKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'ctrlKey', $elm$json$Json$Decode$bool),
	$Gizra$elm_keyboard_event$Keyboard$Event$decodeKey,
	A2($elm$json$Json$Decode$map, $SwiftsNamesake$proper_keyboard$Keyboard$Key$fromCode, $Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyCode),
	A2($elm$json$Json$Decode$field, 'metaKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'repeat', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'shiftKey', $elm$json$Json$Decode$bool));
var $author$project$Point$Point = F2(
	function (x, y) {
		return {j: x, h: y};
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Main$decodeMouseOffset = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Point$Point,
	A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float));
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {bp: processes, bz: taggers};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 1) {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.bp;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.bz);
		if (_v0.$ === 1) {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$browser$Browser$Events$Document = 0;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {bk: pids, by: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {a6: event, bW: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.bk,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.bW;
		var event = _v0.a6;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.by);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, 0, 'keydown');
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, 0, 'keyup');
var $elm$browser$Browser$Events$onMouseMove = A2($elm$browser$Browser$Events$on, 0, 'mousemove');
var $elm$browser$Browser$Events$onMouseUp = A2($elm$browser$Browser$Events$on, 0, 'mouseup');
var $elm$browser$Browser$Events$Window = 1;
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		1,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $author$project$Main$subscriptions = function (model) {
	var baseSubscriptions = _List_fromArray(
		[
			$elm$browser$Browser$Events$onMouseMove(
			A2($elm$json$Json$Decode$map, $author$project$Main$MouseMove, $author$project$Main$decodeMouseOffset)),
			$elm$browser$Browser$Events$onMouseUp(
			$elm$json$Json$Decode$succeed($author$project$Main$MouseUp)),
			$elm$browser$Browser$Events$onKeyDown(
			A2($elm$json$Json$Decode$map, $author$project$Main$KeyDown, $Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyboardEvent)),
			$elm$browser$Browser$Events$onKeyUp(
			A2($elm$json$Json$Decode$map, $author$project$Main$KeyUp, $Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyboardEvent)),
			$elm$browser$Browser$Events$onResize($author$project$Main$WindowResized)
		]);
	var _v0 = model.d;
	switch (_v0.$) {
		case 0:
			return $elm$core$Platform$Sub$batch(baseSubscriptions);
		case 1:
			var canDrag = _v0.a.aF;
			return canDrag ? $elm$core$Platform$Sub$batch(baseSubscriptions) : $elm$core$Platform$Sub$batch(
				A2(
					$elm$core$List$cons,
					A2(
						$elm$time$Time$every,
						100,
						function (_v1) {
							return $author$project$Main$SetCanDrag;
						}),
					baseSubscriptions));
		case 2:
			return $elm$core$Platform$Sub$batch(baseSubscriptions);
		case 3:
			return $elm$core$Platform$Sub$batch(baseSubscriptions);
		case 4:
			return $elm$core$Platform$Sub$batch(baseSubscriptions);
		default:
			return $elm$core$Platform$Sub$batch(baseSubscriptions);
	}
};
var $author$project$Main$Clicking = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$Dragging = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$Drawing = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$DrawingArc = function (a) {
	return {$: 8, a: a};
};
var $author$project$Main$DrawingArcRotation = function (a) {
	return {$: 5, a: a};
};
var $author$project$Main$DrawingArcTo = {$: 0};
var $author$project$Main$Selecting = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$Typing = {$: 5};
var $author$project$Point$add = F2(
	function (point1, point2) {
		return {j: point1.j + point2.j, h: point1.h + point2.h};
	});
var $author$project$Path$Absolute = 0;
var $author$project$Path$ArcSegment = function (a) {
	return {$: 4, a: a};
};
var $author$project$Path$CloseSegment = function (a) {
	return {$: 5, a: a};
};
var $author$project$Path$CubicCurveSegment = function (a) {
	return {$: 2, a: a};
};
var $author$project$Path$LineSegment = function (a) {
	return {$: 1, a: a};
};
var $author$project$Path$MoveSegment = function (a) {
	return {$: 0, a: a};
};
var $author$project$Path$QuadraticCurveSegment = function (a) {
	return {$: 3, a: a};
};
var $author$project$Point$subtract = F2(
	function (point1, point2) {
		return {j: point1.j - point2.j, h: point1.h - point2.h};
	});
var $author$project$Point$reflectOver = F2(
	function (reflection, point) {
		return A2(
			$author$project$Point$add,
			reflection,
			A2($author$project$Point$subtract, reflection, point));
	});
var $author$project$Path$componentEndState = F2(
	function (component, endState) {
		var _v0 = component.af;
		switch (_v0.$) {
			case 0:
				var params = _v0.a;
				return _Utils_update(
					endState,
					{bN: params.a, bQ: params.a, s: params.a, ak: params.a});
			case 1:
				var params = _v0.a;
				return _Utils_update(
					endState,
					{bN: params.a, bQ: params.a, s: params.a});
			case 2:
				var params = _v0.a;
				return _Utils_update(
					endState,
					{bN: params.a, bQ: params._, s: params.a});
			case 3:
				var params = _v0.a;
				return _Utils_update(
					endState,
					{bN: params.aw, bQ: params.a, s: params.a});
			case 4:
				var params = _v0.a;
				return _Utils_update(
					endState,
					{bN: params.a, bQ: params.a, s: params.a});
			default:
				return _Utils_update(
					endState,
					{bN: endState.ak, bQ: endState.ak, s: endState.ak});
		}
	});
var $author$project$Path$initEndState = {bN: $author$project$Point$zero, bQ: $author$project$Point$zero, s: $author$project$Point$zero, ak: $author$project$Point$zero};
var $author$project$Path$toEndState = function (path) {
	return A3($elm$core$List$foldl, $author$project$Path$componentEndState, $author$project$Path$initEndState, path.A);
};
var $author$project$Path$appendCommand = F2(
	function (path, command) {
		var endState = $author$project$Path$toEndState(path);
		var newSegment = function () {
			var _v0 = command.e;
			switch (_v0.$) {
				case 0:
					var params = _v0.a;
					return $author$project$Path$MoveSegment(
						{
							ap: endState.s,
							a: (!command.i) ? params.a : A2($author$project$Point$add, endState.s, params.a)
						});
				case 1:
					var params = _v0.a;
					return $author$project$Path$LineSegment(
						{
							ap: endState.s,
							a: (!command.i) ? params.a : A2($author$project$Point$add, endState.s, params.a)
						});
				case 2:
					var params = _v0.a;
					return $author$project$Path$LineSegment(
						{
							ap: endState.s,
							a: (!command.i) ? {j: params.bA, h: endState.s.h} : {j: endState.s.j + params.bA, h: endState.s.h}
						});
				case 3:
					var params = _v0.a;
					return $author$project$Path$LineSegment(
						{
							ap: endState.s,
							a: (!command.i) ? {j: endState.s.j, h: params.bB} : {j: endState.s.j, h: endState.s.h + params.bB}
						});
				case 4:
					var params = _v0.a;
					return $author$project$Path$CubicCurveSegment(
						{
							_: (!command.i) ? params._ : A2($author$project$Point$add, endState.s, params._),
							ap: endState.s,
							U: (!command.i) ? params.U : A2($author$project$Point$add, endState.s, params.U),
							a: (!command.i) ? params.a : A2($author$project$Point$add, endState.s, params.a)
						});
				case 5:
					var params = _v0.a;
					return $author$project$Path$CubicCurveSegment(
						{
							_: (!command.i) ? params._ : A2($author$project$Point$add, endState.s, params._),
							ap: endState.s,
							U: A2($author$project$Point$reflectOver, endState.s, endState.bQ),
							a: (!command.i) ? params.a : A2($author$project$Point$add, endState.s, params.a)
						});
				case 6:
					var params = _v0.a;
					return $author$project$Path$QuadraticCurveSegment(
						{
							aw: (!command.i) ? params.aw : A2($author$project$Point$add, endState.s, params.aw),
							ap: endState.s,
							a: (!command.i) ? params.a : A2($author$project$Point$add, endState.s, params.a)
						});
				case 7:
					var params = _v0.a;
					return $author$project$Path$QuadraticCurveSegment(
						{
							aw: A2($author$project$Point$reflectOver, endState.s, endState.bN),
							ap: endState.s,
							a: (!command.i) ? params.a : A2($author$project$Point$add, endState.s, params.a)
						});
				case 8:
					var params = _v0.a;
					return $author$project$Path$ArcSegment(
						{
							r: params.r,
							ap: endState.s,
							p: params.p,
							H: params.H,
							v: params.v,
							a: (!command.i) ? params.a : A2($author$project$Point$add, endState.s, params.a)
						});
				default:
					return $author$project$Path$CloseSegment(
						{ap: endState.s, a: endState.ak});
			}
		}();
		var newComponent = {t: command, af: newSegment};
		return _Utils_update(
			path,
			{
				A: _Utils_ap(
					path.A,
					_List_fromArray(
						[newComponent]))
			});
	});
var $author$project$Main$DrawingArcAngle = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$DrawingArcRadiusX = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$DrawingArcRadiusY = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$DrawingArcSize = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$DrawingControl = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$DrawingCubicCurve = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$DrawingCubicCurveTo = {$: 0};
var $author$project$Main$DrawingEndControl = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$DrawingOnePointCurveTo = {$: 0};
var $author$project$Main$DrawingQuadraticCurve = function (a) {
	return {$: 6, a: a};
};
var $author$project$Main$DrawingSmoothCubicCurve = function (a) {
	return {$: 5, a: a};
};
var $author$project$Main$DrawingStartControl = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * $elm$core$Basics$pi) / 180;
};
var $elm$core$Basics$sin = _Basics_sin;
var $author$project$Path$arcSegmentTransformedFrom = function (params) {
	var halfDiffY = (params.ap.h - params.a.h) / 2;
	var halfDiffX = (params.ap.j - params.a.j) / 2;
	var angleRadians = $elm$core$Basics$degrees(params.r);
	var transformedX = ($elm$core$Basics$cos(angleRadians) * halfDiffX) + ($elm$core$Basics$sin(angleRadians) * halfDiffY);
	var transformedY = ((-$elm$core$Basics$sin(angleRadians)) * halfDiffX) + ($elm$core$Basics$cos(angleRadians) * halfDiffY);
	return {j: transformedX, h: transformedY};
};
var $elm$core$Basics$pow = _Basics_pow;
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$Path$arcSegmentAdjustedRadii = function (params) {
	var transformedFrom = $author$project$Path$arcSegmentTransformedFrom(params);
	var absRY = $elm$core$Basics$abs(params.p.h);
	var lambdaY = A2($elm$core$Basics$pow, transformedFrom.h, 2) / A2($elm$core$Basics$pow, absRY, 2);
	var absRX = $elm$core$Basics$abs(params.p.j);
	var lambdaX = A2($elm$core$Basics$pow, transformedFrom.j, 2) / A2($elm$core$Basics$pow, absRX, 2);
	var lambda = lambdaX + lambdaY;
	return (lambda <= 1) ? {j: absRX, h: absRY} : {
		j: $elm$core$Basics$sqrt(lambda) * absRX,
		h: $elm$core$Basics$sqrt(lambda) * absRY
	};
};
var $author$project$Path$arcSegmentCenterPoint = function (params) {
	var transformedFrom = $author$project$Path$arcSegmentTransformedFrom(params);
	var halfSumY = (params.ap.h + params.a.h) / 2;
	var halfSumX = (params.ap.j + params.a.j) / 2;
	var coefficientSign = function () {
		var _v0 = _Utils_Tuple2(params.v, params.H);
		if (!_v0.a) {
			if (!_v0.b) {
				var _v1 = _v0.a;
				var _v2 = _v0.b;
				return -1;
			} else {
				var _v3 = _v0.a;
				var _v4 = _v0.b;
				return 1;
			}
		} else {
			if (!_v0.b) {
				var _v5 = _v0.a;
				var _v6 = _v0.b;
				return 1;
			} else {
				var _v7 = _v0.a;
				var _v8 = _v0.b;
				return -1;
			}
		}
	}();
	var angleRadians = $elm$core$Basics$degrees(params.r);
	var adjustedRadii = $author$project$Path$arcSegmentAdjustedRadii(params);
	var rX = adjustedRadii.j;
	var rXtY = A2($elm$core$Basics$pow, rX, 2) * A2($elm$core$Basics$pow, transformedFrom.h, 2);
	var rY = adjustedRadii.h;
	var rYtX = A2($elm$core$Basics$pow, rY, 2) * A2($elm$core$Basics$pow, transformedFrom.j, 2);
	var coefficientDenominator = rXtY + rYtX;
	var coefficientNumerator = ((A2($elm$core$Basics$pow, rX, 2) * A2($elm$core$Basics$pow, rY, 2)) - rXtY) - rYtX;
	var adjustedCoefficientNumerator = A2($elm$core$Basics$max, 0, coefficientNumerator);
	var coefficient = coefficientSign * $elm$core$Basics$sqrt(adjustedCoefficientNumerator / coefficientDenominator);
	var tcX = coefficient * ((rX * transformedFrom.h) / rY);
	var tcY = coefficient * (-((rY * transformedFrom.j) / rX));
	var centerX = (($elm$core$Basics$cos(angleRadians) * tcX) - ($elm$core$Basics$sin(angleRadians) * tcY)) + halfSumX;
	var centerY = (($elm$core$Basics$sin(angleRadians) * tcX) + ($elm$core$Basics$cos(angleRadians) * tcY)) + halfSumY;
	return {j: centerX, h: centerY};
};
var $elm$core$Basics$atan2 = _Basics_atan2;
var $author$project$Path$Clockwise = 0;
var $author$project$Path$defaultArcRotation = 0;
var $author$project$Path$Large = 0;
var $author$project$Path$defaultArcSize = 0;
var $author$project$Path$CubicCurveCommand = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Path$Spaces = function (a) {
	return {$: 0, a: a};
};
var $author$project$Path$defaultSeparator = $author$project$Path$Spaces(1);
var $author$project$Path$defaultPointSeparator = {j: $author$project$Path$defaultSeparator, h: $author$project$Path$defaultSeparator};
var $author$project$Path$preFormattedCubicCurve = function (params) {
	return {
		e: A2(
			$author$project$Path$CubicCurveCommand,
			params,
			{V: $author$project$Path$defaultPointSeparator, g: $author$project$Path$defaultSeparator, au: $author$project$Path$defaultPointSeparator, n: $author$project$Path$defaultPointSeparator}),
		i: 0
	};
};
var $author$project$Path$HorizontalLineCommand = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Path$preFormattedHorizontalLine = function (params) {
	return {
		e: A2(
			$author$project$Path$HorizontalLineCommand,
			params,
			{g: $author$project$Path$defaultSeparator, aD: $author$project$Path$defaultSeparator}),
		i: 0
	};
};
var $author$project$Path$LineCommand = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Path$preFormattedLine = function (params) {
	return {
		e: A2(
			$author$project$Path$LineCommand,
			params,
			{g: $author$project$Path$defaultSeparator, n: $author$project$Path$defaultPointSeparator}),
		i: 0
	};
};
var $author$project$Path$MoveCommand = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Path$preFormattedMove = function (params) {
	return {
		e: A2(
			$author$project$Path$MoveCommand,
			params,
			{g: $author$project$Path$defaultSeparator, n: $author$project$Path$defaultPointSeparator}),
		i: 0
	};
};
var $author$project$Path$QuadraticCurveCommand = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $author$project$Path$preFormattedQuadraticCurve = function (params) {
	return {
		e: A2(
			$author$project$Path$QuadraticCurveCommand,
			params,
			{at: $author$project$Path$defaultPointSeparator, g: $author$project$Path$defaultSeparator, n: $author$project$Path$defaultPointSeparator}),
		i: 0
	};
};
var $author$project$Path$SmoothCubicCurveCommand = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$Path$preFormattedSmoothCubicCurve = function (params) {
	return {
		e: A2(
			$author$project$Path$SmoothCubicCurveCommand,
			params,
			{V: $author$project$Path$defaultPointSeparator, g: $author$project$Path$defaultSeparator, n: $author$project$Path$defaultPointSeparator}),
		i: 0
	};
};
var $author$project$Path$SmoothQuadraticCurveCommand = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $author$project$Path$preFormattedSmoothQuadraticCurve = function (params) {
	return {
		e: A2(
			$author$project$Path$SmoothQuadraticCurveCommand,
			params,
			{g: $author$project$Path$defaultSeparator, n: $author$project$Path$defaultPointSeparator}),
		i: 0
	};
};
var $author$project$Path$VerticalLineCommand = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Path$preFormattedVerticalLine = function (params) {
	return {
		e: A2(
			$author$project$Path$VerticalLineCommand,
			params,
			{g: $author$project$Path$defaultSeparator, aE: $author$project$Path$defaultSeparator}),
		i: 0
	};
};
var $elm$core$Basics$round = _Basics_round;
var $author$project$Path$arcRotationToString = function (rotation) {
	if (!rotation) {
		return '1';
	} else {
		return '0';
	}
};
var $author$project$Path$arcSizeToString = function (size) {
	if (!size) {
		return '1';
	} else {
		return '0';
	}
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $author$project$Path$separatorToString = function (separator) {
	switch (separator.$) {
		case 0:
			var count = separator.a;
			return A2($elm$core$String$repeat, count, ' ');
		case 1:
			var spacesBefore = separator.a.b5;
			var spacesAfter = separator.a.b4;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						A2($elm$core$String$repeat, spacesBefore, ' '),
						',',
						A2($elm$core$String$repeat, spacesAfter, ' ')
					]));
		default:
			return '';
	}
};
var $author$project$Path$pointToString = F2(
	function (point, separator) {
		return $elm$core$String$concat(
			_List_fromArray(
				[
					$elm$core$String$fromFloat(point.j),
					$author$project$Path$separatorToString(separator.j),
					$elm$core$String$fromFloat(point.h),
					$author$project$Path$separatorToString(separator.h)
				]));
	});
var $elm$core$String$toLower = _String_toLower;
var $author$project$Path$commandToString = function (_v0) {
	var relation = _v0.i;
	var commandType = _v0.e;
	var letter = F2(
		function (separator, upper) {
			if (separator.$ === 2) {
				return '';
			} else {
				if (!relation) {
					return _Utils_ap(
						upper,
						$author$project$Path$separatorToString(separator));
				} else {
					return _Utils_ap(
						$elm$core$String$toLower(upper),
						$author$project$Path$separatorToString(separator));
				}
			}
		});
	switch (commandType.$) {
		case 0:
			var to = commandType.a.a;
			var afterLetter = commandType.b.g;
			var afterTo = commandType.b.n;
			return _Utils_ap(
				A2(letter, afterLetter, 'M'),
				A2($author$project$Path$pointToString, to, afterTo));
		case 1:
			var to = commandType.a.a;
			var afterLetter = commandType.b.g;
			var afterTo = commandType.b.n;
			return _Utils_ap(
				A2(letter, afterLetter, 'L'),
				A2($author$project$Path$pointToString, to, afterTo));
		case 2:
			var toX = commandType.a.bA;
			var afterLetter = commandType.b.g;
			var afterToX = commandType.b.aD;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						A2(letter, afterLetter, 'H'),
						$elm$core$String$fromFloat(toX),
						$author$project$Path$separatorToString(afterToX)
					]));
		case 3:
			var toY = commandType.a.bB;
			var afterLetter = commandType.b.g;
			var afterToY = commandType.b.aE;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						A2(letter, afterLetter, 'V'),
						$elm$core$String$fromFloat(toY),
						$author$project$Path$separatorToString(afterToY)
					]));
		case 4:
			var parameters = commandType.a;
			var format = commandType.b;
			var _v2 = parameters;
			var startControl = _v2.U;
			var endControl = _v2._;
			var to = _v2.a;
			var _v3 = format;
			var afterLetter = _v3.g;
			var afterStartControl = _v3.au;
			var afterEndControl = _v3.V;
			var afterTo = _v3.n;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						A2(letter, afterLetter, 'C'),
						A2($author$project$Path$pointToString, startControl, afterStartControl),
						A2($author$project$Path$pointToString, endControl, afterEndControl),
						A2($author$project$Path$pointToString, to, afterTo)
					]));
		case 5:
			var parameters = commandType.a;
			var format = commandType.b;
			var _v4 = parameters;
			var endControl = _v4._;
			var to = _v4.a;
			var _v5 = format;
			var afterLetter = _v5.g;
			var afterEndControl = _v5.V;
			var afterTo = _v5.n;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						A2(letter, afterLetter, 'S'),
						A2($author$project$Path$pointToString, endControl, afterEndControl),
						A2($author$project$Path$pointToString, to, afterTo)
					]));
		case 6:
			var parameters = commandType.a;
			var format = commandType.b;
			var _v6 = parameters;
			var control = _v6.aw;
			var to = _v6.a;
			var _v7 = format;
			var afterLetter = _v7.g;
			var afterControl = _v7.at;
			var afterTo = _v7.n;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						A2(letter, afterLetter, 'Q'),
						A2($author$project$Path$pointToString, control, afterControl),
						A2($author$project$Path$pointToString, to, afterTo)
					]));
		case 7:
			var to = commandType.a.a;
			var afterLetter = commandType.b.g;
			var afterTo = commandType.b.n;
			return _Utils_ap(
				A2(letter, afterLetter, 'T'),
				A2($author$project$Path$pointToString, to, afterTo));
		case 8:
			var parameters = commandType.a;
			var format = commandType.b;
			var _v8 = parameters;
			var radii = _v8.p;
			var angle = _v8.r;
			var size = _v8.v;
			var rotation = _v8.H;
			var to = _v8.a;
			var _v9 = format;
			var afterRadii = _v9.aL;
			var afterAngle = _v9.aK;
			var afterSize = _v9.aN;
			var afterRotation = _v9.aM;
			var afterTo = _v9.n;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						A2(letter, format.g, 'A'),
						A2($author$project$Path$pointToString, radii, afterRadii),
						$elm$core$String$fromFloat(angle),
						$author$project$Path$separatorToString(afterAngle),
						$author$project$Path$arcSizeToString(size),
						$author$project$Path$separatorToString(afterSize),
						$author$project$Path$arcRotationToString(rotation),
						$author$project$Path$separatorToString(afterRotation),
						A2($author$project$Path$pointToString, to, afterTo)
					]));
		default:
			var afterLetter = commandType.a.g;
			return A2(letter, afterLetter, 'Z');
	}
};
var $author$project$Path$toString = function (_v0) {
	var components = _v0.A;
	var step = F2(
		function (_v1, commandString) {
			var command = _v1.t;
			return _Utils_ap(
				commandString,
				$author$project$Path$commandToString(command));
		});
	return A3($elm$core$List$foldl, step, '', components);
};
var $author$project$Main$handleDraw = F2(
	function (model, drawingState) {
		switch (drawingState.$) {
			case 0:
				var moveCommand = $author$project$Path$preFormattedMove(
					{a: model.b});
				var newPath = A2($author$project$Path$appendCommand, model.c, moveCommand);
				return _Utils_update(
					model,
					{
						c: newPath,
						o: $author$project$Path$toString(newPath)
					});
			case 1:
				var lineCommand = $author$project$Path$preFormattedLine(
					{a: model.b});
				var newPath = A2($author$project$Path$appendCommand, model.c, lineCommand);
				return _Utils_update(
					model,
					{
						c: newPath,
						o: $author$project$Path$toString(newPath)
					});
			case 2:
				var horizontalLineCommand = $author$project$Path$preFormattedHorizontalLine(
					{bA: model.b.j});
				var newPath = A2($author$project$Path$appendCommand, model.c, horizontalLineCommand);
				return _Utils_update(
					model,
					{
						c: newPath,
						o: $author$project$Path$toString(newPath)
					});
			case 3:
				var verticalLineCommand = $author$project$Path$preFormattedVerticalLine(
					{bB: model.b.h});
				var newPath = A2($author$project$Path$appendCommand, model.c, verticalLineCommand);
				return _Utils_update(
					model,
					{
						c: newPath,
						o: $author$project$Path$toString(newPath)
					});
			case 4:
				var drawingCubicCurveState = drawingState.a;
				switch (drawingCubicCurveState.$) {
					case 0:
						return _Utils_update(
							model,
							{
								d: $author$project$Main$Drawing(
									$author$project$Main$DrawingCubicCurve(
										$author$project$Main$DrawingStartControl(
											{a: model.b})))
							});
					case 1:
						var to = drawingCubicCurveState.a.a;
						return _Utils_update(
							model,
							{
								d: $author$project$Main$Drawing(
									$author$project$Main$DrawingCubicCurve(
										$author$project$Main$DrawingEndControl(
											{U: model.b, a: to})))
							});
					default:
						var to = drawingCubicCurveState.a.a;
						var startControl = drawingCubicCurveState.a.U;
						var newState = $author$project$Main$Drawing(
							$author$project$Main$DrawingCubicCurve($author$project$Main$DrawingCubicCurveTo));
						var cubicCurveCommand = $author$project$Path$preFormattedCubicCurve(
							{_: model.b, U: startControl, a: to});
						var newPath = A2($author$project$Path$appendCommand, model.c, cubicCurveCommand);
						return _Utils_update(
							model,
							{
								c: newPath,
								o: $author$project$Path$toString(newPath),
								d: newState
							});
				}
			case 5:
				var drawingOnePointCurveState = drawingState.a;
				if (!drawingOnePointCurveState.$) {
					return _Utils_update(
						model,
						{
							d: $author$project$Main$Drawing(
								$author$project$Main$DrawingSmoothCubicCurve(
									$author$project$Main$DrawingControl(
										{a: model.b})))
						});
				} else {
					var to = drawingOnePointCurveState.a.a;
					var smoothCubicCurveCommand = $author$project$Path$preFormattedSmoothCubicCurve(
						{_: model.b, a: to});
					var newState = $author$project$Main$Drawing(
						$author$project$Main$DrawingSmoothCubicCurve($author$project$Main$DrawingOnePointCurveTo));
					var newPath = A2($author$project$Path$appendCommand, model.c, smoothCubicCurveCommand);
					return _Utils_update(
						model,
						{
							c: newPath,
							o: $author$project$Path$toString(newPath),
							d: newState
						});
				}
			case 6:
				var drawingOnePointCurveState = drawingState.a;
				if (!drawingOnePointCurveState.$) {
					return _Utils_update(
						model,
						{
							d: $author$project$Main$Drawing(
								$author$project$Main$DrawingQuadraticCurve(
									$author$project$Main$DrawingControl(
										{a: model.b})))
						});
				} else {
					var to = drawingOnePointCurveState.a.a;
					var quadraticCurveCommand = $author$project$Path$preFormattedQuadraticCurve(
						{aw: model.b, a: to});
					var newState = $author$project$Main$Drawing(
						$author$project$Main$DrawingQuadraticCurve($author$project$Main$DrawingOnePointCurveTo));
					var newPath = A2($author$project$Path$appendCommand, model.c, quadraticCurveCommand);
					return _Utils_update(
						model,
						{
							c: newPath,
							o: $author$project$Path$toString(newPath),
							d: newState
						});
				}
			case 7:
				var smoothQuadraticCurveCommand = $author$project$Path$preFormattedSmoothQuadraticCurve(
					{a: model.b});
				var newPath = A2($author$project$Path$appendCommand, model.c, smoothQuadraticCurveCommand);
				return _Utils_update(
					model,
					{
						c: newPath,
						o: $author$project$Path$toString(newPath)
					});
			default:
				var drawingArcState = drawingState.a;
				switch (drawingArcState.$) {
					case 0:
						return _Utils_update(
							model,
							{
								d: $author$project$Main$Drawing(
									$author$project$Main$DrawingArc(
										$author$project$Main$DrawingArcRadiusX(
											{a: model.b})))
							});
					case 1:
						var to = drawingArcState.a.a;
						var endState = $author$project$Path$toEndState(model.c);
						var differenceX = to.j - endState.s.j;
						var midX = endState.s.j + (differenceX / 2);
						var radiusX = model.b.j - midX;
						return _Utils_update(
							model,
							{
								d: $author$project$Main$Drawing(
									$author$project$Main$DrawingArc(
										$author$project$Main$DrawingArcRadiusY(
											{aU: radiusX, a: to})))
							});
					case 2:
						var to = drawingArcState.a.a;
						var radiusX = drawingArcState.a.aU;
						var endState = $author$project$Path$toEndState(model.c);
						var differenceY = to.h - endState.s.h;
						var midY = endState.s.h + (differenceY / 2);
						var radiusY = model.b.h - midY;
						return _Utils_update(
							model,
							{
								d: $author$project$Main$Drawing(
									$author$project$Main$DrawingArc(
										$author$project$Main$DrawingArcAngle(
											{
												p: {j: radiusX, h: radiusY},
												a: to
											})))
							});
					case 3:
						var to = drawingArcState.a.a;
						var radii = drawingArcState.a.p;
						var endPoint = $author$project$Path$toEndState(model.c).s;
						var arcSegmentParams = {r: 0, ap: endPoint, p: radii, H: $author$project$Path$defaultArcRotation, v: $author$project$Path$defaultArcSize, a: to};
						var centerPoint = $author$project$Path$arcSegmentCenterPoint(arcSegmentParams);
						var angleRadians = A2($elm$core$Basics$atan2, model.b.h - centerPoint.h, model.b.j - centerPoint.j);
						var angle = $elm$core$Basics$round((angleRadians * 180) / $elm$core$Basics$pi);
						return _Utils_update(
							model,
							{
								d: $author$project$Main$Drawing(
									$author$project$Main$DrawingArc(
										$author$project$Main$DrawingArcSize(
											{r: angle, p: radii, a: to})))
							});
					case 4:
						return model;
					default:
						return model;
				}
		}
	});
var $author$project$Main$DrawingHorizontalLine = {$: 2};
var $author$project$Main$DrawingLine = {$: 1};
var $author$project$Main$DrawingMove = {$: 0};
var $author$project$Main$DrawingSmoothQuadraticCurve = {$: 7};
var $author$project$Main$DrawingVerticalLine = {$: 3};
var $elm$core$Basics$not = _Basics_not;
var $author$project$ViewBox$pan = F3(
	function (deltaX, deltaY, viewBox) {
		return _Utils_update(
			viewBox,
			{ac: viewBox.ac + deltaX, ad: viewBox.ad + deltaY});
	});
var $author$project$Path$CloseCommand = function (a) {
	return {$: 9, a: a};
};
var $author$project$Path$preFormattedClose = {
	e: $author$project$Path$CloseCommand(
		{g: $author$project$Path$defaultSeparator}),
	i: 0
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$savePath = function (path) {
	return _Utils_update(
		path,
		{y: $elm$core$Maybe$Nothing, w: _List_Nil});
};
var $author$project$Main$redo = function (model) {
	var currentSavedModel = {
		c: $author$project$Main$savePath(model.c),
		o: model.o
	};
	var _v0 = $elm$core$List$head(model.T);
	if (!_v0.$) {
		var savedModel = _v0.a;
		return _Utils_update(
			model,
			{
				c: {A: savedModel.c.A, y: $elm$core$Maybe$Nothing, w: savedModel.c.w},
				o: savedModel.o,
				T: A2($elm$core$List$drop, 1, model.T),
				N: A2($elm$core$List$cons, currentSavedModel, model.N)
			});
	} else {
		return model;
	}
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Main$trackedKey = function (key) {
	if (key.$ === 30) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Main$setActiveKey = F2(
	function (key, model) {
		return (A2($elm$core$List$member, key, model.Y) || (!$author$project$Main$trackedKey(key))) ? model : _Utils_update(
			model,
			{
				Y: A2($elm$core$List$cons, key, model.Y)
			});
	});
var $author$project$Main$undo = function (model) {
	var currentSavedModel = {
		c: $author$project$Main$savePath(model.c),
		o: model.o
	};
	var _v0 = $elm$core$List$head(model.N);
	if (!_v0.$) {
		var savedModel = _v0.a;
		return _Utils_update(
			model,
			{
				c: {A: savedModel.c.A, y: $elm$core$Maybe$Nothing, w: savedModel.c.w},
				o: savedModel.o,
				T: A2($elm$core$List$cons, currentSavedModel, model.T),
				N: A2($elm$core$List$drop, 1, model.N)
			});
	} else {
		return model;
	}
};
var $author$project$Main$handleKeyDown = F2(
	function (_v0, model) {
		var keyCode = _v0.bh;
		switch (keyCode.$) {
			case 89:
				return _Utils_update(
					model,
					{ay: true});
			case 20:
				return $author$project$Main$undo(model);
			case 17:
				return $author$project$Main$redo(model);
			case 4:
				var newZoomFactor = model.X * 0.5;
				return _Utils_update(
					model,
					{
						m: A2($author$project$ViewBox$zoom, newZoomFactor, model.m),
						X: newZoomFactor
					});
			case 22:
				var newZoomFactor = model.X * 2;
				return _Utils_update(
					model,
					{
						m: A2($author$project$ViewBox$zoom, newZoomFactor, model.m),
						X: newZoomFactor
					});
			case 26:
				return _Utils_update(
					model,
					{
						m: A3($author$project$ViewBox$pan, -5, 0, model.m)
					});
			case 28:
				return _Utils_update(
					model,
					{
						m: A3($author$project$ViewBox$pan, 0, -5, model.m)
					});
			case 29:
				return _Utils_update(
					model,
					{
						m: A3($author$project$ViewBox$pan, 0, 5, model.m)
					});
			case 27:
				return _Utils_update(
					model,
					{
						m: A3($author$project$ViewBox$pan, 5, 0, model.m)
					});
			case 12:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing($author$project$Main$DrawingMove)
					});
			case 11:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing($author$project$Main$DrawingLine)
					});
			case 7:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing($author$project$Main$DrawingHorizontalLine)
					});
			case 21:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing($author$project$Main$DrawingVerticalLine)
					});
			case 2:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing(
							$author$project$Main$DrawingCubicCurve($author$project$Main$DrawingCubicCurveTo))
					});
			case 18:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing(
							$author$project$Main$DrawingSmoothCubicCurve($author$project$Main$DrawingOnePointCurveTo))
					});
			case 16:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing(
							$author$project$Main$DrawingQuadraticCurve($author$project$Main$DrawingOnePointCurveTo))
					});
			case 19:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing($author$project$Main$DrawingSmoothQuadraticCurve)
					});
			case 0:
				return _Utils_update(
					model,
					{
						d: $author$project$Main$Drawing(
							$author$project$Main$DrawingArc($author$project$Main$DrawingArcTo))
					});
			case 25:
				var newPath = A2($author$project$Path$appendCommand, model.c, $author$project$Path$preFormattedClose);
				return _Utils_update(
					model,
					{
						c: newPath,
						o: $author$project$Path$toString(newPath)
					});
			case 23:
				var _v2 = model.d;
				if (_v2.$ === 4) {
					return _Utils_update(
						model,
						{d: $author$project$Main$Neutral});
				} else {
					return model;
				}
			default:
				return (!model.ay) ? A2($author$project$Main$setActiveKey, keyCode, model) : model;
		}
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Path$absolutePreviousControl = F3(
	function (previousSegment, command, defaultPoint) {
		var _v0 = _Utils_Tuple2(previousSegment, command.e);
		_v0$2:
		while (true) {
			switch (_v0.a.$) {
				case 2:
					if (_v0.b.$ === 5) {
						var params = _v0.a.a;
						var _v1 = _v0.b;
						return params._;
					} else {
						break _v0$2;
					}
				case 3:
					if (_v0.b.$ === 7) {
						var params = _v0.a.a;
						var _v2 = _v0.b;
						return params.aw;
					} else {
						break _v0$2;
					}
				default:
					break _v0$2;
			}
		}
		return defaultPoint;
	});
var $author$project$Path$buildComponent = F2(
	function (command, componentBuilder) {
		var _v0 = componentBuilder;
		var components = _v0.A;
		var currentPoint = _v0.aG;
		var firstConnectedPoint = _v0.aI;
		var previousControl = function () {
			var _v8 = $elm$core$List$head(components);
			if (!_v8.$) {
				var segment = _v8.a.af;
				return A3($author$project$Path$absolutePreviousControl, segment, command, currentPoint);
			} else {
				return currentPoint;
			}
		}();
		var _v1 = command;
		var relation = _v1.i;
		var commandType = _v1.e;
		var absolute = function (ambiguousPoint) {
			if (!relation) {
				return ambiguousPoint;
			} else {
				return A2($author$project$Point$add, currentPoint, ambiguousPoint);
			}
		};
		var newSegment = function () {
			switch (commandType.$) {
				case 0:
					var to = commandType.a.a;
					return $author$project$Path$MoveSegment(
						{
							ap: currentPoint,
							a: absolute(to)
						});
				case 1:
					var to = commandType.a.a;
					return $author$project$Path$LineSegment(
						{
							ap: currentPoint,
							a: absolute(to)
						});
				case 2:
					var toX = commandType.a.bA;
					if (!relation) {
						return $author$project$Path$LineSegment(
							{
								ap: currentPoint,
								a: {j: toX, h: currentPoint.h}
							});
					} else {
						return $author$project$Path$LineSegment(
							{
								ap: currentPoint,
								a: {j: currentPoint.j + toX, h: currentPoint.h}
							});
					}
				case 3:
					var toY = commandType.a.bB;
					if (!relation) {
						return $author$project$Path$LineSegment(
							{
								ap: currentPoint,
								a: {j: currentPoint.j, h: toY}
							});
					} else {
						return $author$project$Path$LineSegment(
							{
								ap: currentPoint,
								a: {j: currentPoint.j, h: currentPoint.h + toY}
							});
					}
				case 4:
					var startControl = commandType.a.U;
					var endControl = commandType.a._;
					var to = commandType.a.a;
					return $author$project$Path$CubicCurveSegment(
						{
							_: absolute(endControl),
							ap: currentPoint,
							U: absolute(startControl),
							a: absolute(to)
						});
				case 5:
					var endControl = commandType.a._;
					var to = commandType.a.a;
					return $author$project$Path$CubicCurveSegment(
						{
							_: absolute(endControl),
							ap: currentPoint,
							U: A2($author$project$Point$reflectOver, currentPoint, previousControl),
							a: absolute(to)
						});
				case 6:
					var control = commandType.a.aw;
					var to = commandType.a.a;
					return $author$project$Path$QuadraticCurveSegment(
						{
							aw: absolute(control),
							ap: currentPoint,
							a: absolute(to)
						});
				case 7:
					var to = commandType.a.a;
					return $author$project$Path$QuadraticCurveSegment(
						{
							aw: A2($author$project$Point$reflectOver, currentPoint, previousControl),
							ap: currentPoint,
							a: absolute(to)
						});
				case 8:
					var radii = commandType.a.p;
					var angle = commandType.a.r;
					var size = commandType.a.v;
					var rotation = commandType.a.H;
					var to = commandType.a.a;
					return $author$project$Path$ArcSegment(
						{
							r: angle,
							ap: currentPoint,
							p: radii,
							H: rotation,
							v: size,
							a: absolute(to)
						});
				default:
					return $author$project$Path$CloseSegment(
						{ap: currentPoint, a: firstConnectedPoint});
			}
		}();
		var newComponent = {t: command, af: newSegment};
		var nextCurrentPoint = function () {
			switch (newSegment.$) {
				case 0:
					var to = newSegment.a.a;
					return to;
				case 1:
					var to = newSegment.a.a;
					return to;
				case 2:
					var to = newSegment.a.a;
					return to;
				case 3:
					var to = newSegment.a.a;
					return to;
				case 4:
					var to = newSegment.a.a;
					return to;
				default:
					var to = newSegment.a.a;
					return to;
			}
		}();
		var nextFirstConnectedPoint = function () {
			if (!newSegment.$) {
				var to = newSegment.a.a;
				return to;
			} else {
				return firstConnectedPoint;
			}
		}();
		return {
			A: A2($elm$core$List$cons, newComponent, components),
			aG: nextCurrentPoint,
			aI: nextFirstConnectedPoint
		};
	});
var $author$project$Path$initComponentBuilder = {A: _List_Nil, aG: $author$project$Point$zero, aI: $author$project$Point$zero};
var $author$project$Path$buildComponents = function (commands) {
	return $elm$core$List$reverse(
		A3($elm$core$List$foldl, $author$project$Path$buildComponent, $author$project$Path$initComponentBuilder, commands).A);
};
var $elm$parser$Parser$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 0, a: a};
};
var $author$project$Path$Parser$Invalid = function (a) {
	return {$: 1, a: a};
};
var $author$project$Path$Parser$addInvalidResult = F2(
	function (builder, invalidString) {
		var _v0 = builder.C;
		if (_v0.b && (_v0.a.$ === 1)) {
			var existing = _v0.a.a;
			var rest = _v0.b;
			return _Utils_update(
				builder,
				{
					C: A2(
						$elm$core$List$cons,
						$author$project$Path$Parser$Invalid(
							_Utils_ap(existing, invalidString)),
						rest)
				});
		} else {
			return (invalidString === '') ? builder : _Utils_update(
				builder,
				{
					C: A2(
						$elm$core$List$cons,
						$author$project$Path$Parser$Invalid(invalidString),
						builder.C)
				});
		}
	});
var $author$project$Path$Parser$ParsingParameterizedCommandType = function (a) {
	return {$: 1, a: a};
};
var $author$project$Path$Parser$Valid = function (a) {
	return {$: 0, a: a};
};
var $author$project$Path$Parser$addValidResult = F3(
	function (_v0, relation, commandType) {
		var results = _v0.C;
		var state = _v0.d;
		var newState = function () {
			if (state.$ === 1) {
				var stateParams = state.a;
				return $author$project$Path$Parser$ParsingParameterizedCommandType(
					_Utils_update(
						stateParams,
						{aJ: true}));
			} else {
				return state;
			}
		}();
		var command = {e: commandType, i: relation};
		return {
			C: A2(
				$elm$core$List$cons,
				$author$project$Path$Parser$Valid(command),
				results),
			d: newState
		};
	});
var $author$project$Path$NoLetter = {$: 2};
var $author$project$Path$Parser$FormattedArc = F5(
	function (radii, angle, size, rotation, to) {
		return {r: angle, p: radii, H: rotation, v: size, a: to};
	});
var $author$project$Path$CounterClockwise = 1;
var $author$project$Path$Parser$FormattedArcRotation = F2(
	function (rotation, afterRotation) {
		return {aM: afterRotation, H: rotation};
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0;
		var parseB = _v1;
		return function (s0) {
			var _v2 = parseA(s0);
			if (_v2.$ === 1) {
				var p = _v2.a;
				var x = _v2.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v2.a;
				var a = _v2.b;
				var s1 = _v2.c;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (!_v1.$) {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $author$project$Path$Comma = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0;
	return function (s0) {
		var _v1 = parse(s0);
		if (_v1.$ === 1) {
			var x = _v1.b;
			return A2($elm$parser$Parser$Advanced$Bad, false, x);
		} else {
			var a = _v1.b;
			var s1 = _v1.c;
			return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
		}
	};
};
var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
var $elm$parser$Parser$Advanced$getOffset = function (s) {
	return A3($elm$parser$Parser$Advanced$Good, false, s.R, s);
};
var $elm$parser$Parser$getOffset = $elm$parser$Parser$Advanced$getOffset;
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.f);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.R, offset) < 0,
					0,
					{a3: col, k: s0.k, q: s0.q, R: offset, bv: row, f: s0.f});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return function (s) {
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.R, s.bv, s.a3, s);
	};
};
var $elm$parser$Parser$Advanced$spaces = $elm$parser$Parser$Advanced$chompWhile(
	function (c) {
		return (c === ' ') || ((c === '\n') || (c === '\r'));
	});
var $elm$parser$Parser$spaces = $elm$parser$Parser$Advanced$spaces;
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {a3: col, bM: contextStack, bn: problem, bv: row};
	});
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.bv, s.a3, x, s.k));
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.R, s.bv, s.a3, s.f);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{a3: newCol, k: s.k, q: s.q, R: newOffset, bv: newRow, f: s.f});
	};
};
var $elm$parser$Parser$token = function (str) {
	return $elm$parser$Parser$Advanced$token(
		$elm$parser$Parser$toToken(str));
};
var $author$project$Path$Parser$separator = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			$elm$parser$Parser$backtrackable(
			A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(
								F4(
									function (before1, after1, before2, after2) {
										return $author$project$Path$Comma(
											{b4: after2 - before2, b5: after1 - before1});
									})),
							A2($elm$parser$Parser$ignorer, $elm$parser$Parser$getOffset, $elm$parser$Parser$spaces)),
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$getOffset,
							$elm$parser$Parser$token(','))),
					A2($elm$parser$Parser$ignorer, $elm$parser$Parser$getOffset, $elm$parser$Parser$spaces)),
				$elm$parser$Parser$getOffset)),
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F2(
						function (before, after) {
							return $author$project$Path$Spaces(after - before);
						})),
				A2($elm$parser$Parser$ignorer, $elm$parser$Parser$getOffset, $elm$parser$Parser$spaces)),
			$elm$parser$Parser$getOffset)
		]));
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 8, a: a};
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $author$project$Path$Parser$formattedArcRotation = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$Path$Parser$FormattedArcRotation),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(0),
					$elm$parser$Parser$symbol('1')),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(1),
					$elm$parser$Parser$symbol('0'))
				]))),
	$author$project$Path$Parser$separator);
var $author$project$Path$Parser$FormattedArcSize = F2(
	function (size, afterSize) {
		return {aN: afterSize, v: size};
	});
var $author$project$Path$Small = 1;
var $author$project$Path$Parser$formattedArcSize = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$Path$Parser$FormattedArcSize),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(0),
					$elm$parser$Parser$symbol('1')),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(1),
					$elm$parser$Parser$symbol('0'))
				]))),
	$author$project$Path$Parser$separator);
var $author$project$Path$Parser$FormattedFloat = F2(
	function (value, afterValue) {
		return {a_: afterValue, bD: value};
	});
var $elm$parser$Parser$ExpectingFloat = {$: 5};
var $elm$parser$Parser$Advanced$consumeBase = _Parser_consumeBase;
var $elm$parser$Parser$Advanced$consumeBase16 = _Parser_consumeBase16;
var $elm$parser$Parser$Advanced$bumpOffset = F2(
	function (newOffset, s) {
		return {a3: s.a3 + (newOffset - s.R), k: s.k, q: s.q, R: newOffset, bv: s.bv, f: s.f};
	});
var $elm$parser$Parser$Advanced$chompBase10 = _Parser_chompBase10;
var $elm$parser$Parser$Advanced$isAsciiCode = _Parser_isAsciiCode;
var $elm$parser$Parser$Advanced$consumeExp = F2(
	function (offset, src) {
		if (A3($elm$parser$Parser$Advanced$isAsciiCode, 101, offset, src) || A3($elm$parser$Parser$Advanced$isAsciiCode, 69, offset, src)) {
			var eOffset = offset + 1;
			var expOffset = (A3($elm$parser$Parser$Advanced$isAsciiCode, 43, eOffset, src) || A3($elm$parser$Parser$Advanced$isAsciiCode, 45, eOffset, src)) ? (eOffset + 1) : eOffset;
			var newOffset = A2($elm$parser$Parser$Advanced$chompBase10, expOffset, src);
			return _Utils_eq(expOffset, newOffset) ? (-newOffset) : newOffset;
		} else {
			return offset;
		}
	});
var $elm$parser$Parser$Advanced$consumeDotAndExp = F2(
	function (offset, src) {
		return A3($elm$parser$Parser$Advanced$isAsciiCode, 46, offset, src) ? A2(
			$elm$parser$Parser$Advanced$consumeExp,
			A2($elm$parser$Parser$Advanced$chompBase10, offset + 1, src),
			src) : A2($elm$parser$Parser$Advanced$consumeExp, offset, src);
	});
var $elm$parser$Parser$Advanced$finalizeInt = F5(
	function (invalid, handler, startOffset, _v0, s) {
		var endOffset = _v0.a;
		var n = _v0.b;
		if (handler.$ === 1) {
			var x = handler.a;
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				true,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		} else {
			var toValue = handler.a;
			return _Utils_eq(startOffset, endOffset) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				_Utils_cmp(s.R, startOffset) < 0,
				A2($elm$parser$Parser$Advanced$fromState, s, invalid)) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				toValue(n),
				A2($elm$parser$Parser$Advanced$bumpOffset, endOffset, s));
		}
	});
var $elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, row, col, x, context));
	});
var $elm$core$String$toFloat = _String_toFloat;
var $elm$parser$Parser$Advanced$finalizeFloat = F6(
	function (invalid, expecting, intSettings, floatSettings, intPair, s) {
		var intOffset = intPair.a;
		var floatOffset = A2($elm$parser$Parser$Advanced$consumeDotAndExp, intOffset, s.f);
		if (floatOffset < 0) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				true,
				A4($elm$parser$Parser$Advanced$fromInfo, s.bv, s.a3 - (floatOffset + s.R), invalid, s.k));
		} else {
			if (_Utils_eq(s.R, floatOffset)) {
				return A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting));
			} else {
				if (_Utils_eq(intOffset, floatOffset)) {
					return A5($elm$parser$Parser$Advanced$finalizeInt, invalid, intSettings, s.R, intPair, s);
				} else {
					if (floatSettings.$ === 1) {
						var x = floatSettings.a;
						return A2(
							$elm$parser$Parser$Advanced$Bad,
							true,
							A2($elm$parser$Parser$Advanced$fromState, s, invalid));
					} else {
						var toValue = floatSettings.a;
						var _v1 = $elm$core$String$toFloat(
							A3($elm$core$String$slice, s.R, floatOffset, s.f));
						if (_v1.$ === 1) {
							return A2(
								$elm$parser$Parser$Advanced$Bad,
								true,
								A2($elm$parser$Parser$Advanced$fromState, s, invalid));
						} else {
							var n = _v1.a;
							return A3(
								$elm$parser$Parser$Advanced$Good,
								true,
								toValue(n),
								A2($elm$parser$Parser$Advanced$bumpOffset, floatOffset, s));
						}
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$number = function (c) {
	return function (s) {
		if (A3($elm$parser$Parser$Advanced$isAsciiCode, 48, s.R, s.f)) {
			var zeroOffset = s.R + 1;
			var baseOffset = zeroOffset + 1;
			return A3($elm$parser$Parser$Advanced$isAsciiCode, 120, zeroOffset, s.f) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.bV,
				c.bc,
				baseOffset,
				A2($elm$parser$Parser$Advanced$consumeBase16, baseOffset, s.f),
				s) : (A3($elm$parser$Parser$Advanced$isAsciiCode, 111, zeroOffset, s.f) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.bV,
				c.bj,
				baseOffset,
				A3($elm$parser$Parser$Advanced$consumeBase, 8, baseOffset, s.f),
				s) : (A3($elm$parser$Parser$Advanced$isAsciiCode, 98, zeroOffset, s.f) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.bV,
				c.a1,
				baseOffset,
				A3($elm$parser$Parser$Advanced$consumeBase, 2, baseOffset, s.f),
				s) : A6(
				$elm$parser$Parser$Advanced$finalizeFloat,
				c.bV,
				c.a7,
				c.bf,
				c.a8,
				_Utils_Tuple2(zeroOffset, 0),
				s)));
		} else {
			return A6(
				$elm$parser$Parser$Advanced$finalizeFloat,
				c.bV,
				c.a7,
				c.bf,
				c.a8,
				A3($elm$parser$Parser$Advanced$consumeBase, 10, s.R, s.f),
				s);
		}
	};
};
var $elm$parser$Parser$Advanced$float = F2(
	function (expecting, invalid) {
		return $elm$parser$Parser$Advanced$number(
			{
				a1: $elm$core$Result$Err(invalid),
				a7: expecting,
				a8: $elm$core$Result$Ok($elm$core$Basics$identity),
				bc: $elm$core$Result$Err(invalid),
				bf: $elm$core$Result$Ok($elm$core$Basics$toFloat),
				bV: invalid,
				bj: $elm$core$Result$Err(invalid)
			});
	});
var $elm$parser$Parser$float = A2($elm$parser$Parser$Advanced$float, $elm$parser$Parser$ExpectingFloat, $elm$parser$Parser$ExpectingFloat);
var $author$project$Path$Parser$float = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed($elm$core$Basics$negate),
				$elm$parser$Parser$symbol('-')),
			$elm$parser$Parser$float),
			$elm$parser$Parser$float
		]));
var $author$project$Path$Parser$formattedFloat = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$Path$Parser$FormattedFloat),
		$author$project$Path$Parser$float),
	$author$project$Path$Parser$separator);
var $author$project$Path$Parser$FormattedPoint = F4(
	function (x, afterX, y, afterY) {
		return {a$: afterX, a0: afterY, j: x, h: y};
	});
var $author$project$Path$Parser$formattedPoint = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($author$project$Path$Parser$FormattedPoint),
				$author$project$Path$Parser$float),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$float),
	$author$project$Path$Parser$separator);
var $author$project$Path$Parser$formattedArc = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed($author$project$Path$Parser$FormattedArc),
					$author$project$Path$Parser$formattedPoint),
				$author$project$Path$Parser$formattedFloat),
			$author$project$Path$Parser$formattedArcSize),
		$author$project$Path$Parser$formattedArcRotation),
	$author$project$Path$Parser$formattedPoint);
var $author$project$Path$ArcCommand = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $author$project$Path$Parser$getArcRotation = function (_v0) {
	var rotation = _v0.H;
	return rotation;
};
var $author$project$Path$Parser$getArcRotationSeparator = function (_v0) {
	var afterRotation = _v0.aM;
	return afterRotation;
};
var $author$project$Path$Parser$getArcSize = function (_v0) {
	var size = _v0.v;
	return size;
};
var $author$project$Path$Parser$getArcSizeSeparator = function (_v0) {
	var afterSize = _v0.aN;
	return afterSize;
};
var $author$project$Path$Parser$getFloat = function (_v0) {
	var value = _v0.bD;
	return value;
};
var $author$project$Path$Parser$getFloatSeparator = function (_v0) {
	var afterValue = _v0.a_;
	return afterValue;
};
var $author$project$Path$Parser$getPoint = function (_v0) {
	var x = _v0.j;
	var y = _v0.h;
	return {j: x, h: y};
};
var $author$project$Path$Parser$getPointSeparator = function (_v0) {
	var afterX = _v0.a$;
	var afterY = _v0.a0;
	return {j: afterX, h: afterY};
};
var $author$project$Path$Parser$makeArc = F2(
	function (sep, _v0) {
		var radii = _v0.p;
		var angle = _v0.r;
		var size = _v0.v;
		var rotation = _v0.H;
		var to = _v0.a;
		return A2(
			$author$project$Path$ArcCommand,
			{
				r: $author$project$Path$Parser$getFloat(angle),
				p: $author$project$Path$Parser$getPoint(radii),
				H: $author$project$Path$Parser$getArcRotation(rotation),
				v: $author$project$Path$Parser$getArcSize(size),
				a: $author$project$Path$Parser$getPoint(to)
			},
			{
				aK: $author$project$Path$Parser$getFloatSeparator(angle),
				g: sep,
				aL: $author$project$Path$Parser$getPointSeparator(radii),
				aM: $author$project$Path$Parser$getArcRotationSeparator(rotation),
				aN: $author$project$Path$Parser$getArcSizeSeparator(size),
				n: $author$project$Path$Parser$getPointSeparator(to)
			});
	});
var $author$project$Path$Parser$arcCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeArc($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedArc) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeArc),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedArc);
};
var $author$project$Path$Parser$Chomping = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$UnexpectedChar = {$: 11};
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return function (s) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.R, s.f);
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{a3: 1, k: s.k, q: s.q, R: s.R + 1, bv: s.bv + 1, f: s.f}) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{a3: s.a3 + 1, k: s.k, q: s.q, R: newOffset, bv: s.bv, f: s.f}));
		};
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					A2(
						func,
						A3($elm$core$String$slice, s0.R, s1.R, s0.f),
						a),
					s1);
			}
		};
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					func(a),
					s1);
			} else {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			}
		};
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $author$project$Path$Parser$chompOne = F2(
	function (chompedString, builder) {
		return A2(
			$elm$parser$Parser$map,
			function (chompedChar) {
				return _Utils_update(
					builder,
					{
						d: $author$project$Path$Parser$Chomping(
							_Utils_ap(chompedString, chompedChar))
					});
			},
			$elm$parser$Parser$getChompedString(
				$elm$parser$Parser$chompIf(
					function (_v0) {
						return true;
					})));
	});
var $author$project$Path$CloseFormat = function (afterLetter) {
	return {g: afterLetter};
};
var $author$project$Path$Parser$ParsedClose = {$: 2};
var $author$project$Path$Relative = 1;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Path$Parser$closeCommand = function (builder) {
	var relationFormat = A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Tuple$pair),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(0),
						$elm$parser$Parser$token('Z')),
						A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(1),
						$elm$parser$Parser$token('z'))
					]))),
		A2($elm$parser$Parser$map, $author$project$Path$CloseFormat, $author$project$Path$Parser$separator));
	var result = A2(
		$elm$parser$Parser$map,
		function (_v0) {
			var relation = _v0.a;
			var format = _v0.b;
			return $author$project$Path$Parser$Valid(
				{
					e: $author$project$Path$CloseCommand(format),
					i: relation
				});
		},
		relationFormat);
	return A2(
		$elm$parser$Parser$map,
		function (newResult) {
			return {
				C: A2($elm$core$List$cons, newResult, builder.C),
				d: $author$project$Path$Parser$ParsedClose
			};
		},
		result);
};
var $author$project$Path$Parser$FormattedCubicCurve = F3(
	function (startControl, endControl, to) {
		return {_: endControl, U: startControl, a: to};
	});
var $author$project$Path$Parser$formattedCubicCurve = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$FormattedCubicCurve),
			$author$project$Path$Parser$formattedPoint),
		$author$project$Path$Parser$formattedPoint),
	$author$project$Path$Parser$formattedPoint);
var $author$project$Path$Parser$makeCubicCurve = F2(
	function (sep, _v0) {
		var startControl = _v0.U;
		var endControl = _v0._;
		var to = _v0.a;
		return A2(
			$author$project$Path$CubicCurveCommand,
			{
				_: $author$project$Path$Parser$getPoint(endControl),
				U: $author$project$Path$Parser$getPoint(startControl),
				a: $author$project$Path$Parser$getPoint(to)
			},
			{
				V: $author$project$Path$Parser$getPointSeparator(endControl),
				g: sep,
				au: $author$project$Path$Parser$getPointSeparator(startControl),
				n: $author$project$Path$Parser$getPointSeparator(to)
			});
	});
var $author$project$Path$Parser$cubicCurveCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeCubicCurve($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedCubicCurve) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeCubicCurve),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedCubicCurve);
};
var $author$project$Path$Parser$makeHorizontalLine = F2(
	function (sep, formattedToX) {
		return A2(
			$author$project$Path$HorizontalLineCommand,
			{
				bA: $author$project$Path$Parser$getFloat(formattedToX)
			},
			{
				g: sep,
				aD: $author$project$Path$Parser$getFloatSeparator(formattedToX)
			});
	});
var $author$project$Path$Parser$horizontalLineCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeHorizontalLine($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedFloat) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeHorizontalLine),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedFloat);
};
var $author$project$Path$Parser$makeLine = F2(
	function (sep, formattedTo) {
		return A2(
			$author$project$Path$LineCommand,
			{
				a: $author$project$Path$Parser$getPoint(formattedTo)
			},
			{
				g: sep,
				n: $author$project$Path$Parser$getPointSeparator(formattedTo)
			});
	});
var $author$project$Path$Parser$lineCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeLine($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedPoint) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeLine),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedPoint);
};
var $author$project$Path$Parser$makeMove = F2(
	function (sep, formattedTo) {
		return A2(
			$author$project$Path$MoveCommand,
			{
				a: $author$project$Path$Parser$getPoint(formattedTo)
			},
			{
				g: sep,
				n: $author$project$Path$Parser$getPointSeparator(formattedTo)
			});
	});
var $author$project$Path$Parser$moveCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeLine($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedPoint) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeMove),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedPoint);
};
var $author$project$Path$Parser$Arc = 8;
var $author$project$Path$Parser$CubicCurve = 4;
var $author$project$Path$Parser$HorizontalLine = 2;
var $author$project$Path$Parser$Line = 1;
var $author$project$Path$Parser$Move = 0;
var $author$project$Path$Parser$QuadraticCurve = 6;
var $author$project$Path$Parser$SmoothCubicCurve = 5;
var $author$project$Path$Parser$SmoothQuadraticCurve = 7;
var $author$project$Path$Parser$VerticalLine = 3;
var $author$project$Path$Parser$commandLetterToStateBuilder = function () {
	var paramStateBuilder = F2(
		function (commandType, relation) {
			return $author$project$Path$Parser$ParsingParameterizedCommandType(
				{e: commandType, aJ: false, i: relation});
		});
	return _List_fromArray(
		[
			_Utils_Tuple2(
			'M',
			paramStateBuilder(0)),
			_Utils_Tuple2(
			'L',
			paramStateBuilder(1)),
			_Utils_Tuple2(
			'H',
			paramStateBuilder(2)),
			_Utils_Tuple2(
			'V',
			paramStateBuilder(3)),
			_Utils_Tuple2(
			'C',
			paramStateBuilder(4)),
			_Utils_Tuple2(
			'S',
			paramStateBuilder(5)),
			_Utils_Tuple2(
			'Q',
			paramStateBuilder(6)),
			_Utils_Tuple2(
			'T',
			paramStateBuilder(7)),
			_Utils_Tuple2(
			'A',
			paramStateBuilder(8))
		]);
}();
var $author$project$Path$Parser$parameterizedCommandLetters = function (builder) {
	var letterParser = function (_v0) {
		var letter = _v0.a;
		var stateBuilder = _v0.b;
		return A2(
			$elm$parser$Parser$map,
			function (state) {
				return _Utils_update(
					builder,
					{d: state});
			},
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(
							stateBuilder(0)),
						$elm$parser$Parser$token(letter)),
						A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(
							stateBuilder(1)),
						$elm$parser$Parser$token(
							$elm$core$String$toLower(letter)))
					])));
	};
	return $elm$parser$Parser$oneOf(
		A2($elm$core$List$map, letterParser, $author$project$Path$Parser$commandLetterToStateBuilder));
};
var $author$project$Path$Parser$FormattedQuadraticCurve = F2(
	function (control, to) {
		return {aw: control, a: to};
	});
var $author$project$Path$Parser$formattedQuadraticCurve = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$Path$Parser$FormattedQuadraticCurve),
		$author$project$Path$Parser$formattedPoint),
	$author$project$Path$Parser$formattedPoint);
var $author$project$Path$Parser$makeQuadraticCurve = F2(
	function (sep, _v0) {
		var control = _v0.aw;
		var to = _v0.a;
		return A2(
			$author$project$Path$QuadraticCurveCommand,
			{
				aw: $author$project$Path$Parser$getPoint(control),
				a: $author$project$Path$Parser$getPoint(to)
			},
			{
				at: $author$project$Path$Parser$getPointSeparator(control),
				g: sep,
				n: $author$project$Path$Parser$getPointSeparator(to)
			});
	});
var $author$project$Path$Parser$quadraticCurveCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeQuadraticCurve($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedQuadraticCurve) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeQuadraticCurve),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedQuadraticCurve);
};
var $author$project$Path$Parser$FormattedSmoothCubicCurve = F2(
	function (endControl, to) {
		return {_: endControl, a: to};
	});
var $author$project$Path$Parser$formattedSmoothCubicCurve = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$Path$Parser$FormattedSmoothCubicCurve),
		$author$project$Path$Parser$formattedPoint),
	$author$project$Path$Parser$formattedPoint);
var $author$project$Path$Parser$makeSmoothCubicCurve = F2(
	function (sep, _v0) {
		var endControl = _v0._;
		var to = _v0.a;
		return A2(
			$author$project$Path$SmoothCubicCurveCommand,
			{
				_: $author$project$Path$Parser$getPoint(endControl),
				a: $author$project$Path$Parser$getPoint(to)
			},
			{
				V: $author$project$Path$Parser$getPointSeparator(endControl),
				g: sep,
				n: $author$project$Path$Parser$getPointSeparator(to)
			});
	});
var $author$project$Path$Parser$smoothCubicCurveCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeSmoothCubicCurve($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedSmoothCubicCurve) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeSmoothCubicCurve),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedSmoothCubicCurve);
};
var $author$project$Path$Parser$makeSmoothQuadraticCurve = F2(
	function (sep, formattedTo) {
		return A2(
			$author$project$Path$SmoothQuadraticCurveCommand,
			{
				a: $author$project$Path$Parser$getPoint(formattedTo)
			},
			{
				g: sep,
				n: $author$project$Path$Parser$getPointSeparator(formattedTo)
			});
	});
var $author$project$Path$Parser$smoothQuadraticCurveCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeSmoothQuadraticCurve($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedPoint) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeSmoothQuadraticCurve),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedPoint);
};
var $author$project$Path$Parser$makeVerticalLine = F2(
	function (sep, formattedToY) {
		return A2(
			$author$project$Path$VerticalLineCommand,
			{
				bB: $author$project$Path$Parser$getFloat(formattedToY)
			},
			{
				g: sep,
				aE: $author$project$Path$Parser$getFloatSeparator(formattedToY)
			});
	});
var $author$project$Path$Parser$verticalLineCommand = function (parsedOne) {
	return parsedOne ? A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			$author$project$Path$Parser$makeVerticalLine($author$project$Path$NoLetter)),
		$author$project$Path$Parser$formattedFloat) : A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$Path$Parser$makeVerticalLine),
			$author$project$Path$Parser$separator),
		$author$project$Path$Parser$formattedFloat);
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $author$project$Path$Parser$withAllBranches = F4(
	function (builder, parsedOne, letter, commandTypeParser) {
		return $elm$parser$Parser$oneOf(
			$elm$core$List$concat(
				_List_fromArray(
					[
						_List_fromArray(
						[commandTypeParser]),
						parsedOne ? _List_fromArray(
						[
							$author$project$Path$Parser$parameterizedCommandLetters(builder),
							$author$project$Path$Parser$closeCommand(builder)
						]) : _List_Nil,
						_List_fromArray(
						[
							A2($author$project$Path$Parser$chompOne, letter, builder)
						])
					])));
	});
var $author$project$Path$Parser$builderStep = function (builder) {
	var _v0 = builder.d;
	switch (_v0.$) {
		case 0:
			var chompedString = _v0.a;
			var builderWithResult = A2($author$project$Path$Parser$addInvalidResult, builder, chompedString);
			return $elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						$author$project$Path$Parser$parameterizedCommandLetters(builderWithResult),
						$author$project$Path$Parser$closeCommand(builderWithResult),
						A2($author$project$Path$Parser$chompOne, chompedString, builder)
					]));
		case 1:
			var commandType = _v0.a.e;
			var relation = _v0.a.i;
			var parsedOne = _v0.a.aJ;
			var letter = function (uppercase) {
				if (parsedOne) {
					return '';
				} else {
					if (!relation) {
						return uppercase;
					} else {
						return $elm$core$String$toLower(uppercase);
					}
				}
			};
			var makeParser = F2(
				function (commandTypeParser, uppercase) {
					return A4(
						$author$project$Path$Parser$withAllBranches,
						builder,
						parsedOne,
						letter(uppercase),
						A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(
								A2($author$project$Path$Parser$addValidResult, builder, relation)),
							$elm$parser$Parser$backtrackable(commandTypeParser)));
				});
			switch (commandType) {
				case 0:
					return A2(
						makeParser,
						$author$project$Path$Parser$moveCommand(parsedOne),
						'M');
				case 1:
					return A2(
						makeParser,
						$author$project$Path$Parser$lineCommand(parsedOne),
						'L');
				case 2:
					return A2(
						makeParser,
						$author$project$Path$Parser$horizontalLineCommand(parsedOne),
						'H');
				case 3:
					return A2(
						makeParser,
						$author$project$Path$Parser$verticalLineCommand(parsedOne),
						'V');
				case 4:
					return A2(
						makeParser,
						$author$project$Path$Parser$cubicCurveCommand(parsedOne),
						'C');
				case 5:
					return A2(
						makeParser,
						$author$project$Path$Parser$smoothCubicCurveCommand(parsedOne),
						'S');
				case 6:
					return A2(
						makeParser,
						$author$project$Path$Parser$quadraticCurveCommand(parsedOne),
						'Q');
				case 7:
					return A2(
						makeParser,
						$author$project$Path$Parser$smoothQuadraticCurveCommand(parsedOne),
						'T');
				default:
					return A2(
						makeParser,
						$author$project$Path$Parser$arcCommand(parsedOne),
						'A');
			}
		default:
			return $elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						$author$project$Path$Parser$parameterizedCommandLetters(builder),
						$author$project$Path$Parser$closeCommand(builder),
						A2($author$project$Path$Parser$chompOne, '', builder)
					]));
	}
};
var $elm$parser$Parser$ExpectingEnd = {$: 10};
var $elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			$elm$core$String$length(s.f),
			s.R) ? A3($elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $author$project$Path$Parser$initBuilder = {
	C: _List_Nil,
	d: $author$project$Path$Parser$Chomping('')
};
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0;
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (!step.$) {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return function (s) {
			return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (!step.$) {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $author$project$Path$Parser$builderLoop = A2(
	$elm$parser$Parser$loop,
	$author$project$Path$Parser$initBuilder,
	function (builder) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return $elm$parser$Parser$Done(builder);
					},
					$elm$parser$Parser$end),
					A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed($elm$parser$Parser$Loop),
					$author$project$Path$Parser$builderStep(builder))
				]));
	});
var $author$project$Path$Parser$finishBuilder = function (_v0) {
	var results = _v0.C;
	var state = _v0.d;
	switch (state.$) {
		case 0:
			var chompedString = state.a;
			return A2(
				$author$project$Path$Parser$addInvalidResult,
				{C: results, d: state},
				chompedString).C;
		case 1:
			var commandType = state.a.e;
			var relation = state.a.i;
			var parsedOne = state.a.aJ;
			var letterCase = function () {
				if (!relation) {
					return $elm$core$Basics$identity;
				} else {
					return $elm$core$String$toLower;
				}
			}();
			var letter = function () {
				switch (commandType) {
					case 0:
						return letterCase('M');
					case 1:
						return letterCase('L');
					case 2:
						return letterCase('H');
					case 3:
						return letterCase('V');
					case 4:
						return letterCase('C');
					case 5:
						return letterCase('S');
					case 6:
						return letterCase('Q');
					case 7:
						return letterCase('T');
					default:
						return letterCase('A');
				}
			}();
			return parsedOne ? results : A2(
				$author$project$Path$Parser$addInvalidResult,
				{C: results, d: state},
				letter).C;
		default:
			return results;
	}
};
var $author$project$Path$fromComponents = function (components) {
	return {A: components, y: $elm$core$Maybe$Nothing, w: _List_Nil};
};
var $author$project$Path$Parser$resultToCommand = function (result) {
	if (!result.$) {
		var command = result.a;
		return $elm$core$Maybe$Just(command);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {a3: col, bn: problem, bv: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.bv, p.a3, p.bn);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0;
		var _v1 = parse(
			{a3: 1, k: _List_Nil, q: 1, R: 0, bv: 1, f: src});
		if (!_v1.$) {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (!_v0.$) {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $author$project$Path$Parser$parse = function (commandString) {
	var _v0 = A2($elm$parser$Parser$run, $author$project$Path$Parser$builderLoop, commandString);
	if (!_v0.$) {
		var builder = _v0.a;
		return $author$project$Path$fromComponents(
			$author$project$Path$buildComponents(
				$elm$core$List$reverse(
					A2(
						$elm$core$List$filterMap,
						$author$project$Path$Parser$resultToCommand,
						$author$project$Path$Parser$finishBuilder(builder)))));
	} else {
		return $author$project$Path$fromComponents(_List_Nil);
	}
};
var $author$project$Path$preFormattedArc = function (params) {
	return {
		e: A2(
			$author$project$Path$ArcCommand,
			params,
			{aK: $author$project$Path$defaultSeparator, g: $author$project$Path$defaultSeparator, aL: $author$project$Path$defaultPointSeparator, aM: $author$project$Path$defaultSeparator, aN: $author$project$Path$defaultSeparator, n: $author$project$Path$defaultPointSeparator}),
		i: 0
	};
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Main$shouldSave = F2(
	function (oldSavedModel, newSavedModel) {
		var oldSegments = A2(
			$elm$core$List$map,
			function ($) {
				return $.af;
			},
			oldSavedModel.c.A);
		var newSegments = A2(
			$elm$core$List$map,
			function ($) {
				return $.af;
			},
			newSavedModel.c.A);
		return !_Utils_eq(oldSegments, newSegments);
	});
var $author$project$Main$saveModel = F2(
	function (oldModel, newModel) {
		var oldSavedModel = {
			c: $author$project$Main$savePath(oldModel.c),
			o: oldModel.o
		};
		var newSavedModel = {
			c: $author$project$Main$savePath(newModel.c),
			o: newModel.o
		};
		var _v0 = $elm$core$List$head(newModel.N);
		if (!_v0.$) {
			return A2($author$project$Main$shouldSave, oldSavedModel, newSavedModel) ? _Utils_update(
				newModel,
				{
					T: _List_Nil,
					N: A2($elm$core$List$cons, oldSavedModel, newModel.N)
				}) : newModel;
		} else {
			return A2($author$project$Main$shouldSave, oldSavedModel, newSavedModel) ? _Utils_update(
				newModel,
				{
					T: _List_Nil,
					N: _List_fromArray(
						[oldSavedModel])
				}) : newModel;
		}
	});
var $author$project$ViewBox$scalePoint = F2(
	function (_v0, _v1) {
		var width = _v0.aY;
		var height = _v0.aO;
		var actualWidth = _v0.bH;
		var actualHeight = _v0.bG;
		var x = _v1.j;
		var y = _v1.h;
		return {
			j: A3($elm$core$Basics$composeL, $elm$core$Basics$toFloat, $elm$core$Basics$round, (x * width) / actualWidth),
			h: A3($elm$core$Basics$composeL, $elm$core$Basics$toFloat, $elm$core$Basics$round, (y * height) / actualHeight)
		};
	});
var $author$project$Path$Control = 3;
var $author$project$Path$EndControl = 2;
var $author$project$Path$EndPoint = 0;
var $author$project$Path$StartControl = 1;
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$Point$withinBounds = F3(
	function (point, bounds1, bounds2) {
		var minY = A2($elm$core$Basics$min, bounds1.h, bounds2.h);
		var minX = A2($elm$core$Basics$min, bounds1.j, bounds2.j);
		var maxY = A2($elm$core$Basics$max, bounds1.h, bounds2.h);
		var maxX = A2($elm$core$Basics$max, bounds1.j, bounds2.j);
		return (_Utils_cmp(minX, point.j) < 1) && ((_Utils_cmp(point.j, maxX) < 1) && ((_Utils_cmp(minY, point.h) < 1) && (_Utils_cmp(point.h, maxY) < 1)));
	});
var $author$project$Path$componentSelectionsWithin = F4(
	function (bounds1, bounds2, index, component) {
		var withinBounds = function (point) {
			return A3($author$project$Point$withinBounds, point, bounds1, bounds2);
		};
		var _v0 = component.af;
		switch (_v0.$) {
			case 0:
				var to = _v0.a.a;
				return withinBounds(to) ? _List_fromArray(
					[
						{E: 0, F: index}
					]) : _List_Nil;
			case 1:
				var to = _v0.a.a;
				return withinBounds(to) ? _List_fromArray(
					[
						{E: 0, F: index}
					]) : _List_Nil;
			case 2:
				var startControl = _v0.a.U;
				var endControl = _v0.a._;
				var to = _v0.a.a;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							withinBounds(startControl) ? _List_fromArray(
							[
								{E: 1, F: index}
							]) : _List_Nil,
							withinBounds(endControl) ? _List_fromArray(
							[
								{E: 2, F: index}
							]) : _List_Nil,
							withinBounds(to) ? _List_fromArray(
							[
								{E: 0, F: index}
							]) : _List_Nil
						]));
			case 3:
				var control = _v0.a.aw;
				var to = _v0.a.a;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							withinBounds(control) ? _List_fromArray(
							[
								{E: 3, F: index}
							]) : _List_Nil,
							withinBounds(to) ? _List_fromArray(
							[
								{E: 0, F: index}
							]) : _List_Nil
						]));
			case 4:
				var to = _v0.a.a;
				return withinBounds(to) ? _List_fromArray(
					[
						{E: 0, F: index}
					]) : _List_Nil;
			default:
				var to = _v0.a.a;
				return withinBounds(to) ? _List_fromArray(
					[
						{E: 0, F: index}
					]) : _List_Nil;
		}
	});
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$Path$selectionsWithin = F3(
	function (bounds1, bounds2, path) {
		return A2(
			$elm$core$List$concatMap,
			function (_v0) {
				var index = _v0.a;
				var component = _v0.b;
				return A4($author$project$Path$componentSelectionsWithin, bounds1, bounds2, index, component);
			},
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, path.A));
	});
var $author$project$Main$shiftPressed = function (model) {
	return A2(
		$elm$core$List$any,
		function (key) {
			if (key.$ === 30) {
				return true;
			} else {
				return false;
			}
		},
		model.Y);
};
var $author$project$Path$addSelection = F2(
	function (path, selection) {
		return A2($elm$core$List$member, selection, path.w) ? path : _Utils_update(
			path,
			{
				w: A2($elm$core$List$cons, selection, path.w)
			});
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$Path$removeSelection = F2(
	function (path, selection) {
		var removedFromSelected = A2(
			$elm$core$List$filter,
			function (s) {
				return !_Utils_eq(s, selection);
			},
			path.w);
		return _Utils_update(
			path,
			{w: removedFromSelected});
	});
var $author$project$Path$toggleSelection = F2(
	function (path, selection) {
		return A2($elm$core$List$member, selection, path.w) ? A2($author$project$Path$removeSelection, path, selection) : A2($author$project$Path$addSelection, path, selection);
	});
var $author$project$Main$unsetActiveKey = F2(
	function (key, model) {
		return _Utils_update(
			model,
			{
				Y: A2(
					$elm$core$List$filter,
					$elm$core$Basics$neq(key),
					model.Y)
			});
	});
var $author$project$Path$initUpdateBuilder = F2(
	function (selected, offset) {
		return {
			l: {
				e: $author$project$Path$CloseCommand(
					{g: $author$project$Path$NoLetter}),
				i: 0
			},
			P: $author$project$Point$zero,
			G: $author$project$Point$zero,
			aq: $author$project$Point$zero,
			R: offset,
			w: selected,
			D: _List_Nil
		};
	});
var $author$project$Point$scale = F2(
	function (factor, _v0) {
		var x = _v0.j;
		var y = _v0.h;
		return {j: factor * x, h: factor * y};
	});
var $author$project$Point$midpoint = F2(
	function (point1, point2) {
		return A2(
			$author$project$Point$scale,
			0.5,
			A2($author$project$Point$add, point1, point2));
	});
var $author$project$Point$isReflectionOver = F3(
	function (reflection, point1, point2) {
		return _Utils_eq(
			A2($author$project$Point$midpoint, point1, point2),
			reflection);
	});
var $author$project$Path$commandInSequence = F2(
	function (lastCommand, thisCommand) {
		var sameRelation = _Utils_eq(lastCommand.i, thisCommand.i);
		var _v0 = _Utils_Tuple2(lastCommand.e, thisCommand.e);
		_v0$11:
		while (true) {
			switch (_v0.a.$) {
				case 0:
					switch (_v0.b.$) {
						case 0:
							var _v1 = _v0.a;
							var _v2 = _v0.b;
							return sameRelation;
						case 1:
							var _v3 = _v0.a;
							var _v4 = _v0.b;
							return sameRelation;
						default:
							break _v0$11;
					}
				case 1:
					if (_v0.b.$ === 1) {
						var _v5 = _v0.a;
						var _v6 = _v0.b;
						return sameRelation;
					} else {
						break _v0$11;
					}
				case 2:
					if (_v0.b.$ === 2) {
						var _v7 = _v0.a;
						var _v8 = _v0.b;
						return sameRelation;
					} else {
						break _v0$11;
					}
				case 3:
					if (_v0.b.$ === 3) {
						var _v9 = _v0.a;
						var _v10 = _v0.b;
						return sameRelation;
					} else {
						break _v0$11;
					}
				case 4:
					if (_v0.b.$ === 4) {
						var _v11 = _v0.a;
						var _v12 = _v0.b;
						return sameRelation;
					} else {
						break _v0$11;
					}
				case 5:
					if (_v0.b.$ === 5) {
						var _v13 = _v0.a;
						var _v14 = _v0.b;
						return sameRelation;
					} else {
						break _v0$11;
					}
				case 6:
					if (_v0.b.$ === 6) {
						var _v15 = _v0.a;
						var _v16 = _v0.b;
						return sameRelation;
					} else {
						break _v0$11;
					}
				case 7:
					if (_v0.b.$ === 7) {
						var _v17 = _v0.a;
						var _v18 = _v0.b;
						return sameRelation;
					} else {
						break _v0$11;
					}
				case 8:
					if (_v0.b.$ === 8) {
						var _v19 = _v0.a;
						var _v20 = _v0.b;
						return sameRelation;
					} else {
						break _v0$11;
					}
				default:
					if (_v0.b.$ === 9) {
						return sameRelation;
					} else {
						break _v0$11;
					}
			}
		}
		return false;
	});
var $author$project$Path$updateFormat = F2(
	function (format, inSequence) {
		return ((!inSequence) && _Utils_eq(format.g, $author$project$Path$NoLetter)) ? _Utils_update(
			format,
			{g: $author$project$Path$defaultSeparator}) : format;
	});
var $author$project$Path$updateArc = F2(
	function (_v0, params) {
		var command = _v0.t;
		var inSequence = A2($author$project$Path$commandInSequence, params.l, command);
		var newCommand = function () {
			var _v1 = _Utils_Tuple2(command.i, command.e);
			_v1$2:
			while (true) {
				if (!_v1.a) {
					if (_v1.b.$ === 8) {
						var _v2 = _v1.a;
						var _v3 = _v1.b;
						var format = _v3.b;
						return _Utils_update(
							command,
							{
								e: A2(
									$author$project$Path$ArcCommand,
									{r: params.r, p: params.p, H: params.H, v: params.v, a: params.a},
									A2($author$project$Path$updateFormat, format, inSequence))
							});
					} else {
						break _v1$2;
					}
				} else {
					if (_v1.b.$ === 8) {
						var _v4 = _v1.a;
						var _v5 = _v1.b;
						var format = _v5.b;
						return _Utils_update(
							command,
							{
								e: A2(
									$author$project$Path$ArcCommand,
									{
										r: params.r,
										p: params.p,
										H: params.H,
										v: params.v,
										a: A2($author$project$Point$subtract, params.a, params.ap)
									},
									A2($author$project$Path$updateFormat, format, inSequence))
							});
					} else {
						break _v1$2;
					}
				}
			}
			return command;
		}();
		return {
			t: newCommand,
			af: $author$project$Path$ArcSegment(
				{r: params.r, ap: params.ap, p: params.p, H: params.H, v: params.v, a: params.a})
		};
	});
var $author$project$Path$defaultCubicCurveCommand = function (relation) {
	return {
		e: A2(
			$author$project$Path$CubicCurveCommand,
			{_: $author$project$Point$zero, U: $author$project$Point$zero, a: $author$project$Point$zero},
			{
				V: $author$project$Path$defaultPointSeparator,
				g: $author$project$Path$Spaces(0),
				au: $author$project$Path$defaultPointSeparator,
				n: $author$project$Path$defaultPointSeparator
			}),
		i: relation
	};
};
var $author$project$Path$expandSmoothCubicFormat = function (_v0) {
	var afterLetter = _v0.g;
	var afterEndControl = _v0.V;
	var afterTo = _v0.n;
	return {V: afterEndControl, g: afterLetter, au: afterEndControl, n: afterTo};
};
var $author$project$Path$updateCubicCurve = F2(
	function (_v0, params) {
		var command = _v0.t;
		var newStartControlOffset = A2($author$project$Point$subtract, params.U, params.ap);
		var newOffset = A2($author$project$Point$subtract, params.a, params.ap);
		var newEndControlOffset = A2($author$project$Point$subtract, params._, params.ap);
		var inSequenceExpansion = A2(
			$author$project$Path$commandInSequence,
			params.l,
			$author$project$Path$defaultCubicCurveCommand(command.i));
		var inSequence = A2($author$project$Path$commandInSequence, params.l, command);
		var newCommand = function () {
			var _v1 = _Utils_Tuple2(command.i, command.e);
			_v1$4:
			while (true) {
				if (!_v1.a) {
					switch (_v1.b.$) {
						case 4:
							var _v2 = _v1.a;
							var _v3 = _v1.b;
							var format = _v3.b;
							return _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$CubicCurveCommand,
										{_: params._, U: params.U, a: params.a},
										A2($author$project$Path$updateFormat, format, inSequence))
								});
						case 5:
							var _v6 = _v1.a;
							var _v7 = _v1.b;
							var format = _v7.b;
							return params.aa ? _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$SmoothCubicCurveCommand,
										{_: params._, a: params.a},
										A2($author$project$Path$updateFormat, format, inSequence))
								}) : _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$CubicCurveCommand,
										{_: params._, U: params.U, a: params.a},
										$author$project$Path$expandSmoothCubicFormat(
											A2($author$project$Path$updateFormat, format, inSequenceExpansion)))
								});
						default:
							break _v1$4;
					}
				} else {
					switch (_v1.b.$) {
						case 4:
							var _v4 = _v1.a;
							var _v5 = _v1.b;
							var format = _v5.b;
							return _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$CubicCurveCommand,
										{_: newEndControlOffset, U: newStartControlOffset, a: newOffset},
										A2($author$project$Path$updateFormat, format, inSequence))
								});
						case 5:
							var _v8 = _v1.a;
							var _v9 = _v1.b;
							var format = _v9.b;
							return params.aa ? _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$SmoothCubicCurveCommand,
										{_: newEndControlOffset, a: newOffset},
										A2($author$project$Path$updateFormat, format, inSequence))
								}) : _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$CubicCurveCommand,
										{_: newEndControlOffset, U: newStartControlOffset, a: newOffset},
										$author$project$Path$expandSmoothCubicFormat(
											A2($author$project$Path$updateFormat, format, inSequenceExpansion)))
								});
						default:
							break _v1$4;
					}
				}
			}
			return command;
		}();
		return {
			t: newCommand,
			af: $author$project$Path$CubicCurveSegment(
				{_: params._, ap: params.ap, U: params.U, a: params.a})
		};
	});
var $author$project$Path$defaultLineCommand = function (relation) {
	return {
		e: A2(
			$author$project$Path$LineCommand,
			{a: $author$project$Point$zero},
			{
				g: $author$project$Path$Spaces(0),
				n: $author$project$Path$defaultPointSeparator
			}),
		i: relation
	};
};
var $author$project$Path$expandHorizontalFormat = function (_v0) {
	var afterLetter = _v0.g;
	var afterToX = _v0.aD;
	return {
		g: afterLetter,
		n: _Utils_eq(
			afterToX,
			$author$project$Path$Spaces(0)) ? {
			j: $author$project$Path$defaultSeparator,
			h: $author$project$Path$Spaces(0)
		} : {j: afterToX, h: afterToX}
	};
};
var $author$project$Path$expandVerticalFormat = function (_v0) {
	var afterLetter = _v0.g;
	var afterToY = _v0.aE;
	return {
		g: afterLetter,
		n: _Utils_eq(
			afterToY,
			$author$project$Path$Spaces(0)) ? {
			j: $author$project$Path$defaultSeparator,
			h: $author$project$Path$Spaces(0)
		} : {j: afterToY, h: afterToY}
	};
};
var $author$project$Path$updateLine = F2(
	function (_v0, params) {
		var command = _v0.t;
		var newOffset = A2($author$project$Point$subtract, params.a, params.ap);
		var inSequenceExpansion = A2(
			$author$project$Path$commandInSequence,
			params.l,
			$author$project$Path$defaultLineCommand(command.i));
		var inSequence = A2($author$project$Path$commandInSequence, params.l, command);
		var newCommand = function () {
			var _v1 = _Utils_Tuple2(command.i, command.e);
			_v1$6:
			while (true) {
				if (!_v1.a) {
					switch (_v1.b.$) {
						case 1:
							var _v2 = _v1.a;
							var _v3 = _v1.b;
							var format = _v3.b;
							return _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$LineCommand,
										{a: params.a},
										A2($author$project$Path$updateFormat, format, inSequence))
								});
						case 2:
							var _v6 = _v1.a;
							var _v7 = _v1.b;
							var format = _v7.b;
							return (!newOffset.h) ? _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$HorizontalLineCommand,
										{bA: params.a.j},
										A2($author$project$Path$updateFormat, format, inSequence))
								}) : _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$LineCommand,
										{a: params.a},
										$author$project$Path$expandHorizontalFormat(
											A2($author$project$Path$updateFormat, format, inSequenceExpansion)))
								});
						case 3:
							var _v10 = _v1.a;
							var _v11 = _v1.b;
							var format = _v11.b;
							return (!newOffset.j) ? _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$VerticalLineCommand,
										{bB: params.a.h},
										A2($author$project$Path$updateFormat, format, inSequence))
								}) : _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$LineCommand,
										{a: params.a},
										$author$project$Path$expandVerticalFormat(
											A2($author$project$Path$updateFormat, format, inSequenceExpansion)))
								});
						default:
							break _v1$6;
					}
				} else {
					switch (_v1.b.$) {
						case 1:
							var _v4 = _v1.a;
							var _v5 = _v1.b;
							var format = _v5.b;
							return _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$LineCommand,
										{a: newOffset},
										A2($author$project$Path$updateFormat, format, inSequence))
								});
						case 2:
							var _v8 = _v1.a;
							var _v9 = _v1.b;
							var format = _v9.b;
							return (!newOffset.h) ? _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$HorizontalLineCommand,
										{bA: newOffset.j},
										A2($author$project$Path$updateFormat, format, inSequence))
								}) : _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$LineCommand,
										{a: newOffset},
										$author$project$Path$expandHorizontalFormat(
											A2($author$project$Path$updateFormat, format, inSequenceExpansion)))
								});
						case 3:
							var _v12 = _v1.a;
							var _v13 = _v1.b;
							var format = _v13.b;
							return (!newOffset.j) ? _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$VerticalLineCommand,
										{bB: newOffset.h},
										A2($author$project$Path$updateFormat, format, inSequence))
								}) : _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$LineCommand,
										{a: newOffset},
										$author$project$Path$expandVerticalFormat(
											A2($author$project$Path$updateFormat, format, inSequenceExpansion)))
								});
						default:
							break _v1$6;
					}
				}
			}
			return command;
		}();
		return {
			t: newCommand,
			af: $author$project$Path$LineSegment(
				{ap: params.ap, a: params.a})
		};
	});
var $author$project$Path$updateMove = F2(
	function (_v0, params) {
		var command = _v0.t;
		var inSequence = A2($author$project$Path$commandInSequence, params.l, command);
		var newCommand = function () {
			var _v1 = _Utils_Tuple2(command.i, command.e);
			_v1$2:
			while (true) {
				if (!_v1.a) {
					if (!_v1.b.$) {
						var _v2 = _v1.a;
						var _v3 = _v1.b;
						var format = _v3.b;
						return _Utils_update(
							command,
							{
								e: A2(
									$author$project$Path$MoveCommand,
									{a: params.a},
									A2($author$project$Path$updateFormat, format, inSequence))
							});
					} else {
						break _v1$2;
					}
				} else {
					if (!_v1.b.$) {
						var _v4 = _v1.a;
						var _v5 = _v1.b;
						var format = _v5.b;
						return _Utils_update(
							command,
							{
								e: A2(
									$author$project$Path$MoveCommand,
									{
										a: A2($author$project$Point$subtract, params.a, params.ap)
									},
									A2($author$project$Path$updateFormat, format, inSequence))
							});
					} else {
						break _v1$2;
					}
				}
			}
			return command;
		}();
		return {
			t: newCommand,
			af: $author$project$Path$MoveSegment(
				{ap: params.ap, a: params.a})
		};
	});
var $author$project$Path$expandSmoothQuadraticFormat = function (_v0) {
	var afterLetter = _v0.g;
	var afterTo = _v0.n;
	return {
		at: _Utils_eq(
			afterTo.h,
			$author$project$Path$Spaces(0)) ? {j: afterTo.j, h: $author$project$Path$defaultSeparator} : afterTo,
		g: afterLetter,
		n: afterTo
	};
};
var $author$project$Path$updateQuadraticCurve = F2(
	function (_v0, params) {
		var command = _v0.t;
		var newOffset = A2($author$project$Point$subtract, params.a, params.ap);
		var newControlOffset = A2($author$project$Point$subtract, params.aw, params.ap);
		var inSequenceExpansion = A2(
			$author$project$Path$commandInSequence,
			params.l,
			$author$project$Path$defaultCubicCurveCommand(command.i));
		var inSequence = A2($author$project$Path$commandInSequence, params.l, command);
		var newCommand = function () {
			var _v1 = _Utils_Tuple2(command.i, command.e);
			_v1$4:
			while (true) {
				if (!_v1.a) {
					switch (_v1.b.$) {
						case 6:
							var _v2 = _v1.a;
							var _v3 = _v1.b;
							var format = _v3.b;
							return _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$QuadraticCurveCommand,
										{aw: params.aw, a: params.a},
										A2($author$project$Path$updateFormat, format, inSequence))
								});
						case 7:
							var _v6 = _v1.a;
							var _v7 = _v1.b;
							var format = _v7.b;
							return params.aa ? _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$SmoothQuadraticCurveCommand,
										{a: params.a},
										A2($author$project$Path$updateFormat, format, inSequence))
								}) : _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$QuadraticCurveCommand,
										{aw: params.aw, a: params.a},
										$author$project$Path$expandSmoothQuadraticFormat(
											A2($author$project$Path$updateFormat, format, inSequenceExpansion)))
								});
						default:
							break _v1$4;
					}
				} else {
					switch (_v1.b.$) {
						case 6:
							var _v4 = _v1.a;
							var _v5 = _v1.b;
							var format = _v5.b;
							return _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$QuadraticCurveCommand,
										{aw: newControlOffset, a: newOffset},
										A2($author$project$Path$updateFormat, format, inSequence))
								});
						case 7:
							var _v8 = _v1.a;
							var _v9 = _v1.b;
							var format = _v9.b;
							return params.aa ? _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$SmoothQuadraticCurveCommand,
										{a: newOffset},
										A2($author$project$Path$updateFormat, format, inSequence))
								}) : _Utils_update(
								command,
								{
									e: A2(
										$author$project$Path$QuadraticCurveCommand,
										{aw: newControlOffset, a: newOffset},
										$author$project$Path$expandSmoothQuadraticFormat(
											A2($author$project$Path$updateFormat, format, inSequenceExpansion)))
								});
						default:
							break _v1$4;
					}
				}
			}
			return command;
		}();
		return {
			t: newCommand,
			af: $author$project$Path$QuadraticCurveSegment(
				{aw: params.aw, ap: params.ap, a: params.a})
		};
	});
var $author$project$Path$updateComponent = F2(
	function (_v0, builder) {
		var index = _v0.a;
		var component = _v0.b;
		var updatingStartControl = A2(
			$elm$core$List$member,
			{E: 1, F: index},
			builder.w);
		var updatingEndPoint = A2(
			$elm$core$List$member,
			{E: 0, F: index},
			builder.w);
		var updatingEndControl = A2(
			$elm$core$List$member,
			{E: 2, F: index},
			builder.w);
		var updatingControl = A2(
			$elm$core$List$member,
			{E: 3, F: index},
			builder.w);
		var _v1 = component.af;
		switch (_v1.$) {
			case 0:
				var params = _v1.a;
				var newParams = {
					ap: builder.G,
					l: builder.l,
					a: updatingEndPoint ? A2($author$project$Point$add, params.a, builder.R) : params.a
				};
				var updatedComponent = A2($author$project$Path$updateMove, component, newParams);
				return _Utils_update(
					builder,
					{
						l: updatedComponent.t,
						P: newParams.a,
						G: newParams.a,
						aq: newParams.a,
						D: A2($elm$core$List$cons, updatedComponent, builder.D)
					});
			case 1:
				var params = _v1.a;
				var newParams = {
					ap: builder.G,
					l: builder.l,
					a: updatingEndPoint ? A2($author$project$Point$add, params.a, builder.R) : params.a
				};
				var updatedComponent = A2($author$project$Path$updateLine, component, newParams);
				return _Utils_update(
					builder,
					{
						l: updatedComponent.t,
						P: newParams.a,
						G: newParams.a,
						D: A2($elm$core$List$cons, updatedComponent, builder.D)
					});
			case 2:
				var params = _v1.a;
				var newParams = {
					_: updatingEndControl ? A2($author$project$Point$add, params._, builder.R) : params._,
					ap: builder.G,
					aa: A3($author$project$Point$isReflectionOver, params.ap, params.U, builder.P),
					l: builder.l,
					U: updatingStartControl ? A2($author$project$Point$add, params.U, builder.R) : params.U,
					a: updatingEndPoint ? A2($author$project$Point$add, params.a, builder.R) : params.a
				};
				var updatedComponent = A2($author$project$Path$updateCubicCurve, component, newParams);
				return _Utils_update(
					builder,
					{
						l: updatedComponent.t,
						P: newParams._,
						G: newParams.a,
						D: A2($elm$core$List$cons, updatedComponent, builder.D)
					});
			case 3:
				var params = _v1.a;
				var newParams = {
					aw: updatingControl ? A2($author$project$Point$add, params.aw, builder.R) : params.aw,
					ap: builder.G,
					aa: A3($author$project$Point$isReflectionOver, params.ap, params.aw, builder.P),
					l: builder.l,
					a: updatingEndPoint ? A2($author$project$Point$add, params.a, builder.R) : params.a
				};
				var updatedComponent = A2($author$project$Path$updateQuadraticCurve, component, newParams);
				return _Utils_update(
					builder,
					{
						l: updatedComponent.t,
						P: newParams.aw,
						G: newParams.a,
						D: A2($elm$core$List$cons, updatedComponent, builder.D)
					});
			case 4:
				var params = _v1.a;
				var newParams = {
					r: params.r,
					ap: builder.G,
					l: builder.l,
					p: params.p,
					H: params.H,
					v: params.v,
					a: updatingEndPoint ? A2($author$project$Point$add, params.a, builder.R) : params.a
				};
				var updatedComponent = A2($author$project$Path$updateArc, component, newParams);
				return _Utils_update(
					builder,
					{
						l: updatedComponent.t,
						P: newParams.a,
						G: newParams.a,
						D: A2($elm$core$List$cons, updatedComponent, builder.D)
					});
			default:
				var updatedComponent = _Utils_update(
					component,
					{
						af: $author$project$Path$CloseSegment(
							{ap: builder.G, a: builder.aq})
					});
				return _Utils_update(
					builder,
					{
						l: updatedComponent.t,
						P: builder.aq,
						G: builder.aq,
						D: A2($elm$core$List$cons, updatedComponent, builder.D)
					});
		}
	});
var $author$project$Path$update = F2(
	function (path, offset) {
		var updatedBuilder = A3(
			$elm$core$List$foldl,
			$author$project$Path$updateComponent,
			A2($author$project$Path$initUpdateBuilder, path.w, offset),
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, path.A));
		var updatedComponents = $elm$core$List$reverse(updatedBuilder.D);
		return _Utils_update(
			path,
			{A: updatedComponents});
	});
var $author$project$Path$updateWithSelection = F3(
	function (path, offset, selection) {
		return A2(
			$author$project$Path$removeSelection,
			A2(
				$author$project$Path$update,
				A2($author$project$Path$addSelection, path, selection),
				offset),
			selection);
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var save = $author$project$Main$saveModel(model);
		switch (msg.$) {
			case 0:
				var newViewBox = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{m: newViewBox}),
					$elm$core$Platform$Cmd$none);
			case 1:
				var newPathString = msg.a;
				return _Utils_Tuple2(
					save(
						_Utils_update(
							model,
							{
								c: $author$project$Path$Parser$parse(newPathString),
								o: newPathString
							})),
					$elm$core$Platform$Cmd$none);
			case 2:
				var newWidth = msg.a;
				var newHeight = msg.b;
				var newViewBox = {bG: newHeight, bH: newWidth, aO: newHeight, ac: model.m.ac, ad: model.m.ad, aY: newWidth};
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							m: A2($author$project$ViewBox$zoom, model.X, newViewBox)
						}),
					$elm$core$Platform$Cmd$none);
			case 3:
				var selection = msg.a;
				var _v1 = model;
				var path = _v1.c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							c: _Utils_update(
								path,
								{y: selection})
						}),
					$elm$core$Platform$Cmd$none);
			case 4:
				var newOffset = msg.a;
				var newState = function () {
					var _v2 = model.d;
					switch (_v2.$) {
						case 0:
							return model.d;
						case 1:
							var position = _v2.a.aS;
							var temporarySelection = _v2.a.ag;
							var canDrag = _v2.a.aF;
							return canDrag ? $author$project$Main$Dragging(
								{aH: position, ag: temporarySelection}) : model.d;
						case 2:
							return model.d;
						case 3:
							return model.d;
						case 4:
							return model.d;
						default:
							return model.d;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							b: A2(
								$author$project$Point$add,
								A2($author$project$ViewBox$scalePoint, model.m, newOffset),
								{j: model.m.ac, h: model.m.ad}),
							d: newState
						}),
					$elm$core$Platform$Cmd$none);
			case 5:
				var selection = msg.a;
				var _v3 = model.d;
				switch (_v3.$) {
					case 0:
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									d: $author$project$Main$Clicking(
										{aF: false, aS: model.b, ag: selection})
								}),
							$elm$core$Platform$Cmd$none);
					case 1:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 2:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 3:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 4:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 6:
				var _v4 = model.d;
				switch (_v4.$) {
					case 0:
						return $author$project$Main$shiftPressed(model) ? _Utils_Tuple2(
							_Utils_update(
								model,
								{
									d: $author$project$Main$Selecting(model.b)
								}),
							$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
							save(
								_Utils_update(
									model,
									{
										c: {A: model.c.A, y: model.c.y, w: _List_Nil}
									})),
							$elm$core$Platform$Cmd$none);
					case 1:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 2:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 3:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 4:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 7:
				var _v5 = model.d;
				switch (_v5.$) {
					case 0:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 1:
						var temporarySelection = _v5.a.ag;
						var newPath = $author$project$Main$shiftPressed(model) ? A2($author$project$Path$toggleSelection, model.c, temporarySelection) : {
							A: model.c.A,
							y: model.c.y,
							w: A2($elm$core$List$member, temporarySelection, model.c.w) ? _List_Nil : _List_fromArray(
								[temporarySelection])
						};
						return _Utils_Tuple2(
							save(
								_Utils_update(
									model,
									{c: newPath, d: $author$project$Main$Neutral})),
							$elm$core$Platform$Cmd$none);
					case 2:
						var dragStart = _v5.a.aH;
						var temporarySelection = _v5.a.ag;
						var dragOffset = A2($author$project$Point$subtract, model.b, dragStart);
						var alreadySelected = A2($elm$core$List$member, temporarySelection, model.c.w);
						var newPath = alreadySelected ? A2($author$project$Path$update, model.c, dragOffset) : A3($author$project$Path$updateWithSelection, model.c, dragOffset, temporarySelection);
						var newPathString = $author$project$Path$toString(newPath);
						return _Utils_Tuple2(
							save(
								_Utils_update(
									model,
									{c: newPath, o: newPathString, d: $author$project$Main$Neutral})),
							$elm$core$Platform$Cmd$none);
					case 3:
						var selectStart = _v5.a;
						var viewBoxOffset = {j: model.m.ac, h: model.m.ad};
						var actualSelectStart = A2($author$project$Point$add, selectStart, viewBoxOffset);
						var actualMouseOffset = A2($author$project$Point$add, model.b, viewBoxOffset);
						return _Utils_Tuple2(
							save(
								_Utils_update(
									model,
									{
										c: {
											A: model.c.A,
											y: model.c.y,
											w: A3($author$project$Path$selectionsWithin, actualSelectStart, actualMouseOffset, model.c)
										},
										d: $author$project$Main$Neutral
									})),
							$elm$core$Platform$Cmd$none);
					case 4:
						var drawingState = _v5.a;
						return _Utils_Tuple2(
							save(
								A2($author$project$Main$handleDraw, model, drawingState)),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 8:
				var _v6 = model.d;
				switch (_v6.$) {
					case 0:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 1:
						var clickingParams = _v6.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									d: $author$project$Main$Clicking(
										_Utils_update(
											clickingParams,
											{aF: true}))
								}),
							$elm$core$Platform$Cmd$none);
					case 2:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 3:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 4:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 9:
				var keyboardEvent = msg.a;
				return _Utils_eq(model.d, $author$project$Main$Typing) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					A2($author$project$Main$handleKeyDown, keyboardEvent, model),
					$elm$core$Platform$Cmd$none);
			case 10:
				var keyCode = msg.a.bh;
				if (keyCode.$ === 89) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{ay: false}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						A2($author$project$Main$unsetActiveKey, keyCode, model),
						$elm$core$Platform$Cmd$none);
				}
			case 11:
				return _Utils_Tuple2(
					$author$project$Main$undo(model),
					$elm$core$Platform$Cmd$none);
			case 12:
				return _Utils_Tuple2(
					$author$project$Main$redo(model),
					$elm$core$Platform$Cmd$none);
			case 13:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{d: $author$project$Main$Typing}),
					$elm$core$Platform$Cmd$none);
			case 14:
				var newState = _Utils_eq(model.d, $author$project$Main$Typing) ? $author$project$Main$Neutral : model.d;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{d: newState}),
					$elm$core$Platform$Cmd$none);
			case 15:
				var arcSize = msg.a;
				var _v8 = model.d;
				if (((_v8.$ === 4) && (_v8.a.$ === 8)) && (_v8.a.a.$ === 4)) {
					var to = _v8.a.a.a.a;
					var radii = _v8.a.a.a.p;
					var angle = _v8.a.a.a.r;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								d: $author$project$Main$Drawing(
									$author$project$Main$DrawingArc(
										$author$project$Main$DrawingArcRotation(
											{r: angle, p: radii, v: arcSize, a: to})))
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			default:
				var arcRotation = msg.a;
				var _v9 = model.d;
				if (((_v9.$ === 4) && (_v9.a.$ === 8)) && (_v9.a.a.$ === 5)) {
					var params = _v9.a.a.a;
					var arcCommand = $author$project$Path$preFormattedArc(
						{r: params.r, p: params.p, H: arcRotation, v: params.v, a: params.a});
					var newPath = A2($author$project$Path$appendCommand, model.c, arcCommand);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								c: newPath,
								o: $author$project$Path$toString(newPath),
								d: $author$project$Main$Drawing(
									$author$project$Main$DrawingArc($author$project$Main$DrawingArcTo))
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
		}
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$svg$Svg$Attributes$display = _VirtualDom_attribute('display');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $author$project$ViewBox$toString = function (_v0) {
	var minX = _v0.ac;
	var minY = _v0.ad;
	var width = _v0.aY;
	var height = _v0.aO;
	return A2(
		$elm$core$String$join,
		' ',
		_List_fromArray(
			[
				$elm$core$String$fromFloat(minX),
				$elm$core$String$fromFloat(minY),
				$elm$core$String$fromFloat(width),
				$elm$core$String$fromFloat(height)
			]));
};
var $author$project$Main$MouseDownCanvas = {$: 6};
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$svg$Svg$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$svg$Svg$Attributes$opacity = _VirtualDom_attribute('opacity');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $author$project$Main$css = {
	ai: {bb: 'grid-axes', y: 'hovered', c: 'path', bl: 'point', w: 'selected'},
	aP: {ba: 'grid'}
};
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $author$project$Main$viewAxes = function (_v0) {
	var viewBox = _v0.m;
	return A2(
		$elm$svg$Svg$g,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$class($author$project$Main$css.ai.bb)
			]),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$line,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x1(
						$elm$core$String$fromFloat(viewBox.ac)),
						$elm$svg$Svg$Attributes$y1('0'),
						$elm$svg$Svg$Attributes$x2(
						$elm$core$String$fromFloat(viewBox.ac + viewBox.aY)),
						$elm$svg$Svg$Attributes$y2('0')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$line,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x1('0'),
						$elm$svg$Svg$Attributes$y1(
						$elm$core$String$fromFloat(viewBox.ad)),
						$elm$svg$Svg$Attributes$x2('0'),
						$elm$svg$Svg$Attributes$y2(
						$elm$core$String$fromFloat(viewBox.ad + viewBox.aO))
					]),
				_List_Nil)
			]));
};
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$Main$viewBackground = function (model) {
	return A2(
		$elm$svg$Svg$g,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$rect,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x(
						$elm$core$String$fromFloat(model.m.ac)),
						$elm$svg$Svg$Attributes$y(
						$elm$core$String$fromFloat(model.m.ad)),
						$elm$svg$Svg$Attributes$width('100%'),
						$elm$svg$Svg$Attributes$height('100%'),
						$elm$svg$Svg$Attributes$fill('url(#grid)'),
						$elm$svg$Svg$Events$onMouseDown($author$project$Main$MouseDownCanvas),
						$elm$svg$Svg$Attributes$opacity('10%')
					]),
				_List_Nil),
				$author$project$Main$viewAxes(model)
			]));
};
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$pattern = $elm$svg$Svg$trustedNode('pattern');
var $elm$svg$Svg$Attributes$patternUnits = _VirtualDom_attribute('patternUnits');
var $author$project$Main$viewDefs = function () {
	var gridSize = 5;
	var gridSizeString = $elm$core$String$fromInt(gridSize);
	var viewBoxString = A2(
		$elm$core$String$join,
		',',
		_List_fromArray(
			['0', '0', gridSizeString, gridSizeString]));
	return A2(
		$elm$svg$Svg$defs,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$pattern,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$id($author$project$Main$css.aP.ba),
						$elm$svg$Svg$Attributes$viewBox(viewBoxString),
						$elm$svg$Svg$Attributes$width(gridSizeString),
						$elm$svg$Svg$Attributes$height(gridSizeString),
						$elm$svg$Svg$Attributes$patternUnits('userSpaceOnUse'),
						$elm$svg$Svg$Attributes$x('-0.25'),
						$elm$svg$Svg$Attributes$y('-0.25')
					]),
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$line,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$x1('0'),
								$elm$svg$Svg$Attributes$y1('0'),
								$elm$svg$Svg$Attributes$x2(gridSizeString),
								$elm$svg$Svg$Attributes$y2('0')
							]),
						_List_Nil),
						A2(
						$elm$svg$Svg$line,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$x1('0'),
								$elm$svg$Svg$Attributes$y1('0'),
								$elm$svg$Svg$Attributes$x2('0'),
								$elm$svg$Svg$Attributes$y2(gridSizeString)
							]),
						_List_Nil)
					]))
			]));
}();
var $elm$svg$Svg$Attributes$cursor = _VirtualDom_attribute('cursor');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $author$project$Main$viewPath = F2(
	function (attributes, pathString) {
		return A2(
			$elm$svg$Svg$path,
			A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$class($author$project$Main$css.ai.c),
				A2(
					$elm$core$List$cons,
					$elm$svg$Svg$Attributes$d(pathString),
					attributes)),
			_List_Nil);
	});
var $author$project$Path$componentPoints = function (component) {
	var _v0 = component.af;
	switch (_v0.$) {
		case 0:
			var to = _v0.a.a;
			return _List_fromArray(
				[to]);
		case 1:
			var to = _v0.a.a;
			return _List_fromArray(
				[to]);
		case 2:
			var startControl = _v0.a.U;
			var endControl = _v0.a._;
			var to = _v0.a.a;
			return _List_fromArray(
				[startControl, endControl, to]);
		case 3:
			var control = _v0.a.aw;
			var to = _v0.a.a;
			return _List_fromArray(
				[control, to]);
		case 4:
			var to = _v0.a.a;
			return _List_fromArray(
				[to]);
		default:
			var to = _v0.a.a;
			return _List_fromArray(
				[to]);
	}
};
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $author$project$Main$viewPoint = F2(
	function (attributes, _v0) {
		var x = _v0.j;
		var y = _v0.h;
		var pointAttributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$class($author$project$Main$css.ai.bl),
				$elm$svg$Svg$Attributes$cx(
				$elm$core$String$fromFloat(x)),
				$elm$svg$Svg$Attributes$cy(
				$elm$core$String$fromFloat(y)),
				$elm$svg$Svg$Attributes$r('0.5')
			]);
		return A2(
			$elm$svg$Svg$circle,
			_Utils_ap(pointAttributes, attributes),
			_List_Nil);
	});
var $author$project$Main$viewSelectedPoints = function (path) {
	var viewSelectedPoint = $author$project$Main$viewPoint(
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$fill('black'),
				$elm$svg$Svg$Attributes$stroke('none'),
				$elm$svg$Svg$Attributes$r('1')
			]));
	var makeSelection = F2(
		function (index, element) {
			return {E: element, F: index};
		});
	var possibleSelections = function (index) {
		return A2(
			$elm$core$List$map,
			makeSelection(index),
			_List_fromArray(
				[0, 1, 2, 3]));
	};
	var isSelected = function (index) {
		return A2(
			$elm$core$List$any,
			function (selection) {
				return A2($elm$core$List$member, selection, path.w);
			},
			possibleSelections(index));
	};
	var indexedComponents = A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, path.A);
	var selectedComponents = A2(
		$elm$core$List$filterMap,
		function (_v0) {
			var index = _v0.a;
			var component = _v0.b;
			return isSelected(index) ? $elm$core$Maybe$Just(component) : $elm$core$Maybe$Nothing;
		},
		indexedComponents);
	var selectedPoints = A2($elm$core$List$concatMap, $author$project$Path$componentPoints, selectedComponents);
	return A2($elm$core$List$map, viewSelectedPoint, selectedPoints);
};
var $author$project$Main$viewDraggingPreview = F3(
	function (model, dragStart, temporarySelection) {
		var previewPath = A2(
			$author$project$Path$update,
			A2($author$project$Path$addSelection, model.c, temporarySelection),
			A2($author$project$Point$subtract, model.b, dragStart));
		var viewPreviewPath = A2(
			$author$project$Main$viewPath,
			_List_Nil,
			$author$project$Path$toString(previewPath));
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$fill('none'),
					$elm$svg$Svg$Attributes$stroke('black'),
					$elm$svg$Svg$Attributes$strokeWidth('0.5'),
					$elm$svg$Svg$Attributes$strokeDasharray('0.5 0.5'),
					$elm$svg$Svg$Attributes$opacity('0.5'),
					$elm$svg$Svg$Attributes$cursor('grab')
				]),
			A2(
				$elm$core$List$cons,
				viewPreviewPath,
				$author$project$Main$viewSelectedPoints(previewPath)));
	});
var $elm$core$String$append = _String_append;
var $author$project$Path$arcSegmentMinimumRadius = F2(
	function (from, to) {
		var differenceVector = A2($author$project$Point$subtract, to, from);
		return $elm$core$Basics$sqrt(
			A2($elm$core$Basics$pow, differenceVector.j, 2) + A2($elm$core$Basics$pow, differenceVector.h, 2)) / 2;
	});
var $author$project$Point$vectorMagnitude = function (_v0) {
	var x = _v0.j;
	var y = _v0.h;
	return $elm$core$Basics$sqrt((x * x) + (y * y));
};
var $author$project$Path$arcSegmentRadiusXEndPoint = F2(
	function (params, isAdjusted) {
		var radii = isAdjusted ? $author$project$Path$arcSegmentAdjustedRadii(params) : params.p;
		var centerPoint = $author$project$Path$arcSegmentCenterPoint(params);
		var angleRadians = $elm$core$Basics$degrees(params.r);
		var radiusXDelta = {
			j: $elm$core$Basics$cos(angleRadians) * radii.j,
			h: $elm$core$Basics$sin(angleRadians) * radii.j
		};
		var radiusXEndPoint = A2($author$project$Point$add, centerPoint, radiusXDelta);
		return radiusXEndPoint;
	});
var $author$project$Path$arcSegmentRadiusYEndPoint = F2(
	function (params, isAdjusted) {
		var radii = isAdjusted ? $author$project$Path$arcSegmentAdjustedRadii(params) : params.p;
		var centerPoint = $author$project$Path$arcSegmentCenterPoint(params);
		var angleRadians = $elm$core$Basics$degrees(params.r);
		var radiusYDelta = {
			j: $elm$core$Basics$cos(
				angleRadians + $elm$core$Basics$degrees(90)) * radii.h,
			h: $elm$core$Basics$sin(
				angleRadians + $elm$core$Basics$degrees(90)) * radii.h
		};
		var radiusYEndPoint = A2($author$project$Point$subtract, centerPoint, radiusYDelta);
		return radiusYEndPoint;
	});
var $author$project$Main$viewArcRadiusLine = F3(
	function (attributes, centerPoint, endPoint) {
		return A2(
			$elm$svg$Svg$line,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x1(
						$elm$core$String$fromFloat(centerPoint.j)),
						$elm$svg$Svg$Attributes$y1(
						$elm$core$String$fromFloat(centerPoint.h)),
						$elm$svg$Svg$Attributes$x2(
						$elm$core$String$fromFloat(endPoint.j)),
						$elm$svg$Svg$Attributes$y2(
						$elm$core$String$fromFloat(endPoint.h))
					]),
				attributes),
			_List_Nil);
	});
var $author$project$Main$viewArcControls = F2(
	function (attributes, params) {
		var radiusYEndPoint = A2($author$project$Path$arcSegmentRadiusYEndPoint, params, false);
		var radiusXEndPoint = A2($author$project$Path$arcSegmentRadiusXEndPoint, params, false);
		var displayAttributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$stroke('black'),
				$elm$svg$Svg$Attributes$strokeWidth('0.25')
			]);
		var centerPoint = $author$project$Path$arcSegmentCenterPoint(params);
		var adjustedRadiusYEndPoint = A2($author$project$Path$arcSegmentRadiusYEndPoint, params, true);
		var adjustedRadiusXEndPoint = A2($author$project$Path$arcSegmentRadiusXEndPoint, params, true);
		var adjustedDisplayAttributes = A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$strokeDasharray('0.5 0.5'),
			displayAttributes);
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2($author$project$Main$viewPoint, attributes, centerPoint),
					A3(
					$author$project$Main$viewArcRadiusLine,
					_Utils_ap(displayAttributes, attributes),
					centerPoint,
					radiusXEndPoint),
					A3(
					$author$project$Main$viewArcRadiusLine,
					_Utils_ap(displayAttributes, attributes),
					centerPoint,
					radiusYEndPoint),
					A3(
					$author$project$Main$viewArcRadiusLine,
					_Utils_ap(adjustedDisplayAttributes, attributes),
					centerPoint,
					adjustedRadiusXEndPoint),
					A3(
					$author$project$Main$viewArcRadiusLine,
					_Utils_ap(adjustedDisplayAttributes, attributes),
					centerPoint,
					adjustedRadiusYEndPoint)
				]));
	});
var $author$project$Main$DrawingArcRotationSelected = function (a) {
	return {$: 16, a: a};
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$svg$Svg$foreignObject = $elm$svg$Svg$trustedNode('foreignObject');
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Main$viewArcRotationSelection = F2(
	function (model, _v0) {
		var to = _v0.a;
		var radii = _v0.p;
		var angle = _v0.r;
		var size = _v0.v;
		var from = $author$project$Path$toEndState(model.c).s;
		var counterClockwiseButton = A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Main$DrawingArcRotationSelected(1)),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('CCW')
				]));
		var clockwiseButton = A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Main$DrawingArcRotationSelected(0)),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('CW')
				]));
		var centerPoint = $author$project$Path$arcSegmentCenterPoint(
			{r: angle, ap: from, p: radii, H: $author$project$Path$defaultArcRotation, v: size, a: to});
		return A2(
			$elm$svg$Svg$foreignObject,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromFloat(centerPoint.j - 55)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromFloat(centerPoint.h + 5)),
					$elm$svg$Svg$Attributes$width('110'),
					$elm$svg$Svg$Attributes$height('25')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'flex'),
							A2($elm$html$Html$Attributes$style, 'gap', '10px')
						]),
					_List_fromArray(
						[clockwiseButton, counterClockwiseButton]))
				]));
	});
var $author$project$Main$DrawingArcSizeSelected = function (a) {
	return {$: 15, a: a};
};
var $author$project$Main$viewArcSizeSelection = F2(
	function (model, _v0) {
		var to = _v0.a;
		var radii = _v0.p;
		var angle = _v0.r;
		var smallButton = A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Main$DrawingArcSizeSelected(1)),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Small')
				]));
		var largeButton = A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Main$DrawingArcSizeSelected(0)),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Large')
				]));
		var from = $author$project$Path$toEndState(model.c).s;
		var centerPoint = $author$project$Path$arcSegmentCenterPoint(
			{r: angle, ap: from, p: radii, H: $author$project$Path$defaultArcRotation, v: $author$project$Path$defaultArcSize, a: to});
		return A2(
			$elm$svg$Svg$foreignObject,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromFloat(centerPoint.j - 55)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromFloat(centerPoint.h + 5)),
					$elm$svg$Svg$Attributes$width('110'),
					$elm$svg$Svg$Attributes$height('25')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'flex'),
							A2($elm$html$Html$Attributes$style, 'gap', '10px')
						]),
					_List_fromArray(
						[largeButton, smallButton]))
				]));
	});
var $author$project$Main$viewDrawingPreview = F2(
	function (model, drawingState) {
		var previewPoint = function (point) {
			return A2(
				$author$project$Main$viewPoint,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('black'),
						$elm$svg$Svg$Attributes$stroke('none'),
						$elm$svg$Svg$Attributes$r('1')
					]),
				point);
		};
		var previewAttributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$fill('none'),
				$elm$svg$Svg$Attributes$stroke('black'),
				$elm$svg$Svg$Attributes$strokeWidth('0.5'),
				$elm$svg$Svg$Attributes$opacity('0.5')
			]);
		var endState = $author$project$Path$toEndState(model.c);
		var pathEndPointString = $author$project$Path$commandToString(
			$author$project$Path$preFormattedMove(
				{a: endState.s}));
		switch (drawingState.$) {
			case 0:
				var previewMoveString = $author$project$Path$commandToString(
					$author$project$Path$preFormattedLine(
						{a: model.b}));
				return A2(
					$elm$svg$Svg$g,
					previewAttributes,
					_List_fromArray(
						[
							A2(
							$author$project$Main$viewPath,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$strokeDasharray('2 2')
								]),
							_Utils_ap(pathEndPointString, previewMoveString)),
							previewPoint(model.b)
						]));
			case 1:
				var previewLineString = $author$project$Path$commandToString(
					$author$project$Path$preFormattedLine(
						{a: model.b}));
				return A2(
					$elm$svg$Svg$g,
					previewAttributes,
					_List_fromArray(
						[
							A2(
							$author$project$Main$viewPath,
							_List_Nil,
							_Utils_ap(pathEndPointString, previewLineString)),
							previewPoint(model.b)
						]));
			case 2:
				var previewHorizontalLineString = $author$project$Path$commandToString(
					$author$project$Path$preFormattedHorizontalLine(
						{bA: model.b.j}));
				return A2(
					$elm$svg$Svg$g,
					previewAttributes,
					_List_fromArray(
						[
							A2(
							$author$project$Main$viewPath,
							_List_Nil,
							_Utils_ap(pathEndPointString, previewHorizontalLineString)),
							previewPoint(
							{j: model.b.j, h: endState.s.h})
						]));
			case 3:
				var previewVerticalLineString = $author$project$Path$commandToString(
					$author$project$Path$preFormattedVerticalLine(
						{bB: model.b.h}));
				return A2(
					$elm$svg$Svg$g,
					previewAttributes,
					_List_fromArray(
						[
							A2(
							$author$project$Main$viewPath,
							_List_Nil,
							_Utils_ap(pathEndPointString, previewVerticalLineString)),
							previewPoint(
							{j: endState.s.j, h: model.b.h})
						]));
			case 4:
				var drawingCubicCurveState = drawingState.a;
				switch (drawingCubicCurveState.$) {
					case 0:
						var previewCubicCurveString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedCubicCurve(
								{_: model.b, U: model.b, a: model.b}));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							_List_fromArray(
								[
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewCubicCurveString)),
									previewPoint(model.b)
								]));
					case 1:
						var to = drawingCubicCurveState.a.a;
						var previewStartControlString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedLine(
								{a: model.b}));
						var previewCubicCurveString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedCubicCurve(
								{_: to, U: model.b, a: to}));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							_List_fromArray(
								[
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewCubicCurveString)),
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewStartControlString)),
									previewPoint(to),
									previewPoint(model.b)
								]));
					default:
						var to = drawingCubicCurveState.a.a;
						var startControl = drawingCubicCurveState.a.U;
						var previewStartControlString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedLine(
								{a: startControl}));
						var previewEndPointString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedMove(
								{a: to}));
						var previewEndControlString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedLine(
								{a: model.b}));
						var previewCubicCurveString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedCubicCurve(
								{_: model.b, U: startControl, a: to}));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							_List_fromArray(
								[
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewCubicCurveString)),
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewStartControlString)),
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(previewEndPointString, previewEndControlString)),
									previewPoint(to),
									previewPoint(startControl),
									previewPoint(model.b)
								]));
				}
			case 5:
				var drawingSmoothCubicCurveState = drawingState.a;
				var smoothStartControl = A2($author$project$Point$reflectOver, endState.s, endState.bQ);
				if (!drawingSmoothCubicCurveState.$) {
					var previewStartControlString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedLine(
							{a: smoothStartControl}));
					var previewCubicCurveString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedCubicCurve(
							{_: model.b, U: smoothStartControl, a: model.b}));
					return A2(
						$elm$svg$Svg$g,
						previewAttributes,
						_List_fromArray(
							[
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(pathEndPointString, previewCubicCurveString)),
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(pathEndPointString, previewStartControlString)),
								previewPoint(model.b),
								previewPoint(smoothStartControl)
							]));
				} else {
					var to = drawingSmoothCubicCurveState.a.a;
					var previewStartControlString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedLine(
							{a: smoothStartControl}));
					var previewEndPointString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedMove(
							{a: to}));
					var previewEndControlString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedLine(
							{a: model.b}));
					var previewCubicCurveString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedCubicCurve(
							{_: model.b, U: smoothStartControl, a: to}));
					return A2(
						$elm$svg$Svg$g,
						previewAttributes,
						_List_fromArray(
							[
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(pathEndPointString, previewCubicCurveString)),
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(pathEndPointString, previewStartControlString)),
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(previewEndPointString, previewEndControlString)),
								previewPoint(to),
								previewPoint(smoothStartControl),
								previewPoint(model.b)
							]));
				}
			case 6:
				var drawingQuadraticCurve = drawingState.a;
				if (!drawingQuadraticCurve.$) {
					var previewQuadraticCurveString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedQuadraticCurve(
							{aw: model.b, a: model.b}));
					return A2(
						$elm$svg$Svg$g,
						previewAttributes,
						_List_fromArray(
							[
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(pathEndPointString, previewQuadraticCurveString)),
								previewPoint(model.b)
							]));
				} else {
					var to = drawingQuadraticCurve.a.a;
					var previewQuadraticCurveString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedQuadraticCurve(
							{aw: model.b, a: to}));
					var previewEndPointString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedMove(
							{a: to}));
					var previewControlString = $author$project$Path$commandToString(
						$author$project$Path$preFormattedLine(
							{a: model.b}));
					return A2(
						$elm$svg$Svg$g,
						previewAttributes,
						_List_fromArray(
							[
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(pathEndPointString, previewQuadraticCurveString)),
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(pathEndPointString, previewControlString)),
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								_Utils_ap(previewEndPointString, previewControlString)),
								previewPoint(to),
								previewPoint(model.b)
							]));
				}
			case 7:
				var smoothControl = A2($author$project$Point$reflectOver, endState.s, endState.bN);
				var previewQuadraticCurveString = $author$project$Path$commandToString(
					$author$project$Path$preFormattedQuadraticCurve(
						{aw: smoothControl, a: model.b}));
				var previewEndPointString = $author$project$Path$commandToString(
					$author$project$Path$preFormattedMove(
						{a: model.b}));
				var previewControlString = $author$project$Path$commandToString(
					$author$project$Path$preFormattedLine(
						{a: smoothControl}));
				return A2(
					$elm$svg$Svg$g,
					previewAttributes,
					_List_fromArray(
						[
							A2(
							$author$project$Main$viewPath,
							_List_Nil,
							_Utils_ap(pathEndPointString, previewQuadraticCurveString)),
							A2(
							$author$project$Main$viewPath,
							_List_Nil,
							_Utils_ap(pathEndPointString, previewControlString)),
							A2(
							$author$project$Main$viewPath,
							_List_Nil,
							_Utils_ap(previewEndPointString, previewControlString)),
							previewPoint(model.b),
							previewPoint(smoothControl)
						]));
			default:
				var drawingArcState = drawingState.a;
				switch (drawingArcState.$) {
					case 0:
						var radius = A2($author$project$Path$arcSegmentMinimumRadius, endState.s, model.b);
						var previewArcString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedArc(
								{
									r: 0,
									p: {j: radius, h: radius},
									H: $author$project$Path$defaultArcRotation,
									v: $author$project$Path$defaultArcSize,
									a: model.b
								}));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							_List_fromArray(
								[
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewArcString)),
									previewPoint(model.b)
								]));
					case 1:
						var to = drawingArcState.a.a;
						var minRadius = A2($author$project$Path$arcSegmentMinimumRadius, endState.s, to);
						var differenceX = to.j - endState.s.j;
						var midX = endState.s.j + (differenceX / 2);
						var rawRadiusX = model.b.j - midX;
						var radiusX = (rawRadiusX > 0) ? A2($elm$core$Basics$max, rawRadiusX, 1) : A2($elm$core$Basics$min, rawRadiusX, -1);
						var arcSegmentParams = {
							r: 0,
							ap: endState.s,
							p: {
								j: radiusX,
								h: A2($elm$core$Basics$min, radiusX, minRadius)
							},
							H: $author$project$Path$defaultArcRotation,
							v: $author$project$Path$defaultArcSize,
							a: to
						};
						var arcParams = {
							r: 0,
							p: {
								j: radiusX,
								h: A2($elm$core$Basics$min, radiusX, minRadius)
							},
							H: $author$project$Path$defaultArcRotation,
							v: $author$project$Path$defaultArcSize,
							a: to
						};
						var previewArcString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedArc(arcParams));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							_List_fromArray(
								[
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewArcString)),
									previewPoint(to),
									A2($author$project$Main$viewArcControls, _List_Nil, arcSegmentParams)
								]));
					case 2:
						var to = drawingArcState.a.a;
						var radiusX = drawingArcState.a.aU;
						var differenceY = to.h - endState.s.h;
						var midY = endState.s.h + (differenceY / 2);
						var rawRadiusY = midY - model.b.h;
						var radiusY = (rawRadiusY > 0) ? A2($elm$core$Basics$max, rawRadiusY, 1) : A2($elm$core$Basics$min, rawRadiusY, -1);
						var arcSegmentParams = {
							r: 0,
							ap: endState.s,
							p: {j: radiusX, h: radiusY},
							H: $author$project$Path$defaultArcRotation,
							v: $author$project$Path$defaultArcSize,
							a: to
						};
						var arcParams = {
							r: 0,
							p: {j: radiusX, h: radiusY},
							H: $author$project$Path$defaultArcRotation,
							v: $author$project$Path$defaultArcSize,
							a: to
						};
						var previewArcString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedArc(arcParams));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							_List_fromArray(
								[
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewArcString)),
									previewPoint(to),
									A2($author$project$Main$viewArcControls, _List_Nil, arcSegmentParams)
								]));
					case 3:
						var to = drawingArcState.a.a;
						var radii = drawingArcState.a.p;
						var previewAngleArcScale = 2;
						var arcSegmentParams = {r: 0, ap: endState.s, p: radii, H: $author$project$Path$defaultArcRotation, v: $author$project$Path$defaultArcSize, a: to};
						var centerPoint = $author$project$Path$arcSegmentCenterPoint(arcSegmentParams);
						var previewAngleDifference = A2($author$project$Point$subtract, model.b, centerPoint);
						var previewAngleMagnitude = $author$project$Point$vectorMagnitude(previewAngleDifference);
						var previewAngleEndPoint = A2(
							$author$project$Point$add,
							centerPoint,
							A2($author$project$Point$scale, previewAngleArcScale / previewAngleMagnitude, previewAngleDifference));
						var previewAngleArcParams = {
							r: 0,
							p: {j: previewAngleArcScale, h: previewAngleArcScale},
							H: 0,
							v: (_Utils_cmp(model.b.h, centerPoint.h) < 0) ? 0 : 1,
							a: previewAngleEndPoint
						};
						var previewAngleArcString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedArc(previewAngleArcParams));
						var previewAngleLine = A2(
							$elm$svg$Svg$line,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$strokeDasharray('2 2'),
									$elm$svg$Svg$Attributes$x1(
									$elm$core$String$fromFloat(centerPoint.j)),
									$elm$svg$Svg$Attributes$y1(
									$elm$core$String$fromFloat(centerPoint.h)),
									$elm$svg$Svg$Attributes$x2(
									$elm$core$String$fromFloat(model.b.j)),
									$elm$svg$Svg$Attributes$y2(
									$elm$core$String$fromFloat(model.b.h))
								]),
							_List_Nil);
						var previewAngleStartPoint = {j: centerPoint.j + previewAngleArcScale, h: centerPoint.h};
						var previewAngleStartPointString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedMove(
								{a: previewAngleStartPoint}));
						var previewAngleArc = (_Utils_cmp(previewAngleMagnitude, previewAngleArcScale) < 0) ? _List_Nil : _List_fromArray(
							[
								A2(
								$author$project$Main$viewPath,
								_List_Nil,
								A2($elm$core$String$append, previewAngleStartPointString, previewAngleArcString))
							]);
						var angleRadians = A2($elm$core$Basics$atan2, model.b.h - centerPoint.h, model.b.j - centerPoint.j);
						var angle = (angleRadians * 180) / $elm$core$Basics$pi;
						var arcParams = {r: angle, p: radii, H: $author$project$Path$defaultArcRotation, v: $author$project$Path$defaultArcSize, a: to};
						var previewArcString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedArc(arcParams));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							A2(
								$elm$core$List$append,
								previewAngleArc,
								_List_fromArray(
									[
										A2(
										$author$project$Main$viewPath,
										_List_Nil,
										_Utils_ap(pathEndPointString, previewArcString)),
										previewPoint(to),
										previewAngleLine
									])));
					case 4:
						var params = drawingArcState.a;
						var arcParams = {r: params.r, p: params.p, H: $author$project$Path$defaultArcRotation, v: $author$project$Path$defaultArcSize, a: params.a};
						var previewArcString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedArc(arcParams));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							_List_fromArray(
								[
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewArcString)),
									A2($author$project$Main$viewArcSizeSelection, model, params)
								]));
					default:
						var params = drawingArcState.a;
						var arcParams = {r: params.r, p: params.p, H: $author$project$Path$defaultArcRotation, v: params.v, a: params.a};
						var previewArcString = $author$project$Path$commandToString(
							$author$project$Path$preFormattedArc(arcParams));
						return A2(
							$elm$svg$Svg$g,
							previewAttributes,
							_List_fromArray(
								[
									A2(
									$author$project$Main$viewPath,
									_List_Nil,
									_Utils_ap(pathEndPointString, previewArcString)),
									A2($author$project$Main$viewArcRotationSelection, model, params)
								]));
				}
		}
	});
var $author$project$Path$Segment = 4;
var $author$project$Path$pointToStringNoSpace = function (point) {
	return A2(
		$elm$core$String$join,
		' ',
		_List_fromArray(
			[
				$elm$core$String$fromFloat(point.j),
				$elm$core$String$fromFloat(point.h)
			]));
};
var $author$project$Path$pointToStringOneSpace = function (point) {
	return A2(
		$elm$core$String$join,
		' ',
		_List_fromArray(
			[
				$elm$core$String$fromFloat(point.j),
				$elm$core$String$fromFloat(point.h),
				''
			]));
};
var $author$project$Path$segmentToString = function (segment) {
	var moveString = function (from) {
		return 'M' + $author$project$Path$pointToStringNoSpace(from);
	};
	switch (segment.$) {
		case 0:
			var from = segment.a.ap;
			var to = segment.a.a;
			return _Utils_ap(
				moveString(from),
				moveString(to));
		case 1:
			var from = segment.a.ap;
			var to = segment.a.a;
			return moveString(from) + ('L' + $author$project$Path$pointToStringNoSpace(to));
		case 2:
			var startControl = segment.a.U;
			var endControl = segment.a._;
			var from = segment.a.ap;
			var to = segment.a.a;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						moveString(from),
						'C',
						$author$project$Path$pointToStringOneSpace(startControl),
						$author$project$Path$pointToStringOneSpace(endControl),
						$author$project$Path$pointToStringNoSpace(to)
					]));
		case 3:
			var control = segment.a.aw;
			var from = segment.a.ap;
			var to = segment.a.a;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						moveString(from),
						'Q',
						$author$project$Path$pointToStringOneSpace(control),
						$author$project$Path$pointToStringNoSpace(to)
					]));
		case 4:
			var radii = segment.a.p;
			var angle = segment.a.r;
			var size = segment.a.v;
			var rotation = segment.a.H;
			var from = segment.a.ap;
			var to = segment.a.a;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						moveString(from),
						'A',
						$author$project$Path$pointToStringOneSpace(radii),
						$elm$core$String$fromFloat(angle) + ' ',
						$author$project$Path$arcSizeToString(size) + ' ',
						$author$project$Path$arcRotationToString(rotation) + ' ',
						$author$project$Path$pointToStringNoSpace(to)
					]));
		default:
			var from = segment.a.ap;
			var to = segment.a.a;
			return moveString(from) + ('L' + $author$project$Path$pointToStringNoSpace(to));
	}
};
var $author$project$Main$MouseDownElement = function (a) {
	return {$: 5, a: a};
};
var $author$project$Main$SetHoveredElement = function (a) {
	return {$: 3, a: a};
};
var $elm$svg$Svg$Events$onMouseOut = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseout',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$svg$Svg$Events$onMouseOver = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseover',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Main$selectionMouseEvents = function (selection) {
	return _List_fromArray(
		[
			$elm$svg$Svg$Events$onMouseOver(
			$author$project$Main$SetHoveredElement(
				$elm$core$Maybe$Just(selection))),
			$elm$svg$Svg$Events$onMouseOut(
			$author$project$Main$SetHoveredElement($elm$core$Maybe$Nothing)),
			$elm$svg$Svg$Events$onMouseDown(
			$author$project$Main$MouseDownElement(selection))
		]);
};
var $author$project$Main$selectionAttributes = F2(
	function (_v0, selection) {
		var hovered = _v0.y;
		var selected = _v0.w;
		var isSelected = A2($elm$core$List$member, selection, selected);
		var isHovered = function () {
			if (!hovered.$) {
				var hoveredSelection = hovered.a;
				return _Utils_eq(selection, hoveredSelection);
			} else {
				return false;
			}
		}();
		return isHovered ? A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$class($author$project$Main$css.ai.y),
			$author$project$Main$selectionMouseEvents(selection)) : (isSelected ? A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$class($author$project$Main$css.ai.w),
			$author$project$Main$selectionMouseEvents(selection)) : $author$project$Main$selectionMouseEvents(selection));
	});
var $author$project$Main$buildSegment = F2(
	function (_v0, builder) {
		var index = _v0.a;
		var component = _v0.b;
		var _v1 = component.af;
		switch (_v1.$) {
			case 0:
				var to = _v1.a.a;
				var selection = {E: 0, F: index};
				var endPoint = A2(
					$author$project$Main$viewPoint,
					A2($author$project$Main$selectionAttributes, builder, selection),
					to);
				return _Utils_update(
					builder,
					{
						J: A2($elm$core$List$cons, endPoint, builder.J)
					});
			case 1:
				var to = _v1.a.a;
				var segmentSelection = {E: 4, F: index};
				var lineSegment = A2(
					$author$project$Main$viewPath,
					A2($author$project$Main$selectionAttributes, builder, segmentSelection),
					$author$project$Path$segmentToString(component.af));
				var endSelection = {E: 0, F: index};
				var endPoint = A2(
					$author$project$Main$viewPoint,
					A2($author$project$Main$selectionAttributes, builder, endSelection),
					to);
				return _Utils_update(
					builder,
					{
						J: A2($elm$core$List$cons, endPoint, builder.J),
						K: A2($elm$core$List$cons, lineSegment, builder.K)
					});
			case 2:
				var startControl = _v1.a.U;
				var endControl = _v1.a._;
				var to = _v1.a.a;
				var startSelection = {E: 1, F: index};
				var startControlPoint = A2(
					$author$project$Main$viewPoint,
					A2($author$project$Main$selectionAttributes, builder, startSelection),
					startControl);
				var segmentSelection = {E: 4, F: index};
				var endSelection = {E: 2, F: index};
				var endPointSelection = {E: 0, F: index};
				var endPoint = A2(
					$author$project$Main$viewPoint,
					A2($author$project$Main$selectionAttributes, builder, endPointSelection),
					to);
				var endControlPoint = A2(
					$author$project$Main$viewPoint,
					A2($author$project$Main$selectionAttributes, builder, endSelection),
					endControl);
				var curveSegment = A2(
					$author$project$Main$viewPath,
					A2($author$project$Main$selectionAttributes, builder, segmentSelection),
					$author$project$Path$segmentToString(component.af));
				return _Utils_update(
					builder,
					{
						J: A2(
							$elm$core$List$append,
							_List_fromArray(
								[startControlPoint, endControlPoint, endPoint]),
							builder.J),
						K: A2($elm$core$List$cons, curveSegment, builder.K)
					});
			case 3:
				var control = _v1.a.aw;
				var to = _v1.a.a;
				var segmentSelection = {E: 4, F: index};
				var endSelection = {E: 0, F: index};
				var endPoint = A2(
					$author$project$Main$viewPoint,
					A2($author$project$Main$selectionAttributes, builder, endSelection),
					to);
				var curveSegment = A2(
					$author$project$Main$viewPath,
					A2($author$project$Main$selectionAttributes, builder, segmentSelection),
					$author$project$Path$segmentToString(component.af));
				var controlSelection = {E: 3, F: index};
				var controlPoint = A2(
					$author$project$Main$viewPoint,
					A2($author$project$Main$selectionAttributes, builder, controlSelection),
					control);
				return _Utils_update(
					builder,
					{
						J: A2(
							$elm$core$List$cons,
							controlPoint,
							A2($elm$core$List$cons, endPoint, builder.J)),
						K: A2($elm$core$List$cons, curveSegment, builder.K)
					});
			case 4:
				var params = _v1.a;
				var segmentSelection = {E: 4, F: index};
				var endSelection = {E: 0, F: index};
				var endPoint = A2(
					$author$project$Main$viewPoint,
					A2($author$project$Main$selectionAttributes, builder, endSelection),
					params.a);
				var arcSegment = A2(
					$author$project$Main$viewPath,
					A2($author$project$Main$selectionAttributes, builder, segmentSelection),
					$author$project$Path$segmentToString(component.af));
				var arcControls = A2($author$project$Main$viewArcControls, _List_Nil, params);
				return _Utils_update(
					builder,
					{
						J: A2(
							$elm$core$List$cons,
							endPoint,
							A2($elm$core$List$cons, arcControls, builder.J)),
						K: A2($elm$core$List$cons, arcSegment, builder.K)
					});
			default:
				var segmentSelection = {E: 4, F: index};
				var closeSegment = A2(
					$author$project$Main$viewPath,
					A2($author$project$Main$selectionAttributes, builder, segmentSelection),
					$author$project$Path$segmentToString(component.af));
				return _Utils_update(
					builder,
					{
						K: A2($elm$core$List$cons, closeSegment, builder.K)
					});
		}
	});
var $author$project$Main$initOverlayBuilder = function (_v0) {
	var hovered = _v0.y;
	var selected = _v0.w;
	return {y: hovered, J: _List_Nil, K: _List_Nil, w: selected};
};
var $author$project$Main$viewOverlay = function (path) {
	var initialBuilder = $author$project$Main$initOverlayBuilder(path);
	var indexedComponents = A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, path.A);
	return function (builder) {
		return _Utils_ap(builder.K, builder.J);
	}(
		A3($elm$core$List$foldl, $author$project$Main$buildSegment, initialBuilder, indexedComponents));
};
var $elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var $elm$svg$Svg$Attributes$strokeOpacity = _VirtualDom_attribute('stroke-opacity');
var $author$project$Main$viewSelectionBox = F2(
	function (model, selectionStart) {
		var minY = A2($elm$core$Basics$min, selectionStart.h, model.b.h);
		var minX = A2($elm$core$Basics$min, selectionStart.j, model.b.j);
		var width = A2($elm$core$Basics$max, selectionStart.j, model.b.j) - minX;
		var height = A2($elm$core$Basics$max, selectionStart.h, model.b.h) - minY;
		return A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$fill('black'),
					$elm$svg$Svg$Attributes$stroke('black'),
					$elm$svg$Svg$Attributes$strokeWidth('0.5'),
					$elm$svg$Svg$Attributes$strokeDasharray('0.5 0.5'),
					$elm$svg$Svg$Attributes$fillOpacity('0.05'),
					$elm$svg$Svg$Attributes$strokeOpacity('0.5'),
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromFloat(minX + model.m.ac)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromFloat(minY + model.m.ad)),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromFloat(width)),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromFloat(height))
				]),
			_List_Nil);
	});
var $author$project$Main$viewCanvas = function (model) {
	var baseOverlay = $author$project$Main$viewOverlay(model.c);
	var overlay = function () {
		var _v0 = model.d;
		switch (_v0.$) {
			case 0:
				return baseOverlay;
			case 1:
				return baseOverlay;
			case 2:
				var dragStart = _v0.a.aH;
				var temporarySelection = _v0.a.ag;
				return A2(
					$elm$core$List$cons,
					A3($author$project$Main$viewDraggingPreview, model, dragStart, temporarySelection),
					baseOverlay);
			case 3:
				var selectionStart = _v0.a;
				return A2(
					$elm$core$List$cons,
					A2($author$project$Main$viewSelectionBox, model, selectionStart),
					baseOverlay);
			case 4:
				var drawingState = _v0.a;
				return A2(
					$elm$core$List$cons,
					A2($author$project$Main$viewDrawingPreview, model, drawingState),
					baseOverlay);
			default:
				return baseOverlay;
		}
	}();
	return A2(
		$elm$svg$Svg$svg,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$viewBox(
				$author$project$ViewBox$toString(model.m)),
				$elm$svg$Svg$Attributes$width('100vw'),
				$elm$svg$Svg$Attributes$height('100vh'),
				$elm$svg$Svg$Attributes$display('block')
			]),
		_List_fromArray(
			[
				$author$project$Main$viewDefs,
				$author$project$Main$viewBackground(model),
				A2($elm$svg$Svg$g, _List_Nil, overlay)
			]));
};
var $author$project$Main$PathStringChanged = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$PathStringInputBlurred = {$: 14};
var $author$project$Main$PathStringInputFocused = {$: 13};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Events$onBlur = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'blur',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$viewPathStringInput = function (pathString) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'padding', '10px')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$value(pathString),
						$elm$html$Html$Events$onInput($author$project$Main$PathStringChanged),
						$elm$html$Html$Events$onFocus($author$project$Main$PathStringInputFocused),
						$elm$html$Html$Events$onBlur($author$project$Main$PathStringInputBlurred),
						A2($elm$html$Html$Attributes$style, 'width', '100%'),
						A2($elm$html$Html$Attributes$style, 'font-size', '64px')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(pathString)
					]))
			]));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $author$project$Path$selectionToString = function (_v0) {
	var index = _v0.F;
	var element = _v0.E;
	switch (element) {
		case 0:
			return 'EndPoint ' + $elm$core$String$fromInt(index);
		case 1:
			return 'StartControl ' + $elm$core$String$fromInt(index);
		case 2:
			return 'EndControl ' + $elm$core$String$fromInt(index);
		case 3:
			return 'Control ' + $elm$core$String$fromInt(index);
		default:
			return 'Segment ' + $elm$core$String$fromInt(index);
	}
};
var $author$project$Point$toString = function (point) {
	return $elm$core$String$fromFloat(point.j) + (',' + $elm$core$String$fromFloat(point.h));
};
var $author$project$Main$stateToString = function (state) {
	switch (state.$) {
		case 0:
			return 'Neutral: ';
		case 1:
			var position = state.a.aS;
			var temporarySelection = state.a.ag;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Clicking: ',
						$author$project$Point$toString(position),
						' -> ',
						$author$project$Path$selectionToString(temporarySelection),
						' | '
					]));
		case 2:
			var dragStart = state.a.aH;
			var temporarySelection = state.a.ag;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Dragging: ',
						$author$project$Point$toString(dragStart),
						' -> ',
						$author$project$Path$selectionToString(temporarySelection),
						' | '
					]));
		case 3:
			var selectionStart = state.a;
			return $elm$core$String$concat(
				_List_fromArray(
					[
						'Selecting: ',
						$author$project$Point$toString(selectionStart),
						' | '
					]));
		case 4:
			var drawingState = state.a;
			switch (drawingState.$) {
				case 0:
					return 'Drawing Move: ';
				case 1:
					return 'Drawing Line: ';
				case 2:
					return 'Drawing Horizontal Line: ';
				case 3:
					return 'Drawing Vertical Line: ';
				case 4:
					var drawingCubicCurveState = drawingState.a;
					switch (drawingCubicCurveState.$) {
						case 0:
							return 'Drawing Cubic Curve - EndPoint: ';
						case 1:
							return 'Drawing Cubic Curve - Start Control: ';
						default:
							return 'Drawing Cubic Curve - End Control: ';
					}
				case 5:
					var drawingOnePointCurveState = drawingState.a;
					if (!drawingOnePointCurveState.$) {
						return 'Drawing Smooth Cubic Curve - EndPoint: ';
					} else {
						return 'Drawing Smooth Cubic Curve - End Control: ';
					}
				case 6:
					var drawingOnePointCurveState = drawingState.a;
					if (!drawingOnePointCurveState.$) {
						return 'Drawing Quadratic Curve - EndPoint: ';
					} else {
						return 'Drawing Quadratic Curve - End Control: ';
					}
				case 7:
					return 'Drawing Smooth Quadratic Curve: ';
				default:
					return 'Drawing Arc: ';
			}
		default:
			return 'Typing: ';
	}
};
var $author$project$Main$viewState = F2(
	function (state, offset) {
		return A2(
			$elm$html$Html$p,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'padding-left', '10px')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					_Utils_ap(
						$author$project$Main$stateToString(state),
						$author$project$Point$toString(offset)))
				]));
	});
var $author$project$Main$Redo = {$: 12};
var $author$project$Main$Undo = {$: 11};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $author$project$Main$viewUndoRedo = function (_v0) {
	var undoStack = _v0.N;
	var redoStack = _v0.T;
	var undoCount = $elm$core$List$length(undoStack);
	var redoCount = $elm$core$List$length(redoStack);
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'padding-left', '10px')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$Undo),
						$elm$html$Html$Attributes$disabled(!undoCount)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						$elm$core$String$concat(
							_List_fromArray(
								[
									'Undo (',
									$elm$core$String$fromInt(undoCount),
									')'
								])))
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$Redo),
						$elm$html$Html$Attributes$disabled(!redoCount)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						$elm$core$String$concat(
							_List_fromArray(
								[
									'Redo (',
									$elm$core$String$fromInt(redoCount),
									')'
								])))
					]))
			]));
};
var $author$project$Main$viewViewBoxSize = F2(
	function (viewBox, zoomFactor) {
		return A2(
			$elm$html$Html$p,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'margin', '0'),
					A2($elm$html$Html$Attributes$style, 'padding-left', '10px')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Width: '),
					$elm$html$Html$text(
					$elm$core$String$fromInt(
						$elm$core$Basics$round(viewBox.aY))),
					$elm$html$Html$text(', Height: '),
					$elm$html$Html$text(
					$elm$core$String$fromInt(
						$elm$core$Basics$round(viewBox.aO))),
					$elm$html$Html$text(', Zoom: '),
					$elm$html$Html$text(
					$elm$core$String$fromFloat(zoomFactor))
				]));
	});
var $author$project$Main$viewUI = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'flex-direction', 'column'),
				A2($elm$html$Html$Attributes$style, 'width', '100%'),
				A2($elm$html$Html$Attributes$style, 'bottom', '0')
			]),
		_List_fromArray(
			[
				A2($author$project$Main$viewViewBoxSize, model.m, model.X),
				$author$project$Main$viewUndoRedo(model),
				A2($author$project$Main$viewState, model.d, model.b),
				$author$project$Main$viewPathStringInput(model.o)
			]));
};
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$Main$viewCanvas(model),
				$author$project$Main$viewUI(model)
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{bU: $author$project$Main$init, b6: $author$project$Main$subscriptions, b8: $author$project$Main$update, b9: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));