exports.interleaveArray = function(xs) {
  return function(ys) {
    if (xs.length === 0) {
      return ys;
    }
    if (ys.length === 0) {
      return xs;
    }
    var n = Math.min(xs.length, ys.length);
    var i = 0;
    var result = [];
    while (i < n) {
      result.splice(i*2, 0, xs[i], ys[i]);
      i++;
    }
    if (i === xs.length) {
      return result.concat(ys.slice(i));
    }
    if (i === ys.length) {
      return result.concat(xs.slice(i));
    }
  };
};
