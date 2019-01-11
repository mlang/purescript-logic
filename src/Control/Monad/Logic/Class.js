exports.interleaveArray = function(xs) {
  return function(ys) {
    if (xs.length === 0) {
      return ys;
    }
    if (ys.length === 0) {
      return xs;
    }
    var result = [xs[0], ys[0]];
    var i = 1;
    var n = Math.min(xs.length, ys.length);
    while (i < n) {
      result.splice(i*2, 0, xs[i], ys[i]);
      i++;
    }
    if (i === xs.length) {
      return result.concat(ys.slice(i));
    }
    return result.concat(xs.slice(i));
  };
};
