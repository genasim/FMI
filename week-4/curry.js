function trippleAdd(a, b, c) {
  return a + b + c;
}

function curry(func) {
  return function accumulator(...args) {
    if (args.length >= func.length) {
      return func(...args);
    }
    return (...nextArgs) => accumulator(...args, ...nextArgs);
  };
}

const curriedAdd = curry(trippleAdd);

console.log(curriedAdd(1)(2)(3));
console.log(curriedAdd(1, 2)(3));
console.log(curriedAdd(1)(2, 3));
console.log(curriedAdd(1, 2, 3));

