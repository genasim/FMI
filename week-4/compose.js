var addOne = (x) => x + 1;
var square = (x) => x * x;
var log = (x) => console.log(x);

// function compose(...funcs) {
//     return function(input) {
//         return funcs.reduceRight(function(prev, curr) {
//             return curr(prev);
//         }, input)
//     }
// }

// const composed1 = compose(square, addOne)
// log(composed1(10));
// const composed = compose(log, square, addOne);

const compose = (...funcs) => {
  return (input) =>
    funcs.reduce((accumulatedInput, currFunc) => {
      return currFunc(accumulatedInput);
    }, input);
};

const composed = compose(addOne, square, log);
composed(10);
