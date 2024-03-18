"use strict";

/* Will print all 10s since before the first timeout callback executes i is already at 10 **/
// var i;
// for (i = 0; i < 10; i++) {
//     setTimeout(function () {
//         console.log(i);
//     }, i * 1000)
// }

/* Using closure to preserve the current i as function state **/
var i;
for (i = 0; i < 10; i++) {
  (function (n) {
    setTimeout(function () {
      console.log(n);
    }, i * 1000);
  })(i);
}

/* Using ES6 let to scope i **/
for (let i = 0; i < 10; i++) {
  setTimeout(function () {
    console.log(i);
  }, i * 1000);
}
