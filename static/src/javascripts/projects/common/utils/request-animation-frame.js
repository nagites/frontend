define(function () {

    function requestAnimationFrame(callback) {
        var lastTime = 0,
            vendors = ['ms', 'moz', 'webkit', 'o'],
            x;

        for (x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
            window.requestAnimationFrame = window[vendors[x] + 'RequestAnimationFrame'];
        }

        if (!window.requestAnimationFrame) {
            window.requestAnimationFrame = function (callback) {
                var currTime = new Date().getTime(),
                    timeToCall = Math.max(0, 16 - (currTime - lastTime)),
                    id = window.setTimeout(function () { callback(currTime + timeToCall); }, timeToCall);
                lastTime = currTime + timeToCall;
                return id;
            };
        } else {
            window.requestAnimationFrame(callback);
        }
    }
    return requestAnimationFrame;

}); // define
