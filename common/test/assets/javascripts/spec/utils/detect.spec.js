define([
    'bonzo',
    'common/utils/$',
    'common/utils/detect'
], function(
    bonzo,
    $,
    detect
) {

    describe('Breakpoint', function() {

        var $style;

        beforeEach(function () {
            $style = $.create('<style></style>')
                .appendTo('head');
        });

        afterEach(function () {
            $style.remove();
        });

        describe('getBreakpoint', function () {

            it('should default to "mobile" breakpoint', function(){
                expect(detect.getBreakpoint()).toBe('mobile');
            });

            it('should return the breakpoint', function(){
                var breakpoint = 'desktop';
                $style.html('body:after { content: "' + breakpoint + '"; }');
                expect(detect.getBreakpoint()).toBe(breakpoint);
            });

            it('should return the correct breakpoint if on tweakpoint', function(){
                $style.html('body:after { content: "faciaLeftCol"; }');
                expect(detect.getBreakpoint()).toBe('desktop');
            });

            it('should return the tweakpoint if required', function(){
                var breakpoint = 'faciaLeftCol';
                $style.html('body:after { content: "' + breakpoint + '"; }');
                expect(detect.getBreakpoint(true)).toBe(breakpoint);
            });

        });

        describe('hasCrossedBreakpoint', function () {

            var callback,
                hasBreakpointChanged;

            beforeEach(function () {
                callback = sinon.spy();
                $style.html('body:after { content: "desktop"; }');
                hasBreakpointChanged = detect.hasCrossedBreakpoint();
            });

            it('should return a function to test layout dimension changes', function(){
                var hasBreakpointChanged = detect.hasCrossedBreakpoint();
                expect(typeof hasBreakpointChanged).toBe('function');
            });

            it('should fire if crosses breakpoint', function(){
                $style.html('body:after { content: "mobile"; }');
                hasBreakpointChanged(callback);
                expect(callback).toHaveBeenCalledWith('mobile', 'desktop');
            });

            it('should not fire if no breakpoint crossed', function(){
                hasBreakpointChanged(callback);
                expect(callback).not.toHaveBeenCalled();
            });

            it('should not fire if similar tweakpoints crossed', function(){
                $style.html('body:after { content: "faciaLeftCol"; }');
                var hasBreakpointChanged = detect.hasCrossedBreakpoint();
                $style.html('body:after { content: "leftCol"; }');
                hasBreakpointChanged(callback);
                expect(callback).not.toHaveBeenCalled();
            });

            it('should fire if crosses a breakpoint', function(){
                $style.html('body:after { content: "mobile"; }');
                hasBreakpointChanged(callback);
                expect(callback).toHaveBeenCalledWith('mobile', 'desktop');
            });

            it('should fire if crosses a tweakpoint', function(){
                $style.html('body:after { content: "rightCol"; }');
                hasBreakpointChanged(callback);
                expect(callback).toHaveBeenCalledWith('tablet', 'desktop');
            });

            it('should return tweakpoint if requested', function(){
                var hasBreakpointChanged = detect.hasCrossedBreakpoint(true);
                $style.html('body:after { content: "rightCol"; }');
                hasBreakpointChanged(callback);
                expect(callback).toHaveBeenCalledWith('rightCol', 'desktop');
            });

        });

    });

    describe("Connection speed", function() {

        it("should default to 'high' speed", function(){
            window.performance = null;
            expect(detect.getConnectionSpeed()).toBe('high');
        });

        it("should calculate the speed of a slow, medium & fast client request", function(){

            expect(detect.getConnectionSpeed({ timing: { requestStart: 1, responseEnd: 8000 } })).toBe('low');

            expect(detect.getConnectionSpeed({ timing: { requestStart: 1, responseEnd: 3000 } })).toBe('medium');

            expect(detect.getConnectionSpeed({ timing: { requestStart: 1, responseEnd: 750 } })).toBe('high');

        });

        it("should return low if CELL connection can be determined", function() {

            expect(detect.getConnectionSpeed(null, { type: 3} )).toBe('low'); // type 3 is CELL_2G

            expect(detect.getConnectionSpeed(null, { type: 4} )).toBe('low'); // type 4 is CELL_3G

            expect(detect.getConnectionSpeed({ timing: { requestStart: 1, responseEnd: 750 } }, { type: 4} )).toBe('low');

            expect(detect.getConnectionSpeed({ timing: { requestStart: 1, responseEnd: 8000 } }, { type: 6} )).toBe('low');

            expect(detect.getConnectionSpeed({ timing: { requestStart: 1, responseEnd: 750 } }, { type: 6} )).toBe('high');

        });

        it("should return high or unknown if the speed can't be determined", function() {

            expect(detect.getConnectionSpeed(null, null)).toBe('high');

            expect(detect.getConnectionSpeed(null, null, true)).toBe('unknown');

        });
    });

    describe("Font support", function() {

        var ttfUserAgents = [
            'Mozilla/5.0 (Linux; U; Android 2.2; en-us; Nexus One Build/FRF91) ...'
        ];

        var woffUserAgents = [
            'Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) AppleWebKit/53',
            'Mozilla/5.0 (iPhone; CPU iPhone OS 5_0 like Mac OS X) AppleWebKit/ ...'
        ];

        var woff2UserAgents = [
            'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/38.0.2107.3 Safari/537.36',
            'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.67 Safari/537.36'
        ];

        it("should default to WOFF format", function(){
            var ua = "an unknown user agent string";
            expect(detect.getFontFormatSupport(ua)).toBe('woff');
        });

        it("should detect WOFF and TTF support based on the user agent string", function(){

            ttfUserAgents.forEach(function(ua){
                expect(detect.getFontFormatSupport(ua)).toBe('ttf');
            })

            woffUserAgents.forEach(function(ua){
                expect(detect.getFontFormatSupport(ua)).toBe('woff');
            })

            woff2UserAgents.forEach(function(ua){
                expect(detect.getFontFormatSupport(ua)).toBe('woff2');
            })

        });
    });

});
