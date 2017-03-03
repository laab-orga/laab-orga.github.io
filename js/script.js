function ready(fn) {
    if (document.readyState != 'loading') {
        fn();
    } else {
        document.addEventListener('DOMContentLoaded', fn);
    }
}
ready(toload);

function toload() {

    var loadme = document.getElementById('LoadMe');
    console.log("blah blah");
    var links = document.querySelectorAll('a.pageFetcher');
    Array.prototype.forEach.call(links, function (el, i) {
        el.onclick = function () {
            console.log(this.getAttribute('href'));
            var url = this.getAttribute('href');
            fetch(url)
                .then(function (response) {
                    console.log("blouh");
                    return response.text();
                })
                .then(function (body) {
                    loadme.innerHTML = body;
                });
            return false;
        };
    });
}

