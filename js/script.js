function ready(fn, target, origin) {
    if (document.readyState != 'loading') {
        fn(target, origin);
    } else {
        document.addEventListener('DOMContentLoaded', function() {
            fn(target, origin);
        });
    }
}

ready(toload, 'LoadMe', 'a.pageFetcher');
ready(toload, 'content', 'a.noir')

function toload(target, origin) {

    var loadme = document.getElementById(target);
    var links = document.querySelectorAll(origin);
    Array.prototype.forEach.call(links, function(el, i) {
        el.onclick = function() {
            console.log(this.getAttribute('href'));
            var url = this.getAttribute('href');
            fetch(url)
                .then(function(response) {
                    return response.text();
                })
                .then(function(body) {
                    loadme.innerHTML = body;
                });
            return false;
        };
    });
}