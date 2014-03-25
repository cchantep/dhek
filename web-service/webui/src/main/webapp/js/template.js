(function($) {
    var ps = self.location.href.split('?'),
    idx = (ps.length == 2) ? ps[1].indexOf("t=") : -1,
    token = (idx == 0) ? ps[1].substring(2) : null;

    if (token == null) {
        // TODO: Better alert
        alert("Un-authenticated access: no token");
        return
    }

    // ---

    $("a[href='#back']").click(function() {
        location.href = "my-account.gz.html?t=" + token;
        return false
    })
})(jQuery);
