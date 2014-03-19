(function($) {
    $(".btn.login").click(function() {
        $._ajax("/rest/token", function(d) {
            if (!d || !d['token']) {
                $("#dhek-message").
                    attr("class", "alert alert-warning").
                    css("display", "block").
                    text("Authentication mismatch");
                return false
            }

            window.location = "/upload.html";
        })

        return false
    })
})(jQuery);
