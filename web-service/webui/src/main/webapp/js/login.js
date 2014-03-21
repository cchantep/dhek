(function($) {
    $(".btn.login").click(function() {
        $._ajax($._login['validateUrl'], function(d) {
            if (!d || !d['token']) {
                $("#dhek-message").
                    attr("class", "alert alert-warning").
                    css("display", "block").
                    text("Authentication mismatch");
                return false
            }

            window.location = "my-account.gz.html?t=" + d.token;
        })

        return false
    })
})(jQuery);
