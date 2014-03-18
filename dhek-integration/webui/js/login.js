(function($) {
    // TODO: Change Rest URL
    $(".btn.login").click(function() {
        $._ajax("token", function(d) {
            if (!d || !d['token']) {
                $("#dhek-message").
                    attr("class", "alert alert-warning").
                    css("display", "block").
                    text("Authentication mismatch");
                return false
            }

            window.location = "step1";
        })

        return false
    })
})(jQuery);
