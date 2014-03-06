(function($) {
    // TODO: Change Rest URL
    $(".btn.login").click(function() {
        $._ajax("_json/login.json", function(d) {
            if (!d || !d['token']) {
                $("#dhek-message").
                    attr("class", "alert alert-warning").
                    css("display", "block").
                    text("Authentication mismatch");
                return false
            }
            
            // ---

            // TODO: Go to next screen
            $("#dhek-message").
                attr("class", "alert alert-success").
                css("display", "block").
                text("Token: " + d.token);
            
            return true
        })

        return false
    })
})(jQuery);
