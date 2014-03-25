(function($) {
    $(".btn.login").click(function() {
        $._ajax({
            url: $._login['validateUrl'],
            type: "POST",
            cache: false,
            dataType: "json",
            data: { 
                "email": $("#email").val(), "password": $("#password").val()
            }
        }, function(d) {
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
