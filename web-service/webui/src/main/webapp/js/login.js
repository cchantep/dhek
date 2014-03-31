(function($) {
    var sb = $("#signIn .btn"), rb = $("#register .btn"),
    toggle = function(a, b, v) {
        if (v) { // valid
            a.removeAttr("disabled").
                removeClass("btn-default").addClass("btn-primary");

            b.removeClass("btn-primary").addClass("btn-default")
        } else {
            a.attr("disabled", "disabled").
                removeClass("btn-primary").addClass("btn-default")

        }
    }, authed = function(d) {
        if (!d || !d['token']) {
            $("#dhek-message").
                attr("class", "alert alert-warning").
                css("display", "block").
                text("Authentication mismatch");
            return false
        }
        
        window.location = "my-account.gz.html?t=" + d.token;
    };

    // Sign in
    var se = $("#signIn .email"), sp = $("#signIn .password");
    
    $("#signIn input").on("keyup change", function() {
        toggle(sb, rb, $.trim(se.val()) != "" && $.trim(sp.val()) != "")
    });

    sb.click(function() {
        $._ajax({
            url: $._login['signInUrl'],
            type: "POST",
            cache: false,
            dataType: "json",
            data: { "email": se.val(), "password": sp.val() }
        }, authed);

        return false
    });

    // Register
    var re = $("#register .email"), rp = $("#register .password");
    
    $("#register input").on("keyup change", function() {
        toggle(rb, sb, $.trim(re.val()) != "" && $.trim(rp.val()) != "")
    });

    rb.click(function() {
        $._ajax({
            url: $._login['registerUrl'],
            type: "POST",
            cache: false,
            dataType: "json",
            data: { "email": re.val(), "password": rp.val() }
        }, authed);

        return false
    })
})(jQuery);
