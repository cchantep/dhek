(function($) {
    var withCaptcha = function(f) {
        $._ajax({ 
            'url': $._captcha['temporalUrl'], 'type': "POST" 
        }, function(d) {
            if ((typeof d) != "object") return false;

            // ---

            return f('<div class="row dhek-passphrase"><div class="col-md-4 col-sm-12"><img src="'+$._captcha['imageUrl']+'/'+d.value+'.png" alt="'+d.code+'" /></div><div class="col-md-8 col-sm-12"><input type="text" class="form-control passphrase" name="passphrase" placeholder="Passphrase" /></div></div>');

        });
    }, checkCaptcha = function(c, t, f) {
        $.ajax({
            'url': $._captcha['checkUrl'], 'type': "POST",
            'data': { 'code': c, 'text': t }
        }).done(f)
    };
    
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

    withCaptcha(function(c) { 
        $(c).insertBefore("#signIn .btn");

        var i = $("#signIn .dhek-passphrase img"),
        x = i.attr("alt"), t = $("#signIn .passphrase"),
        e = t.parent();

        $("#signIn input").on("keyup change", function() {
            if ($.trim(t.val()) == "") {
                e.removeClass("has-error"); // empty: no error but blocks
                toggle(sb, rb, false) 
            } else {
                checkCaptcha(x, t.val(), function(d) {
                    if (!d) {
                        e.addClass("has-error");
                        return toggle(sb, rb, false)
                    }

                    // ---
                    
                    e.removeClass("has-error");
                    toggle(sb, rb, 
                           $.trim(se.val()) != "" && $.trim(sp.val()) != "")
                })
            }
        })
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
    });

    withCaptcha(function(c) { 
        $(c).insertBefore("#register .btn");

        var i = $("#register .dhek-passphrase img"),
        x = i.attr("alt"), t = $("#register .passphrase"),
        e = t.parent();

        $("#register input").on("keyup change", function() {
            if ($.trim(t.val()) == "") {
                e.removeClass("has-error"); // empty: no error but blocks
                toggle(rb, sb, false) 
            } else {
                checkCaptcha(x, t.val(), function(d) {
                    if (!d) {
                        e.addClass("has-error");
                        return toggle(rb, sb, false)
                    }

                    // ---
                    
                    e.removeClass("has-error");
                    toggle(rb, sb, 
                           $.trim(re.val()) != "" && $.trim(rp.val()) != "")
                })
            }
        })
    })
})(jQuery);
