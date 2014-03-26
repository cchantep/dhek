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

    var edit = function() {},
    itemAction = edit;

    $._ajax({
        'url': $._myAccount['templatesUrl'],
        'type': "POST",
        'cache': false,
        'data': 't=' + token,
        'dataType': "json"
    }, function(d) {
        if (!d || (typeof d) != "object" || !d['length']) {
            alert("No template");
            return false
        }

        // ---
        var l = $("#my-templates .list-group");
        $.each(d, function(i,t) {
            $('<a href="#'+t.id+'" class="list-group-item"><span class="name">' 
              + t.name + '</span> <span class="label label-default">'
              + t.id + '</span></a>').appendTo(l).click(function() {
                  itemAction()
              })

        })
    });

    var create = $("#create-template"), rm = $("#remove-template");

    rm.click(function() { 
        if (rm.hasClass("active")) {
            create.removeAttr("disabled");
            rm.removeClass("active");
            $("#my-templates .list-group-item > .fa").remove();

            itemAction = edit
        } else {
            create.attr("disabled", "disabled");
            rm.addClass("active");
            $("#my-templates .list-group-item").each(function(i,e){
                $(e).prepend('<i class="fa fa-times-circle"> </i>').hover(
                    function() { $(this).addClass("text-danger") },
                    function() { $(this).removeClass("text-danger") })
            })

            itemAction = function() { alert("x") }
        }
        
        return false 
    });

    create.click(function() {
        location.href = "template.gz.html?t=" + token;
        return false
    })
})(jQuery);
