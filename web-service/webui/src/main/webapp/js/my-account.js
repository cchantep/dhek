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
    itemAction = edit,
    templateList = function(d) {
        if (!d || (typeof d) != "object" || !d['length']) {
            alert("No template");
            return false
        }

        // ---

        var l = $("#my-templates .list-group").empty();
        $.each(d, function(i,t) {
            $('<a href="#'+t.id+'" class="list-group-item"><span class="name">' 
              + t.name + '</span> <span class="label label-default">'
              + t.id + '</span></a>').appendTo(l).click(function() {
                  itemAction($(this)); return false
              })

        });
    };

    $._ajax({
        url: $._myAccount['templatesUrl'],
        type: "POST",
        cache: false,
        dataType: "json",
        data: { "token": token },
    }, templateList);

    var create = $("#create-template"), rm = $("#remove-template");

    rm.click(function() {
        var activeClass = "list-group-item-danger active";

        if (rm.hasClass("active")) {
            create.removeAttr("disabled");
            rm.removeClass("active");
            $(".badge", rm).remove(); // count badge
            $("#my-templates .list-group-item > .fa").remove();

            $("#my-templates .list-group-item").
                off("mouseenter mouseleave"); // disable hover

            itemAction = edit;

            var selected = [];
            $("#my-templates .list-group-item-warning").
                removeClass("list-group-item-warning").each(function(i,e){
                    selected.push($(e).attr("href").substring(1))
                });

            $._ajax({
                url: $._myAccount['rmTemplatesUrl'],
                type: "POST",
                cache: false,
                dataType: "json",
                data: { "token": token, "template": selected }
            }, templateList)
        } else {
            create.attr("disabled", "disabled");
            rm.addClass("active").append('<span class="badge">0</span>').
                attr("disabled", "disabled").data("count", 0);

            $("#my-templates .list-group-item").each(function(i,e){
                $(e).prepend('<i class="fa fa-times-circle"> </i>').hover(
                    function(){ 
                        var t = $(this);
                        t.addClass(activeClass);
                        $(".label", t).addClass("label-danger")
                    },
                    function(){ 
                        var t = $(this);
                        t.removeClass(activeClass);
                        $(".label", t).removeClass("label-danger")
                    })
            });

            itemAction = function(e) { 
                var selected = e.hasClass("list-group-item-warning"),
                c = (!selected) ? parseInt(rm.data("count"))+1
                : parseInt(rm.data("count"))-1;

                $(".badge", rm.data("count", c)).text(c); // update count

                if (c == 0) rm.attr("disabled", "disabled");
                else rm.removeAttr("disabled");

                if (!selected) e.addClass("list-group-item-warning");
                else e.removeClass("list-group-item-warning");
            }
        }
        
        return false 
    });

    create.click(function() {
        location.href = "template.gz.html?t=" + token;
        return false
    })
})(jQuery);
