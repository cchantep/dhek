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

    $._ajax({
        'url': $._myAccount['templatesUrl'],
        'type': "POST",
        'cache': false,
        'dataType': "json"
    }, function(d) {
        if (!d || (typeof d) != "object" || !d['length']) {
            alert("No template");
            return false
        }

        // ---
        var l = $("#my-templates .list-group");
        $.each(d, function(i,t) {
            $('<a href="#" class="list-group-item"><span class="name">' 
              + t.name + '</span> <span class="label label-default">'
              + t.id + '</span></a>').appendTo(l)

        })
    })
})(jQuery);
