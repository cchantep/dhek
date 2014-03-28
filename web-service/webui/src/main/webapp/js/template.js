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

    $("a[href='#back']").click(function() {
        location.href = "my-account.gz.html?t=" + token;
        return false
    });

    $("#name, #pdf, #json").change(function() {
        var n = $.trim($("#name").val()),
        j = $("#json").val(), p = $("#pdf").val(),
        b = $("#save");

        if (n && n != "" && 
            p && p.substring(p.length-3) == "pdf" &&
            j && j.substring(j.length-4) == "json") { // valid
            b.removeAttr("disabled")
        } else b.attr("disabled", "disabled");

        return true
    });

    var f = $("#templateForm").attr("action", $._template['saveUrl']),
    upf = $('<iframe name="upload" id="upload" src="#upload"></iframe>').
        css({'visibility':"hidden"}).insertAfter(f.attr("target", "upload"));

    f.append('<input type="hidden" name="token" value="'+token+'" />');
    
    upf.load(function() {
        if ($("#json").val() == "") { return false /*fix IE early event*/ }

        var r = $(window.upload.document).text();
        
        $._unlock();

        $("#dhek-controls strong").remove();

        if (r && r.substring(0, 3) == "OK:") {
            token = r.substring(3);
            $('<strong class="text-success">Template is now saved</strong>').appendTo("#dhek-controls");
        } else $._displayUnexpectedError(r);
    });

    $("#save").click(function() { $._progress("Saving...", 25) });
})(jQuery);
