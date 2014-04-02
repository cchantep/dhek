(function ($) {
    "use strict";

    var mergeUrl = "http://dhek.applicius.fr/api/merge";

    var merge = function(arg) {
        if ((typeof arg['dhek_token']) != "string") alert("No application token");
        if ((typeof arg['dhek_template']) != "string") alert("No template ID");
        
        var data = function() {
            if ((typeof arg['data']) == "object") return arg.data;
            else {
                var d = $(arg['data']);
                if (d.length != 1) return alert("Neither a dictionary nor a form element: " + arg['data']);
                else if (d.get(0).tagName.toLowerCase() != "form") return alert("Element as data is not a form: " + arg['data']);
                
                var r = {};
                $("input[type!='checkbox' & type!='radio'], input[type='checkbox'|type='radio']:checked, select, textarea", d).
                    each(function(i,e){
                        var x = $(e), n = x.attr("name"), v = x.val();
                        if ((typeof n) != "string") { alert("Missing name for form input"); return false }
                        
                        r[n] = v
                    });
                
                return r
            }
        }, f = $('<form action="'+mergeUrl+'" method="POST"><input type="hidden" name="dhek_token" value="'+arg.dhek_token+'" /><input type="hidden" name="dhek_template" value="'+arg.dhek_template+'" /></form>');

        if ((typeof arg['form_target']) == "string") f.attr("target", arg.form_target);
        
        $.each(data(), function(k,v){
            $('<input type="hidden" name="'+k+'" value="'+v+'" />').appendTo(f);
        });
        
        f.appendTo("body").trigger('submit').remove()
    };    
    
    $.dhek = function(arg) {
        if (arg && arg['action'] == "merge") return merge(arg);
        return false
    }
})(jQuery);
