(function ($) {
    "use strict";

    $._lock = function() {
        if ($("#dhek-lock").length > 0) return false;
        
        $('<div aria-hidden="true" aria-labelledby="dhek-load-message" role="dialog" class="modal fade" id="dhek-lock"><div class="modal-dialog"><div class="modal-content alert alert-block alert-info"><div class="modal-body"><p class="lead">Loading ...</p><div class="progress progress-striped active"><div style="width: 90%" aria-valuemax="100" aria-valuemin="0" aria-valuenow="90" role="progressbar" class="progress-bar"><span class="sr-only">90%</span></div></div></div></div></div></div>').appendTo("body").modal({ keyboard: false }).on("hidden.bs.modal", function() { $("#dhek-lock").remove() });
    };

    $._unlock = function() { $("#dhek-lock").modal('hide') };

    $._displayUnexpectedError = function(cause) {
        $('<div id="dhek-unexpected-error" class="modal fade" role="dialog" aria-labelledby="dhek-unexpected-error" aria-hidden="false"><div class="modal-dialog"><div class="modal-content alert alert-block alert-danger"><div class="modal-header"><button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button><h4 class="modal-title">Unexpected error</h4></div><div class="modal-body"><p class="lead">An error has occured, please try again later.<br />Check error details with button thereafter.</p><button data-target="#error-details" data-toggle="collapse" class="btn" type="button">Details</button><button class="btn btn-danger pull-right" data-dismiss="modal">Close</button></div><div id="error-details" class="modal-footer collapse"><h5>Error details</h5>' + cause + '</div></div></div></div>').appendTo("body").modal().on("hidden.bs.modal", function() { $("#dhek-unexpected-error").remove() })
    };

    var _req = function(fn, target, callback) {
        $._lock();

        fn(target).done(function(d) { 
            if (!d || !d['exception']) return callback(d); // OK

            var url = ((typeof target == "object") && target['url']) 
            ? target.url : target;
            
            return $._displayUnexpectedError("<p>Unavailable : <em>" +
                                             url + "</em></p><pre>" + 
                                             d.exception + "</pre>")
        }).fail(function(r, s, e) {
            var url = ((typeof target == "object") && target['url']) 
            ? target.url : target;

            var msg = (r['responseJSON'] && r.responseJSON['exception'])
            ? r.responseJSON.exception : e;

            $._displayUnexpectedError("<p>Unavailable : <em>" + url + "</em></p><pre>" + s + " - " + msg + "</pre>")
        }).always($._unlock)
    };

    $._ajax = function(target, callback) {  
        return _req($.ajax, target, callback)
    };

    $._getJSON = function(target, callback) {
        return $._ajax({
            'url': target,
            'type': "GET",
            'cache': false,
            'dataType': "json"
        }, callback)
    };
})(jQuery);
