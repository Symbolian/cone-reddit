// cone_reddit.js
// ---------------
// Copyright Symbolian 2016
// All rights reserved
// ---------------
// Author: Vincent Ahrend
// Email:  vincent.ahrend@symbolian.net

/*jslint browser:true, devel:true */
/*global $, PS*/

// global variables, accessible from PS module

var gl;
var globDivCanvas;
var coneAPI;
var tutorial_counter = 5;

$(document).ready(function () {
    "use strict";

    // blur the iframe
    $("#democontent").blur();
    // set focus on #glcanvas
    $("#glcanvas").focus();

    var continuation = function (api) {
        return function () {
            $('.spinner').hide();
            coneAPI = api;
        };
    };

    PS.Main.main(continuation)();
});

var globCanvasHide = function () {
    "use strict";
    if (!globDivCanvas.is(":hidden")) {
        globDivCanvas.slideUp();
    }
};
var globCanvasExpose = function () {
    "use strict";
    if (globDivCanvas.is(":hidden")) {
        globDivCanvas.show("slow");
    }
};

// Hide the tutorial after five movements outside the top layer
function cond_hide_tutorial() {
    "use strict";
    if (tutorial_counter === 0) {
        $(".tutorial").fadeOut();
    } else {
        tutorial_counter = tutorial_counter - 1;
    }
}

// Transform escaped html returned by reddit api into dom nodes
function weird_double_parser(content) {
    "use strict";
    var new_content = $.parseHTML(content);
    if (new_content && new_content.length > 0) {
        new_content = $.parseHTML(new_content[0].data);
    }
    return new_content;
}

// Show post in sidebar when selected in ConeCanvas
function show_post(score, title, data) {
    "use strict";
    // console.log("Selected post '" + title);

    cond_hide_tutorial();

    var contents = data.split(';-;'),
        author = contents[1],
        permalink = contents[2],
        url = contents[3],
        body = contents[4],
        domain,
        short_ending,
        long_ending,
        webmsource,
        mp4source;

    // Show appropriate containers
    $("#overview").slideUp(function () {
        $(".comment_content").fadeOut(function () {
            $(".post_content").fadeIn();
        });
    });
    $(".img_content").hide();
    $(".movie_content").hide();
    $(".imgur").hide();

    if (url !== "") {
        // Extract domain from URL
        domain = "";
        if (url.indexOf("://") > -1) {
            domain = url.split('/')[2];
        } else {
            domain = url.split('/')[0];
        }

        // Show appropriate containers
        $(".post_content .text_post").hide();
        $(".post_content .link_post").show();
        $(".post_content .content").hide();

        // Setup link for URL
        $(".post_content .source").attr('href', url);
        $(".post_content .domain").html(domain);

        // Conditional show image
        short_ending = url.substr(-3).toLowerCase();
        long_ending = url.substr(-4).toLowerCase();

        if ($.inArray(short_ending, ["jpg", "png", "gif"]) >= 0 || long_ending === "jpeg") {
            $(".img_content").show();
            $(".img_content").attr("src", url);
        } else if (long_ending === "gifv") {
            webmsource = url.slice(0, -4) + "webm";
            mp4source = url.slice(0, -4) + "mp4";
            $(".movie_content").html("<video class='img-responsive' autoplay='' loop='' muted='' preload=''><source src='" + webmsource + "' type='video/webm'><source src='" + mp4source + "' type='video/mp4'></video>");
            $(".movie_content").show();
        } else if (domain.indexOf("imgur") > -1) {
            show_imgur(url);
        }

    } else {
        // Text post appropriate containers
        $(".post_content .text_post").show();
        $(".post_content .link_post").hide();

        // Show text content if available
        if (body !== "") {
            $(".post_content .content").show();
            $(".post_content .content").html("");
            $(".post_content .content").append(
                weird_double_parser(body)
            );
        } else {
            $(".post_content .content").hide();
        }
    }

    // Show title and metadata for link
    $(".post_content .title").html(title);
    $(".post_content .score").html(score);
    $(".post_content .permalink").attr('href', permalink);
    $(".post_content .author").html(author);
}

function show_imgur(url) {
    "use strict";
    var matches = url.match(/(a\/)?(\w+)/g),
        id = matches[matches.length - 1];
    $(".imgur").html('<blockquote class="imgur-embed-pub" lang="en" data-id="' + id + '" data-context="false"><a href="//imgur.com/' + id + '">View post on imgur.com</a></blockquote><script async src="//s.imgur.com/min/embed.js" charset="utf-8"></script>').show();
}

// Show comment beneath post when selected in ConeCanvas
function show_comment(author, data) {
    "use strict";
    var contents = data.split(';-;'),
        comment_id = contents[1],
        body = contents[2],
        score = contents[3],
        comment_url;

    cond_hide_tutorial();

    $("#overview").slideUp(function () {
        $(".post_content .content").slideUp(function () {
            $(".comment_content").fadeIn();
        });
    });

    $(".comment_content .score").html(score);
    $(".comment_content .author").html(author);

    $(".comment_content .content").html("");
    $(".comment_content .content").append(
        weird_double_parser(body)
    );

    comment_url = $(".post_content .permalink").attr('href') + comment_id;
    $(".comment_content .source").attr("href", comment_url);

}

// Highlight subreddits selected in ConeCanvas
function show_subreddit(name) {
    "use strict";
    // console.log("Selected subreddit '" + name + "'");

    $(".post_content").fadeOut(function () {
        $(".comment_content").fadeOut(function () {
            $("#overview").slideDown();
        });
    });

    $(".subreddit_list li").removeClass("bg-primary");
    $(".subreddit_list strong:contains('" + name + "')")
        .parent().addClass("bg-primary");
}


/*

$(".subreddit_list li").click(function () {
    "use strict";
    var url = "http://reddit.com/r/" + $(this).children('strong')[0].innerHTML;
    coneAPI.navigateToCone(url);
});

*/

// Entry point which is called when selection in ConeCanvas changes
var globSelectedPrim = function (entry) {
    "use strict";
    var comment, comType, xhr, content, ajaxUrl;
    if (entry.comment.hasOwnProperty("value0")) {
        comment = entry.comment.value0;
        comType = comment[0];

        if (comType === "n") {
            show_subreddit(entry.label);
        } else {
            xhr = new XMLHttpRequest();
            xhr.onreadystatechange = function () {
                if (xhr.readyState === 4 && xhr.status === 200) {
                    content = xhr.responseText;
                    if (comType === "p") {
                        show_post(entry.targetUri.value0, entry.label, content);
                    } else if (comType === "c") {
                        show_comment(entry.label, content);
                    } else {
                        alert("Data error. Please reload page");
                    }
                }
            };

            ajaxUrl = "/content?id=".concat(comment.slice(1));
            // console.log("content request: GET " + ajaxUrl);
            xhr.open("GET", ajaxUrl, true);
            xhr.timeout = 2000;
            xhr.ontimeout =
                function () {
                    console.log("ERROR(cone-reddit): timeout getting content");
                };
            xhr.send();
        }
    }
};

var globSetURL = function (newUrl) {
    "use strict";
    // console.log("globSetURL: " + newUrl);
};
